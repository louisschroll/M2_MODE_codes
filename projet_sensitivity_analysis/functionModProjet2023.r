library(tidyverse)


### Mod�le dont la sensibilit� doit �tre analys�e dans le cadre du projet MODE-MPI 2023-2024

### Le mod�le est ici d�fini sous forme de fonction pour faciliter vos analyses de sensibilit� (AS)
### La fonction renvoie les sorties ponctuelles qui sont � analyser dans l'AS

modAppli_effectif <- function(parametre){  
  # cette version de modAppli retourne MAT et les nouvelles infections plutot que
  # les 4 sorties
  # CONDITIONS DE SIMULATION
  temps = 2*365; # nb de pas de temps (en jours)
  
  # boucle des scenarios de l'echantillonnage de l'AS
  for (i in 1:nrow(parametre)) { 
    
    # STRUCTURE & PARAMETRES DU MODELE
    
    # Parametres decrivant la population
    K = parametre[i,1];		# capacite de charge
    sr = parametre[i,2];	# taux de survie des nourissons ?
    m1 = parametre[i,3];	# taux de mortalite dans la classe d'�ge 1 (enfant)
    m2 = parametre[i,4];	# taux de mortalite dans la classe d'�ge 2 (adulte)
    m3 = parametre[i,5];	# taux de mortalite dans la classe d'�ge 3 (senior)
    f2 = parametre[i,6];	# taux de fertilite pour la classe d'age 2 (adulte)
    f3 = parametre[i,7];	# taux de fertilite pour la classe d'age 3 (senior)
    portee = parametre[i,8];	# nombre de portees a chaque pas de temps
    t1 = parametre[i,9];	# proportion d'individus passant de la classe d'�ge 1 � la classe d'�ge 2 � chaque instant t
    t2 = parametre[i,10];	# proportion d'individus passant de la classe d'�ge 2 � la classe d'�ge 3 � chaque instant t
    
    # Parametres relatifs � la maladie
    trans = parametre[i,11]; # taux de transmission
    lat = parametre[i,12];	# taux de passage de l'etat latent a l'etat infecte
    rec = parametre[i,13];	# taux de recuperation
    loss = parametre[i,14];	# taux de perte d'immunite 
    madd = parametre[i,15];	# taux de mortalite du a la maladie
    
    # INITIALISATION
    MAT <- array(0, dim=c(4,4,temps)); # nb indiv par classe d'?ge en ligne (derni?re ligne = pop tot), ?tat de sant? en colonne, pas de temps (dimension 3)
    nouvellesInfections <- array(0, dim=c(temps));
    # conditions initiales (la population est � sa structure d'�quilibre, calcul�e par ailleurs)
    MAT[1,1,1] <- 27; # nombre d'enfants sains
    MAT[2,1,1] <- 23; # nombre d'adulte sains
    MAT[3,1,1] <- 36; # nombre de seniors sains
    MAT[3,3,1] <- 1;  # nombre de seniors infectes
    # effectifs par etat de sante
    MAT[4,1,1] <- sum(MAT[1:3,1,1]); MAT[4,2,1] <- sum(MAT[1:3,2,1]); MAT[4,3,1] <- sum(MAT[1:3,3,1]); MAT[4,4,1] <- sum(MAT[1:3,4,1]);
    
    # SIMULATIONS
    # boucle du temps
    for (t in 1:(temps-1)) { 
      # classe d'�ge 1 : Enfant (non mature pour la reproduction)
      # RQ : les naissances sont XX, les nouveaux n�s �tant dans l'�tat susceptible
      N <- sum(MAT[4,,t]);	# taille de la pop en t
      MAT[1,1,t+1] <- MAT[1,1,t]*(1-m1-t1-trans*MAT[4,3,t]/N) + loss*MAT[1,4,t]      + max(0, sr*portee*(sum(MAT[2,,t])*f2 + sum(MAT[3,,t])*f3) * (1 - N/K)); 
      MAT[1,2,t+1] <- MAT[1,2,t]*(1-m1-t1-lat)			  + trans*MAT[1,1,t]*MAT[4,3,t]/N; 
      MAT[1,3,t+1] <- MAT[1,3,t]*(1-m1-madd-t1-rec)  		  + lat*MAT[1,2,t]; 
      MAT[1,4,t+1] <- MAT[1,4,t]*(1-m1-t1-loss) 		  + rec*MAT[1,3,t]; 
      # classe d'�ge 2 : Adulte 
      MAT[2,1,t+1] <- MAT[1,1,t]*t1	+ MAT[2,1,t]*(1-m2-t2-trans*MAT[4,3,t]/N) + loss*MAT[2,4,t];
      MAT[2,2,t+1] <- MAT[1,2,t]*t1	+ MAT[2,2,t]*(1-m2-t2-lat)			+ trans*MAT[2,1,t]*MAT[4,3,t]/N;
      MAT[2,3,t+1] <- MAT[1,3,t]*t1	+ MAT[2,3,t]*(1-m2-madd-t2-rec)		+ lat*MAT[2,2,t];
      MAT[2,4,t+1] <- MAT[1,4,t]*t1	+ MAT[2,4,t]*(1-m2-t2-loss)			+ rec*MAT[2,3,t];
      # classe d'�ge 3 : Senior 
      MAT[3,1,t+1] <- MAT[2,1,t]*t2	+ MAT[3,1,t]*(1-m3-trans*MAT[4,3,t]/N) 	+ loss*MAT[3,4,t];
      MAT[3,2,t+1] <- MAT[2,2,t]*t2	+ MAT[3,2,t]*(1-m3-lat)				+ trans*MAT[3,1,t]*MAT[4,3,t]/N;
      MAT[3,3,t+1] <- MAT[2,3,t]*t2	+ MAT[3,3,t]*(1-m3-madd-rec)			+ lat*MAT[3,2,t];
      MAT[3,4,t+1] <- MAT[2,4,t]*t2	+ MAT[3,4,t]*(1-m3-loss)			+ rec*MAT[3,3,t];
      # calcul des effectifs par etat de sante
      MAT[4,1,t+1] <- sum(MAT[1:3,1,t+1]); MAT[4,2,t+1] <- sum(MAT[1:3,2,t+1]); MAT[4,3,t+1] <- sum(MAT[1:3,3,t+1]); MAT[4,4,t+1] <- sum(MAT[1:3,4,t+1]);
      nouvellesInfections[t+1]   <- trans*MAT[4,1,t]*MAT[4,3,t]/N
      
    }# fin boucle temps
    
    
  }# fin boucle scenarios AS
  
  return(MAT)
} # fin fonction du modele

# END

modAppli <- function(parametre){  
  # Version donn�e par les profs
  # CONDITIONS DE SIMULATION
  temps = 2*365; # nb de pas de temps (en jours)
  # initialisation pour la sauvegarde de 4 sorties ponctuelles pour chaque jeu de param�tres
  sorties <- matrix(0, nrow=nrow(parametre), ncol=4)
  
  # boucle des sc�narios de l'�chantillonnage de l'AS
  for (i in 1:nrow(parametre)) { 
    
    # STRUCTURE & PARAMETRES DU MODELE
    
    # Parametres decrivant la population
    K = parametre[i,1];		# capacite de charge
    sr = parametre[i,2];	# Sex ratio
    m1 = parametre[i,3];	# taux de mortalite dans la classe d'�ge 1 (enfant)
    m2 = parametre[i,4];	# taux de mortalite dans la classe d'�ge 2 (adulte)
    m3 = parametre[i,5];	# taux de mortalite dans la classe d'�ge 3 (senior)
    f2 = parametre[i,6];	# taux de fertilite pour la classe d'age 2 (adulte)
    f3 = parametre[i,7];	# taux de fertilite pour la classe d'age 3 (senior)
    portee = parametre[i,8];	# nombre de portees a chaque pas de temps
    t1 = parametre[i,9];	# proportion d'individus passant de la classe d'�ge 1 � la classe d'�ge 2 � chaque instant t
    t2 = parametre[i,10];	# proportion d'individus passant de la classe d'�ge 2 � la classe d'�ge 3 � chaque instant t
    
    # Parametre relatif � la maladie
    trans = parametre[i,11]; # taux de transmission
    lat = parametre[i,12];	# taux de passage de l'etat latent a l'etat infecte
    rec = parametre[i,13];	# taux de recuperation
    loss = parametre[i,14];	# taux de perte d'immunite 
    madd = parametre[i,15];	# taux de mortalite du a la maladie
    
    # INITIALISATION
    MAT <- array(0, dim=c(4,4,temps)); # nb indiv par classe d'�ge en ligne (derni�re ligne = pop tot), �tat de sant� en colonne, pas de temps (dimension 3)
    nvinf <- array(0, dim=c(temps));
    # conditions initiales (la population est � sa structure d'�quilibre, calcul�e par ailleurs)
    MAT[1,1,1] <- 27; # nombre d'enfants sains
    MAT[2,1,1] <- 23; # nombre d'adulte sains
    MAT[3,1,1] <- 36; # nombre de seniors sains
    MAT[3,3,1] <- 1;  # nombre de seniors infectes
    # effectifs par �tat de sant�
    MAT[4,1,1] <- sum(MAT[1:3,1,1]); MAT[4,2,1] <- sum(MAT[1:3,2,1]); MAT[4,3,1] <- sum(MAT[1:3,3,1]); MAT[4,4,1] <- sum(MAT[1:3,4,1]);
    
    # SIMULATIONS
    # boucle du temps
    for (t in 1:(temps-1)) { 
      # classe d'�ge 1 : Enfant (non mature pour la reproduction)
      # RQ : les naissances sont XX, les nouveaux n�s �tant dans l'�tat susceptible 
      N <- sum(MAT[4,,t]);	# taille de la pop en t
      MAT[1,1,t+1] <- MAT[1,1,t]*(1-m1-t1-trans*MAT[4,3,t]/N) + loss*MAT[1,4,t]      + max(0, sr*portee*(sum(MAT[2,,t])*f2 + sum(MAT[3,,t])*f3) * (1 - N/K)); 
      MAT[1,2,t+1] <- MAT[1,2,t]*(1-m1-t1-lat)			  + trans*MAT[1,1,t]*MAT[4,3,t]/N; 
      MAT[1,3,t+1] <- MAT[1,3,t]*(1-m1-madd-t1-rec)  		  + lat*MAT[1,2,t]; 
      MAT[1,4,t+1] <- MAT[1,4,t]*(1-m1-t1-loss) 		  + rec*MAT[1,3,t]; 
      # classe d'�ge 2 : Adulte 
      MAT[2,1,t+1] <- MAT[1,1,t]*t1	+ MAT[2,1,t]*(1-m2-t2-trans*MAT[4,3,t]/N) + loss*MAT[2,4,t];
      MAT[2,2,t+1] <- MAT[1,2,t]*t1	+ MAT[2,2,t]*(1-m2-t2-lat)			+ trans*MAT[2,1,t]*MAT[4,3,t]/N;
      MAT[2,3,t+1] <- MAT[1,3,t]*t1	+ MAT[2,3,t]*(1-m2-madd-t2-rec)		+ lat*MAT[2,2,t];
      MAT[2,4,t+1] <- MAT[1,4,t]*t1	+ MAT[2,4,t]*(1-m2-t2-loss)			+ rec*MAT[2,3,t];
      # classe d'�ge 3 : Senior 
      MAT[3,1,t+1] <- MAT[2,1,t]*t2	+ MAT[3,1,t]*(1-m3-trans*MAT[4,3,t]/N) 	+ loss*MAT[3,4,t];
      MAT[3,2,t+1] <- MAT[2,2,t]*t2	+ MAT[3,2,t]*(1-m3-lat)				+ trans*MAT[3,1,t]*MAT[4,3,t]/N;
      MAT[3,3,t+1] <- MAT[2,3,t]*t2	+ MAT[3,3,t]*(1-m3-madd-rec)			+ lat*MAT[3,2,t];
      MAT[3,4,t+1] <- MAT[2,4,t]*t2	+ MAT[3,4,t]*(1-m3-loss)			+ rec*MAT[3,3,t];
      # calcul des effectifs par �tat de sant�
      MAT[4,1,t+1] <- sum(MAT[1:3,1,t+1]); MAT[4,2,t+1] <- sum(MAT[1:3,2,t+1]); MAT[4,3,t+1] <- sum(MAT[1:3,3,t+1]); MAT[4,4,t+1] <- sum(MAT[1:3,4,t+1]);
      nvinf[t+1]   <- trans*MAT[4,1,t]*MAT[4,3,t]/N
      
    }# fin boucle temps
    
    # sorties ponctuelles � analyser
    # Pr�valence = nb d'individus latents + nb d'infectieux / nombre total d'individu � la fin de la p�riode d'�tude
    sortie1 <- (MAT[4,2,temps]+MAT[4,3,temps])/sum(MAT[4,,temps])
    # Incidence = nombre de nouvelles infections le dernier jour de la p�riode d'�tude
    sortie2 <- nvinf[temps]
    # Pic de l'�pid�mie : nombre d'individus infect�s maximal atteint durant la p�riode d'�tude
    sortie3 <- max(MAT[4,3,1:temps])
    # Nombre d'infection totale r�alis� la premi�re ann�e
    sortie4 <- sum(nvinf[1:365])
    
    sorties[i,1] <- sortie1;
    sorties[i,2] <- sortie2;
    sorties[i,3] <- sortie3;
    sorties[i,4] <- sortie4;
    
  }# fin boucle sc�narios AS
  return(sorties)
} # fin fonction du mod�le

# END
