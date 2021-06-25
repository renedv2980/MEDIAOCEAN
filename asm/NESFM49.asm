*          DATA SET NESFM49    AT LEVEL 115 AS OF 03/22/06                      
*PHASE T31C49A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*                                                                               
*  TITLE: T31C49 - DUNNING LETTERS                                              
*                                                                               
***********************************************************************         
         TITLE 'T31C49 DUNNING LETTERS'                                         
T31C49   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DUNNING,R7,RR=RE                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RE,RELO                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   ABINSRCH,CBINSRCH   A(BINSRCH)                                   
         DROP  RF                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
EXIT     XIT1                                                                   
MAXPRD   EQU   252                                                              
MAXPRD1  EQU   220                MAX # OF PRODUCTS IN CLIST                    
MAXPRD2  EQU   35                 MAX # OF PRODUCTS IN CLIST2                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         BRAS  RE,GUSERINF        GET USER-ID INFORMATION                       
*                                                                               
         XC    CLTBLK(CLTBLKLN),CLTBLK                                          
         XC    PRDBLK(PRDBLKLN),PRDBLK                                          
         XC    ESTBLK(ESTBLKLN),ESTBLK                                          
*                                                                               
         XC    NETA,NETA                                                        
         XC    STARTC,STARTC                                                    
         XC    ENDC,ENDC                                                        
         XC    PERIOD,PERIOD                                                    
         XC    GNBA,GNBA           GROSS/NET/BOTH                               
         XC    RLA,RLA             REPORT/LETTERS                               
         MVI   SVOFFILT,0                                                       
*                                                                               
         MVI   FILTFLAG,0                                                       
         MVI   PRNTFLAG,0                                                       
*                                                                               
         LA    R2,DUNMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALIMED             AGY/MED IN BAGYMD                            
*                                                                               
         LA    R2,DUNCLTH          CLIENT                                       
         MVC   CLTA,=C'ALL'        DEFAULT TO ALL                               
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   *+12                                                             
         OI    FILTFLAG,ALLCLT                                                  
         B     VK20                                                             
*                                                                               
         CLI   8(R2),C'*'          OFFICE FILTER?                               
         BNE   VK15                                                             
*                                                                               
         OI    FILTFLAG,ALLCLT                                                  
         CLC   9(3,R2),=C'ALL'     ALL OFFICES                                  
         BE    VK20                                                             
         MVC   SVOFFILT,9(R2)      OFFICE FILTER                                
         B     VK20                                                             
*                                                                               
VK15     DS    0H                                                               
         MVC   AIO,AIO3            READ CLIENT RECORD INTO AIO3                 
         GOTO1 VALICLT             2 BYTE CLT IN BCLT                           
         MVC   CLT,BCLT                                                         
         GOTO1 CLUNPK,DMCB,CLT,CLTA                                             
*                                                                               
VK20     DS    0H                                                               
         LA    R2,DUNPRDH          PRODUCT                                      
*                                                                               
         MVC   PRDA,=C'ALL'                                                     
         MVC   PRDNAME(3),=C'ALL'                                               
         OC    PRDNAME,SPACES                                                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK22                                                             
         CLC   8(3,R2),=C'POL'                                                  
         BE    VK22                                                             
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK25                                                             
VK22     OI    FILTFLAG,ALLPRD                                                  
         B     VK50                                                             
*                                                                               
VK25     DS    0H                                                               
         CLC   8(4,R2),=C'PGR='                                                 
         BNE   VK27                                                             
         CLC   CLTA,=C'ALL'        DO ALL CLIENTS?                              
         BE    INVLPGR1                                                         
         BAS   RE,VALPGRP          YES - VALIDATE                               
         B     VK50                                                             
*                                                                               
VK27     DS    0H                                                               
         CLC   CLTA,=C'ALL'        DO ALL CLIENTS?                              
         BE    INVLPRD1            ERROR                                        
         MVC   PRDA,8(R2)                                                       
         OC    PRDA,SPACES         SPACE PADDED                                 
*                                                                               
         MVC   CLTNAME,CLTNM       CLIENT NAME                                  
*                                                                               
*  VALIDATE PRODUCT CODE GET BINARY EQUIVALENT                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   PLSTTYPE(2),=X'0DF1'                                             
         MVC   PLSTAM,BAGYMD     AGY/MEDIA                                      
         MVC   PLSTCLT,CLT       CLIENT                                         
         MVC   PLSTPRD,PRDA      PRODUCT                                        
         CLC   PRDA,=CL3'POL'                                                   
         BNE   *+8                                                              
         MVI   PLSTXFF,X'FF'                                                    
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(9),KEYSAVE                                                   
         BNE   INVLPRD                                                          
         MVC   PRD,PLSTBPRD+1                                                   
*                                                                               
*  OLD WAY OF VALIDATING PRODUCT CODE COMMENTED OUT                             
*                                                                               
***         LA    R6,SVCLIST                                                    
***         LA    R5,MAXPRD1          MAX # OF PRODUCTS                         
*                                                                               
***VK30     DS    0H                                                            
***         OC    0(4,R6),0(R6)                                                 
***         BZ    VK35                NOT IN CLIST, CHECK CLIST2                
*                                                                               
***         CLC   0(3,R6),PRDA        PRODUCT MATCH?                            
***         BE    VK40                                                          
*                                                                               
***         LA    R6,4(R6)                                                      
***         BCT   R5,VK30                                                       
*                                                                               
***VK35     DS    0H                                                            
***         LA    R6,SVCLIST2                                                   
***         LA    R5,MAXPRD2          MAX # OF PRODUCTS                         
*                                                                               
***VK37     DS    0H                                                            
***         OC    0(4,R6),0(R6)                                                 
***         BZ    INVLPRD             NOT IN CLIST2 EITHER, ERROR               
*                                                                               
***         CLC   0(3,R6),PRDA        PRODUCT MATCH?                            
***         BE    VK40                                                          
*                                                                               
***         LA    R6,4(R6)                                                      
***         BCT   R5,VK37                                                       
***         B     INVLPRD                                                       
*                                                                               
***VK40     DS    0H                                                            
***         MVC   PRD,3(R6)           SAVE PRODUCT EQUATE                       
*                                                                               
VK45     DS    0H                                                               
         LA    RF,PRDTABLE                                                      
         MVC   0(1,RF),PRD                                                      
         MVC   1(3,RF),PRDA                                                     
         MVC   4(2,RF),=X'FFFF'                                                 
*                                                                               
VK50     DS    0H                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         MVI   STAFLAG,0                                                        
*                                                                               
         LA    R2,DUNSTAH          NETWORK                                      
         CLI   5(R2),0                                                          
         BE    VK70                                                             
*                                                                               
         CLC   8(4,R2),=C'ALL,'                                                 
         BNE   VK65                                                             
*                                                                               
         CLI   12(R2),C'-'         EXCLUSION?                                   
         BE    VK60                                                             
*                                                                               
         CLI   12(R2),C'N'         BROADCAST ONLY?                              
         BNE   *+12                                                             
         OI    STAFLAG,BRDSTA                                                   
         B     VK70                                                             
*                                                                               
         CLI   12(R2),C'C'         CABLE ONLY?                                  
         BNE   *+12                                                             
         OI    STAFLAG,CBLSTA                                                   
         B     VK70                                                             
*                                                                               
         CLI   12(R2),C'S'         SYNDICATION ONLY?                            
         BNE   *+12                                                             
         OI    STAFLAG,SYNDSTA                                                  
         B     VK70                                                             
*                                                                               
         CLI   12(R2),C'O'         OTHER ONLY?                                  
         BNE   INVLFLD                                                          
         OI    STAFLAG,OTHSTA                                                   
         B     VK70                                                             
*                                                                               
VK60     DS    0H                                                               
         OI    STAFLAG,BRDSTA                                                   
         OI    STAFLAG,CBLSTA                                                   
         OI    STAFLAG,SYNDSTA                                                  
         OI    STAFLAG,OTHSTA                                                   
*                                                                               
         CLI   13(R2),C'N'         BROADCAST ONLY?                              
         BNE   *+12                                                             
         NI    STAFLAG,X'FF'-BRDSTA                                             
         B     VK70                                                             
*                                                                               
         CLI   13(R2),C'C'         CABLE ONLY?                                  
         BNE   *+12                                                             
         NI    STAFLAG,X'FF'-CBLSTA                                             
         B     VK70                                                             
*                                                                               
         CLI   13(R2),C'S'         SYNDICATION ONLY?                            
         BNE   *+12                                                             
         NI    STAFLAG,X'FF'-SYNDSTA                                            
         B     VK70                                                             
*                                                                               
         CLI   13(R2),C'O'         OTHER ONLY?                                  
         BNE   INVLFLD                                                          
         NI    STAFLAG,X'FF'-OTHSTA                                             
         B     VK70                                                             
*                                                                               
VK65     DS    0H                                                               
         CLC   8(3,R2),=C'ALL'     ALL STATIONS                                 
         BE    VK70                                                             
*                                                                               
         GOTO1 VALINTWK                                                         
*                                                                               
         MVC   NETA,8(R2)                                                       
         OC    NETA,SPACES                                                      
*                                                                               
         MVC   MKTP,BMKTSTA        MARKET                                       
         MVC   STAP,BMKTSTA+2      STATION                                      
*                                                                               
VK70     DS    0H                                                               
         MVC   ESTA(3),=C'NO '                                                  
         LA    R2,DUNESTH          ESTIMATE                                     
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         OI    FILTFLAG,ALLEST                                                  
         B     VK160                                                            
*                                                                               
         MVC   ESTA(3),=C'ALL'                                                  
         CLC   8(4,R2),=C'ALL,'                                                 
         BNE   *+12                                                             
         OI    FILTFLAG,SEPEST                                                  
         B     VK80                                                             
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   *+16                                                             
         OI    FILTFLAG,ALLEST                                                  
         OI    FILTFLAG,SEPEST                                                  
         B     VK160                                                            
*                                                                               
         MVC   ESTA(3),=C'NO '                                                  
         CLC   8(3,R2),=C'NO,'                                                  
         BE    VK80                                                             
         CLC   8(2,R2),=C'NO'                                                   
         BNE   VK100                                                            
         OI    FILTFLAG,ALLEST                                                  
         B     VK160                                                            
*                                                                               
VK80     DS    0H                                                               
         GOTO1 SCANNER,DMCB,(0,DUNESTH),MYBLOCK,C',,'                           
         CLI   DMCB+4,0                                                         
         BE    INVLEST                                                          
*                                                                               
         CLI   MYBLOCK+32,3                                                     
         BL    INVLEST                                                          
         OI    MYFLAG,XXXFILT                                                   
*                                                                               
         OI    FILTFLAG,ALLEST                                                  
         MVC   ESTXXX,MYBLOCK+44                                                
         B     VK160                                                            
*                                                                               
VK100    DS    0H                                                               
         GOTO1 SCANNER,DMCB,(0,DUNESTH),MYBLOCK,C',=,-'                         
         CLI   DMCB+4,0                                                         
         BE    INVLEST                                                          
*                                                                               
         TM    MYBLOCK+2,X'A0'     ALPHA NUMERIC?                               
         BZ    INVLEST                                                          
*                                                                               
         CLI   MYBLOCK+6,0         OVER EST 255?                                
         BNE   INVLEST                                                          
         CLI   MYBLOCK+7,X'FF'     OVER EST 255?                                
         BH    INVLEST                                                          
         MVC   EST1,MYBLOCK+7      BINARY ESTIMATE RANGE START                  
         MVC   EST2,EST1           DEFAULT TO 1 ESTIMATE                        
*                                                                               
         CLI   MYBLOCK+1,0         ANY END RANGE?                               
         BE    VK150                                                            
*                                                                               
         TM    MYBLOCK+3,X'A0'     ALPHA NUMERIC?                               
         BZ    INVLEST                                                          
*                                                                               
         CLI   MYBLOCK+10,0        OVER EST 255?                                
         BNE   INVLEST                                                          
         CLI   MYBLOCK+11,X'FF'    OVER EST 255?                                
         BH    INVLEST                                                          
         MVC   EST2,MYBLOCK+11     BINARY ESTIMATE RANGE END                    
*                                                                               
VK150    DS    0H                                                               
         CLC   EST1,EST2           END EST MUST BE >= TO START                  
         BH    INVLEST                                                          
*                                                                               
         MVC   ESTA,8(R2)                                                       
*                                                                               
VK160    DS    0H                                                               
         NI    MYFLAG,X'FF'-PARTMNTH                                            
*                                                                               
         LA    R2,DUNPERH          PERIOD                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    RF,DUNPER           SETUP DMCB FOR PERVAL CALL                   
         ST    RF,DMCB                                                          
*                                                                               
         MVC   DMCB(1),DUNPERH+5   INPUT LENGTH                                 
*                                                                               
         XC    MYBLOCK,MYBLOCK                                                  
         LA    RF,MYBLOCK                                                       
         ST    RF,DMCB+4                                                        
*                                                                               
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,0                                                         
         BNE   INVLPER                                                          
*                                                                               
         LA    RF,MYBLOCK                                                       
         USING PERVALD,RF                                                       
*                                                                               
         MVC   PERIOD,PVALCPER     DISPLAYABLE PERIOD                           
         MVC   STARTC,PVALCSTA     START OF PERIOD                              
         MVC   ENDC,PVALCEND       END OF PERIOD                                
         DROP  RF                                                               
*                                                                               
         CLI   DUNPERH+5,X'0D'    PARTIAL BROADCAST MONTH?                      
         BNH   VK165                                                            
*                                                                               
         CLC   PERIOD(3),PERIOD+9 SAME MONTH?                                   
         BNE   INVLPART                                                         
*                                                                               
         OI    MYFLAG,PARTMNTH    PARTIAL MONTH                                 
         OI    FILTFLAG,BRDCAST                                                 
*                                                                               
VK165    DS    0H                                                               
         CLI   DUNPERH+5,X'06'                                                  
         BNE   VK170                                                            
         MVC   PERIOD(9),=C'  UP TO  '                                          
         MVC   STARTC,=XL2'A021'  DEFAULT START TO JAN01/80                     
*                                                                               
VK170    DS    0H                                                               
         LA    R2,DUNGNBH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   8(R2),C'G'          GROSS?                                       
         BE    VK175                                                            
         CLI   8(R2),C'N'          NET?                                         
         BE    VK175                                                            
         CLI   8(R2),C'B'          BOTH?                                        
         BNE   INVLFLD                                                          
*                                                                               
VK175    DS    0H                                                               
         MVC   GNBA,8(R2)          GROSS/NET/BOTH                               
*                                                                               
VK180    DS    0H                                                               
         MVI   RLA,C'B'            DEFAULT TO BOTH                              
         LA    R2,DUNRLH                                                        
         CLI   5(R2),0                                                          
         BE    VK185                                                            
         CLI   8(R2),C'B'          BOTH                                         
         BE    VK185                                                            
         CLI   8(R2),C'R'          REPORT?                                      
         BE    VK190                                                            
         CLI   8(R2),C'L'          LETTERS?                                     
         BE    VK185                                                            
         CLI   8(R2),C'F'          FAX BOTH                                     
         BE    VK184                                                            
         CLI   8(R2),C'S'          LETTERS?                                     
         BNE   INVLFLD                                                          
*                                                                               
VK184    OI    PRNTFLAG,FAXIT                                                   
         MVI   PQSW,1              INITIALIZE PQ FOR FAXING                     
         GOTO1 OPENPQ                                                           
*                                                                               
VK185    OI    PRNTFLAG,LETTERS                                                 
*                                                                               
VK190    DS    0H                                                               
         MVC   RLA,8(R2)           REPORT AND/OR LETTERS                        
*                                                                               
VK200    DS    0H                                                               
         LA    R2,DUNSMH           SUPPRESSED MATCH?                            
         CLI   5(R2),0                                                          
         BE    VK210                                                            
         CLI   8(R2),C'X'          INCLUDE ONLY UNITS W/O INVOICES              
         BE    VK210                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   INVLFLD                                                          
*                                                                               
VK210    DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,DUB),(5,SVLTRDAT)                                 
*                                                                               
         LA    R2,DUNLDH           LETTER DATE                                  
         CLI   5(R2),0                                                          
         BE    VK220                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,DUNLD),DUB                                        
         OC    DMCB(4),DMCB                                                     
         BZ    INVLDAT                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(5,SVLTRDAT)                                 
*                                                                               
VK220    DS    0H                                                               
         LA    R2,DUN0LH           PRINT $0 LETTER                              
         CLI   5(R2),0                                                          
         BE    VK230                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   INVLFLD                                                          
*                                                                               
VK230    DS    0H                                                               
         LA    R2,DUNXSRH          EXCLUDE SPECIAL REP $                        
         CLI   5(R2),0                                                          
         BE    VK240                                                            
         CLI   8(R2),C'S'          INCLUDE ONLY SPECIAL REP UNITS               
         BE    VK240                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   INVLFLD                                                          
*                                                                               
VK240    DS    0H                                                               
         LA    R2,DUNPOLH                                                       
         CLI   5(R2),0                                                          
         BE    VK250                                                            
         CLI   8(R2),C'N'                                                       
         BNE   INVLFLD                                                          
*                                                                               
VK250    DS    0H                                                               
         LA    R2,DUNOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         LA    R2,8(R2)                                                         
*                                                                               
VK255    CLI   0(R2),C' '                                                       
         BE    VKX                                                              
*                                                                               
         CLC   0(4,R2),=C'PKG='    PRINT PACKAGE INFO?                          
         BNE   VK260                                                            
         CLI   4(R2),C'Y'                                                       
         BE    VK260                                                            
         CLI   4(R2),C'N'                                                       
         BNE   INVLFLD                                                          
         OI    PRNTFLAG,NOPKG                                                   
         LA    R2,6(R2)                                                         
         B     VK255                                                            
*                                                                               
VK260    DS    0H                                                               
         CLC   0(5,R2),=C'DATE='                                                
         BNE   VKX                                                              
         TM    MYFLAG,PARTMNTH    PARTIAL MONTH?                                
         BO    INVLFLD                                                          
*                                                                               
         CLI   5(R2),C'C'         CALENDAR?                                     
         BE    VK265                                                            
         CLI   5(R2),C'B'         BROADCAST?                                    
         BNE   INVLFLD                                                          
         OI    FILTFLAG,BRDCAST                                                 
VK265    LA    R2,7(R2)                                                         
         B     VK255                                                            
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCT GROUP AND BUILD PRODUCT TABLE                                
***********************************************************************         
VALPGRP  NTR1                                                                   
         LA    R2,DUNPRDH                                                       
         MVC   PRDA(3),12(R2)                                                   
         OC    PRDA,SPACES                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRGRECD,R6                                                       
*                                                                               
         MVC   0(2,R6),=X'0D01'                                                 
         MVC   PRGKAGMD,BAGYMD     AGY/MEDIA                                    
         MVC   PRGKCLT,CLT         CLIENT                                       
         MVC   PRGKID,PRDA         GROUP ID                                     
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),13(R2)      LEFT ALIGNED PWOS                            
         PACK  WORK+10(3),WORK(5)                                               
         MVC   PRGKGRP,WORK+10     GROUP NUMBER                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLPGR2                                                         
*                                                                               
         MVC   SVPGRNUM,PRGKGRP    SAVE PRODUCT GROUP NUMBER                    
*                                                                               
         LA    R4,PRDTABLE         BUILD PRD TABLE FROM PGRP PASSIVES           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   0(2,R6),=X'0D81'                                                 
         MVC   PRGPAGMD,BAGYMD     AGY/MEDIA                                    
         MVC   PRGPCLT,CLT         CLIENT                                       
         MVC   PRGPID,PRDA         GROUP ID                                     
         MVC   PRGPGRP,SVPGRNUM    GROUP NUMBER                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         B     VPG30                                                            
*                                                                               
VPG20    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR ',KEY,KEY,MYDMWRK              
*                                                                               
VPG30    DS    0H                                                               
         CLC   KEY(8),KEYSAVE      SAME CLT/GROUP?                              
         BNE   VPG40                                                            
*                                                                               
         MVC   1(3,R4),PRGPPRD     PRODUCT CODE                                 
         LA    R4,4(R4)                                                         
         B     VPG20                                                            
         DROP  R6                                                               
*                                                                               
VPG40    DS    0H                                                               
         MVC   0(2,R4),=X'FFFF'    DENOTE END OF TABLE                          
*                                                                               
         L     R6,AIO3             AIO3 CONTAINS CLIENT RECORD                  
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R4,PRDTABLE         GET PRODUCT EQUATES FROM CLT REC             
*                                                                               
VPG50    DS    0H                                                               
         CLC   0(2,R4),=X'FFFF'    FINISHED PRODUCTS?                           
         BE    VALPGRPX                                                         
*                                                                               
*  VALIDATE PRODUCT CODE GET BINARY EQUIVALENT                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   PLSTTYPE(2),=X'0DF1'                                             
         MVC   PLSTAM,BAGYMD     AGY/MEDIA                                      
         MVC   PLSTCLT,CLT       CLIENT                                         
         MVC   PLSTPRD,1(R4)     PRODUCT                                        
         CLC   PRDA,=CL3'POL'                                                   
         BNE   *+8                                                              
         MVI   PLSTXFF,X'FF'                                                    
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLPRD                                                          
         MVC   0(1,R4),PLSTBPRD+1                                               
*                                                                               
         LA    R4,4(R4)                                                         
         B     VPG50                                                            
*                                                                               
*  OLD WAY OF VALIDATING PRODUCT CODE COMMENTED OUT                             
*                                                                               
***         LA    R6,SVCLIST                                                    
***         LA    R5,MAXPRD1                                                    
*                                                                               
***VPG55    OC    0(4,R6),0(R6)       ANY MORE CLIENTS                          
***         BZ    VPG56               NOT IN CLIST, CHECK CLIST2                
*                                                                               
***         CLC   0(3,R6),1(R4)       PRODUCT MATCH?                            
***         BE    VPG60                                                         
*                                                                               
***         LA    R6,4(R6)                                                      
***         BCT   R5,VPG55                                                      
*                                                                               
***VPG56    DS    0H                                                            
***         LA    R6,SVCLIST2                                                   
***         LA    R5,MAXPRD2                                                    
*                                                                               
***VPG57    OC    0(4,R6),0(R6)       ANY MORE CLIENTS                          
***         BZ    INVLPGR2            NOT IN CLIST2 EITHER, ERROR               
*                                                                               
***         CLC   0(3,R6),1(R4)       PRODUCT MATCH?                            
***         BE    VPG60                                                         
*                                                                               
***         LA    R6,4(R6)                                                      
***         BCT   R5,VPG57                                                      
***         B     INVLPGR2                                                      
*                                                                               
***VPG60    DS    0H                                                            
***         MVC   0(1,R4),3(R6)                                                 
***         LA    R4,4(R4)                                                      
***         B     VPG50                                                         
         DROP  R6                                                               
*                                                                               
VALPGRPX DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
INVLDAT  MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
INVLEST  MVI   ERROR,INVEST                                                     
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
INVLPER  MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
INVLPRD  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ERROR PRODUCT DOES NOT EXIST FOR CLIENT'          
         B     MYERR                                                            
*                                                                               
INVLPRD1 DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'ERROR MUST ENTER A CLIENT FOR PRODUCT'            
         B     MYERR                                                            
*                                                                               
INVLPGR1 DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ERROR MUST ENTER A CLIENT FOR PRD GROUP'          
         B     MYERR                                                            
*                                                                               
INVLPGR2 DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'ERROR INVALID PRODUCT GROUP'                      
         B     MYERR                                                            
*                                                                               
INVLTIME DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(18),=C'ERROR INVALID TIME'                               
         B     MYERR                                                            
*                                                                               
INVLPART DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'MUST BE WITHIN SAME BROADCAST MONTH'              
         B     MYERR                                                            
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
***********************************************************************         
* PRINT RECORDS                                                                 
***********************************************************************         
PR       DS    0H                                                               
         TM    PRNTFLAG,FAXIT      FAXING?                                      
         BZ    PR10                                                             
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,MCVREMOT-MASTD(RE)                                            
         USING REMOTED,RF                                                       
*                                                                               
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVI   REMOTCPY,1                                                       
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'NNN'                                                 
         MVC   REMOTDST,TWAORIG                                                 
*                                                                               
         OC    TWADEST,TWADEST                                                  
         BZ    *+10                                                             
         MVC   REMOTDST,TWADEST                                                 
*                                                                               
         L     RE,TWADCONS                                                      
         L     RE,TSPFUSER-TWADCOND(,RE)                                        
         MVI   140(RE),C'Y'        SET EASYLINK STUFF PRINTED SW                
*                                                                               
         ICM   R1,15,136(RE)                                                    
         LA    R1,1(,R1)                                                        
         STCM  R1,3,REMOTSUB       FORCE PRT QUE KEY CHANGE                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REMOTFRM+1(3),DUB                                                
         STCM  R1,15,136(RE)                                                    
         DROP  RF                                                               
*                                                                               
         LA    R1,SPOOLKEY         INITIALIZE PRINTQ FOR FAXING                 
         USING PQPLD,R1                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
*                                                                               
         MVC   PLDESC(3),=C'NNN'                                                
         MVI   PLCLASS,C'G'        FOR FAXING                                   
*                                                                               
         OI    GENSTAT3,NOCLRSPK   DON'T CLEAR SPOOLKEY                         
         DROP  R1                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
PR10     DS    0H                                                               
         TM    FILTFLAG,BRDCAST                                                 
         BZ    PR15                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,STARTC),(0,STARTE)                                
         GOTO1 DATCON,DMCB,(2,ENDC),(0,ENDE)                                    
*                                                                               
         MVC   STARTE+4(2),=C'15'  DEFAULT TO MIDDLE OF MONTH                   
         MVC   ENDE+4(2),=C'15'                                                 
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(1,STARTE),WORK,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,WORK),(2,STARTC)                                  
         GOTO1 =V(GETBROAD),DMCB,(1,ENDE),WORK,GETDAY,ADDAY                     
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,ENDC)                                  
*                                                                               
PR15     DS    0H                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                                                               
         LA    R5,DNVLNQ           GETMAIN AREA FOR SORTING ELEMENT             
         L     RE,=A(DNVMAX)       GROUPS WITHIN A PERSON CODE                  
         MR    R4,RE                                                            
         ST    R5,DNVTBLN                                                       
         L     R0,DNVTBLN                                                       
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ADNVTAB          START OF AREA                                
*                                                                               
         L     R2,ADNVTAB          CLEAR BINSRCH TABLE                          
         USING BIND,R2                                                          
         XC    BININ,BININ                                                      
*                                                                               
         LA    R1,DNVLNQ                                                        
         ST    R1,BINLEN                                                        
*!!!!!!! STCM  R1,15,BINLEN        ENTRY LENGTH                                 
         MVI   BINDISP,0           DISPLACEMENT TO KEY                          
*                                                                               
         LA    R1,DNVKLNQ                                                       
         STC   R1,BINKEY+2         LENGTH OF KEY                                
*!!!!!!! STCM  R1,7,BINKEY         LENGTH OF KEY                                
*                                                                               
         L     R1,=A(DNVMAX)                                                    
         ST    R1,BINMAX                                                        
*!!!!!!! STCM  R1,15,BINMAX        MAXIMUM NUMBER OF ENTRIES                    
*                                                                               
         MVI   BINNUM,DNVBCNT                                                   
         MVI   BINFST,DNVBUCK-DNVTABD                                           
         DROP  R2                                                               
*                                                                               
*!!!!!!! GOTO1 =V(PRNTBL),DMCB,=C'BINT',0(R2),C'DUMP',20,=C'1D'                 
*                                                                               
         XC    TOTUNITS,TOTUNITS                                                
         ZAP   GROSSP,=P'0'                                                     
         ZAP   NETP,=P'0'                                                       
*                                                                               
         XC    SUBTU,SUBTU                                                      
         ZAP   SUBGP,=P'0'                                                      
         ZAP   SUBNP,=P'0'                                                      
*                                                                               
         XC    SVACTDAT,SVACTDAT                                                
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NURECD,R6                                                        
*                                                                               
         MVI   0(R6),X'04'                                                      
         MVC   NUKAM,BAGYMD                                                     
*                                                                               
         OI    MYFLAG,NEWCLT       FIRST TIME THROUGH                           
         XC    ACURPRD,ACURPRD                                                  
         NI    MYFLAG,X'FF'-GOTONE                                              
         NI    MYFLAG,X'FF'-PRLETTER                                            
*                                                                               
         TM    FILTFLAG,ALLCLT                                                  
         BO    *+10                                                             
         MVC   NUKCLT,CLT          CLIENT                                       
*                                                                               
         GOTO1 HIGH                                                             
         B     PR20                                                             
*                                                                               
PRSEQ    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PR20     DS    0H                  START FILTERING OUT UNITS                    
         LA    R6,KEY                                                           
         CLC   KEY(2),KEYSAVE      SAME AGY?                                    
         BNE   PR505                                                            
*                                                                               
         MVC   SVUNTKEY(20),KEY    SAVE UNIT KEY                                
*                                                                               
         CLC   NUKCLT,KEYSAVE+2    SAME CLIENT                                  
         BE    *+8                                                              
         OI    MYFLAG,NEWCLT       NEW CLIENT, RE-LOOK UP CLIENT RECORD         
*                                                                               
         TM    FILTFLAG,ALLCLT                                                  
         BO    *+14                                                             
         CLC   NUKCLT,CLT          SAME CLIENT?                                 
         BNE   PR500                                                            
*                                                                               
         MVC   CLT,NUKCLT                                                       
         BRAS  RE,GCLTINFO         GET CLIENT OFFICE #                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),SVUNTKEY    RESTORE UNIT KEY SEQUENCE                    
         GOTO1 HIGH                                                             
*                                                                               
         CLI   SVOFFILT,0          ANY OFFICE FILTER?                           
         BE    PR22                                                             
         CLC   SVOFFILT,CLTOFF     SAME OFFICE NUMBER?                          
         BNE   PRSEQ                                                            
*                                                                               
PR22     DS    0H                                                               
         CLI   NUKSUB,X'C1'        TRAFFIC UNIT?                                
         BNL   PRSEQ                                                            
*                                                                               
         CLI   NETA,0                                                           
         BE    PR24                                                             
         CLC   NUKNET,NETA         SAME FILTER STATION?                         
         BNE   PRSEQ                                                            
         B     PR25                                                             
*                                                                               
PR24     DS    0H                                                               
         TM    STAFLAG,BRDSTA      BROADCAST STATIONS ONLY?                     
         BZ    *+12                                                             
         CLI   NUKSTAT,0                                                        
         BE    PR25                                                             
*                                                                               
         TM    STAFLAG,CBLSTA      CABLE STATIONS ONLY?                         
         BZ    *+12                                                             
         TM    NUKSTAT,X'01'                                                    
         BO    PR25                                                             
*                                                                               
         TM    STAFLAG,SYNDSTA     SYND. STATIONS ONLY?                         
         BZ    *+12                                                             
         TM    NUKSTAT,X'02'                                                    
         BO    PR25                                                             
*                                                                               
         TM    STAFLAG,OTHSTA      OTHER STATIONS ONLY?                         
         BZ    *+12                                                             
         TM    NUKSTAT,X'03'                                                    
         BO    PR25                                                             
*                                                                               
         CLI   STAFLAG,0           ALL STATIONS?                                
         BNE   PRSEQ                                                            
*                                                                               
PR25     DS    0H                                                               
         CLC   NUKDATE,STARTC      AIR DATE WITHIN PERIOD RANGE?                
         BL    PRSEQ                                                            
         CLC   NUKDATE,ENDC                                                     
         BH    PRSEQ                                                            
*                                                                               
         TM    FILTFLAG,ALLEST                                                  
         BO    PR30                                                             
         CLC   NUKEST,EST1         WITHIN ESTIMATE RANGE?                       
         BL    PRSEQ                                                            
         CLC   NUKEST,EST2         WITHIN ESTIMATE RANGE?                       
         BH    PRSEQ                                                            
*                                                                               
PR30     DS    0H                                                               
         TM    MYFLAG,XXXFILT      FILTER BY ESTIMATE XXX                       
         BZ    PR40                                                             
*                                                                               
         EDIT  NUKEST,TMPEST,ZERO=NOBLANK,ALIGN=LEFT  ESTIMATE                  
*                                                                               
         LA    R3,TMPEST                                                        
         LA    R4,ESTXXX                                                        
         LA    R5,3                                                             
*                                                                               
PR32     DS    0H                                                               
         CLI   0(R4),C'-'          SKIP THESE ESTIMATES?                        
         BE    PR36                                                             
         CLI   0(R4),C'X'                                                       
         BE    PR38                                                             
*                                                                               
         CLC   0(1,R3),0(R4)       SAME ESTIMATE NUMBER?                        
         BNE   PRSEQ                                                            
         B     PR38                YES, CHECK NEXT CHARACTER                    
*                                                                               
PR36     DS    0H                                                               
         LA    R4,1(R4)                                                         
*                                                                               
         CLC   0(1,R3),0(R4)       SAME ESTIMATE NUMBER?                        
         BE    PRSEQ               YES - SKIP B/C OF '-'                        
*                                                                               
PR38     DS    0H                                                               
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,PR32                                                          
*                                                                               
PR40     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
PR50     DS    0H                  GET THIS UNITS PRODUCTS                      
         L     R6,AIO                                                           
         MVC   SVUNTKEY(20),KEY    SAVE UNIT KEY                                
*                                                                               
         TM    NUPACKST,X'20'      LOCKED PACKAGE?                              
         BO    PRSEQ                                                            
*                                                                               
         TM    NUUNITST,X'02'      MISSED UNIT?                                 
         BO    PRSEQ                                                            
         TM    NUUNITST,X'40'      PRE-EMPTED UNIT?                             
         BO    PRSEQ                                                            
*                                                                               
         CLI   DUNXSR,C'Y'         EXCLUDE SPECIAL REP $?                       
         BNE   *+14                                                             
         OC    NUSREP,NUSREP       UNIT HAVE SPECIAL REP?                       
         BNZ   PRSEQ                                                            
*                                                                               
         CLI   DUNXSR,C'S'         INCLUDE SPECIAL REP $?                       
         BNE   *+14                                                             
         OC    NUSREP,NUSREP       UNIT HAVE SPECIAL REP?                       
         BZ    PRSEQ                                                            
*                                                                               
         BRAS  RE,BLDPRLST         BUILD PRODUCT LIST W/ PERCENTAGES            
*                                                                               
         MVC   AIO,AIO1            RESET UNIT KEY SEQUENCE                      
         XC    KEY,KEY                                                          
         MVC   KEY(20),SVUNTKEY                                                 
         GOTO1 HIGH                                                             
*                                                                               
         CLC   PRDLIST(2),=X'FFFF'       UNALLOCATED UNIT?                      
         BE    PRSEQ               YES - SKIP                                   
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R2,PRDLIST                                                       
         ST    R2,ACURPRD                                                       
*                                                                               
PR110    DS    0H                                                               
         L     R2,ACURPRD                                                       
         CLC   0(2,R2),=X'FFFF'         END OF PRODUCT LIST?                    
         BE    PRSEQ                                                            
*                                                                               
         TM    FILTFLAG,ALLPRD     DO ALL PRODUCTS?                             
         BO    PR120                                                            
*                                                                               
         LA    R4,PRDTABLE         LIST OF FILTERING PRODUCTS                   
PR112    CLC   0(2,R4),=X'FFFF'    END OF FILTERING PRODUCTS?                   
         BE    PR118               DIDN'T FIND A MATCH SO SKIP                  
*                                                                               
         CLC   1(3,R4),1(R2)       IS FILTERED PRODUCT IN UNIT?                 
         BE    PR120                                                            
         LA    R4,4(R4)                                                         
         B     PR112                                                            
*                                                                               
PR118    DS    0H                  FILTER PRODUCT NOT IN UNIT                   
         LA    R2,6(R2)            CHECK NEXT UNIT PRODUCT                      
         ST    R2,ACURPRD                                                       
         B     PR110                                                            
*                                                                               
PR120    DS    0H                                                               
         MVC   SVPRD,1(R2)         SAVE AWAY PRODUCT ALPHA                      
         MVC   SVPRDPCT,4(R2)      SAVE AWAY PRODUCT PERCENTAGE                 
*                                                                               
         LA    RF,6(R2)                                                         
         ST    RF,ACURPRD                                                       
*                                                                               
PR200    DS    0H                                                               
         L     R6,AIO                                                           
         USING NURECD,R6                                                        
*                                                                               
         MVI   SVFLAG,0                                                         
         OC    NUAFFTIM,NUAFFTIM   IS IT MATCHED?                               
         BZ    *+8                                                              
         OI    SVFLAG,SMAT         UNIT MATCHED                                 
*                                                                               
         MVC   SVCLT,NUKCLT        SAVE CLIENT FOR SORTREC                      
         MVC   SVCLTA,CLTA                                                      
**** NOT SURE WE NEED THIS!                                                     
****     GOTO1 CLUNPK,DMCB,NUKCLT,SVCLTA                                        
**** NOT SURE WE NEED THIS!                                                     
         MVC   SVSTA,NUKNET        SAVE STATION FOR SORTREC                     
*&&DO                                                                           
         MVI   SVSTA+4,C'N'                                                     
         TM    NURSTAT,X'01'       CABLE/OTHER?                                 
         BZ    *+8                                                              
         MVI   SVSTA+4,C'C'                                                     
*&&                                                                             
         XC    TEMPH,TEMPH                                                      
         MVI   TEMPH,X'0C'         BUILD HEADER W/ NETWORK (8+4)                
         MVI   TEMPH+5,X'04'                                                    
         MVC   TEMPH+8(4),NUKNET   NETWORK                                      
*                                                                               
         LA    R2,TEMPH                                                         
         GOTO1 VALINTWK            VALIDATE NETWORK                             
         MVC   SVSTA+4(1),QSTYPE      STATION TYPE                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),SVUNTKEY                                                 
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         TM    STAFLAG,BRDSTA      BROADCAST STATIONS ONLY?                     
         BZ    *+12                                                             
         CLI   SVSTA+4,C'N'                                                     
         BNE   PRSEQ                                                            
*                                                                               
PR202    DS    0H                                                               
         MVC   SVPKG,NUPACK        PACKAGE                                      
         MVC   SVMKT,NUMARKET      SAVE MARKET CODE FOR SORTREC                 
         MVC   SVEST,NUKEST        ESTIMATE                                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(3,DUB)       SAVE MM/YY (BIN)           
*                                                                               
         TM    FILTFLAG,BRDCAST    DO BROADCAST MONTH?                          
         BZ    PR203                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(0,STARTE)                               
         GOTO1 =V(GETBROAD),DMCB,(1,STARTE),WORK,GETDAY,ADDAY                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,PERIODB)                               
         GOTO1 DATCON,DMCB,(2,PERIODB),(3,DUB)       SAVE MM/YY (BIN)           
*                                                                               
PR203    DS    0H                                                               
         MVC   SVPERIOD(1),DUB     YY (BINARY)       FOR SORTREC                
         MVC   SVPERIOD+1(1),DUB+1 MM (BINARY)                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(3,DUB)                                  
         MVI   DUB+2,X'01'         DEFAULT TO FIRST DATE OF MONTH               
         GOTO1 DATCON,DMCB,(3,DUB),(2,SVMOS)                                    
         XC    SVMOS,=XL2'FFFF'    GET COMPLEMENT                               
*                                                                               
         MVC   MYGROSS,NUACTUAL    GROSS $                                      
         XC    MYNET,MYNET                                                      
         DROP  R6                                                               
*                                                                               
         BRAS  RE,GETPROS          GET NN PROFILE                               
*                                                                               
         XC    SVNINV,SVNINV                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'18'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         USING NUDTAD,R6                                                        
*                                                                               
         MVC   SVNINV,NUDTINVN     INVOICE NUMBER                               
         DROP  R6                                                               
*                                                                               
         OC    SVNINV,SVNINV                                                    
         BZ    PR205                                                            
         CLI   DUNSM,C'X'          SUPPRESS NO INVOICE NUMBER UNITS?            
         BE    PRSEQ                                                            
*                                                                               
PR204    BRAS  RE,GETNINV          GET INVOICE DATE                             
*                                                                               
PR205    DS    0H                                                               
         L     R6,AIO                                                           
         USING NURECD,R6                                                        
*                                                                               
         CLC   SVPRDPCT,=X'2710'   PRODUCT 100% OF UNIT                         
         BE    PR210                                                            
*                                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
*                                                                               
         ICM   R5,15,NUACTUAL      GROSS $                                      
         LH    R3,SVPRDPCT         PRODUCT PERCENTAGE                           
*                                                                               
         MR    R4,R3                                                            
*                                                                               
*!!      SR    R4,R4                                                            
         D     R4,=F'10000'                                                     
         STCM  R5,15,MYGROSS                                                    
*                                                                               
PR210    DS    0H                                                               
         MVC   FULL,MYGROSS        CALCULATE NET                                
         SR    R4,R4                                                            
         L     R5,FULL                                                          
         M     R4,=F'85'                                                        
*                                                                               
         SR    R4,R4                                                            
         D     R4,=F'100'                                                       
*                                                                               
         ST    R5,MYNET                                                         
         DROP  R6                                                               
*                                                                               
PR220    DS    0H                  GET PAID $                                   
         XC    SVPAYGRS,SVPAYGRS                                                
         XC    SVPAYNET,SVPAYNET                                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PR300                                                            
         B     PR230                                                            
         USING NUPAYD,R6                                                        
*                                                                               
PR225    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   PR250                                                            
*                                                                               
PR230    DS    0H                                                               
         CLI   NUPAYTYP,C'T'       TIME ONLY?                                   
         BNE   PR225                                                            
         MVC   SVPAYGRS,NUPAYGRS   GROSS PAYMENT                                
         MVC   SVPAYNET,NUPAYNET   NET PAYMENT                                  
*                                                                               
         CLC   SVACTDAT,NUPAYDAT                                                
         BL    PR250                                                            
         MVC   SVACTDAT,NUPAYDAT   THIS IS THE LATEST PAY DATE                  
         DROP  R6                                                               
*                                                                               
PR250    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PR300                                                            
         USING NUSDRD,R6                                                        
*                                                                               
         OC    NUSDAFDT,NUSDAFDT   UNIT MATCHED?                                
         BZ    *+8                                                              
         OI    SVFLAG,SMAT         UNIT MATCHED                                 
         DROP  R6                                                               
*                                                                               
PR300    DS    0H                                                               
         XC    SORTREC,SORTREC                                                  
*                                                                               
         LA    RF,SORTREC                                                       
         USING SORTRECD,RF                                                      
*                                                                               
         MVC   SSTA,SVSTA          STATION                                      
*                                                                               
         TM    PRNTFLAG,NOPKG      DO NOT SHOW PACKAGE?                         
         BO    *+10                                                             
         MVC   SPKG,SVPKG          PACKAGE                                      
*                                                                               
         MVC   SPERIOD,SVPERIOD    PERIOD                                       
*                                                                               
         MVC   SPRD,SVPRD          PRODUCT                                      
         CLI   DUNPOL,C'N'         POL BREAKOUT?                                
         BNE   *+10                                                             
         MVC   SPRD,=C'POL'                                                     
*                                                                               
         MVC   SEST,SVEST          ESTIMATE                                     
         MVC   SGROSS,MYGROSS      GROSS $                                      
         MVC   SNET,MYNET          NET $                                        
         MVC   SCLEAR,SVPAYGRS     GROSS PAID (CLEAR)                           
         MVC   SCLT,SVCLT          CLIENT                                       
         MVC   SFLAG,SVFLAG        FLAG                                         
         MVC   SCOFF,CLTOFF        CLIENT OFFICE                                
         GOTO1 DATCON,DMCB,(2,SVACTDAT),(3,SACTDATE)  ACTIVITY DATE             
         DROP  RF                                                               
*                                                                               
         LA    RF,SORTREC                                                       
         ST    RF,ASORTREC                                                      
*                                                                               
*!!!     BRAS  RE,PRLINE                                                        
*                                                                               
         OI    MYFLAG,GOTONE       GOT AT LEAST ONE UNIT                        
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*&&DO                                                                           
         GOTO1 HEXOUT,DMCB,SORTREC,P,L'SORTREC,=C'TOG',0                        
         BRAS  RE,PRINTIT                                                       
*&&                                                                             
         B     PR110                                                            
*                                                                               
PR500    DS    0H                                                               
         XC    PREVREC,PREVREC                                                  
         XC    NUMSPOTS,NUMSPOTS                                                
         XC    TOTGROSS,TOTGROSS                                                
         XC    NUMMATCH,NUMMATCH                                                
         XC    TOTNET,TOTNET                                                    
         XC    TOTPAID,TOTPAID                                                  
         XC    PREVMKT,PREVMKT                                                  
         XC    PREVSTA,PREVSTA                                                  
         XC    LASTPRNT,LASTPRNT                                                
         XC    ACTDATE,ACTDATE                                                  
         NI    MYFLAG,X'FF'-LASTREC                                             
*                                                                               
PR505    DS    0H                                                               
         TM    MYFLAG,GOTONE       DID WE GET AT LEAST ONE UNIT?                
         BZ    PRX                 NO - EXIT                                    
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   PR510                                                            
         OI    MYFLAG,LASTREC                                                   
*                                                                               
PR510    DS    0H                                                               
         L     R6,4(R1)            A(SORTED RECORD)                             
         ST    R6,ASORTREC                                                      
         USING SORTRECD,R6                                                      
*                                                                               
*&&DO                                                                           
         GOTO1 HEXOUT,DMCB,0(R6),P,L'SORTREC,=C'TOG',0                          
         BRAS  RE,PRINTIT                                                       
*&&                                                                             
         TM    MYFLAG,LASTREC                                                   
         BO    PR519                                                            
*                                                                               
         CLI   DUNSM,C'Y'          SUPPRESSED MATCHED UNITS?                    
         BNE   *+12                                                             
         TM    SFLAG,SMAT          IS THIS UNIT MATCHED?                        
         BO    PR519                                                            
*                                                                               
         BRAS  RE,PUTDNV           PUT RECORD INTO BINSRCH TABLE                
*                                                                               
PR519    DS    0H                                                               
         MVC   SORTREC,0(R6)                                                    
         LA    R6,SORTREC                                                       
*                                                                               
         OC    PREVREC,PREVREC                                                  
         BNZ   PR520                                                            
         MVC   PREVREC,0(R6)                                                    
*                                                                               
PR520    DS    0H                                                               
         CLI   DUNPOL,C'N'         POL PRODUCTS?                                
         BNE   PR530                                                            
         CLC   0(9,R6),PREVREC     SAME STA/PKG/PER/EST?                        
         BNE   PR600                                                            
         B     PR540                                                            
*                                                                               
PR530    DS    0H                                                               
         CLC   0(12,R6),PREVREC    SAME STA/PKG/PER/EST/PRD?                    
         BNE   PR600                                                            
*                                                                               
PR540    DS    0H                                                               
         L     RF,NUMSPOTS                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NUMSPOTS                                                      
*                                                                               
         ICM   R2,15,SGROSS        ACCUMULATE GROSS                             
         L     R3,TOTGROSS                                                      
         AR    R2,R3                                                            
         ST    R2,TOTGROSS                                                      
*                                                                               
         ICM   R2,15,SNET          ACCUMULATE NET                               
         L     R3,TOTNET                                                        
         AR    R2,R3                                                            
         ST    R2,TOTNET                                                        
*                                                                               
         ICM   R2,15,SCLEAR        ACCUMULATE PAID                              
         L     R3,TOTPAID                                                       
         AR    R2,R3                                                            
         ST    R2,TOTPAID                                                       
*                                                                               
         TM    SFLAG,SMAT          UNIT MATCHED?                                
         BZ    PR560                                                            
         L     RF,NUMMATCH         UPDATE MATCH COUNTER                         
         LA    RF,1(RF)                                                         
         ST    RF,NUMMATCH                                                      
*                                                                               
         CLC   ACTDATE,SACTDATE    IS THIS A LATER ACTIVITY DATE?               
         BNL   *+10                                                             
         MVC   ACTDATE,SACTDATE    UPDATE ACTIVITY DATE                         
*                                                                               
PR560    DS    0H                                                               
         B     PR505                                                            
*                                                                               
PR600    DS    0H                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         LA    R6,PREVREC                                                       
*                                                                               
         CLC   SSTA,PREVSTA                                                     
         BE    PR610                                                            
*                                                                               
         TM    PRNTFLAG,FAXIT                                                   
         BZ    *+8                                                              
         OI    PRNTFLAG,FAXHEAD                                                 
*                                                                               
         MVC   PSTA(5),SSTA             STATION                                 
         MVC   PREVSTA,SSTA                                                     
*                                                                               
         CLI   DUNPOL,C'N'              DON'T BREAK OUT THE PRODUCTS            
         BE    PR610                                                            
         GOTO1 CLUNPK,DMCB,SCLT,PCLT    CLIENT                                  
*                                                                               
PR610    DS    0H                                                               
         CLC   TOTGROSS,TOTPAID    IF GROSS = CLEARED THEN DON'T PRINT          
         BE    PR650                                                            
*                                                                               
         TM    PRNTFLAG,NOPKG      PRINT PACKAGE INFO?                          
         BO    PR610B                                                           
         EDIT  SPKG,PPKG,ZERO=NOBLANK   PACKAGE                                 
*                                                                               
PR610B   DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),SPERIOD      YEAR                                         
         MVC   DUB+1(1),SPERIOD+1  MONTH                                        
         MVI   DUB+2,X'01'         DAY                                          
         GOTO1 DATCON,DMCB,(3,DUB),(6,PPERIOD)                                  
*                                                                               
         CLC   SCLT,PREVCLT        SAME AS PREVIOUS CLIENT?                     
         BE    PR611                                                            
         MVC   PREVCLT,SCLT                                                     
*                                                                               
         GOTO1 CLUNPK,DMCB,SCLT,PCLT    CLIENT                                  
*                                                                               
PR611    DS    0H                                                               
         MVC   PPRD(3),SPRD             PRODUCT                                 
         CLI   DUNPOL,C'N'         POL BREAKOUT?                                
         BNE   *+10                                                             
         MVC   PPRD(3),=C'POL'                                                  
*                                                                               
         CLC   DUNEST(2),=C'NO'         IF EST=NO, DON'T PRINT EST              
         BE    PR612                                                            
         EDIT  SEST,PEST,ZERO=NOBLANK   ESTIMATE                                
*                                                                               
PR612    EDIT  NUMSPOTS,PUNITS,ZERO=NOBLANK   NUMBER OF SPOTS                   
*                                                                               
         L     R4,TOTUNITS         KEEP TOTAL UNITS FOR REQUEST                 
         L     R5,NUMSPOTS                                                      
         AR    R4,R5                                                            
         ST    R4,TOTUNITS                                                      
*                                                                               
         L     R4,SUBTU            KEEP SUBTOTAL UNITS FOR REQUEST              
         L     R5,NUMSPOTS                                                      
         AR    R4,R5                                                            
         ST    R4,SUBTU                                                         
*                                                                               
PR616    DS    0H                                                               
         CLI   DUNGNB,C'N'         JUST PRINT NET?                              
         BE    PR617                                                            
         EDIT  TOTGROSS,PGROSS,2,ALIGN=RIGHT,ZERO=NOBLANK                       
*                                                                               
PR617    DS    0H                                                               
         CLI   DUNGNB,C'G'         JUST PRINT GROSS?                            
         BE    PR618                                                            
         EDIT  TOTNET,PNET,2,ALIGN=RIGHT,ZERO=NOBLANK                           
*                                                                               
PR618    DS    0H                                                               
         L     RF,TOTGROSS                                                      
         CVD   RF,DUB                                                           
         AP    GROSSP,DUB                                                       
*                                                                               
         L     RF,TOTNET                                                        
         CVD   RF,DUB                                                           
         AP    NETP,DUB                                                         
*                                                                               
         L     RF,TOTGROSS         SUBTOTALS                                    
         CVD   RF,DUB                                                           
         AP    SUBGP,DUB                                                        
*                                                                               
         L     RF,TOTNET           SUBTOTALS                                    
         CVD   RF,DUB                                                           
         AP    SUBNP,DUB                                                        
*                                                                               
         MVC   PMATCH(4),=C'NONE'                                               
         OC    NUMMATCH,NUMMATCH   ANY MATCHED UNITS?                           
         BZ    PR620                                                            
         MVC   PMATCH(4),=C'ALL '                                               
         CLC   NUMSPOTS,NUMMATCH   ALL UNITS MATCHED?                           
         BE    PR620                                                            
         MVC   PMATCH(4),=C'PART'  NO - ONLY PARTIAL                            
*                                                                               
PR620    DS    0H                                                               
         MVC   PCLEAR(4),=C'NONE'                                               
         OC    TOTPAID,TOTPAID                                                  
         BZ    *+10                                                             
         MVC   PCLEAR(4),=C'PART'                                               
*                                                                               
         MVC   PACTDATE,=C' NO INV '                                            
         OC    ACTDATE,ACTDATE                                                  
         BZ    PR630                                                            
         GOTO1 DATCON,DMCB,(3,ACTDATE),(5,PACTDATE)                             
*                                                                               
PR630    DS    0H                                                               
         CLI   RLA,C'L'            LETTERS ONLY?                                
         BE    PR650                                                            
         CLI   RLA,C'S'            LETTERS ONLY?                                
         BE    PR650                                                            
*                                                                               
         CLI   DUNSM,C'Y'          SUPRESSED MATCHED?                           
         BNE   PR640                                                            
         CLC   PMATCH(3),=C'ALL'   WERE ALL UNITS MATCHED?                      
         BNE   PR640                                                            
*                                                                               
         L     R4,TOTUNITS                                                      
         L     R5,SUBTU                                                         
         SR    R4,R5                                                            
         ST    R4,TOTUNITS                                                      
*                                                                               
         SP    GROSSP,SUBGP                                                     
         SP    NETP,SUBNP                                                       
*                                                                               
         B     PR650                                                            
*                                                                               
PR640    DS    0H                                                               
         TM    FILTFLAG,SEPEST     SEPERATE PAGE PER EST?                       
         BZ    PR645                                                            
*                                                                               
         CLI   LASTPRNT,0          FIRST TIME THROUGH                           
         BE    PR645                                                            
*                                                                               
         CLC   LASTPRNT,0(R6)      SAME STA/PKG/PER/EST/PRD                     
         BE    PR645               AS LAST ONE?                                 
*                                                                               
         MVC   MYBLOCK(PLNQ),P                                                  
         XC    P,P                                                              
*                                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   P(PLNQ),MYBLOCK                                                  
         MVC   PSTA(5),SSTA             STATION                                 
*                                                                               
         GOTO1 CLUNPK,DMCB,SCLT,PCLT    CLIENT                                  
*                                                                               
PR645    DS    0H                                                               
         TM    PRNTFLAG,FAXHEAD                                                 
         BZ    PR648                                                            
*                                                                               
         MVC   MYBLOCK(PLNQ),P                                                  
         XC    P,P                                                              
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         TM    PRNTFLAG,PRNTREP    STARTED PRINTING REPORT?                     
         BZ    PR646                                                            
*                                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   P(26),=C'*** END OF DDS MESSAGE ***'                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
PR646    DS    0H                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   CURSTA,SSTA                                                      
         GOTO1 CLUNPK,DMCB,SCLT,CLTA                                            
         BRAS  RE,PRNTFAX          PRINT FAX HEADERS                            
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
         NI    PRNTFLAG,X'FF'-FAXHEAD                                           
*                                                                               
         MVC   P,MYBLOCK                                                        
*                                                                               
PR648    BRAS  RE,PRINTIT                                                       
         MVC   LASTPRNT,PREVREC                                                 
         OI    PRNTFLAG,PRNTREP    STARTED PRINTING REPORT                      
         DROP  R2,R6                                                            
*                                                                               
PR650    DS    0H                                                               
         MVC   PREVREC,SORTREC                                                  
*                                                                               
         BRAS  RE,REINITOT         RE  INITIALIZE TOTALS                        
*                                                                               
         TM    MYFLAG,LASTREC                                                   
         BO    PR660                                                            
         B     PR505                                                            
*                                                                               
PR660    DS    0H                  PRINT TOTALS FOR REQUEST                     
         CLI   RLA,C'L'            LETTERS ONLY?                                
         BE    PR700                                                            
         CLI   RLA,C'S'            LETTERS ONLY?                                
         BE    PR700                                                            
*                                                                               
         XC    P,P                                                              
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVI   P+3,C'-'                                                         
         MVC   P+4(44),P+3                                                      
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVI   P+3,C'*'                                                         
         MVC   P+22(5),=C'TOTAL'                                                
         MVI   P+47,C'*'                                                        
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVI   P+3,C'-'                                                         
         MVC   P+4(44),P+3                                                      
         BRAS  RE,PRINTIT                                                       
*                                                                               
         LA    R2,P+6                                                           
         USING TOTD,R2                                                          
         MVC   TOTHEAD,=C'# OF UNITS:'                                          
         EDIT  TOTUNITS,TOTDATA,ZERO=NOBLANK,ALIGN=LEFT                         
*                                                                               
         MVI   P+3,C'*'                                                         
         MVI   P+47,C'*'                                                        
         BRAS  RE,PRINTIT                                                       
*                                                                               
         LA    R2,P+6                                                           
         MVC   TOTHEAD,=C'GROSS COST:'                                          
         EDIT  GROSSP,TOTDATA,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=$,       +        
               COMMAS=YES                                                       
*                                                                               
         MVI   P+3,C'*'                                                         
         MVI   P+47,C'*'                                                        
         BRAS  RE,PRINTIT                                                       
*                                                                               
         LA    R2,P+6                                                           
         MVC   TOTHEAD,=C'NET   COST:'                                          
         EDIT  NETP,TOTDATA,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=$,         +        
               COMMAS=YES                                                       
*                                                                               
         MVI   P+3,C'*'                                                         
         MVI   P+47,C'*'                                                        
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVI   P+3,C'-'                                                         
         MVC   P+4(44),P+3                                                      
         BRAS  RE,PRINTIT                                                       
         DROP  R2                                                               
*                                                                               
         TM    PRNTFLAG,FAXIT                                                   
         BZ    PR700                                                            
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   P(26),=C'*** END OF DDS MESSAGE ***'                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
PR700    DS    0H                                                               
         TM    PRNTFLAG,LETTERS    PRINT DUNNING LETTERS                        
         BZ    PRX                                                              
*                                                                               
         L     R6,ADNVTAB          A(LETTER TABLE)                              
         USING BIND,R6                                                          
         ICM   R4,15,BININ         # OF ENTRIES IN TABLE                        
         BZ    PRX                 NONE - EXIT                                  
*                                                                               
         LA    R6,BINTAB           A(TABLE)                                     
         DROP  R6                                                               
         USING DNVTABD,R6                                                       
*                                                                               
PR720    DS    0H                                                               
         ST    R6,ACURLTR                                                       
*                                                                               
         CLI   DUN0L,C'Y'          INCLUDE $0 LETTER?                           
         BE    *+14                                                             
         OC    DNVGROSS,DNVGROSS   $0?                                          
         BZ    PR760               DON'T PRINT - SKIP                           
*                                                                               
         CLC   DNVGROSS,DNVCLEAR   GROSS $ = CLEAR $?                           
         BE    PR760                                                            
*                                                                               
PR750    BRAS  RE,PRNTLTR          PRINT DUNNING LETTER                         
*                                                                               
PR760    LA    R6,DNVLNQ(R6)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R4,PR720            PROCESS NEXT LETTER                          
*                                                                               
PRX      DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
         LM    R0,R1,DNVTBLN                                                    
         FREEMAIN R,LV=(0),A=(1)                                                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,5,A,6,1,A,7,6,A),FORMAT=BI,WORK=1'           
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=60'                                    
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,46,C'STATION INVOICE CONTROL REPORT'                          
         SSPEC H2,46,C'------------------------------'                          
*!!!     SSPEC H1,93,AGYNAME                                                    
*!!!     SSPEC H2,93,AGYADD                                                     
         SSPEC H3,46,C'PERIOD FROM'                                             
         SSPEC H5,93,PAGE                                                       
         SSPEC H5,102,REQUESTOR                                                 
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         MVC   H1+92(33),LTRNAME          NAME                                  
         MVC   H2+92(33),LTRADD           ADDRESS                               
*                                                                               
         CLI   LTRADD2,X'40'                                                    
         BE    *+10                                                             
         MVC   H3+92(33),LTRADD2          ADDRESS                               
*                                                                               
         CLI   LTRADD2,X'40'                                                    
         BE    *+10                                                             
         MVC   H4+92(33),LTRADD3          ADDRESS                               
*                                                                               
         CLI   DUNCLT,C'*'         OFFICE FILTER?                               
         BNE   HDRTN10                                                          
         MVC   H3+3(7),=C'OFFICE:'                                              
*                                                                               
         MVC   H3+11(1),SVOFFILT                                                
         CLC   DUNCLT+1(3),=C'ALL'    FILTER ON ALL OFFICES?                    
         BNE   HDRTN20                                                          
         MVC   H3+11(3),=C'ALL'                                                 
         B     HDRTN20                                                          
*                                                                               
HDRTN10  DS    0H                                                               
         MVC   H3+3(7),=C'CLIENT:'                                              
*                                                                               
         MVC   H3+11(3),=C'ALL'       DEFAULT TO ALL                            
         CLC   DUNCLT(3),=C'ALL'      ALL CLIENTS?                              
         BE    *+10                                                             
         MVC   H3+11(3),CLTA                                                    
*                                                                               
HDRTN20  DS    0H                                                               
         MVC   H3+57(17),PERIOD    PERIOD                                       
*                                                                               
         LA    RF,H7                                                            
         USING PLINED,RF                                                        
*                                                                               
         MVC   PCLT,=C'CLT'                                                     
         MVC   PSTA,=C'STATION'                                                 
*                                                                               
         TM    PRNTFLAG,NOPKG      PRINT PACKAGE INFO?                          
         BO    *+10                                                             
         MVC   PPKG,=C'PKG'                                                     
*                                                                               
         MVC   PPERIOD,=C'PERIOD'                                               
         MVC   PPRD,=C'PRODUCT'                                                 
         MVC   PEST,=C'EST'                                                     
         MVC   PUNITS,=C'UNITS'                                                 
*                                                                               
         CLI   DUNGNB,C'N'         JUST PRINT NET?                              
         BE    *+10                                                             
         MVC   PGROSS+5(10),=C'GROSS COST'                                      
*                                                                               
         CLI   DUNGNB,C'G'         JUST PRINT GROSS?                            
         BE    *+10                                                             
         MVC   PNET+5(10),=C'  NET COST'                                        
*                                                                               
         MVC   PMATCH,=C'MATCH'                                                 
         MVC   PCLEAR,=C'CLEAR'                                                 
         MVC   PACTDATE,=C'ACT DATE'                                            
         DROP  RF                                                               
*                                                                               
         MVI   H8+45,0                                                          
         CLI   DUNXSR,C'Y'         EXCLUDE SPECIAL REP $                        
         JNE   *+10                                                             
         MVC   H8+45(32),=C'SPECIAL REP DOLLARS ARE EXCLUDED'                   
*                                                                               
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
*!!!     MVI   BOXCOLS+(PUNITS-PLINED-1),C'C'                                   
         MVI   BOXCOLS+PLNQ+2,C'R'                                              
*                                                                               
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R2                                                               
*                                                                               
HDRTNX   J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
*    PRINT FAX HEADERS                                                          
***********************************************************************         
PRNTFAX  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETFAX           GET FAX RECORD                               
*                                                                               
         LA    R2,P                PRINT HEADER FAX CARD                        
         USING FAXHDRD,R2                                                       
*                                                                               
         MVC   FAXOID(2),AGENCY    ORIGIN ID                                    
         MVC   FAXHDR,=C'*HDR*'                                                 
*                                                                               
         MVC   FAXDEST(5),CURSTA                                                
         CLI   SVFAXNUM,C' '       ANY FAX #?                                   
         BE    PRF20                                                            
         MVC   FAXDEST(5),=C'FAXW '                                             
         MVC   FAXDEST+5(16),SVFAXNUM FAX NUMBER                                
*                                                                               
PRF20    DS    0H                                                               
         MVI   FAXWIDE,C'W'        WIDE REPORT                                  
         MVC   FAXDESTN,SVFAXNUM   FAX NUMBER                                   
*                                                                               
         BRAS  RE,PRINTIT                                                       
         DROP  R2                                                               
*                                                                               
         LA    R2,P                PRINT ++DDS CARD                             
         USING FAXDDSD,R2                                                       
*                                                                               
         MVC   FAXDDS,=C'++DDS'                                                 
         MVC   FAXDSYS,=C'NE'                                                   
         MVC   FAXDRTYP,=C'VNN'                                                 
         MVC   FAXDTRN,=C'TRN'                                                  
*                                                                               
         LA    R3,FAXDDATA                                                      
         USING FAXDATAD,R3                                                      
*                                                                               
         MVC   FDMED,DUNMED        REQUESTED MEDIA                              
         MVC   FDCLT,CLTA          REQUESTED CLIENT                             
         MVC   FDPRD,DUNPRD        REQUESTED PRODUCT                            
         MVC   FDSTA,CURSTA        REQUESTED STATION                            
         MVC   FDEST,DUNEST        REQUESTED ESTIMATE                           
         MVC   FDPERIOD,DUNPER     REQUESTED PERIOD                             
*                                                                               
         BRAS  RE,PRINTIT                                                       
         DROP  R2,R3                                                            
*                                                                               
PRFAXX   DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
*    GET FAX RECORD                                                             
***********************************************************************         
GETFAX   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
*                                                                               
         MVI   CTFXKTYP,C'9'                                                    
         MVC   CTFXAGY,AGENCY      AGENCY                                       
         MVC   CTFXCODE(2),=C'NN'  FAX CODE MUST START WITH NN                  
         MVC   CTFXCODE+2(4),CURSTA  STATION                                    
         OC    CTFXCODE,SPACES                                                  
*                                                                               
         MVC   SVFAXKEY,KEY                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY,AIO3,MYDMWRK             
*                                                                               
         CLC   SVFAXKEY,KEY                                                     
         BE    GETF20                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
*                                                                               
         MVI   CTFXKTYP,C'9'                                                    
         MVC   CTFXAGY,AGENCY      AGENCY                                       
         MVC   CTFXCODE(4),CURSTA  STATION                                      
         OC    CTFXCODE,SPACES                                                  
*                                                                               
         MVC   SVFAXKEY,KEY                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY,AIO3,MYDMWRK             
*                                                                               
         CLC   SVFAXKEY,KEY                                                     
         BNE   GETFAXX                                                          
*                                                                               
GETF20   DS    0H                                                               
         L     R4,AIO3                                                          
         LA    R4,CTFXEL1                                                       
         XC    SVFAXNUM,SVFAXNUM                                                
         XC    SVATTN,SVATTN                                                    
*                                                                               
         CLI   0(R4),X'01'                                                      
         BNE   GETFAXX                                                          
         ZIC   R3,1(R4)            L'FAX # ELEMENT                              
         SHI   R3,3                                                             
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   SVFAXNUM(0),2(R4)   FAX NUMBER                                   
         OC    SVFAXNUM,SPACES                                                  
*                                                                               
         L     R4,AIO3                                                          
         LA    R4,CTFXEL1                                                       
         ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
*                                                                               
         CLI   0(R4),X'02'         ATTN: ELEMENT?                               
         BNE   GETFAXX                                                          
         MVC   SVATTN,2(R4)        ATTN                                         
         OC    SVATTN,SPACES                                                    
*                                                                               
GETFAXX  DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* RE-INITIALIE TOTALS                                                           
*    JUST PRINTED RECORD, NOW INIT TOTALS TO CURRENT REC                        
***********************************************************************         
REINITOT NTR1  BASE=*,LABEL=*                                                   
         LA    R6,SORTREC                                                       
         USING SORTRECD,R6                                                      
*                                                                               
         XC    NUMSPOTS,NUMSPOTS                                                
         XC    NUMMATCH,NUMMATCH                                                
         XC    TOTNET,TOTNET                                                    
         XC    ACTDATE,ACTDATE                                                  
*                                                                               
         CLC   TOTGROSS,TOTPAID    IF GROSS = CLEARED THEN REINIT               
         BE    *+10                                                             
         MVC   ACTDATE,SACTDATE    ACTIVITY DATE                                
*                                                                               
         XC    TOTPAID,TOTPAID                                                  
         XC    TOTGROSS,TOTGROSS                                                
*                                                                               
         XC    SUBTU,SUBTU                                                      
         ZAP   SUBGP,=P'0'                                                      
         ZAP   SUBNP,=P'0'                                                      
*                                                                               
         L     RF,NUMSPOTS                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NUMSPOTS                                                      
*                                                                               
         ICM   R2,15,SGROSS        ACCUMULATE GROSS                             
         L     R3,TOTGROSS                                                      
         AR    R2,R3                                                            
         ST    R2,TOTGROSS                                                      
*                                                                               
         ICM   R2,15,SNET          ACCUMULATE NET                               
         L     R3,TOTNET                                                        
         AR    R2,R3                                                            
         ST    R2,TOTNET                                                        
*                                                                               
         ICM   R2,15,SCLEAR        ACCUMULATE PAID                              
         L     R3,TOTPAID                                                       
         AR    R2,R3                                                            
         ST    R2,TOTPAID                                                       
*                                                                               
         TM    SFLAG,SMAT          UNIT MATCHED?                                
         BZ    REINITX                                                          
         L     RF,NUMMATCH         UPDATE MATCH COUNTER                         
         LA    RF,1(RF)                                                         
         ST    RF,NUMMATCH                                                      
         DROP  R6                                                               
*                                                                               
REINITX  DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* PUT RECORD INTO BINSRCH TABLE (DUNNING LETTER)                                
***********************************************************************         
PUTDNV   NTR1  BASE=*,LABEL=*                                                   
         L     R6,ASORTREC                                                      
         USING SORTRECD,R6                                                      
*                                                                               
         XC    DNVREC,DNVREC                                                    
*                                                                               
         LA    R4,DNVREC                                                        
         USING DNVTABD,R4                                                       
*                                                                               
         MVC   DNVCLT,SCLT         CLIENT                                       
         MVC   DNVSTA,SSTA         STATION                                      
         MVC   DNVPRD,SPRD         PRODUCT                                      
         MVC   DNVEST,SEST         ESTIMATE                                     
         MVC   DNVPKG,SPKG         PACKAGE                                      
         MVC   DNVPER,SPERIOD      PERIOD                                       
         MVC   DNVCOFF,SCOFF       CLIENT OFFICE                                
*                                                                               
         MVI   DNVSPOTS+3,X'01'    # OF SPOTS = 1                               
         MVC   DNVGROSS,SGROSS     GROSS $                                      
         MVC   DNVNET,SNET         NET $                                        
         MVC   DNVCLEAR,SCLEAR     CLEAR $                                      
*                                                                               
         GOTO1 =A(BINADD),DMCB,DNVREC,ADNVTAB                                   
         DROP  R4,R6                                                            
*                                                                               
PUTDNVX  DS    0H                                                               
         J     EXIT                                                             
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         USING BIND,R5                                                          
*                                                                               
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,0(R1)            A(ITEM)                                      
         GOTO1 ABINSRCH,DMCB,(X'01',(R3)),(R6)                                  
         OC    DMCB(4),DMCB                                                     
         JNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         JE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         JZ    BINXIT                NO BUCKETS - EXIT                          
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
*                                                                               
BINA10   DS    0H                                                               
         ICM   RF,15,0(R3)         BUCKET OF ITEM TO ADD                        
         ICM   RE,15,0(R4)         BUCKET OF ITEM IN TABLE                      
         AR    RE,RF                                                            
         STCM  RE,15,0(R4)         UPDATE BUCKET                                
*                                                                               
         LA    R3,DNVBLNQ(R3)      BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,DNVBLNQ(R4)      BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,BINA10                                                        
         DROP  R5                                                               
*                                                                               
BINXIT   J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* GET USER-ID INFO (SIGN ON NAME AND ADDRESS)                                   
***********************************************************************         
GUSERINF NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         XC    USERA,USERA         ALPHA USER ID                                
         XC    LTRNAME,LTRNAME                                                  
         XC    LTRADD,LTRADD                                                    
         XC    LTRADD3,LTRADD2                                                  
         XC    LTRADD3,LTRADD3                                                  
*                                                                               
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
*                                                                               
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,TWAORIG     USER ID NUMBER                               
*                                                                               
         MVC   SVIDKEY,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY,AIO3,MYDMWRK             
*                                                                               
         CLC   KEY(25),SVIDKEY                                                  
         BNE   GUSERIX                                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'CTFILE '),(X'02',AIO3),0,0                   
         CLI   12(R1),0                                                         
         BNE   GUSERIX                                                          
*                                                                               
         L     R5,12(R1)                                                        
*                                                                               
         ZIC   RF,1(R5)            ELEM LENGTH                                  
         SHI   RF,2                SUBTRACT OFF CODE/LENGTH                     
         SHI   RF,1                FOR EXMVC                                    
*                                                                               
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   USERA(0),2(R5)      MOVE IN DESCRIPTION                          
         OC    USERA,SPACES                                                     
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
*                                                                               
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,USERA        USER ID ALPHA                                
*                                                                               
         MVC   SVIDKEY,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY,AIO3,MYDMWRK             
*                                                                               
         CLC   KEY(25),SVIDKEY                                                  
         BNE   GUSERIX                                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'CTFILE '),(X'30',AIO3),0,0                   
         CLI   12(R1),0                                                         
         BNE   GUSERIX                                                          
*                                                                               
         L     R5,12(R1)                                                        
         USING CTDSTD,R5                                                        
*                                                                               
         MVC   LTRNAME,CTDSTNAM    ADDRESS NAME                                 
         MVC   LTRADD,CTDSTADD     ADDRESS                                      
         MVC   LTRADD2,CTDSTAD2    ADDRESS                                      
         MVC   LTRADD3,CTDSTAD3    ADDRESS                                      
         DROP  R5                                                               
*                                                                               
GUSERIX  DS    0H                                                               
         OC    LTRNAME,SPACES                                                   
         OC    LTRADD,SPACES                                                    
         OC    LTRADD2,SPACES                                                   
         OC    LTRADD3,SPACES                                                   
*                                                                               
         J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* PRINT DUNNING LETTER                                                          
***********************************************************************         
PRNTLTR  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ACURLTR                                                       
         USING DNVTABD,R6                                                       
         LA    R2,P                                                             
         USING LTRD,R2                                                          
*                                                                               
         TM    PRNTFLAG,FAXIT                                                   
         BZ    PRLT50                                                           
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         XC    P,P                                                              
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   CURSTA,DNVSTA                                                    
         GOTO1 CLUNPK,DMCB,DNVCLT,CLTA                                          
         BRAS  RE,PRNTFAX          PRINT FAX HEADERS                            
*                                                                               
PRLT50   DS    0H                                                               
         LA    R2,P                                                             
         OI    MYFLAG,PRLETTER     NOW PRINTING LETTERS                         
*                                                                               
         MVC   CLT,DNVCLT          CLIENT                                       
         BRAS  RE,GCLTINFO         GET CLIENT NAME                              
*                                                                               
         MVC   CLT,DNVCLT          CLIENT                                       
         MVC   PRDA,DNVPRD         PRODUCT                                      
         BRAS  RE,GPRDINFO         GET PRODUCT INFO                             
*                                                                               
         LA    R1,HEADING2                                                      
         ST    R1,SPECS                                                         
         LA    R1,HDRTN2                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   LINE,X'99'          START ON NEW PAGE                            
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   LTRDATA+30(8),SVLTRDAT  LETTER DATE                              
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ADDRECD,R4                                                       
*                                                                               
         MVI   0(R4),C'A'                                                       
         MVI   ADDKMED,C'N'        MEDIA                                        
         MVC   ADDKCALL,DNVSTA     STATION                                      
         MVC   ADDKAGY,AGENCY      AGENCY                                       
         MVC   ADDKFILL,=C'000000'                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO3,MYDMWRK             
         L     R4,AIO3                                                          
*                                                                               
         LA    R2,P                                                             
         MVC   LTRHEAD(2),=C'TO'                                                
         MVC   LTRDATA(20),ANAME   ADDRESS NAME                                 
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   LTRDATA(24),A1LINE  ADDRESS LINE 1                               
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   LTRDATA(24),A2LINE    CITY                                       
         MVC   LTRDATA+25(3),A3LINE  STATE CODE                                 
         MVC   LTRDATA+29(10),ABIGZIP ZIP CODE                                  
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
         DROP  R4                                                               
*                                                                               
         MVC   LTRHEAD(5),=C'ATTN:'                                             
         MVC   LTRDATA(25),SVATTN                                               
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   LTRHEAD(7),=C'STATION'                                           
         MVC   LTRDATA(5),DNVSTA                                                
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   LTRHEAD(6),=C'CLIENT'                                            
         GOTO1 CLUNPK,DMCB,DNVCLT,CLTA                                          
         MVC   LTRDATA(3),CLTA                                                  
         MVC   LTRDATA+6(20),CLTNAME                                            
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   LTRHEAD(7),=C'PRODUCT'                                           
         MVC   LTRDATA(3),DNVPRD                                                
         MVC   LTRDATA+6(20),PRDNAME                                            
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   LTRHEAD(8),=C'ESTIMATE'                                          
         EDIT  DNVEST,ESTA,ZERO=NOBLANK,ALIGN=LEFT                              
         MVC   LTRDATA(3),ESTA                                                  
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         TM    PRNTFLAG,NOPKG                                                   
         BO    PRLT70                                                           
*                                                                               
         MVC   LTRHEAD(7),=C'PACKAGE'                                           
         EDIT  DNVPKG,PKGA,ZERO=NOBLANK,ALIGN=LEFT                              
         MVC   LTRDATA(3),PKGA                                                  
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
PRLT70   DS    0H                                                               
         MVC   LTRHEAD(5),=C'MONTH'                                             
         XC    DUB,DUB                                                          
         MVC   DUB(1),DNVPER       YEAR                                         
         MVC   DUB+1(1),DNVPER+1   MONTH                                        
         MVI   DUB+2,X'01'         DAY                                          
         GOTO1 DATCON,DMCB,(3,DUB),(6,LTRDATA)                                  
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   LTRHEAD(5),=C'UNITS'                                             
         EDIT  DNVSPOTS,TEMPDISP,ALIGN=LEFT,ZERO=NOBLANK                        
         MVC   LTRDATA(10),TEMPDISP                                             
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         CLI   GNBA,C'N'           NET ONLY?                                    
         BE    PRLT90                                                           
*                                                                               
         MVC   LTRHEAD(5),=C'GROSS'                                             
         EDIT  DNVGROSS,TEMPDISP,2,ALIGN=LEFT,ZERO=NOBLANK                      
         MVC   LTRDATA(15),TEMPDISP                                             
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         CLI   GNBA,C'G'           GROSS ONLY?                                  
         BE    PRLT100                                                          
*                                                                               
PRLT90   DS    0H                                                               
         MVC   LTRHEAD(5),=C'NET  '                                             
         EDIT  DNVNET,TEMPDISP,2,ALIGN=LEFT,ZERO=NOBLANK                        
         MVC   LTRDATA(15),TEMPDISP                                             
         BRAS  RE,PRINTIT                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
PRLT100  DS    0H                                                               
         MVC   LTRHEAD(23),=C'TO WHOM IT MAY CONCERN:'                          
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   CLT,DNVCLT                                                       
         MVC   CLTOFF,DNVCOFF                                                   
         BRAS  RE,PRNTNVT          PRINT NVT/NVL RECORD                         
         DROP  R2,R6                                                            
*                                                                               
         TM    PRNTFLAG,FAXIT                                                   
         BZ    PRNTLTRX                                                         
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   P(26),=C'*** END OF DDS MESSAGE ***'                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   LINE,X'99'                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
PRNTLTRX DS    0H                                                               
         J     EXIT                                                             
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING2 DS    0H                                                               
*!!!!    SSPEC H1,43,AGYNAME                                                    
*!!!!    SSPEC H2,43,AGYADD                                                     
         DC    X'00'                                                            
*                                                                               
HDRTN2   NTR1                                                                   
         MVC   H1+42(33),LTRNAME          NAME                                  
         MVC   H2+42(33),LTRADD           ADDRESS                               
*                                                                               
         CLI   LTRADD2,X'40'                                                    
         BE    *+10                                                             
         MVC   H3+42(33),LTRADD2          ADDRESS                               
*                                                                               
         CLI   LTRADD2,X'40'                                                    
         BE    *+10                                                             
         MVC   H4+42(33),LTRADD3          ADDRESS                               
*                                                                               
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         MVI   BOXOFF,C'Y'                                                      
         DROP  R2                                                               
*                                                                               
HDRTN2X  DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
PRINTIT  NTR1  BASE=*,LABEL=*                                                   
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
*  PRINT NVT/NVL INFO                                                           
***********************************************************************         
PRNTNVT  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NVTRECD,R6                                                       
         LA    R2,P                                                             
         USING LTRD,R2                                                          
*                                                                               
         MVC   0(2,R6),=X'0D0C'                                                 
         MVC   COMKAGY,BAGYMD      AGY/MED                                      
         MVI   COMCTYPE,C'N'       NVT                                          
         MVC   COMKCLT,CLT         CLIENT                                       
*                                                                               
PNVT10   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(13),KEYSAVE                                                  
         JNE   PNVT20                                                           
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL ',KEY+14,AIO,MYDMWRK           
         J     PNVT50                                                           
*                                                                               
PNVT20   DS    0H                  FIND OFFICE LEVEL                            
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         MVC   0(2,R6),=X'0D0C'                                                 
         MVC   COMKAGY,BAGYMD      AGY/MED                                      
         MVI   COMCTYPE,C'N'       NVT                                          
*                                                                               
         MVI   COMKCLT,C'*'                                                     
         MVC   COMKCLT+1(1),CLTOFF                                              
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(13),KEYSAVE                                                  
         JNE   PNVT30              CHECK AGENCY LEVEL                           
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL ',KEY+14,AIO,MYDMWRK           
         J     PNVT50                                                           
*                                                                               
PNVT30   DS    0H                  FIND AGY LEVEL                               
         XC    KEY,KEY                                                          
*                                                                               
         MVC   0(2,R6),=X'0D0C'                                                 
         MVC   COMKAGY,BAGYMD      AGY/MED                                      
         MVI   COMCTYPE,C'N'       NVT                                          
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(13),KEYSAVE                                                  
         JNE   PNVL100             NO NVT, CHECK NVL                            
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL ',KEY+14,AIO,MYDMWRK           
*                                                                               
PNVT50   DS    0H                                                               
         L     R6,AIO                                                           
         MVC   DATADISP,=H'24'     SPOT FILE                                    
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         JE    PNVT56                                                           
         J     PRNTNVTX                                                         
*                                                                               
PNVT55   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   PRNTNVTX                                                         
*                                                                               
PNVT56   DS    0H                                                               
         MVC   ELEMLNQ,1(R6)                                                    
         LA    R4,2(R6)            RF POINTS TO DATA                            
*                                                                               
         CLI   2(R6),C'+'          SKIP A LINE                                  
         BNE   PNVT58                                                           
*                                                                               
         MVC   NUMSKIP,3(R6)       NUMBER OF LINES TO SKIP                      
         NI    NUMSKIP,X'0F'                                                    
         ZIC   R5,NUMSKIP                                                       
*                                                                               
PNVT57   DS    0H                                                               
         BRAS  RE,PRINTIT                                                       
         BCT   R5,PNVT57                                                        
*                                                                               
         LA    R4,2(R4)            POINT PAST "+1"                              
*                                                                               
         CLC   0(3,R4),=C'***'     IF LINE OF *, THEN INSERT 2 EXTRA            
         BNE   *+10                                                             
         MVC   LTRHEAD(2),=C'**'                                                
*                                                                               
         LA    R5,LTRHEAD+2                                                     
*                                                                               
         ZIC   RF,ELEMLNQ                                                       
         SHI   RF,2                                                             
         STC   RF,ELEMLNQ                                                       
         B     *+8                                                              
*                                                                               
PNVT58   DS    0H                                                               
         LA    R5,LTRHEAD                                                       
*                                                                               
         ZIC   RF,ELEMLNQ          ELEM LENGTH                                  
         SHI   RF,2                SUBTRACT OFF CODE/LENGTH                     
         SHI   RF,1                FOR EXMVC                                    
*                                                                               
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE IN DESCRIPTION                          
*                                                                               
         BRAS  RE,PRINTIT                                                       
         J     PNVT55                                                           
*                                                                               
PNVL100  DS    0H                                                               
         DROP  R2,R6                                                            
*                                                                               
PRNTNVTX DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
*  GET PRODUCT INFO                                                             
***********************************************************************         
GPRDINFO NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             GET CLIENT RECORD                            
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,CLT                                                      
         MVC   PKEYPRD,PRDA                                                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(13),KEYSAVE                                                  
         JNE   INVLPRD                                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL ',KEY+14,AIO,MYDMWRK           
*                                                                               
         L     R6,AIO                                                           
         MVC   PRDNAME,PNAME       PRODUCT NAME                                 
         DROP  R6                                                               
*                                                                               
GPRDINFX DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
*  GET CLIENT INFO                                                              
***********************************************************************         
GCLTINFO NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             GET CLIENT RECORD                            
         LA    R6,KEY                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,CLT                                                      
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(13),KEYSAVE                                                  
         JNE   INVLCLI                                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL ',KEY+14,AIO,MYDMWRK           
*                                                                               
         L     R6,AIO                                                           
         MVC   CLTNAME,CNAME       CLIENT NAME                                  
         MVC   CLTOFF,COFFICE      OFFICE CODE                                  
         DROP  R6                                                               
*                                                                               
GCLTINFX DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* PRINT INFORMATION                                                             
***********************************************************************         
PRLINE   NTR1  BASE=*,LABEL=*                                                   
         L     R6,ASORTREC                                                      
         USING SORTRECD,R6                                                      
         LA    R2,P+10                                                          
         USING PLINED,R2                                                        
*                                                                               
         XC    P,P                                                              
         OC    P,SPACES                                                         
*                                                                               
         MVC   P(4),=C'GET*'                                                    
*                                                                               
         MVC   PSTA(5),SSTA           STATION                                   
         EDIT  SPKG,PPKG,ZERO=NOBLANK MARKET #                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),SPERIOD+1    YEAR                                         
         MVC   DUB+1(1),SPERIOD    MONTH                                        
         MVI   DUB+2,X'01'         DAY                                          
         GOTO1 DATCON,DMCB,(3,DUB),(6,PPERIOD)                                  
*                                                                               
         MVC   PPRD(3),SPRD             PRODUCT                                 
         EDIT  SEST,PEST,ZERO=NOBLANK   ESTIMATE                                
*                                                                               
         EDIT  SGROSS,PGROSS,2,ALIGN=RIGHT,ZERO=NOBLANK                         
         EDIT  SNET,PNET,2,ALIGN=RIGHT,ZERO=NOBLANK                             
*                                                                               
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R2,R6                                                            
*                                                                               
PRLINEX  DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* GET PRODUCT ALPHA                                                             
***********************************************************************         
GETPRDA  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO3                                                         
*                                                                               
         L     RF,AIO              MAKE SURE WE HAVE THE CLIENT RECORD          
         CLI   0(RF),0             IN AIO3                                      
         BNE   GPRDA05                                                          
         CLC   1(1,RF),BAGYMD                                                   
         BNE   GPRDA05                                                          
         CLC   2(2,RF),CLT                                                      
         BNE   GPRDA05                                                          
*                                                                               
         TM    MYFLAG,NEWCLT                                                    
         JZ    GPRDA10                                                          
*                                                                               
GPRDA05  DS    0H                                                               
         GOTO1 CLUNPK,DMCB,CLT,CLTA                                             
*                                                                               
         MVI   BYTE,C'A'                                                        
         GOTO1 GETCLT,DMCB,CLTA    GET CLIENT RECORD AND SVCLIST                
         NI    MYFLAG,X'FF'-NEWCLT                                              
*                                                                               
GPRDA10  DS    0H                                                               
         LA    R6,SVCLIST                                                       
         LA    R5,MAXPRD1          MAX # OF PRODUCTS                            
*                                                                               
GPRDA20  DS    0H                                                               
         OC    0(4,R6),0(R6)                                                    
         JZ    GPRDA25             NOT IN CLIST, CHECK CLIST2                   
*                                                                               
         CLC   3(1,R6),FULL+3                                                   
         JE    GPRDA30                                                          
*                                                                               
         LA    R6,4(R6)                                                         
         BCT   R5,GPRDA20                                                       
*                                                                               
GPRDA25  DS    0H                                                               
         LA    R6,SVCLIST2                                                      
         LA    R5,MAXPRD2          MAX # OF PRODUCTS                            
*                                                                               
GPRDA27  DS    0H                                                               
         OC    0(4,R6),0(R6)                                                    
         JZ    GETPRDAX            NOT IN CLIST2 EITHER                         
*                                                                               
         CLC   3(1,R6),FULL+3                                                   
         JE    GPRDA30                                                          
*                                                                               
         LA    R6,4(R6)                                                         
         BCT   R5,GPRDA27                                                       
         J     GETPRDAX                                                         
*                                                                               
GPRDA30  DS    0H                                                               
         MVC   FULL(3),0(R6)       SAVE ALPHA PRODUCT                           
*                                                                               
GETPRDAX DS    0H                                                               
         MVC   AIO,AIO1                                                         
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
*    GET INVOICE ACTIVITY DATE                                                  
***********************************************************************         
GETNINV  NTR1  BASE=*,LABEL=*                                                   
         XC    TEMPH,TEMPH                                                      
         LA    R2,TEMPH                                                         
         MVI   0(R2),X'0C'         HDR(8)+NET(4)                                
         MVI   5(R2),X'04'         L'NET                                        
         MVC   8(4,R2),SVSTA                                                    
*                                                                               
         GOTO1 VALINTWK                                                         
*                                                                               
         XC    SVACTDAT,SVACTDAT                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SNVKEYD,R4                                                       
*                                                                               
         MVC   SNVKEY(2),=XL2'0E03'                                             
         MVC   SNVKAM,BAGYMD       AGY/MED                                      
         MVC   SNVKCLT,SVCLT       CLIENT                                       
         MVC   SNVKSTA,BMKTSTA+2   PACKED STATION CODE                          
         MVC   SNVKMOS,SVMOS       COMPRESSED YYMM01                            
         MVC   SNVKINV,SVNINV      INVOICE #                                    
         MVC   SNVKMINK,=XL6'FFFFFFFFFFFF'                                      
*                                                                               
         MVC   SVXSPKEY,KEY                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR ',KEY,KEY,MYDMWRK              
         CLI   DMCB+8,0            ANY PROBLEMS FINDING IT?                     
         BE    GETNI20                                                          
*                                                                               
         GOTO1 HEXOUT,DMCB,SVXSPKEY,P,L'SVXSPKEY,=C'TOG',0                      
         BRAS  RE,PRINTIT                                                       
         GOTO1 HEXOUT,DMCB,KEY,P,L'SVXSPKEY,=C'TOG',0                           
         BRAS  RE,PRINTIT                                                       
         DC    H'00'                                                            
*                                                                               
GETNI20  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL ',KEY+36,AIO2,MYDMWRK          
         DROP  R4                                                               
*&&DO                                                                           
         L     R4,AIO2                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'NINV',0(R4),C'DUMP',2000,=C'1D'               
*&&                                                                             
         TM    FILTFLAG,INVORIG    USE ORIGINAL ACTIVITY DATE?                  
         BZ    GETNI30                                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'XSPFIL '),(X'10',AIO2),0,0                   
         CLI   12(R1),0                                                         
         BNE   GETNI50                                                          
*                                                                               
         L     R4,12(R1)                                                        
         USING SNVHDELD,R4                                                      
*                                                                               
         MVC   SVACTDAT,SNVHDCDT   CREATION DATE                                
         B     GETNI50                                                          
         DROP  R4                                                               
*                                                                               
GETNI30  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'XSPFIL '),(X'E8',AIO2)                       
         CLI   12(R1),0                                                         
         BNE   GETNI50                                                          
*                                                                               
         L     R4,12(R1)                                                        
         USING SNVMMELD,R4                                                      
*                                                                               
         MVC   SVACTDAT,SNVMMDAT   LATEST ACTIVITY DATE                         
         DROP  R4                                                               
*                                                                               
GETNI50  DS    0H                                                               
         XC    KEY,KEY             RESTORE UNIT KEY                             
         MVC   KEY(20),SVUNTKEY                                                 
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
GETNINVX DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
*    GET NN PROFILE                                                             
***********************************************************************         
GETPROS  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTRECD,R4                                                       
*                                                                               
         MVC   CKEYAM,BAGYMD       AGY/MED                                      
         MVC   CKEYCLT,SVCLT       CLIENT                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL ',KEY+14,AIO3,MYDMWRK          
*                                                                               
         L     R4,AIO3                                                          
         MVC   SVCOFF,COFFICE                                                   
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),=C'S0NN'                                                  
         MVC   KEY+4(2),AGENCY     AGENCY                                       
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),SVCLTA     CLIENT                                       
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),SVCOFF    CLIENT OFFICE CODE                           
*                                                                               
         GOTO1 GETPROF,DMCB,KEY,WORK,DATAMGR                                    
*                                                                               
         CLI   WORK+1,C'N'                                                      
         BNE   *+8                                                              
         OI    FILTFLAG,INVORIG    GET ORIGINAL ACTIVITY DATE                   
*                                                                               
         XC    KEY,KEY             RESTORE UNIT KEY                             
         MVC   KEY(20),SVUNTKEY                                                 
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
GETPROSX DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* BUILD LIST OF PRODUCTS FOR THIS UNIT W/ PERCENTAGES                           
***********************************************************************         
BLDPRLST NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
*                                                                               
         LA    R2,PRDLIST                                                       
         MVC   0(2,R2),=X'FFFF'                                                 
*                                                                               
         DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'19'                                                     
         BRAS  RE,GETEL                                                         
         JNE   BPR100              UNALLOCATED UNIT - SKIP                      
         USING NUPDED,R6                                                        
*                                                                               
*  PULL PRODUCT INFO FROM THE 19 ELEMENT                                        
*                                                                               
         SR    R4,R4                                                            
         ZIC   R5,NUPDELEN         GET # OF PRODUCTS                            
         SHI   R5,3                                                             
         D     R4,=F'7'                                                         
*                                                                               
         LA    R6,NUPDEPR          PRODUCT CODE                                 
*                                                                               
BPR20    DS    0H                                                               
         MVI   0(R2),0                                                          
         MVC   1(3,R2),0(R6)       PRODUCT CODE                                 
         TM    NUPDEIND,X'C0'      CHECK IF COPY/TRIGGY                         
         BZ    *+10                                                             
         MVC   4(2,R2),3(R6)       PRODUCT PERCENTAGE                           
*                                                                               
         LA    R2,6(R2)                                                         
         MVC   0(2,R2),=X'FFFF'                                                 
*                                                                               
         LA    R6,7(R6)            BUMP TO NEXT PRODUCT                         
         BCT   R5,BPR20                                                         
*  IF UNIT NOT A COPYSPLIT OR PIGGYBACK                                         
*  PULL THE PRODUCT PERCENTAGES FROM THE 01 ELEMENT                             
         TM    NUPDEIND,X'C0'      CHECK IF COPY/TRIGGY                         
         BNZ   BLDPRX                                                           
         LA    R2,PRDLIST                                                       
         DROP  R6                                                               
*  CHECK IF FIRST PRODUCT INPUTTED                                              
         CLC   0(2,R2),=X'FFFF'                                                 
         BE    BLDPRX                                                           
*                                                                               
         L     R6,AIO                                                           
         USING NURECD,R6                                                        
         MVC   4(2,R2),NUP1SHR     PRODUCT PERCENTAGE                           
*                                                                               
         CLC   6(2,R2),=X'FFFF'     IF THERE'S NO 2ND PRODUCT                   
         JNE   BPR40               AND THE 1ST HAS NO PERCENTAGE                
         OC    NUP1SHR,NUP1SHR     THEN DEFAULT TO 100 %                        
         JNZ   BPR60                                                            
         MVC   4(2,R2),=X'2710'                                                 
         J     BPR60                                                            
*                                                                               
BPR40    DS    0H                                                               
         OC    NUP1SHR,NUP1SHR     BUT IF THERE IS A 2ND PRODUCT                
         JNZ   BPR60               WITHOUT A 1ST %                              
         MVC   4(2,R2),=X'1388'    THEN IT'S 50/50                              
*                                                                               
BPR60    DS    0H                                                               
         MVC   SV1SHR,4(R2)                                                     
*                                                                               
         LA    R2,6(R2)                                                         
         CLC   0(2,R2),=X'FFFF'    ANY 2ND PRODUCT?                             
         JE    BLDPRX                                                           
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         SR    R4,R4               GET 2ND PRODUCT PERCENTAGE                   
         SR    R5,R5                                                            
*                                                                               
         AH    R4,=H'10000'                                                     
         ICM   R5,3,SV1SHR                                                      
         SR    R4,R5                                                            
*                                                                               
         STCM  R4,3,DUB                                                         
         MVC   4(2,R2),DUB         2ND  PRODUCT PERCENTAGE                      
*                                                                               
         LA    R2,6(R2)                                                         
         MVC   0(2,R2),=X'FFFF'                                                 
         J     BLDPRX                                                           
         DROP  R6                                                               
*                                                                               
*  PULL PRODUCT INFO FROM THE 01 ELEMENT                                        
*                                                                               
BPR100   L     R6,AIO                                                           
         USING NURECD,R6                                                        
         MVC   CLT,NUKCLT                                                       
*                                                                               
         CLI   NUPRD,0                                                          
         JE    BPR250              CHECK X'14' ELEMENTS                         
*                                                                               
         MVC   0(1,R2),NUPRD       PRODUCT EQUATE                               
*                                                                               
         MVC   FULL+3(1),NUPRD                                                  
         BRAS  RE,GETPRDA                                                       
         MVC   1(3,R2),FULL        PRODUCT ALPHA                                
*                                                                               
         L     R6,AIO                                                           
         MVC   4(2,R2),NUP1SHR     PRODUCT PERCENTAGE                           
*                                                                               
         CLI   NUPRD2,0            IF THERE'S NO 2ND PRODUCT                    
         JNE   BPR120              AND THE 1ST HAS NO PERCENTAGE                
         OC    NUP1SHR,NUP1SHR     THEN DEFAULT TO 100 %                        
         JNZ   BPR150                                                           
         MVC   4(2,R2),=X'2710'                                                 
         J     BPR150                                                           
*                                                                               
BPR120   DS    0H                                                               
         OC    NUP1SHR,NUP1SHR     BUT IF THERE IS A 2ND PRODUCT                
         JNZ   BPR150              WITHOUT A 1ST %                              
         MVC   4(2,R2),=X'1388'    THEN IT'S 50/50                              
*                                                                               
BPR150   DS    0H                                                               
         MVC   SV1SHR,4(R2)                                                     
*                                                                               
         LA    R2,6(R2)                                                         
         MVC   0(2,R2),=X'FFFF'                                                 
*                                                                               
         L     R6,AIO                                                           
         CLI   NUPRD2,0            ANY 2ND PRODUCT?                             
         JE    BLDPRX                                                           
*                                                                               
         MVC   0(1,R2),NUPRD2      PRODUCT EQUATE                               
*                                                                               
         MVC   FULL+3(1),NUPRD2                                                 
         BRAS  RE,GETPRDA                                                       
         MVC   1(3,R2),FULL        PRODUCT ALPHA                                
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         SR    R4,R4               GET 2ND PRODUCT PERCENTAGE                   
         SR    R5,R5                                                            
*                                                                               
         AH    R4,=H'10000'                                                     
         ICM   R5,3,SV1SHR                                                      
         SR    R4,R5                                                            
*                                                                               
         STCM  R4,3,DUB                                                         
         MVC   4(2,R2),DUB         2ND  PRODUCT PERCENTAGE                      
*                                                                               
         LA    R2,6(R2)                                                         
         MVC   0(2,R2),=X'FFFF'                                                 
         J     BLDPRX                                                           
         DROP  R6                                                               
*                                                                               
*  PULL PRODUCT INFO FROM THE 14 ELEMENT                                        
*                                                                               
BPR250   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'14'                                                     
         BRAS  RE,GETEL                                                         
         JNE   BLDPRX              UNALLOCATED UNIT - SKIP                      
         USING NUPRDD,R6                                                        
*                                                                               
         SR    R4,R4                                                            
         ZIC   R5,NUPRDLEN         GET # OF PRODUCTS                            
         SHI   R5,3                                                             
         D     R4,=F'6'                                                         
*                                                                               
         LA    R6,NUPRDPR          PRODUCT CODE                                 
*                                                                               
BPR255   DS    0H                                                               
         MVC   0(1,R2),0(R6)       PRODUCT CODE                                 
         MVC   4(2,R2),NUPRDPCT    PRODUCT PERCENTAGE                           
*                                                                               
         MVC   FULL+3(1),0(R6)                                                  
         BRAS  RE,GETPRDA                                                       
         MVC   1(3,R2),FULL        PRODUCT ALPHA                                
*                                                                               
         LA    R2,6(R2)                                                         
         MVC   0(2,R2),=X'FFFF'                                                 
*                                                                               
         LA    R6,6(R6)            BUMP TO NEXT PRODUCT                         
         BCT   R5,BPR255                                                        
         DROP  R6                                                               
*                                                                               
BLDPRX   DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM9ED                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
*******  ORG   SYSSPARE+220                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C49 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
*                           *******  T31C49 WORK AREA  *******                  
LTRNAME  DS    CL33                NAME FOR LETTER                              
LTRADD   DS    CL33                ADDRESS FOR LETTER                           
LTRADD2  DS    CL33                ADDRESS FOR LETTER                           
LTRADD3  DS    CL33                ADDRESS FOR LETTER                           
*                                                                               
MYBLOCK  DS    XL150                                                            
*                                                                               
RELO     DS    F                                                                
MYDMWRK  DS    12D                                                              
*                                                                               
PREVMKT  DS    XL2                 PREVIOUS MARKET                              
PREVSTA  DS    CL5                 PREVIOUS STATION                             
CURSTA   DS    CL5                 CURRENT STATION BEING PRINTED                
*                                                                               
PREVCLT  DS    XL2                                                              
*                                                                               
PREVREC  DS    XL60                BASE SORT RECORD TO COMPARE WITH             
ASORTREC DS    A                   A('GET' SORT REC)                            
*                                                                               
SVSPTKEY DS    XL13                                                             
SVUNTKEY DS    XL20                                                             
SVXSPKEY DS    XL32                                                             
SVFAXKEY DS    XL25                                                             
SVIDKEY  DS    XL25                                                             
SVAREC   DS    A                   A(RECORD)                                    
*                                                                               
SVDATE   DS    XL2                 AFFID DATE                                   
SVTIME   DS    H                   START TIME (MILITARY)                        
*                                                                               
TOTGROSS DS    F                   GROSS                                        
TOTNET   DS    F                   NET                                          
TOTPAID  DS    F                   PAID (TIME ONLY)                             
TOTUNPD  DS    F                   ACTUAL - PAID                                
NUMMATCH DS    F                                                                
TOTUNMAT DS    F                                                                
*                                                                               
GROSSP   DS    PL8                 TOTAL GROSS COST                             
NETP     DS    PL8                 TOTAL NET COST                               
TOTUNITS DS    F                   TOTAL # OF UNITS                             
*                                                                               
TEMPP    DS    PL8                 TEMP PACKED #                                
*                                                                               
SUBGP    DS    PL8                 SUBTOTAL GROSS COST                          
SUBNP    DS    PL8                 SUBTOTAL NET COST                            
SUBTU    DS    F                   SUBTOTAL # OF UNITS                          
*                                                                               
NUMSPOTS DS    F                   # OF SPOTS                                   
*                                                                               
MYGROSS  DS    F                   GROSS                                        
MYNET    DS    F                   NET                                          
MYPAID   DS    F                   PAID (TIME ONLY)                             
MYUNPAID DS    F                   ACTUAL - PAID                                
MYMATCH  DS    F                                                                
MYUNMAT  DS    F                                                                
*                                                                               
SV1SHR   DS    XL2                 1ST % SHARE                                  
*                                                                               
ATABLE   DS    A                   A(UPPER STORAGE TABLE)                       
*                                                                               
*                                                                               
CLTBLK   DS    0X                                                               
CLT      DS    XL2                 CLIENT                                       
CLTA     DS    CL3                 CLIENT ALPHA                                 
CLTNAME  DS    CL20                CLIENT NAME                                  
CLTOFF   DS    CL1                 CLIENT OFFICE                                
CLTBLKLN EQU   *-CLTBLK                                                         
*                                                                               
SVOFFICE DS    CL1                 OFFICE CODE                                  
SVOFFILT DS    CL1                 OFFICE CODE                                  
*                                                                               
PRDBLK   DS    0X                                                               
PRD      DS    XL1                 PRODUCT                                      
PRDA     DS    CL3                 PRODUCT ALPHA                                
PRDNAME  DS    CL20                PRODUCT NAME                                 
PRDBLKLN EQU   *-PRDBLK                                                         
*                                                                               
SVPGRNUM DS    XL2                 PRODUCT GROUP NUMBER                         
*                                                                               
TEMPDISP DS    XL15                                                             
*                                                                               
ACURPRD  DS    A                   A(CURRENT PRD IN UNIT PRD LIST)              
ACURLTR  DS    A                   A(CURRENT DUNNING LETTER TO PRINT)           
*                                                                               
ABINSRCH DS    A                   A(BINSRCH)                                   
*                                                                               
DNVTBLN  DS    F                                                                
ADNVTAB  DS    A                   A(BINSRCH TABLE)                             
*                                                                               
SVPRDPCT DS    H                   PRODUCT PERCENTAGE OF DOLLARS                
*                                                                               
NETA     DS    CL4                 NETWORK ALPHA                                
MKTP     DS    XL2                 MARKET PACKED                                
STAP     DS    XL3                 STATION PACKED                               
*                                                                               
ESTBLK   DS    0X                                                               
EST1     DS    XL1                 ESTIMATE START RANGE                         
EST2     DS    XL1                 ESTIMATE END RANGE                           
EST      DS    XL1                 ESTIMATE                                     
ESTA     DS    CL7                 ESTIMATE ALPHA                               
ESTBLKLN EQU   *-ESTBLK                                                         
*                                                                               
TMPEST   DS    CL3                                                              
ESTXXX   DS    CL6                 ESTIMATE XXX FILTER                          
*                                                                               
PKGA     DS    CL3                 PACKAGE ALPHA                                
*                                                                               
GNBA     DS    CL1                 GROSS/NET/BOTH                               
RLA      DS    CL1                 REPORT/LETTERS                               
*                                                                               
STARTE   DS    CL6                 START DATE (EBCDIC YYMMDD)                   
ENDE     DS    CL6                 END DATE (EBCDIC YYMMDD)                     
*                                                                               
PERIODB  DS    XL2                 BROADCAST PERIOD (COMPRESSED)                
*                                                                               
STARTC   DS    XL2                 START DATE (COMPRESSED)                      
ENDC     DS    XL2                 END DATE (COMPRESSED)                        
PERIOD   DS    CL17                DISPLAYABLE PERIOD (AFTER PERVAL)            
*                                                                               
SVLTRDAT DS    CL8                 DUNNING LETTER DATE                          
*                                                                               
PRNTFLAG DS    XL1                 PRINT FLAG                                   
LETTERS  EQU   X'01'               PRINT DUNNING LETTERS                        
FAXIT    EQU   X'02'               FAX REPORT AND/OR LETTERS                    
FAXHEAD  EQU   X'04'               FAX HEADER LINES                             
PRNTREP  EQU   X'08'               ALREADY STARTED REPORT                       
NOPKG    EQU   X'10'               DO NOT PRINT PACKAGE INFO                    
*                                                                               
MYFLAG   DS    XL1                                                              
NEWCLT   EQU   X'01'               NEW CLIENT                                   
LASTREC  EQU   X'02'               LAST RECORD FROM SORTER                      
GOTONE   EQU   X'04'               GOT AT LEAST ONE UNIT                        
PRLETTER EQU   X'08'               NOW PRINTING LETTERS                         
XXXFILT  EQU   X'10'               FILTER ESTIMATE BY XXX                       
PARTMNTH EQU   X'20'               PARTIAL MONTHS - BROADCAST DNE               
FILTFLAG DS    XL1                 FILTER FLAGS                                 
ALLCLT   EQU   X'01'               ALL CLIENTS                                  
ALLPRD   EQU   X'02'               ALL PRODUCTS                                 
ALLEST   EQU   X'04'               ALL ESTIMATES                                
SEPEST   EQU   X'08'               SEPERATE PAGE BY EST                         
BRDCAST  EQU   X'10'               BROADCAST DATES                              
INVORIG  EQU   X'20'               INVOICE ORIGINAL ACTIVITY DATE               
*                                                                               
STAFLAG  DS    XL1                                                              
ALLSTA   EQU   X'01'               ALL STATIONS                                 
BRDSTA   EQU   X'02'               BROADCAST STATIONS ONLY                      
CBLSTA   EQU   X'04'               CABLE STATIONS ONLY                          
SYNDSTA  EQU   X'08'               CABLE STATIONS ONLY                          
OTHSTA   EQU   X'10'               CABLE STATIONS ONLY                          
*                                                                               
SVMKT    DS    XL2                 MARKET #                                     
SVSTA    DS    CL5                 STATION                                      
SVPKG    DS    XL1                 PACKAGE                                      
SVPERIOD DS    XL2                 DATE MM/YY                                   
SVPRD    DS    CL3                 PRODUCT ALPHA                                
SVEST    DS    XL1                 ESTIMATE                                     
SVGROSS  DS    XL4                 GROSS                                        
SVNET    DS    XL4                 NET                                          
SVMATCH  DS    XL4                 MATCH                                        
SVCLEAR  DS    XL4                 CLEAR                                        
SVACTDAT DS    XL2                 ACT DATE (COMPRESSED)                        
SVPAYGRS DS    XL4                 GROSS PAID                                   
SVPAYNET DS    XL4                 NET PAID                                     
SVCLT    DS    XL2                 CLIENT                                       
SVFLAG   DS    XL1                                                              
*                                                                               
SVCLTA   DS    CL3                 CLIENT (ALPHA)                               
SVCOFF   DS    CL1                 CLIENT OFFICE CODE                           
*                                                                               
ACTDATE  DS    XL3                 ACTIVITY DATE OF CURRENT UNITS               
*                                                                               
TEMPH    DS    XL20                TEMPORARY SCREEN FIELD                       
*                                                                               
SVNINV   DS    CL10                INVOICE NUMBER                               
SVMOS    DS    XL2                 COMPRESSED YYMM01                            
*                                                                               
SVFAXNUM DS    CL16                FAX NUMBER                                   
SVATTN   DS    CL25                ATTENTION                                    
*                                                                               
LASTPRNT DS    XL9                                                              
*                                                                               
DNVREC   DS    XL(DNVLNQ)          BINSRCH RECORD                               
*                                                                               
ELEMLNQ  DS    XL1                                                              
NUMSKIP  DS    XL1                 NUMBER OF LINES TO SKIP                      
*                                                                               
SORTREC  DS    XL60                SORT RECORD                                  
PRDLIST  DS    CL44                LIST OF UNIT'S PRODUCTS                      
*                                                                               
USERA    DS    CL10                ALPHA USER ID                                
*                                                                               
PRDTABLE DS    XL1200              TABLE OF PRODUCTS TO FILTER ON               
*                           *******  T31C49 WORK AREA  *******                  
WORKEND  EQU   *                                                                
         EJECT                                                                  
*                                                                               
SORTRECD DSECT                                                                  
SSTA     DS    CL5                 STATION                                      
SPKG     DS    XL1                 PACKAGE                                      
SPERIOD  DS    XL2                 DATE YY/MM                                   
SEST     DS    XL1                 ESTIMATE                                     
SPRD     DS    CL3                 PRODUCT ALPHA                                
SCLT     DS    XL2                 CLIENT                                       
SGROSS   DS    XL4                 GROSS                                        
SNET     DS    XL4                 NET                                          
SMATCH   DS    XL4                 MATCH                                        
SCLEAR   DS    XL4                 CLEAR                                        
SACTDATE DS    XL3                 ACT DATE                                     
SFLAG    DS    XL1                 FLAGS                                        
SMAT     EQU   X'01'               UNIT IS MATCHED                              
SCOFF    DS    CL1                 CLIENT OFFICE                                
SRECLNQ  EQU   *-SSTA                                                           
*                                                                               
LTRD     DSECT                                                                  
LTRHEAD  DS    CL10                HEADER                                       
         DS    CL2                                                              
LTRDATA  DS    CL50                DATA                                         
LTRLNQ   EQU   *-LTRHEAD                                                        
*                                                                               
PLINED   DSECT                                                                  
         DS    CL3                                                              
PSTA     DS    CL7                 STATION                                      
         DS    CL2                                                              
PPKG     DS    CL3                 PACKAGE                                      
         DS    CL2                                                              
PPERIOD  DS    CL6                 PERIOD                                       
         DS    CL2                                                              
PCLT     DS    CL3                 CLIENT                                       
         DS    CL2                                                              
PPRD     DS    CL7                 PRODUCT                                      
         DS    CL2                                                              
PEST     DS    CL3                 ESTIMATE                                     
         DS    CL2                                                              
PUNITS   DS    CL5                 SPOTS                                        
         DS    CL2                                                              
PGROSS   DS    CL15                GROSS                                        
         DS    CL2                                                              
PNET     DS    CL15                NET                                          
         DS    CL2                                                              
PMATCH   DS    CL5                 MATCH                                        
         DS    CL2                                                              
PCLEAR   DS    CL5                 CLEAR                                        
         DS    CL2                                                              
PACTDATE DS    CL8                 ACT DATE                                     
PLNQ     EQU   *-PLINED                                                         
*                                                                               
TOTD     DSECT                                                                  
TOTHEAD  DS    CL11                HEADING                                      
         DS    CL5                                                              
TOTDATA  DS    CL15                TOTAL VALUES                                 
*                                                                               
DNVTABD  DSECT                                                                  
DNVCLT   DS    XL2                 CLIENT                                       
DNVSTA   DS    CL5                 STATION                                      
DNVPRD   DS    CL3                 PRODUCT                                      
DNVEST   DS    XL1                 ESTIMATE                                     
DNVPKG   DS    XL1                 PACKAGE                                      
DNVPER   DS    XL2                 PERIOD (MM/YY BINARY)                        
DNVCOFF  DS    CL1                 CLIENT OFFICE                                
DNVKLNQ  EQU   *-DNVCLT                                                         
*                                                                               
DNVBUCK  DS    0C                                                               
DNVSPOTS DS    XL4                 # OF SPOTS                                   
DNVBLNQ  EQU   *-DNVSPOTS                                                       
*                                                                               
DNVGROSS DS    XL4                 GROSS $                                      
DNVNET   DS    XL4                 NET $                                        
DNVCLEAR DS    XL4                 CLEARED $                                    
DNVBCNT  EQU   (*-DNVBUCK)/DNVBLNQ                                              
DNVLNQ   EQU   *-DNVCLT                                                         
*                                                                               
DNVMAX   EQU   60000                # OF TABLE ENTRIES                          
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
*                                                                               
FAXHDRD  DSECT                                                                  
FAXOID   DS    CL4                 ORIGIN ID                                    
FAXHDR   DS    CL5                 *HDR*                                        
FAXDEST  DS    CL25                REPORT DESTINATION                           
FAXWIDE  DS    CL1                 C'W' - WIDE REPORT                           
FAXP     DS    CL1                 C'P' - REPLACE X'89'                         
FAXS     DS    CL1                 C'S' - STRIP QUOTES                          
FAXD     DS    CL1                 C'D' - DATASET NAME FOLLOWS IN ++DDS         
FAXDESTN DS    CL16                FORMATED DESTINATION NAME                    
FAXEBILL DS    CL13                EASYLINK BILLING INFO                        
FAXDARE  DS    CL1                 C'D' - DARE REPORT                           
FAXLAND  DS    CL1                 C'L' - LANDSCAPE ORIENTATION FOR FAX         
FAXEMAIL DS    CL1                 C'M' - EMAIL ADDRESS INFO IN ++DDS           
FAXCOVER DS    CL1                 C'N' - DON'T GENERATE COVER PAGE             
         DS    CL2                 SPARE                                        
FAXHLNQ  EQU   *-FAXHDRD                                                        
*                                                                               
FAXDDSD  DSECT                                                                  
FAXDDS   DS    CL5                 ++DDS                                        
         DS    CL1                 BLANK                                        
FAXDSYS  DS    CL2                 SYSTEM (NE)                                  
FAXDRTYP DS    CL3                 REPORT TYPE (NNN)                            
FAXDTRN  DS    CL3                 TRN                                          
         DS    CL1                                                              
FAXDDATA DS    CL58                                                             
FAXDLNQ  EQU   *-FAXDDSD                                                        
*                                                                               
FAXDATAD DSECT                                                                  
FDMED    DS    CL1                 MEDIA                                        
FDCLT    DS    CL3                 CLIENT                                       
FDPRD    DS    CL11                PRODUCT                                      
FDSTA    DS    CL5                 STATION CODE                                 
FDPERIOD DS    CL17                PERIOD                                       
FDEST    DS    CL8                 ESTIMATE                                     
*                                                                               
*                                                                               
*                                                                               
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
NVTRECD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE NEGENUNIT                                                      
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115NESFM49   03/22/06'                                      
         END                                                                    
