*          DATA SET SPNWS26    AT LEVEL 022 AS OF 02/27/07                      
*PHASE T20726C,*                                                                
T20726   TITLE 'SPNWS26 - BUYER''S WORKSHEET - GOAL/LIST OVERLAY'               
T20726   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20726**,RR=RE                                                 
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         USING WORKD,R7                                                         
*                                                                               
         L     RC,APALOCAL                                                      
         USING LOCALD,RC                                                        
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO                                                        
*                                                                               
         CLI   APMODE,APMVALP      VALIDATE KEY FOR LIST                        
         BE    VALPAR                                                           
         CLI   APMODE,APMGETS      GET RECORD FOR LIST                          
         BE    GETSEL                                                           
         CLI   APMODE,APMDISS      DISPLAY RECORD FOR LIST                      
         BE    DISSEL                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FOR LIST                                                         
***********************************************************************         
*                                                                               
VALPAR   DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG   KEY FIELDS NOT CHANGED YET             
*****                                                                           
* VALIDATE THE MEDIA                                                            
*****                                                                           
         TM    GLLMEDH+4,FVIVAL    FIELD VALIDATED PREVIOUSLY?                  
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO                                           
*                                                                               
         XC    GLLMDN,GLLMDN                                                    
         OI    GLLMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,GLLMEDH                                                  
         BNE   VPARX                                                            
         OI    GLLMEDH+4,FVIVAL    FIELD VALIDATED                              
         MVC   SVBAGYMD,BAGYMD                                                  
         MVC   GLLMDN,MEDNM                                                     
*****                                                                           
* VALIDATE THE BUYER                                                            
*****                                                                           
         TM    GLLBYRH+4,FVIVAL    FIELD VALIDATED PREVIOUSLY?                  
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO                                           
*                                                                               
         XC    GLLBYN,GLLBYN                                                    
         OI    GLLBYNH+6,FVOXMT                                                 
         GOTO1 AVALBYR,GLLBYRH                                                  
         BNE   VPARX                                                            
         OI    GLLBYRH+4,FVIVAL    FIELD VALIDATED                              
         MVC   SVBBUYER,BBYR                                                    
         MVC   GLLBYN,BYRNM                                                     
*****                                                                           
* VALIDATE THE CAMPAIGN NUMBER                                                  
*****                                                                           
         TM    GLLCAMH+4,FVIVAL    FIELD VALIDATED PREVIOUSLY?                  
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO                                           
*                                                                               
         XC    GLLCAN,GLLCAN                                                    
         OI    GLLCANH+6,FVOXMT                                                 
         GOTO1 AVALCAM,GLLCAMH                                                  
         BNE   VPARX                                                            
         MVC   GLLCAM,QCAM                                                      
         OI    GLLCAMH+4,FVINUM+FVIVAL   FIELD VALIDATED AS NUMERIC             
         MVC   SVBCAMPN,BCAM                                                    
         MVC   GLLCAN,CMPNM                                                     
*****                                                                           
* SAVE IMPORTANT CAMPAIGN INFORMATION                                           
*****                                                                           
         MVC   SVCCLTCD,CMPCLTC    CLIENT CODE                                  
         MVC   SVCPRDCD,CMPPRDN    PRODUCT CODE                                 
         MVC   SVCESTNB,CMPESTN    ESTIMATE NUMBER                              
         MVC   SVCSTDAT,CMPST      START DATE                                   
         MVC   SVCNDDAT,CMPND      END DATE                                     
         MVC   SVCSPTLN,CMPSLN     SPOT LENGTH                                  
*****                                                                           
* SET UP 'APRECKEY' FOR 'GETSEL'                                                
*****                                                                           
         TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGE?                          
         BNZ   VPAR10                                                           
*                                                                               
         OC    SVDGKEY,SVDGKEY     NO, CONTINUE FROM PREV TRANSACTION?          
         BZ    VPAR10                                                           
         XC    APRECKEY,APRECKEY       YES, CONTINUE OUR LIST                   
         MVC   APRECKEY(L'GKEY),SVDGKEY                                         
         B     VPAROKAY                                                         
*                                                                               
VPAR10   XC    APRECKEY,APRECKEY   SET UP VALUES FOR THE GOAL KEY               
         LA    R2,APRECKEY                                                      
         USING GOALRECD,R2                                                      
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM,SVBAGYMD                                                  
         MVC   GKEYCLT,SVCCLTCD                                                 
         MVC   GKEYPRD,SVCPRDCD                                                 
         DROP  R2                                                               
*                                                                               
         MVC   SVDGKEY,APRECKEY    MAKE A COPY FOR LATER                        
*                                                                               
VPAROKAY LA    RE,GLLSEL1H         FIRST FIELD OF LIST LINE                     
         ST    RE,APPARM                                                        
*                                                                               
         TWAXC GLLSEL1H,PROT=Y                                                  
*                                                                               
         MVI   APPARM+4,16         NUMBER OF LIST LINES ON SCREEN               
*                                                                               
         LA    RE,LINNEXT-LINDSECT                                              
         STH   RE,APPARM+6         LENGTH OF LIST LINE                          
*                                                                               
VPARX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET A SELECTION FOR THE LIST SCREEN                                           
***********************************************************************         
*                                                                               
GETSEL   DS    0H                                                               
         MVC   IOKEY,APRECKEY                                                   
*                                                                               
         TM    APINDS,APILRERD     SEQUENTIAL READ PROCESS INTERRUPTED?         
         BZ    GSEL10              NO                                           
         LA    R1,DIRRD+IO1                                                     
         GOTO1 AIO                                                              
         BE    GSEL20                                                           
         B     GSEL90                                                           
*                                                                               
GSEL10   TM    APINDS,APILNSEQ     READ SEQUENTIAL?                             
         BO    GSEL20                                                           
         LA    R1,DIRHI+IO1        NO, READ HIGH                                
         B     *+8                                                              
*                                                                               
GSEL20   LA    R1,DIRSQ+IO1        YES                                          
         GOTO1 AIO                                                              
         BNE   GSEL90                                                           
*                                                                               
         CLC   IOKEY(GKEYMKT-GKEY),IOKEYSAV   MAKE SURE SAME CLT/PRD            
         BNE   GSEL90                                                           
*                                                                               
GSEL30   LA    R2,IOKEY            MAKE SURE THE OTHER INFO MATCHES             
         USING GKEY,R2                                                          
         CLC   GKEYEST,SVCESTNB                                                 
         BNE   GSEL20                                                           
*                                                                               
         CLI   SVCSPTLN,0          NEED TO MATCH SPOT LENGTH?                   
         BZ    GSEL40                                                           
         CLC   GKEYSEC,SVCSPTLN    YES                                          
         BNE   GSEL20                                                           
*                                                                               
GSEL40   CLI   GKEYAGY,0           ONLY REGULAR GOALS                           
         BNE   GSEL20                                                           
*                                                                               
         XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(L'GKEY),IOKEY                                           
         MVC   SVDGKEY,IOKEY                                                    
         MVC   APRECDA,IODA                                                     
         B     GSELX                                                            
*                                                                               
GSEL90   MVI   APMODE,APMEOFS      NO MORE RECORDS FOR GENERAL                  
         XC    SVDGKEY,SVDGKEY                                                  
*                                                                               
GSELX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY SELECTION FOR THE LIST SCREEN                                         
***********************************************************************         
*                                                                               
DISSEL   DS    0H                                                               
         L     R2,APPARM                                                        
         USING LINDSECT,R2                                                      
         LA    R4,APRECKEY                                                      
         USING GOALRECD,R4                                                      
*                                                                               
         EDIT  (B2,GKEYMKT),(4,LINMKT),ALIGN=LEFT                               
         OI    LINMKTH+6,FVOXMT                                                 
         MVC   LINDAYP,GKEYDPT                                                  
         OI    LINDAYPH+6,FVOXMT                                                
         EDIT  (B1,GKEYSEC),(3,LINSPLN),ALIGN=LEFT                              
         OI    LINSPLNH+6,FVOXMT                                                
*                                                                               
         LA    R3,IOKEY            LOOK UP THE MARKET NAME                      
         XC    IOKEY,IOKEY                                                      
         USING MKTKEY,R3                                                        
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,GLLMED                                                   
         EDIT  (B2,GKEYMKT),(4,MKTKMKT),FILL=0                                  
         MVC   MKTKAGY,CUAALF                                                   
         DROP  R3,R4                                                            
*                                                                               
         LA    R1,IOSTAFIL+IOHI+IO1                                             
         GOTO1 AIO                                                              
         BNE   DSELX                                                            
*                                                                               
         L     R4,IOADDR                                                        
         USING MKTRECD,R4                                                       
         MVC   LINMKNM,MKTNAME                                                  
         OI    LINMKNMH+6,FVOXMT                                                
         DROP  R4                                                               
*                                                                               
DSELX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY SELECTION FOR THE LIST SCREEN                                         
***********************************************************************         
LINDSECT DSECT                                                                  
LINSELH  DS    CL8                                                              
LINSEL   DS    CL3                                                              
LINMKTH  DS    CL8                                                              
LINMKT   DS    CL4                                                              
LINMKNMH DS    CL8                                                              
LINMKNM  DS    CL24                                                             
LINDAYPH DS    CL8                                                              
LINDAYP  DS    CL1                                                              
LINSPLNH DS    CL8                                                              
LINSPLN  DS    CL3                                                              
LINNEXT  DS    0C                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
         DS    0A                                                               
DUB      DS    D                                                                
*                                                                               
         DS    0H                                                               
*                                                                               
         DS    0C                                                               
WORK     DS    CL20                                                             
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAG #1                        
MF1KYCHG EQU   X'80'                 KEY FIELD(S) HAVE BEEN CHANGED             
*                                                                               
SVBAGYMD DS    XL1                 SAVED BINARY AGENCY/MEDIA                    
SVBBUYER DS    XL1                       BINARY BUYER CODE                      
SVBCAMPN DS    XL2                       BINARY CAMPAIGN NUMBER                 
*                                                                               
SVCCLTCD DS    XL2                 SAVED CAMPAIGN CLIENT CODE                   
SVCPRDCD DS    XL1                                PRODUCT CODE                  
SVCESTNB DS    XL1                                ESTIMATE NUMBER               
SVCSTDAT DS    XL3                                START DATE                    
SVCNDDAT DS    XL3                                END DATE                      
SVCSPTLN DS    XL1                                SPOT LENGTH                   
*                                                                               
SVDGKEY  DS    CL13                SAVED GOAL KEY                               
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSDAD                                                       
         SPACE 2                                                                
         ORG   SAVOVER                                                          
SVPWIND  DS    XL1                 SAVED PASSWORD INDICATOR                     
SVPWVALD EQU   X'80'                                                            
         EJECT                                                                  
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPNWS26   02/27/07'                                      
         END                                                                    
