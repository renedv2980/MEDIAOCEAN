*          DATA SET SPNWS16    AT LEVEL 015 AS OF 02/26/07                      
*PHASE T20716C,*                                                                
         TITLE 'BWS16 - BUYERS WORK SHEET - CAMPAIGN/MARKET LIST'               
BWS16    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20716**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R2,APRECKEY         R2=A(CAMPAIGN/MARKET HEADER KEY)             
         USING BWHRECD,R2                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         B     VALPAR                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         DC    AL4(0)                                                           
         B     EXIT                FSTLST                                       
         DC    AL4(0)                                                           
         B     EXIT                FSTSCR                                       
         B     LSTSCR                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   LA    R0,LSTL1H           SET VALUES FOR ROOT                          
         ST    R0,APPARM+0                                                      
         LA    R1,LSTL2H                                                        
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   LFLAG,0                                                          
         XC    APRECKEY,APRECKEY   BUILD CAMPAIGN/MARKET HEADER KEY             
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         XC    LSTMDN,LSTMDN       MEDIA                                        
         OI    LSTMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,LSTMEDH                                                  
         BNE   VALPX                                                            
         MVC   LSTMDN,MEDNM                                                     
         MVC   BWHKAGMD,BAGYMD                                                  
         XC    LSTBYN,LSTBYN       BUYER                                        
         OI    LSTBYNH+6,FVOXMT                                                 
         GOTO1 AVALBYR,LSTBYRH                                                  
         BNE   VALPX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   LSTBYN,BYRNM                                                     
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         CHECK BUYER'S PASSWORD                       
         BZ    VALP2                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALPX                                                            
*                                                                               
VALP2    XC    LSTCAN,LSTCAN       CAMPAIGN (OPTIONAL)                          
         OI    LSTCANH+6,FVOXMT                                                 
         CLI   LSTCAMH+5,0                                                      
         BE    VALP4                                                            
         OI    LFLAG,LSELCAM                                                    
         GOTO1 AVALCAM,LSTCAMH                                                  
         BNE   VALPX                                                            
         MVC   BWHKCAM,BCAM                                                     
         MVC   LSTCAN,CMPNM                                                     
*                                                                               
VALP4    B     VALPX                                                            
*                                                                               
VALPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   MVC   IOKEY,APRECKEY                                                   
         TM    APINDS,APILRERD     TEST READ THEN READ SEQUENTIAL               
         BZ    GETS4                                                            
*                                                                               
GETS2    GOTO1 AIO,DIRRD                                                        
         BE    GETS6                                                            
         B     GETS9                                                            
*                                                                               
GETS4    CLI   APINDS,APILNSEQ     TEST READ SEQUENTIAL                         
         BE    GETS6                                                            
         LA    R1,DIRHI                                                         
         B     GETS6+4                                                          
*                                                                               
GETS6    LA    R1,DIRSQ            GET FIRST/NEXT DIRECTORY RECORD              
         GOTO1 AIO                                                              
         BNE   GETS9                                                            
         LA    R2,IOKEY                                                         
         CLC   IOKEY(BWHKCAM-BWHKEY),IOKEYSAV  TEST SAME AGYMED/BUYER           
         BNE   GETS9                                                            
         TM    LFLAG,LSELCAM       TEST CAMPAIGN FILTER                         
         BZ    *+14                                                             
         CLC   BWHKCAM,BCAM                                                     
         BNE   GETS9                                                            
         CLC   IOKEY(BWHKMKT-BWHKEY),IOKEYSAV    TEST NEW CAMPAIGN              
         BNE   GETS8                                                            
         CLI   APINDS,APILFLST     OR FIRST LINE                                
         BE    GETS8                                                            
         CLI   APINDS,APILNLST                                                  
         BNE   *+8                                                              
GETS8    OI    LFLAG,LNEWCAM       YES-GET CAMPAIGN DETAILS                     
*                                                                               
         MVC   APRECKEY,IOKEY                                                   
         MVC   APRECDA,IODA                                                     
         B     GETSX                                                            
*                                                                               
GETS9    MVI   APMODE,APMEOFS      END-OF-LIST                                  
*                                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   LA    R2,APRECKEY                                                      
         L     R3,APPARM                                                        
         ZIC   R1,0(R3)                                                         
         LA    R3,L'FVIHDR(R1,R3)  R3=A(OUTPUT DISPLAY LINE)                    
         USING LISTD,R3                                                         
         TM    LFLAG,LNEWCAM       DISPLAY CAMPAIGN DETAILS FOR NEW CAM         
         BZ    DISS2               AND FIRST LINE                               
         NI    LFLAG,255-LNEWCAM                                                
         GOTO1 AGETCAM,BWHKCAM                                                  
         BE    DISS1                                                            
         MVC   APHALF,BWHKCAM                                                   
         XC    APHALF,=X'FFFF'                                                  
         SR    R1,R1                                                            
         ICM   R1,3,APHALF                                                      
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  QCAM,APDUB                                                       
*                                                                               
DISS1    EDIT  (C5,QCAM),(5,LISTCAM),WRK=APWORK,DUB=APDUB                       
         GOTO1 AGETCLT,CMPCLTC                                                  
         BNE   *+10                                                             
         MVC   LISTCLT,QCLT                                                     
         GOTO1 AGETPRD,CMPPRDN                                                  
         BNE   *+10                                                             
         MVC   LISTPRD,QPRD                                                     
         GOTO1 AGETEST,CMPESTN                                                  
         BNE   *+10                                                             
         MVC   LISTEST,QEST                                                     
         GOTO1 VDATCON,APPARM,(3,CMPST),(5,LISTDAT)                             
         MVI   LISTDAT+8,C'-'                                                   
         GOTO1 (RF),(R1),(3,CMPND),(5,LISTDAT+9)                                
         SR    R0,R0                                                            
         ICM   R0,1,CMPSLN                                                      
         BZ    DISS2                                                            
         EDIT  (R0),(3,LISTSLN),WRK=APWORK,DUB=APDUB                            
*                                                                               
DISS2    GOTO1 AGETMKT,BWHKMKT                                                  
         BE    DISS4                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    RE,RE                                                            
         ICM   RE,3,BWHKMKT                                                     
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  QMKT,APDUB                                                       
         XC    MKTNM,MKTNM                                                      
         MVC   MKTNM(20),=C'**MKT XXXX UNKNOWN**'                               
         MVC   MKTNM+6(4),QMKT                                                  
*                                                                               
DISS4    MVC   LISTMKT(4),QMKT                                                  
         MVC   LISTMKT+5(L'MKTNM),MKTNM                                         
*                                                                               
DISSX    B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
* LAST FOR LIST SCREEN                                                          
*                                                                               
LSTSCR   MVI   APMODE,APMPFKS      ENABLE PROGRAM FUNCTION KEYS                 
         MVC   SCPFKEY,APPFKEY                                                  
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
LFLAG    DS    XL1                                                              
LSELCAM  EQU   X'80'                                                            
LNEWCAM  EQU   X'40'                                                            
*                                                                               
         ORG   LOCALD+4096                                                      
LOCALX   EQU   *                                                                
         SPACE 2                                                                
LISTD    DSECT                     ** LIST/SELECT DISPLAY LINE **               
         DS    CL1                                                              
LISTCAM  DS    CL5                 CAMPAIGN NUMBER                              
         DS    CL1                                                              
LISTCLT  DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
LISTPRD  DS    CL3                 PRODUCT CODE                                 
         DS    CL1                                                              
LISTEST  DS    CL3                 ESTIMATE CODE                                
         DS    CL1                                                              
LISTDAT  DS    CL17                BUYING PERIOD                                
         DS    CL1                                                              
LISTSLN  DS    CL3                 SPOT LENGTH                                  
         DS    CL1                                                              
LISTMKT  DS    CL29                MARKET                                       
         DS    CL1                                                              
LISTDPT  DS    CL5                 DAYPART                                      
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSDCD                                                       
         ORG                                                                    
         EJECT                                                                  
* SPNWSHDR                                                                      
       ++INCLUDE SPNWSHDR                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPNWS16   02/26/07'                                      
         END                                                                    
