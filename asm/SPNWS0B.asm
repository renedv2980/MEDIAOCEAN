*          DATA SET SPNWS0B    AT LEVEL 026 AS OF 02/26/07                      
*PHASE T2070BC,*                                                                
         TITLE 'BWS0B - ADD BWS GOAL RECORDS'                                   
T2070B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2070B**,RR=RE                                                 
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
*                                                                               
         MVC   APPARM,COMPARM                                                   
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         DC    AL4(0)  VALKEY                                                   
         DC    AL4(0)  VALREC                                                   
         DC    AL4(0)  DISKEY                                                   
         DC    AL4(0)  DISREC                                                   
         DC    AL4(0)  DELREC                                                   
         DC    AL4(0)  RESREC                                                   
         B     VALPAR                                                           
         DC    AL4(0)  GETSEL                                                   
         DC    AL4(0)  DISSEL                                                   
         DC    AL4(0)  VALSEL                                                   
         B     EXIT    FSTLST                                                   
         DC    AL4(0)                                                           
         DC    AL4(0)  FSTSCR                                                   
         DC    AL4(0)  LASSCR                                                   
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
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   DS    0H                                                               
         GOTO1 AVALPARM,BWSKY2H    VALIDATE THE KEY                             
         BNE   VALPX                                                            
         TM    TWAFLAG,TWANODET    TEST FOR NO RECORDS                          
         BNZ   VALPX                                                            
         CLI   BPRD,X'FF'          PRODUCT MUST BE POL                          
         BNE   VALP98                                                           
         CLI   CMPPRD1,0           AND NO PIGGBACKS                             
         BNE   VALP95                                                           
         XC    DPTLENTB,DPTLENTB   BUILD DPT/LEN TABLE BY FINDING ALL           
         LA    R2,IOKEY            DPT/LEN'S FOR WHICH THERE ARE GOALS          
         USING GOALRECD,R2                                                      
         XC    IOKEY,IOKEY                                                      
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GKEYCLT,BCLT                                                     
         LA    R4,1                START PRODUCT=1                              
*                                                                               
VALP1    STC   R4,GKEYPRD                                                       
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,BEST                                                     
         XC    GKEYDPT(5),GKEYDPT                                               
         LA    R1,DIRHI                                                         
         B     VALP3                                                            
*                                                                               
VALP2    LA    R1,DIRSQ                                                         
*                                                                               
VALP3    DS    0H                                                               
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GKEY(GKEYPRD-GKEY),IOKEYSAV  A/M,CLT                             
         BNE   VALP12                                                           
         CLC   GKEY(GKEYDPT-GKEY),IOKEYSAV  A/M,CLT,PRD,MKT,EST                 
         BNE   VALP8                                                            
         LA    R8,DPTLENTB         ADD DPT/LEN TO TABLE                         
         LA    R0,L'DPTLENTB/2                                                  
*                                                                               
VALP4    CLI   0(R8),0                                                          
         BE    VALP6                                                            
         CLC   GKEYDPT,0(R8)                                                    
         BNE   *+14                                                             
         CLC   GKEYSEC,1(R8)                                                    
         BE    VALP2                                                            
         LA    R8,2(R8)                                                         
         BCT   R0,VALP4                                                         
         DC    H'0'                                                             
*                                                                               
VALP6    MVC   0(1,R8),GKEYDPT                                                  
         MVC   1(1,R8),GKEYSEC                                                  
         B     VALP2                                                            
*                                                                               
VALP8    CLC   GKEY(GKEYMKT-GKEY),IOKEYSAV  A/M,CLT,PRD                         
         BE    VALP10                                                           
         XC    GKEYMKT(8),GKEYMKT  NEW PRODUCT                                  
         IC    R4,GKEYPRD                                                       
         B     VALP1                                                            
*                                                                               
VALP10   LA    R4,1(R4)            DONE WITH THIS PRODUCT                       
         CH    R4,=H'254'          MOVE ON TO NEXT                              
         BNH   VALP1                                                            
*                                                                               
VALP12   BAS   RE,DELGOAL          DELETE OBSOLETE BWS GOAL RECORDS             
         CLI   DPTLENTB,0          TEST ANY GOALS                               
         BE    VALP97                                                           
         LA    R8,DPTLENTB         YES-CALL GETGOAL FOR EACH DPT/LEN            
         LA    R0,L'DPTLENTB/2                                                  
*                                                                               
VALP14   CLI   0(R8),0                                                          
         BE    VALP20                                                           
         MVC   BDPT,0(R8)                                                       
         MVC   BSLN,1(R8)                                                       
         IC    R9,CMPDPOPT                                                      
         MVI   CMPDPOPT,C'S'       FORCE DPTS SCHEDULED SEPARATELY              
         GOTO1 AGETGOL2                                                         
         STC   R9,CMPDPOPT                                                      
         LA    R3,IOKEY                                                         
         USING BWGRECD,R3                                                       
         XC    IOKEY,IOKEY         READ BWS GOAL RECORD                         
         MVI   BWGKTYP,BWGKTYPQ                                                 
         MVI   BWGKSUB,BWGKSUBQ                                                 
         MVC   BWGKAGMD,BAGYMD                                                  
         OC    BWGKAGMD,BBYRMASK                                                
         MVC   BWGKBYR,BBYR                                                     
         MVC   BWGKCAM,BCAM                                                     
         MVC   BWGKMKT,BMKT                                                     
         MVC   BWGKDPT,0(R8)                                                    
         MVC   BWGKLEN,1(R8)                                                    
         GOTO1 AIO,DIRHID+IO2                                                   
         BL    VALP99                                                           
         BE    *+12                                                             
         TM    IOERR,IOEDEL        DELETED RECORDS ARE OK                       
         BZ    VALP99                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   NEWREC,C'Y'                                                      
         CLC   BWGKEY,IOKEYSAV     TEST RECORD FOUND                            
         BNE   VALP16                                                           
         MVI   NEWREC,C'N'         YES-GET IT                                   
         TM    BWGKCNTL,X'80'      TEST DELETED PREVIOUSLY                      
         BZ    VALP15                                                           
         NI    BWGKCNTL,X'7F'      YES-RESTORE                                  
         GOTO1 AIO,DIRWRT                                                       
         BNE   VALP99                                                           
*                                                                               
VALP15   DS    0H                                                               
         GOTO1 AIO,FILGETU2                                                     
         BL    VALP99                                                           
         BE    *+12                                                             
         TM    IOERR,IOEDEL        DELETED RECORDS ARE OK                       
         BZ    VALP99                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         L     R3,AIOAREA2                                                      
         NI    BWGCNTL,X'7F'       TURN OFF DELETE BIT                          
         B     VALP18                                                           
*                                                                               
VALP16   MVC   IOKEY,IOKEYSAV      NEW RECORD                                   
         L     R3,AIOAREA2                                                      
         XC    0(256,R3),0(R3)                                                  
         MVC   BWGKEY,IOKEY                                                     
         LA    R1,BWGFSTEL-BWGKEY                                               
         LA    R1,BWGELLNQ(R1)                                                  
         STCM  R1,3,BWGLEN                                                      
*                                                                               
VALP18   MVI   BWGELCD,BWGELCDQ    SET GOAL ELEMENT                             
         MVI   BWGELLN,BWGELLNQ                                                 
         L     R1,AIOAREA3                                                      
         MVC   BWGBUDT,0(R1)                                                    
         MVC   BWGGRPT,4(R1)                                                    
         MVC   BWGBUD,8(R1)                                                     
         MVC   BWGGRP,64(R1)                                                    
         LA    R1,FILPUT2          PUT OR ADD THE RECORD                        
         CLI   NEWREC,C'Y'                                                      
         BNE   *+8                                                              
         LA    R1,FILADD2                                                       
         GOTO1 AIO                                                              
         BNE   VALP99                                                           
         LA    R8,2(R8)            NEXT DPT/LEN                                 
         BCT   R0,VALP14                                                        
*                                                                               
*                                       GOALS ADDED SUCCESSFULLY --             
VALP20   TM    CMPIND,CMPIGOAL          TEST GOALS ADDED PREVIOUSLY             
         BO    VALP22                                                           
         OI    CMPIND,CMPIGOAL          NO-GET CAMPAIGN RECORD                  
         XC    IOKEY,IOKEY                 AND SET INDICATOR TO SHOW            
         LA    R2,IOKEY                    GOALS HAVE BEEN ADDED                
         USING CAMRECD,R2                                                       
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         OC    CAMKAGMD,BBYRMASK                                                
         MVC   CAMKBYR,BBYR                                                     
         MVC   CAMKCAM,BCAM                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   VALP99                                                           
         CLC   CAMKEY,IOKEYSAV                                                  
         BNE   VALP96                                                           
         GOTO1 AIO,FILGETU1                                                     
         BNE   VALP99                                                           
         L     R2,AIOAREA1                                                      
         OI    CAMINDS,CAMIGOAL                                                 
         GOTO1 AIO,FILPUT1                                                      
         BNE   VALP99                                                           
*                                                                               
VALP22   MVC   FVMSGNO,=AL2(FVGOLADD)   GOALS ADDED SUCCESSFULLY                
         B     VALPX                                                            
*                                                                               
VALP95   MVC   FVMSGNO,=AL2(FVIPIG)     PIGGYBACKS INVALID                      
         B     VALPX                                                            
*                                                                               
VALP96   MVC   FVMSGNO,=AL2(FVEREC)     RECORD ERROR                            
         B     VALPX                                                            
*                                                                               
VALP97   MVC   FVMSGNO,=AL2(FVNOREC)    NO GOALS ON FILE                        
         B     VALPX                                                            
*                                                                               
VALP98   MVC   FVMSGNO,=AL2(FVIPRDPL)   PRODUCT MUST BE POL                     
         B     VALPX                                                            
*                                                                               
VALP99   OI    FVERRIND,FVEUNWND        UNWIND THE TRANSACTION                  
*                                                                               
VALPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE ALL EXISTING BWS GOAL RECORDS FOR WHICH THERE ARE *         
* NO LONGER ANY GOAL RECORDS                                          *         
***********************************************************************         
         SPACE 1                                                                
DELGOAL  LR    R0,RE                                                            
         LA    R3,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   BWGKTYP,BWGKTYPQ                                                 
         MVI   BWGKSUB,BWGKSUBQ                                                 
         MVC   BWGKAGMD,BAGYMD                                                  
         OC    BWGKAGMD,BBYRMASK                                                
         MVC   BWGKBYR,BBYR                                                     
         MVC   BWGKCAM,BCAM                                                     
         MVC   BWGKMKT,BMKT                                                     
         LA    R1,DIRHI+IO2                                                     
         B     DELG4                                                            
*                                                                               
DELG2    LA    R1,DIRSQ+IO2                                                     
*                                                                               
DELG4    DS    0H                                                               
         GOTO1 AIO                                                              
         LA    R3,IOKEY                                                         
         CLC   BWGKEY(BWGKDPT-BWGKEY),IOKEYSAV                                  
         BNE   DELGX                                                            
         LA    R8,DPTLENTB         LOOK FOR DPT/LEN IN TABLE                    
         LA    RF,L'DPTLENTB/2                                                  
*                                                                               
DELG6    CLI   0(R8),0                                                          
         BE    DELG8                                                            
         CLC   BWGKDPT,0(R8)                                                    
         BNE   *+14                                                             
         CLC   BWGKLEN,1(R8)                                                    
         BE    DELG2                                                            
         LA    R8,2(R8)                                                         
         BCT   RF,DELG6                                                         
*                                                                               
DELG8    OI    BWGKCNTL,X'80'      NOT FOUND - DELETE THE RECORD                
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,FILGETU2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2                                                      
         OI    BWGCNTL,X'80'                                                    
         GOTO1 AIO,FILPUT2                                                      
         BE    DELG2                                                            
         DC    H'0'                                                             
*                                                                               
DELGX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
COMWRK   DS    0C                  COMMON BETWEEN BWS05 AND BWS0b               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPNWS05WRK                                                     
         PRINT ON                                                               
*                                                                               
LOCALD   DSECT                                                                  
         ORG   LOCALD+2048                                                      
*                                                                               
NEWREC   DS    CL1                                                              
DPTLENTB DS    CL256                                                            
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
* SPNWSFBD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSFBD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENGOAL                                                                     
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSGOAL                                                                     
       ++INCLUDE SPNWSGOAL                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPNWS0B   02/26/07'                                      
         END                                                                    
