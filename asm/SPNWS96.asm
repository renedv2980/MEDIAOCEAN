*          DATA SET SPNWS96    AT LEVEL 020 AS OF 02/27/07                      
*PHASE T20796C,*                                                                
         TITLE 'BWS96 - BUYERS WORK SHEET - CALL LETTER CHANGE'                 
T20796   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20796**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
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
* OUTPUT : CALL CHANGE EXECUTED FOR A SINGLE CAMPAIGN                 *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   GOTO1 AVALPARM,BWSKY2H    VALIDATE SELECT PARAMETERS                   
         BNE   VALPX                                                            
         TM    TWAFLAG,TWANODET    TEST FOR NO DETAIL RECORDS                   
         BNZ   VALPX                                                            
*                                                                               
         ZAP   RECCHA,=P'0'                                                     
         L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
         MVI   OLDCD,0                                                          
         MVI   NEWCD,0                                                          
*                                                                               
VALP2    CLI   0(R4),0             SCAN HEADER RECORD FOR STATIONS              
         BE    VALP6                                                            
         CLI   0(R4),BWHELCDQ                                                   
         BNE   VALP4                                                            
         USING BWHEL,R4                                                         
         ZIC   RE,BWHSEQ                                                        
         CLC   BWHSTA,QSTA         TEST OLD STATION                             
         BNE   *+16                                                             
         STC   RE,OLDCD            YES-SAVE ITS CODE                            
         ST    R4,APFULL                                                        
         B     VALP4                                                            
         CLC   BWHSTA,INFCHSTA     TEST NEW STATION                             
         BNE   VALP4                                                            
         STC   RE,NEWCD                                                         
*                                                                               
VALP4    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALP2                                                            
*                                                                               
VALP6    CLI   OLDCD,0             OLD STATION MUST BE PRESENT                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R4,APFULL           R4=A(OLD STATION ELEMENT)                    
         CLI   NEWCD,0             TEST NEW STATION PRESENT                     
         BNE   *+14                                                             
         MVC   BWHSTA,INFCHSTA     NO-SIMPLY REPLACE OLD WITH NEW               
         B     VALP8                                                            
         MVI   0(R4),X'FF'         YES-DELETE OLD STATION ELEMENT               
         MVI   APELEM,X'FF'                                                     
         GOTO1 ADELELS,BWHRECD                                                  
*                                                                               
VALP8    MVC   IODA,HDRDA          WRITE UPDATED HEADER RECORD                  
         GOTO1 AIO,FILGETU2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,FILPUT1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,IOKEY            READ DETEIL RECORDS                          
         USING BWDRECD,R3          FOR OLD STATION CALL LETTERS                 
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),DTLKEY                                                 
         MVC   BWDKELST,OLDCD                                                   
         XC    PKGORBKY,PKGORBKY                                                
*                                                                               
VALP10   LA    R1,MINHI2                                                        
         B     VALP12+4                                                         
*                                                                               
VALP12   LA    R1,MINSEQ2          READ ALL DETAIL RECORDS                      
         GOTO1 AMIN                                                             
         BNE   VALP90                                                           
         CLC   IOKEY(BWDKELPO-BWDKEY),IOKEYSAV  TEST SAME STATION CODE          
         BNE   VALP90                                                           
         AP    RECCHA,=P'1'        YES-                                         
         L     R3,AIOAREA2                                                      
*                                                                               
         CLI   NEWCD,0             TEST NEW STATION ALREADY EXISTS              
         BNE   VALP14                                                           
         MVC   BWDSTA,INFCHSTA     NO-SIMLPY REPLACE THE STATION                
         GOTO1 AMIN,MINWRT2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AMIN,MINHI2                                                      
         B     VALP12                                                           
*                                                                               
VALP14   DS    0H                                                               
         GOTO1 AMIN,MINDEL         YES-DELETE THE RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APRECKEY(13),IOKEY  SAVE THE KEY                                 
         OC    PKGORBKY,PKGORBKY   TEST PROCESSING A PACKAGE/ORBIT              
         BZ    VALP16                                                           
         CLC   IOKEY(BWDKELDY-BWDKEY),PKGORBKY  YES-TEST SAME PKG/ORB           
         BE    *+14                                                             
         XC    PKGORBKY,PKGORBKY   NO-CLEAR SAVED KEY AND CONTINUE              
         B     VALP16                                                           
         CLI   BWDKELSQ,0          YES-MUST BE A SLAVE                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   BWDKELPO,NEWPOSEQ   REPLACE PACKAGE/ORBIT SEQ NUM                
         MVC   BWDPKOR,NEWPOSEQ                                                 
         B     VALP28                                                           
*                                                                               
VALP16   LA    R3,IOKEY            BUILD NEW KEY                                
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT                           
         BE    VALP22                                                           
         CLI   BWDKELSQ,0          YES-MUST BE A MASTER                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PKGORBKY,IOKEY      SAVE ITS KEY                                 
         MVC   BWDKELST,NEWCD      NEW STATION CODE                             
         MVI   BWDKELPO,1          FIND NEXT AVAILABLE PACKAGE/ORBIT            
         XC    BWDKELDY(4),BWDKELDY     SEQUENCE NUMBER FOR NEW STATION         
         SR    R9,R9                                                            
         LA    R1,MINHI3                                                        
         B     VALP18+4                                                         
*                                                                               
VALP18   LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BNE   VALP20                                                           
         CLC   IOKEY(BWDKELPO-BWDKEY),IOKEYSAV                                  
         BNE   VALP20                                                           
         IC    R9,BWDKELPO                                                      
         B     VALP18                                                           
*                                                                               
VALP20   L     R3,AIOAREA2                                                      
         LA    R9,1(R9)                                                         
         STC   R9,NEWPOSEQ         SAVE NEW PKG/ORB SEQ NUM                     
         STC   R9,BWDKELPO         AND MOVE TO RECORD                           
         STC   R9,BWDPKOR                                                       
         B     VALP28                                                           
*                                                                               
VALP22   MVC   BWDKELST,NEWCD      NOT PACKAGE OR ORBIT --                      
         MVI   BWDKELSQ,0                                                       
         SR    R9,R9                                                            
         LA    R1,MINHI3           FIND NEXT AVAILABLE SEQ NUM FOR              
         B     VALP24+4            NEW STATION                                  
*                                                                               
VALP24   LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BNE   VALP26                                                           
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   VALP26                                                           
         IC    R9,BWDKELSQ                                                      
         B     VALP24                                                           
*                                                                               
VALP26   L     R3,AIOAREA2                                                      
         LA    R9,1(R9)                                                         
         STC   R9,BWDKELSQ                                                      
         STC   R9,BWDSEQ                                                        
*                                                                               
VALP28   MVC   BWDKELST,NEWCD      NEW STATION CODE                             
         MVC   BWDSTACD,NEWCD                                                   
         MVC   BWDSTA,INFCHSTA     NEW STATION CALL LETTERS                     
         GOTO1 AMIN,MINADD2        ADD RECORD BACK                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(13),APRECKEY                                               
         B     VALP10                                                           
*                                                                               
VALP90   MVC   FVMSGNO,=AL2(FVFSET)                                             
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         XC    BWSMSG,BWSMSG                                                    
         CLI   NEWCD,0                                                          
         BNE   *+18                                                             
         MVC   BWSMSG(20),=C'** RECORDS CHANGED ='                              
         LA    R1,BWSMSG+21                                                     
         B     *+14                                                             
         MVC   BWSMSG(28),=C'** RECORDS DELETED && ADDED ='                     
         LA    R1,BWSMSG+29                                                     
         UNPK  0(3,R1),RECCHA                                                   
         OI    2(R1),X'F0'                                                      
         MVC   4(2,R1),=C'**'                                                   
*                                                                               
VALPX    B     EXIT                                                             
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
COMWRK   DS    0C                  COMMON BETWEEN BWS05 AND BWS98               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPNWS05WRK                                                     
         PRINT ON                                                               
         SPACE 2                                                                
LOCALD   DSECT                                                                  
         ORG   LOCALD+2048         BWS96 WORK AREA                              
*                                                                               
OLDCD    DS    XL1                                                              
NEWCD    DS    XL1                                                              
RECCHA   DS    PL2                                                              
PKGORBKY DS    XL13                                                             
NEWPOSEQ DS    XL1                                                              
*                                                                               
         ORG   LOCALD+4096                                                      
LOCALX   EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
* SPNWSFBD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSFBD                                                       
         PRINT ON                                                               
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPNWS96   02/27/07'                                      
         END                                                                    
