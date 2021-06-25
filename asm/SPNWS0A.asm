*          DATA SET SPNWS0A    AT LEVEL 045 AS OF 02/26/07                      
*PHASE T2070AC,*                                                                
         TITLE 'BWS0A - SEND ADDS NOTICE OF CONFIRMATION'                       
T2070A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2070A**,RR=RE                                                 
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
*                                                                               
         MVC   APPARM,COMPARM                                                   
         LA    R3,APRECKEY                                                      
         USING BWDRECD,R3                                                       
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
         TM    AFLAG1,X'80'        TEST ADDS VALID FOR AGENCY                   
         BZ    VALP96                                                           
*                                                                               
         CLI   QMED,C'T'           ORDER SENDING DISABLED FOR MEDIA 'T'         
         BE    VALP97                AS PER MARY ELLEN                          
*                                                                               
         XC    IOKEY,IOKEY         READ DETAIL RECORDS UNTIL AT LEAST           
         LA    R4,BWDKELST-BWDKEY  ONE BUY TRANSFER ELEMENT IS FOUND            
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   IOKEY(0),DTLKEY                                                  
         LA    R3,IOKEY                                                         
         OC    BSTA,BSTA                                                        
         BZ    *+14                                                             
         MVC   BWDKELST,BSTACD                                                  
         LA    R4,L'BWDKELST(R4)                                                
         LA    R1,MINHI2                                                        
         B     VALP2                                                            
*                                                                               
VALP1    LA    R1,MINSEQ2                                                       
*                                                                               
VALP2    GOTO1 AMIN                                                             
         BNE   VALP90                                                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   VALP90              NO BUY TRANSFER => NO ADDS DATA              
         L     R3,AIOAREA2                            TO SEND                   
         LA    R8,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
VALP3    CLI   0(R8),0                                                          
         BE    VALP1                                                            
         CLI   0(R8),BTRELCDQ                                                   
         BE    VALP4                                                            
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VALP3                                                            
*                                                                               
VALP4    LA    R2,SPADBLK          BUILD SPADINT BLOCK                          
         USING SPADINTD,R2                                                      
         LA    RF,SPADINTL                                                      
         XCEFL (R2)                                                             
         MVI   ADACTN,ADASEND                                                   
         MVC   ADACOMFC,ACOM                                                    
         MVC   ADATWA,ATWA                                                      
         MVC   ADQAGYMD,BAGYMD                                                  
         MVC   ADQCLT,BCLT                                                      
         MVC   ADQPRD,BPRD                                                      
         MVC   ADQAPRD,QPRD                                                     
         CLI   CMPPRD1,0           TEST PIGGYBACKS                              
         BE    VALP5                                                            
         MVC   ADQPRD,CMPPRD1      YES                                          
         MVC   ADQPRD2,CMPPRD2                                                  
         MVC   ADQAPRD,COMPRD1                                                  
         MVC   ADQAPRD2,COMPRD2                                                 
*                                                                               
VALP5    MVC   ADQEST,BEST                                                      
         MVC   ADQMKT,BMKT                                                      
         MVC   ADQSTA,BSTA         STATION MAYBE NOT DEFINED                    
         MVC   ADQAGY,QAGY                                                      
         MVC   ADQMED,QMED                                                      
         MVC   ADUSRID,CUUSER                                                   
         MVC   ADQUEST,BYRNM                                                    
         MVC   ADRTGSVC,CLTSRC                                                  
         MVC   ADRSMKT,MKTRS                                                    
         MVC   ADQBUYER,QBYR                                                    
         MVC   ADQCAMP,BCAM                                                     
         XC    ADQCAMP,=X'FFFF'                                                 
******** TM    INFIND,INFISEND     OPTION TO SEND CAMPAIGN DETAILS ONLY         
******** BZ    *+8                 ** THIS SHOULD ALWAYS HAPPEN NOW **          
         OI    ADQINDS,ADQICAMP                                                 
         TM    CMPOPTS,CAMODLY     TEST DAILY SCHEDULE                          
         BZ    *+8                                                              
         OI    ADQINDS,ADQIDLY     YES                                          
         GOTO1 VDATCON,APPARM,(3,CMPST),(2,ADQSTART)  PASS CAMPN DATES          
         GOTO1 (RF),(R1),(3,CMPND),(2,ADQEND)                                   
*                                                                               
         GOTO1 VSPADINT,APPARM,(R2)  CALL SPADINT FOR SEND                      
*                                                                               
         TM    ADERRS,ADERREQ      TEST FOR SOON ERROR                          
         BO    VALP94              YES-MESSAGE ALREADY FORMATTED                
         CLI   ADERRS,0            OTHER ERRORS                                 
         BNE   VALP92                                                           
         LA    R4,ADSTAB                                                        
         OC    0(L'ADSTAB,R4),0(R4)  TEST ANYTHING SENT                         
         BZ    VALP90                NO                                         
         USING ADSTAB,R4                                                        
         MVC   APWORK,SPACES         YES-FORMAT MESSAGES                        
         LA    R3,APWORK                                                        
         MVC   0(L'SENDMSG,R3),SENDMSG                                          
         LA    R3,L'SENDMSG+1(R3)                                               
         MVI   FVOMTYP,C'I'                                                     
         MVC   APELEM(80),SPACES                                                
         LA    R8,APELEM                                                        
         XC    SVRPTID,SVRPTID                                                  
         XC    SVRPTNO,SVRPTNO                                                  
         LA    R9,ADSTMAX                                                       
*                                                                               
VALP6    OC    0(L'ADSTAB,R4),0(R4)                                             
         BZ    VALP8                                                            
         TM    ADSERR,ADSEREPI     TEST RECEIVING REP ID NOT FOUND              
         BO    VALP13              YES-SKIP THIS STATION                        
         MVC   LMKTSTA(2),BMKT                                                  
         MVC   LMKTSTA+2(3),ADSTA                                               
         GOTO1 VMSUNPK,APPARM,(X'80',LMKTSTA),APFULL,APDUB                      
         MVC   0(5,R3),APDUB                                                    
         CLI   4(R3),C'T'                                                       
         BNE   *+8                                                              
         MVI   4(R3),C' '                                                       
         CLI   4(R3),C' '                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         CLI   APDUB,C'0'          TEST CABLE                                   
         BL    VALP6A                                                           
         MVI   5(R3),C'/'          YES-DISPLAY NETWORK ALSO                     
         MVC   6(3,R3),APDUB+5                                                  
         LA    R3,4(R3)                                                         
         CLI   APDUB+7,C' '                                                     
         BH    VALP6A                                                           
         BCTR  R3,0                                                             
*                                                                               
VALP6A   MVI   5(R3),C'='                                                       
         ZIC   RF,ADSVER                                                        
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  6(3,R3),APDUB                                                    
         MVI   9(R3),C','                                                       
         LA    R3,10(R3)                                                        
*                                                                               
         OC    SVRPTID,SVRPTID     TEST FIRST STATION                           
         BNZ   VALP7                                                            
         MVC   0(3,R8),ADSRPTID    YES                                          
         MVI   3(R8),C','                                                       
         LA    R8,4(R8)                                                         
         B     VALP10                                                           
*                                                                               
VALP7    SR    RE,RE               NO-TEST THIS REPORT NUM = LAST + 1           
         ICM   RE,3,SVRPTNO                                                     
         LA    RE,1(RE)                                                         
         CLM   RE,3,ADSRPTNO                                                    
         BNE   VALP8                                                            
         MVI   RANGE,C'Y'          YES-INDICATE WE'RE IN A RANGE OF             
         B     VALP12                  REPORT NUMBERS                           
*                                                                               
VALP8    CLI   RANGE,C'Y'          NO-TEST WE WERE IN A RANGE                   
         BNE   VALP9                                                            
         MVI   0(R8),C'-'          YES-FINISH LAST RANGE                        
         LA    R8,1(R8)                                                         
         EDIT  (B2,SVRPTNO),(5,(R8)),ALIGN=LEFT                                 
         AR    R8,R0                                                            
*                                                                               
VALP9    OC    0(L'ADSTAB,R4),0(R4)    TEST DONE LAST STATION                   
         BZ    VALP14                  YES                                      
         MVI   0(R8),C','                                                       
         LA    R8,1(R8)                                                         
*                                                                               
VALP10   EDIT  (B2,ADSRPTNO),(5,(R8)),ALIGN=LEFT                                
         AR    R8,R0                                                            
         MVI   RANGE,C'N'                                                       
*                                                                               
VALP12   MVC   SVRPTID,ADSRPTID    SAVE LAST REPORT ID AND NUMBER               
         MVC   SVRPTNO,ADSRPTNO                                                 
*                                                                               
VALP13   LA    R4,L'ADSTAB(R4)                                                  
         BCT   R9,VALP6            NEXT STATION                                 
         DROP  R4                                                               
*                                                                               
VALP14   CLC   APELEM(80),SPACES   TEST ANY STATION SENT                        
         BE    VALP90                                                           
         BCTR  R3,0                YES-MOVE STATION VERSIONS MESSAGE            
         LA    RF,APWORK                                                        
         SR    R3,RF                                                            
         BNP   VALP16                                                           
         ZIC   RF,WRKCPEH                                                       
         SH    RF,=H'8'                                                         
         BNP   VALP16                                                           
         CR    R3,RF                                                            
         BNH   *+6                                                              
         LR    R3,RF                                                            
         XC    WRKCPE,WRKCPE                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WRKCPE(0),APWORK                                                 
         OI    WRKCPEH+6,FVOXMT                                                 
*                                                                               
VALP16   XC    FVXTRA,FVXTRA       FORMAT 'REPORT(S) XXX,99999 WILL BE          
         LA    RF,APELEM           PROCESSED SOON' MESSAGE                      
         SR    R8,RF                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RF,L'FVXTRA                                                      
         CR    R8,RF                                                            
         BNH   *+6                                                              
         LR    R8,RF                                                            
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),APELEM                                                 
         MVI   FVOMTYP,C'I'                                                     
         MVC   FVMSGNO,=AL2(FVSOON)                                             
         LA    RF,ADSTAB+L'ADSTAB                                               
         OC    0(L'ADSTAB,RF),0(RF)                                             
         BNZ   VALPX                                                            
         MVC   FVMSGNO,=AL2(INFMSG)                                             
         B     VALPX                                                            
*                                                                               
VALP90   MVC   FVMSGNO,=AL2(FVNOADDS)    NO ADDS DATA TO SEND                   
         MVI   FVOMTYP,0                                                        
         B     VALPX                                                            
*                                                                               
VALP92   MVC   FVMSGNO,=AL2(FVIADDS)     SPADINT ERROR                          
         B     VALP99                                                           
*                                                                               
VALP94   MVC   FVMSGNO,=AL2(FVFSET)     SOON PROCESSING ERROR                   
         B     VALP99                                                           
*                                                                               
VALP96   MVC   FVMSGNO,=AL2(FVIADINT)   ADDS INTERFACE INVALID                  
         B     VALPX                                                            
*                                                                               
VALP97   MVC   FVMSGNO,=AL2(FVIORDIS)   ORDER SENDING DISABLED FOR 'T'          
         B     VALPX                                                            
*                                                                               
VALP99   OI    FVERRIND,FVEUNWND        UNWIND THE TRANSACTION                  
*                                                                               
VALPX    B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
SENDMSG  DC    CL16'Version numbers:'                                           
INFMSG   EQU   X'FF00'+21                                                       
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
COMWRK   DS    0C                  COMMON BETWEEN BWS05 AND BWS0A               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPNWS05WRK                                                     
         PRINT ON                                                               
*                                                                               
LOCALD   DSECT                                                                  
         ORG   LOCALD+2048                                                      
*                                                                               
DUB      DS    D                                                                
WORK     DS    CL64                                                             
LMKTSTA  DS    XL5                                                              
RANGE    DS    C                                                                
SVRPTID  DS    CL3                                                              
SVRPTNO  DS    XL2                                                              
SPADBLK  DS    XL(SPADINTL)                                                     
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
* SPNWSFBD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSFBD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPADINTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPADINTD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPNWS0A   02/26/07'                                      
         END                                                                    
