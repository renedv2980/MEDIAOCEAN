*          DATA SET ACBRA2A    AT LEVEL 004 AS OF 03/12/20                      
*PHASE T6242AA                                                                  
                                                                                
ACBRA2A  TITLE '- BrandOcean Person/approver/limit list Upload Server'          
                                                                                
* Level change comments                                                         
* ---------------------                                                         
* NSHE 001 23JAN09 New application server                                       
* NSHE 002 20May16 add new upload request to add/delete person to job           
* MPEN 003 23Oct18 Relink for new DPAPASD                                       
* SGAV 004 14FEB20 DSPCA-3080 Update RSTLN from RSTLN3Q To RSTLN4Q              
*                                                                               
                                                                                
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=1000,REQUEST=*,WORKERKEY=ACBO,   *        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ                   
                                                                                
ENTRY    NMOD1 0,**BO2A**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         ST    R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         B     INITUL                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* Initialise Upload                                                   *         
***********************************************************************         
                                                                                
INITUL   CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         CLI   RUNPMODE,RRUNENDQ   TEST 'LAST TIME' MODE                        
         JNE   EXITY                                                            
         J     EXITY                                                            
                                                                                
INIT     LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
         L     RF,AACCFACS                                                      
         MVC   VACCEMU,X_AACCEMU-X_ACCFACSD(RF)                                 
         MVI   GIND2,GI2ACCT                                                    
         GOTOR (#CPYINI,ACPYINI)   INITIALISE COMPANY VALUES                    
         MVC   AGENCY,CUXCPY                                                    
         MVC   USRID,CUUSER                                                     
         MVC   GLOBID,CPYUID                                                    
                                                                                
INIT02   L     R1,ALP              RESTORE A(LP_D)                              
         MVC   AALIOB,LP_ALIOB     EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Process and Upload Record                                           *         
***********************************************************************         
                                                                                
INPUT    LA    RF,RECTAB                                                        
         USING RECTABD,RF                                                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   Look up record map code                      
         BE    INPUT02                                                          
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                -> unknown record type                       
                                                                                
INPUT02  MVC   RECTYPE,RECTTYPE    -> set known record type                     
                                                                                
         XR    R0,R0               BUILD DOWNLOAD MAP ELEMENT                   
         ICM   R0,3,LP_QMAPN                                                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         GOTOR UPDREC              PROCESS THE INPUT RECORD                     
                                                                                
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Go to Upload Record handling routine                                *         
***********************************************************************         
                                                                                
UPDREC   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LAY   R0,ERRTAB           Clear error table                            
         LA    R1,L'ERRTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LAY   RE,ERRTAB                                                        
         MVI   0(RE),ET_EOTQ       Initialise error table                       
         GOTOR (#SYSCHK,ASYSCHK)                                                
         BE    UPDREC2                                                          
         GOTOR SAVERR,DMCB,=AL2(AE$FLRD),0,0                                    
         J     EXITN                                                            
                                                                                
UPDREC2  XR    RF,RF                                                            
         IC    RF,RECTYPE                                                       
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         CHI   RF,UPDTABL                                                       
         BL    *+6                                                              
         DC    H'0'                                                             
         B     UPDTAB(RF)                                                       
                                                                                
UPDTAB   DS    0XL4                                                             
         J     PROCHDR             Person header  - A0                          
         J     PROCA1              Client Access  - A1                          
         J     PROCA2              Media access   - A2                          
         J     PROCA3              Expend.type access - A3                      
         J     PROCA4              Non client access - A4                       
         J     PROCA5              Staff account access - A5                    
         J     PROCA6              Work code access - A6                        
         J     PROCA7              Report format access - A7                    
         J     PROCA8              Estimate scheme access - A8                  
         J     PROCA9              Supplier access - A9                         
         J     PROCAA              Group list access - AA                       
         J     PROCB0              Global approval - B0                         
         J     PROCCAP             Client approval - B1                         
         J     PROCNCP             Non client approval - B2                     
         J     PROCSAP             Staff approval - B3                          
         J     PROCIOAP            Invoice orders approval - B4                 
         J     PROCBKUP            Back up approval -  B5                       
         J     PROCROL             Role upload    - B6                          
         J     PROCTRL             Person trailer - BF                          
         J     PROCPRAC            Person add/delete to account - 04            
UPDTABL  EQU   *-UPDTAB                                                         
                                                                                
UPDRECY  J     EXITY                                                            
                                                                                
UPDRECN  J     EXITN                                                            
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Person - Header         A0                                          *         
***********************************************************************         
PROCHDR  BASE  ,                                                                
         MVI   TWAMODE,0                                                        
         LAY   R0,ERRTAB           Clear error table                            
         LA    R1,L'ERRTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GOTSAR,DMCB,('TSAINI',0)                                         
         XC    TSARNUMB,TSARNUMB                                                
         J     UPDPERX                                                          
***********************************************************************         
* Person - A1                                                         *         
***********************************************************************         
PROCA1   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA1,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_ACLAC                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA1LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - A2                                                         *         
***********************************************************************         
PROCA2   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA2,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_AMEAC                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA2LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - A3                                                         *         
***********************************************************************         
PROCA3   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA3,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_AEXAC                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA3LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - A4                                                         *         
***********************************************************************         
PROCA4   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA4,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_ANCAC                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA4LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - A5                                                         *         
***********************************************************************         
PROCA5   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA5,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_ASTAC                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA5LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - A6                                                         *         
***********************************************************************         
PROCA6   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA6,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_AWKAC                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA6LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - A7                                                         *         
***********************************************************************         
PROCA7   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA7,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_ARPAC                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA7LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - A8                                                         *         
***********************************************************************         
PROCA8   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA8,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_AESTA                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA8LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - A9                                                         *         
***********************************************************************         
PROCA9   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         CLI   QA_DEFA9,DEFLISTQ                                                
         JNE   UPDPERX             not a list                                   
         XR    RE,RE                                                            
         ICM   RE,7,QA_ASUAC                                                    
         JNZ   *+6                                                              
         DC    H'0'                Should be a list                             
         ST    RE,FULL1                                                         
         MVI   BYTE1,MA9LENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - AA  - send to TSAR as  rec type 99 to force to end         *         
***********************************************************************         
PROCAA   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         SR    RE,RE                                                            
         ICM   RE,7,QA_AGRPL                                                    
         JZ    UPDPERX             not a list                                   
         ST    RE,FULL1                                                         
         MVI   BYTE1,MAALENQ                                                    
         GOTOR SORTMAP                                                          
         GOTOR WMP2TSAR                                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - B0  - no need to save - will be there at the end           *         
***********************************************************************         
PROCB0   BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         J     UPDPERX                                                          
***********************************************************************         
* Person - Client Approval Rights Upload   B1                         *         
***********************************************************************         
PROCCAP  BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         LAY   R0,ERRTAB           Clear error table                            
         LA    R1,L'ERRTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CLI   QA_ESDFA,YESQ        estimate default approver                   
         JNE   PCAP10                                                           
         MVI   BYTE1,JOBPAEST                                                   
         MVI   BYTE2,YESQ           yes - I want the default                    
         GOTOR DUPCHEK                                                          
         JE    PCAP10               not found - good                            
         MVC   ROUERRV,=AL2(AE$DUPAE)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
PCAP10   CLI   QA_INEDA,YESQ        internal estimate default approver          
         JNE   PCAP20                                                           
         MVI   BYTE1,JOBPAESI                                                   
         MVI   BYTE2,YESQ           yes - I want the default                    
         GOTOR DUPCHEK                                                          
         JE    PCAP20               not found - good                            
         MVC   ROUERRV,=AL2(AE$DUPIE)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PCAP20   CLI   QA_E1AB,YESQ         expense lev1 billable approver              
         JNE   PCAP30                                                           
         MVI   BYTE1,JOBPAEXP                                                   
         TM    SCPXEL+CPXSTAT6-CPXELD,CPX2LAEI                                  
         JZ    *+8                                                              
         MVI   BYTE1,JOBPAX1B                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PCAP30               not found - good                            
         MVC   ROUERRV,=AL2(AE$DUPXA)                                           
         TM    SCPXEL+CPXSTAT6-CPXELD,CPX2LAEI                                  
         JZ    *+10                                                             
         MVC   ROUERRV,=AL2(AE$DUPX1)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PCAP30   TM    SCPXEL+CPXSTAT6-CPXELD,CPX2LAEI                                  
         JZ    PCAP60                                                           
         CLI   QA_E1ANB,YESQ        expense lev1 nonbillable approver           
         JNE   PCAP40                                                           
         MVI   BYTE1,JOBPAX1N                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PCAP40                                                           
         MVC   ROUERRV,=AL2(AE$DUPN1)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PCAP40   CLI   QA_E2AB,YESQ         expense lev2 billable approver              
         JNE   PCAP50                                                           
         MVI   BYTE1,JOBPAX2B                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PCAP50                                                           
         MVC   ROUERRV,=AL2(AE$DUPX2)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PCAP50   CLI   QA_E2ANB,YESQ        expense lev2 nonbillable approver           
         JNE   PCAP60                                                           
         MVI   BYTE1,JOBPAX2N                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PCAP60                                                           
         MVC   ROUERRV,=AL2(AE$DUPN2)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PCAP60   CLI   QA_JOBDA,YESQ        job default approver                        
         JNE   PCAP70                                                           
         MVI   BYTE1,JOBPAJOB                                                   
         MVI   BYTE2,YESQ           yes - I want the default                    
         GOTOR DUPCHEK                                                          
         JE    PCAP70                                                           
         MVC   ROUERRV,=AL2(AE$DUPDJ)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PCAP70   CLI   QA_TIMAP,YESQ        time approver                               
         JNE   PCAP100                                                          
         MVI   BYTE1,JOBPATIM                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PCAP100                                                          
         MVC   ROUERRV,=AL2(AE$DUPTA)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PCAP100  LA    R1,TSARREC                                                       
         USING TSAR_D,R1                                                        
         MVC   TSARTYPE,RECTYPE     ALL OK NOW STORE IN TSAR                    
         XC    TSARDATA(TSARLENQ),TSARDATA                                      
         MVC   TSB1_OFF,QA_OFFCO                                                
         MVC   TSB1_CLI,QA_CLICO                                                
         MVC   TSB1_PRO,QA_PROCO                                                
         MVC   TSB1_JBC,QA_JOBCO                                                
         MVC   TSB1_MED,QA_MEDCO                                                
         MVC   TSB1_EST,QA_ESTAP                                                
         MVC   TSB1_ESD,QA_ESDFA                                                
         MVC   TSB1_INA,QA_INTEA                                                
         MVC   TSB1_IDA,QA_INEDA                                                
         MVC   TSB1_X1B,QA_E1AB                                                 
         MVC   TSB1_1NB,QA_E1ANB                                                
         MVC   TSB1_X2B,QA_E2AB                                                 
         MVC   TSB1_2NB,QA_E2ANB                                                
         MVC   TSB1_JBA,QA_JOBAP                                                
         MVC   TSB1_JDA,QA_JOBDA                                                
         MVC   TSB1_TIM,QA_TIMAP                                                
         DROP  R1                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - Non Client Approval Upload    B2                           *         
***********************************************************************         
PROCNCP  BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         LAY   R0,ERRTAB           Clear error table                            
         LA    R1,L'ERRTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CLI   QA_TIME,YESQ         time approver                               
         JNE   PNCP10                                                           
         MVI   BYTE1,NCTPATIM                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PNCP10                                                           
         MVC   ROUERRV,=AL2(AE$DUPTA)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PNCP10   LA    R1,TSARREC                                                       
         USING TSAR_D,R1                                                        
         MVC   TSARTYPE,RECTYPE                                                 
         XC    TSARDATA(TSARLENQ),TSARDATA                                      
         MVC   TSB2_NCC,QA_NCLCO                                                
         MVC   TSB2_TIM,QA_TIME                                                 
         DROP  R1                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - Staff Approval Upload    B3                                *         
***********************************************************************         
PROCSAP  BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         LAY   R0,ERRTAB           Clear error table                            
         LA    R1,L'ERRTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CLI   QA_EXLMA,YESQ        expenses line manager approver              
         JNE   PSAP10                                                           
         MVI   BYTE1,DPAPAEXP                                                   
         TM    SCPXEL+CPXSTAT6-CPXELD,CPX2LAEI                                  
         JZ    *+8                                                              
         MVI   BYTE1,DPAPAEX1                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PSAP10                                                           
         MVC   ROUERRV,=AL2(AE$DUPXL)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PSAP10   CLI   QA_E2LMA,YESQ        expense l2 line manager approver            
         JNE   PSAP30                                                           
         MVI   BYTE1,DPAPAEX2                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PSAP30                                                           
         MVC   ROUERRV,=AL2(AE$DUPL2)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PSAP30   CLI   QA_EXDFA,YESQ        expense default finance approver            
         JNE   PSAP40                                                           
         MVI   BYTE1,DPAPAEXF                                                   
         MVI   BYTE2,YESQ           yes - I want the default                    
         GOTOR DUPCHEK                                                          
         JE    PSAP40                                                           
         MVC   ROUERRV,=AL2(AE$DUPXD)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PSAP40   CLI   QA_TIME,YESQ         time approver                               
         JNE   PSAP50                                                           
         MVI   BYTE1,DPAPATIM                                                   
         MVI   BYTE2,NOQ            no - not the default                        
         GOTOR DUPCHEK                                                          
         JE    PSAP50                                                           
         MVC   ROUERRV,=AL2(AE$DUPTA)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PSAP50   LA    R1,TSARREC                                                       
         USING TSAR_D,R1                                                        
         MVC   TSARTYPE,RECTYPE                                                 
         XC    TSARDATA(TSARLENQ),TSARDATA                                      
         MVC   TSB3_OFF,QA_OFFCO                                                
         MVC   TSB3_DEP,QA_DEPCO                                                
         MVC   TSB3_SUB,QA_SUBDC                                                
         MVC   TSB3_PER,QA_PERCO                                                
         MVC   TSB3_XMG,QA_EXLMA                                                
         MVC   TSB3_XM2,QA_E2LMA                                                
         MVC   TSB3_FIN,QA_EXFAP                                                
         MVC   TSB3_DFN,QA_EXDFA                                                
         MVC   TSB3_TIM,QA_TIME                                                 
         ZAP   TSB3_XMA,PZERO                                                   
         OC    QA_EXAPL,QA_EXAPL                                                
         JZ    *+10                                                             
         ZAP   TSB3_XMA,QA_EXAPL                                                
         DROP  R1                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         J     UPDPERX                                                          
                                                                                
***********************************************************************         
* Person - Invoices Orders Approval Upload  B4                        *         
***********************************************************************         
PROCIOAP BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         LAY   R0,ERRTAB           Clear error table                            
         LA    R1,L'ERRTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   BYTE2,YESQ                                                       
         CLI   QA_OAD,YESQ                                                      
         JNE   PIOAP2                                                           
         MVI   BYTE1,APPPORD                                                    
         GOTOR DUPCHEK                                                          
         JE    PIOAP2                                                           
         MVC   ROUERRV,=AL2(AE$DUPDO)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PIOAP2   CLI   QA_OFD,YESQ                                                      
         JNE   PIOAP4                                                           
         MVI   BYTE1,APPPORDF                                                   
         GOTOR DUPCHEK                                                          
         JE    PIOAP4                                                           
         MVC   ROUERRV,=AL2(AE$DUPDF)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PIOAP4   CLI   QA_IAD,YESQ                                                      
         JNE   PIOAP10                                                          
         MVI   BYTE1,APPPINV                                                    
         GOTOR DUPCHEK                                                          
         JE    PIOAP10                                                          
         MVC   ROUERRV,=AL2(AE$DUPDI)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
                                                                                
PIOAP10  LA    R1,TSARREC                                                       
         USING TSAR_D,R1                                                        
         MVC   TSARTYPE,RECTYPE                                                 
         XC    TSARDATA(TSARLENQ),TSARDATA                                      
         MVC   TSB4_OA,QA_OA                                                    
         MVC   TSB4_OAD,QA_OAD                                                  
         MVC   TSB4_OF,QA_OF                                                    
         MVC   TSB4_OFD,QA_OFD                                                  
         MVC   TSB4_IA,QA_IA                                                    
         MVC   TSB4_IAD,QA_IAD                                                  
         MVC   TSB4_IOT,QA_IOTYP                                                
         MVC   TSB4_CLI,QA_CLICO                                                
         MVC   TSB4_PRO,QA_PROCO                                                
         MVC   TSB4_JBC,QA_JOBCO                                                
         MVC   TSB4_OFF,QA_OFFCO                                                
         MVC   TSB4_DEP,QA_DEPCO                                                
         MVC   TSB4_EXP,QA_EXPCO                                                
         MVC   TSB4_SLC,QA_SUPLC                                                
         MVC   TSB4_SAC,QA_SUPAC                                                
         MVC   TSB4_2PC,QA_2PAC                                                 
         MVC   TSB4_MED,QA_MEDCO                                                
         ZAP   TSB4_SLF,PZERO                                                   
         ZAP   TSB4_APP,PZERO                                                   
         OC    QA_SLFAA,QA_SLFAA                                                
         JZ    *+10                                                             
         ZAP   TSB4_SLF,QA_SLFAA                                                
         OC    QA_APPAM,QA_APPAM                                                
         JZ    *+10                                                             
         ZAP   TSB4_APP,QA_APPAM                                                
         DROP  R1                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         J     UPDPERX                                                          
***********************************************************************         
* Person - Back Up Approval Upload   B5                               *         
***********************************************************************         
PROCBKUP BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         LAY   R0,ERRTAB           Clear error table                            
         LA    R1,L'ERRTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R1,TSARREC                                                       
         USING TSAR_D,R1                                                        
         MVC   TSARTYPE,RECTYPE                                                 
         XC    TSARDATA(TSARLENQ),TSARDATA                                      
         MVC   TSB5_PIN,QA_BUPIN                                                
         MVC   TSB5_TIM,QA_TIME                                                 
         MVC   TSB5_EXP,QA_EXPEN                                                
         MVC   TSB5_ORD,QA_ORDER                                                
         MVC   TSB5_INV,QA_INVCE                                                
         DROP  R1                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         J     UPDPERX                                                          
                                                                                
***********************************************************************         
* Person - Role Upload    B6                                          *         
***********************************************************************         
PROCROL  BASE  ,                                                                
         NI    TWAMODE,255-TWAMERP clear error this time                        
         LAY   R0,ERRTAB           Clear error table                            
         LA    R1,L'ERRTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R1,TSARREC                                                       
         USING TSAR_D,R1                                                        
         MVC   TSARTYPE,RECTYPE                                                 
         XC    TSARDATA(TSARLENQ),TSARDATA                                      
         MVC   TSB6_ROL,QA_ROLE                                                 
         DROP  R1                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         J     UPDPERX                                                          
                                                                                
***********************************************************************         
* Person - Trailer   BF                                               *         
***********************************************************************         
PROCTRL  BASE  ,                                                                
         TM    TWAMODE,TWAMPER     any errors ?                                 
         JNZ   UPDOUTN                                                          
         LA    RF,1                                                             
         STCM  RF,3,TSARNUMB        prepare to get tsar recs back               
         MVI   TWAMODE,0                                                        
         GOTOR LIMNEW               build lidels/rstel from new data            
         GOTOR LIMOLD               examine existing lim access recs            
         GOTOR LIMUPD               update limit list records                   
         GOTOR LIMAUD               update audit rec for limit list             
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,TSARNUMB                                                    
         SHI   RE,1                                                             
         STCM  RE,3,TSARNUMB       reread this tsar rec                         
         GOTOR APPNEW              change TSAR data to elements                 
         GOTOR APPOLD              save other els,write audit rec               
         GOTOR APPUPD              update approver records                      
         GOTOR APPAUD              update audit rec for approver                
                                                                                
         GOTOR ROLEUPD             update role records                          
                                                                                
         GOTOR GRPUPD              update group recs/write audit rec            
                                                                                
         LA    R0,QA_VALS          all done - clear input data                  
         LA    R1,QA_VALSL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     UPDPERX                                                          
                                                                                
***********************************************************************         
* Person add/delete to account - 0004                                 *         
***********************************************************************         
PROCPRAC BASE  ,                                                                
         CLI   QA_ACTN,QA_DELQ     Check action is valid                        
         JE    PRAC002                                                          
         CLI   QA_ACTN,QA_ADDQ                                                  
         JE    PRAC002                                                          
         GOTOR SAVERR,DMCB,=AL2(AE$INACT),0,0  Issue error otherwise            
         J     UPDPERX                                                          
                                                                                
PRAC002  XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,QA_UNIT                                                   
         GOTOR (#SETLDG,ASETLDG)   Check ledger exists                          
         JE    PRAC004                                                          
         GOTOR SAVERR,DMCB,=AL2(AE$INLDG),0,0  Issue error otherwise            
         J     UPDPERX                                                          
                                                                                
PRAC004  OC    QA_APIN,QA_APIN    Do we have a person                           
         JZ    UPDPERX                                                          
         MVC   AC_OFF,SPACES                                                    
         USING ACTRECD,R6          R3=A(Account record)                         
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,SPACES     Build key of account record                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT,QA_UNIT                                                  
         MVC   ACTKLDG,QA_LEDG                                                  
         LLC   RF,LDGAL1                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),QA_ACCT                                               
         EX    RF,0(RE)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    PRAC006                                                          
         GOTOR SAVERR,DMCB,=AL2(AE$INACC),0,0                                   
         J     UPDPERX                                                          
PRAC006  L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
PRAC008  CLI   PPREL,0                                                          
         JE    PRAC014                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    PRAC012                                                          
                                                                                
PRAC010  IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     PRAC008                                                          
                                                                                
PRAC012  CLI   PPRGAOFF,X'40'                                                   
         JNH   PRAC014                                                          
         MVC   AC_OFF,PPRGAOFF                                                  
         OC    AC_OFF,SPACES                                                    
                                                                                
         USING ACTRECD,R6          R3=A(Account record)                         
PRAC014  LA    R6,IOKEY                                                         
         MVC   ACTKEY,SPACES     Build key of account record                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT,QA_UNIT                                                  
         MVC   ACTKLDG,QA_LEDG                                                  
         LLC   RF,LDGAL2                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),QA_ACCT                                               
         EX    RF,0(RE)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    PRAC016                                                          
         GOTOR SAVERR,DMCB,=AL2(AE$INACC),0,0                                   
         J     UPDPERX                                                          
PRAC016  L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
PRAC018  CLI   PPREL,0                                                          
         JE    PRAC024                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    PRAC022                                                          
                                                                                
PRAC020  IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     PRAC018                                                          
                                                                                
PRAC022  CLI   PPRGAOFF,X'40'                                                   
         JNH   PRAC024                                                          
         MVC   AC_OFF,PPRGAOFF                                                  
         OC    AC_OFF,SPACES                                                    
                                                                                
         USING ACTRECD,R6          R3=A(Account record)                         
PRAC024  LA    R6,IOKEY                                                         
         MVC   ACTKEY,SPACES     Build key of account record                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT,QA_UNIT                                                  
         MVC   ACTKLDG,QA_LEDG                                                  
         MVC   ACTKACT,QA_ACCT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    PRAC026                                                          
         GOTOR SAVERR,DMCB,=AL2(AE$INACC),0,0                                   
         J     UPDPERX                                                          
PRAC026  L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,ABLOCK                                                        
         USING CPTRBLK,R4                                                       
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
                                                                                
         L     R6,AIO1                                                          
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('JOBELQ',ACTRECD),0                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,12(R1)                                                        
         USING JOBELD,R2                                                        
         CLC   QA_INDX,JOBREVNO    Check for concurrent updating                
         JE    PRAC028             No other updates fine                        
         GOTOR SAVERR,DMCB,=AL2(AE$FATAL),0,0 Yes - error                       
         J     UPDPERX                                                          
                                                                                
PRAC028  XR    RE,RE                                                            
         ICM   RE,3,JOBREVNO       Increment index number                       
         AHI   RE,1                                                             
         STCM  RE,3,JOBREVNO                                                    
         STCM  RE,3,OA_INDX                                                     
                                                                                
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('LIDELQ',ACTRECD),           +        
               (2,=AL1(L'LIDROLN+L'LIDBPID,LIDTTEAJ))                           
         CLI   12(R1),0            Does client team element exist               
         JE    PRAC040             Yes                                          
         CLI   QA_ACTN,QA_DELQ     If action is delete must be invalid          
         JNE   PRAC030              as no element exists                        
         GOTOR SAVERR,DMCB,=AL2(AE$INACT),0,0                                   
         J     UPDPERX                                                          
                                                                                
PRAC030  XC    ELEMENT,ELEMENT     As element didn't exist before               
         LA    R3,ELEMENT           build from scratch with PIN passed          
         USING LIDELD,R3             and added to account record                
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDITLN,L'LIDROLN+L'LIDBPID                                      
         MVI   LIDTYPE,LIDTTEAJ                                                 
         MVC   LIDBPID,QA_APIN                                                  
         MVI   LIDLN,LIDTMLNQ+LIDLNDQ                                           
         J     PRAC100                                                          
                                                                                
PRAC040  L     R2,12(R1)                                                        
OLD      USING LIDELD,R2                                                        
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         MVI   LIDEL,LIDELQ        Build new elements                           
         MVI   LIDITLN,L'LIDROLN+L'LIDBPID  Set entry length                    
         MVI   LIDTYPE,LIDTTEAJ    Set type of list element                     
         MVI   LIDLN,LIDLNDQ       Set length without data entries              
         LLC   R0,OLD.LIDLN        Length of old element                        
         SHI   R0,LIDTMLNQ         Length of pid role entries                   
         SRL   R0,2                divide by 4 to get number of entries         
         LA    R2,OLD.LIDROLN      R2=A(entries of role and pid on old)         
         LA    R4,LIDROLN          R4=A(entries of role and pid on new)         
O        USING LIDROLN,R2                                                       
N        USING LIDROLN,R4                                                       
PRAC050  OC    O.LIDBPID,O.LIDBPID Do we have a valid PID                       
         JZ    PRAC070             No skip adding                               
         CLC   O.LIDBPID,QA_APIN   Yes - compare person and skip adding         
         JE    PRAC070              them to the new element if same             
PRAC060  MVC   N.LIDROLN(LIDTMLNQ),O.LIDROLN  Add entries from old              
         LLC   R1,LIDLN                        element to new element           
         AHI   R1,LIDTMLNQ                                                      
         STC   R1,LIDLN                                                         
         LA    R4,LIDTMLNQ(R4)      Bump to next entry on new element           
PRAC070  LA    R2,LIDTMLNQ(R2)      Bump to next entry on old element           
         JCT   R0,PRAC050           End of old element?                         
                                                                                
         CLI   QA_ACTN,QA_ADDQ      If action is add                            
         JNE   PRAC080                                                          
         MVC   N.LIDBPID,QA_APIN    add to end of element                       
         LLC   R1,LIDLN             increase length of element                  
         AHI   R1,LIDTMLNQ                                                      
         STC   R1,LIDLN                                                         
                                                                                
PRAC080  GOTO1 VHELLO,DMCB,(C'D',ACCMST),('LIDELQ',ACTRECD),           +        
               (2,=AL1(L'LIDROLN+L'LIDBPID,LIDTTEAJ))                           
         CLI   12(R1),0            Did we delete element ok                     
         JE    *+6                                                              
         DC    H'0'                Die if we didn't                             
                                                                                
PRAC100  CLI   LIDLN,LIDLNDQ   Any persons left on it                           
         JNH   PRAC110         No - so don't add back to record                 
         GOTOR VHELLO,DMCB,(C'P',ACCMST),ACTRECD,LIDELD,ADDCODE                 
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
PRAC110  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CPTRBLK,R4                                                       
         LA    R4,ELEMENT          delete passives                              
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   ACCOFFC,AC_OFF                                                   
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
*                                                                               
         DROP  O,N,OLD,R3,R4                                                    
***********************************************************************         
*   Global exits for person Upload Server                             *         
***********************************************************************         
                                                                                
         USING LIOBD,R4                                                         
UPDPERX  L     R4,AALIOB           after updates pass return data               
         TM    TWAMODE,TWAMERP+TWAMEDP+TWAMUWD exit on all errors               
         JNZ   UPDOUX10                                                         
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         GOTOR VHEXOUT,DMCB,OA_INDX,WORK,L'OA_INDX                              
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#ID#),('LD_CHARQ',WORK),(L'OA_INDX*2,0)              
         J     UPDOUTY                                                          
*                                                                               
UPDOUX10 OI    TWAMODE,TWAMPER                                                  
*                                                                               
         LAY   R2,ERRTAB           Send errors                                  
         USING ET_D,R2             R2=A(Error table)                            
UPDOUX12 SR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
UPDOUX14 LA    R1,DMCB                                                          
         USING GETTXTD,R1          Build error text string in ELEMENT           
         CLI   ET_D,ET_EOTQ        Test end of error table                      
         JE    UPDOUX18                                                         
         XC    GTBLOCK,GTBLOCK     Resolve error text                           
         MVI   GTMAXL,80                                                        
         MVC   GTMSGNO,ET_ERRNO                                                 
         LA    R0,ELEMENT                                                       
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LLC   R0,ET_LN                                                         
         SHI   R0,ET_LN1Q                                                       
         LTR   R0,R0                                                            
         JZ    UPDOUX16                                                         
         STC   R0,GTLTXT           Set length of extra text                     
         LA    R0,ET_EXTRA                                                      
         STCM  R0,7,GTATXT         Set A(Extra text)                            
UPDOUX16 GOTOR VGETTXT,(R1)                                                     
         LLC   R0,4(R1)            Length of text just added                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERR),      +        
               ('LD_CHARQ',ELEMENT),((R0),0)                                    
         LLC   R0,ET_LN            Bump to next error table entry               
         AR    R2,R0                                                            
         J     UPDOUX12                                                         
         DROP  R1,R2                                                            
                                                                                
UPDOUX18 TM    TWAMODE,TWAMUWD exit on all errors                               
         JZ    UPDOUTY                                                          
         OI    GIND1,GIUNWIND  DDLINK will unwind/abend                         
         J     UPDOUTN                                                          
                                                                                
UPDOUTY  LA    R0,QA_VALS                                                       
         LA    R1,QA_VALSL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
                                                                                
UPDOUTN  LA    R0,QA_VALS                                                       
         LA    R1,QA_VALSL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITN                                                            
         DROP  RB                                                               
                                                                                
         EJECT                                                                  
SVRDEF   CSECT                                                                  
***********************************************************************         
* Routines within this server                                         *         
***********************************************************************         
***********************************************************************         
* Interface to TSAR                                                   *         
* for a PUT - TSARNUMB bumped by 1 and used as a sequence number      *         
*              just to make the tsar key unique                       *         
***********************************************************************         
                                                                                
GOTSAR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GOTSAR*'                                                      
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
         LA    R3,TSARRBLK                                                      
         USING TSARD,R3            R3=A(TSAR block)                             
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOTSAR02                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACTN,0(R2)                                                     
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         MVC   TSACOM,ACOMFACS                                                  
         LHI   R0,1024                                                          
         STCM  R0,3,TSBUFFL        Set require 1MB off-line                     
         MVI   TSRECI,TSRTSARB                                                  
         OI    TSRECI,TSRXTN                                                    
         MVI   TSKEYL,TSARKYLQ     Set key length                               
         LHI   R0,TSARLENQ                                                      
         STCM  R0,3,TSRECL         Set maximum record length                    
         GOTOR VTSAR,TSARD                                                      
         TM    TSINDS,TSIINIOK                                                  
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
GOTSAR02 MVC   TSACTN,0(R2)        Set action                                   
         LA    RF,TSARREC                                                       
         CLI   0(R2),TSAGET                                                     
         JNE   *+14                                                             
         MVC   TSRNUM,TSARNUMB     get by record number                         
         J     *+10                                                             
         MVC   TSARSEQ-TSAR_D(L'TSARSEQ,RF),TSARNUMB    If writing              
         SR    RE,RE                                                            
         ICM   RE,3,TSARNUMB                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,TSARNUMB                                                    
         GOTOR VTSAR,TSARD         Call TSAR                                    
         MVC   TSARERRS,TSERRS     Return TSARERRS                              
                                                                                
         CLI   TSARERRS,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*********************************************                                   
*  Move data from WMP to TSAR               *                                   
*  on entry byte1 = length of data          *                                   
*           full1= a(array) in WMP          *                                   
*********************************************                                   
WMP2TSAR NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*WMP2TS*'                                                      
         MVI   TWAMODE,0                                                        
         L     RE,FULL1                                                         
         LA    R4,LW_DATA2-LW_D(RE)                                             
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RE)        Number of entries                   
         LA    R3,TSARREC                                                       
         USING TSAR_D,R3                                                        
WMP2TS5  MVC   TSARTYPE,RECTYPE                                                 
         CLI   RECTYPE,RECTPGRP                                                 
         JNE   *+8                                                              
         MVI   TSARTYPE,RECTPGR2    force to end                                
         XC    TSARDATA(TSARLENQ),TSARDATA                                      
         SR    RE,RE                                                            
         IC    RE,BYTE1             length of data                              
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   TSARDATA(0),0(R4)                                                
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         XR    RE,RE                                                            
         IC    RE,BYTE1                                                         
         AR    R4,RE                point to next entry in WMP array            
         JCT   R0,WMP2TS5                                                       
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*********************************************                                   
*  READ PASSIVE TO VALDATE DUPLICATE        *                                   
*  on entry byte2 = yesq if default reqd    *                                   
*           byte2 = noq  if default not reqd*                                   
*           byte1 = passive type            *                                   
*********************************************                                   
DUPCHEK  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*DUPCHK*'                                                      
*                                   construct account code                      
         MVC   TEMP2(L'QA_CLICO+L'QA_PROCO+L'QA_JOBCO),SPACES                   
         MVC   TEMP2(L'QA_CLICO),QA_CLICO                                       
         SR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         LA    RE,TEMP2                                                         
         AR    RE,RF                                                            
         MVC   0(L'QA_PROCO,RE),QA_PROCO                                        
         SR    R1,R1                                                            
         IC    R1,PPROLEN                                                       
         SR    R1,RF                                                            
         AR    RE,R1                                                            
         MVC   0(L'QA_JOBCO,RE),QA_JOBCO                                        
         OC    TEMP2(L'QA_CLICO+L'QA_PROCO+L'QA_JOBCO),SPACES                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         CLI   RECTYPE,RECTNCAP     non client approval                         
         JE    DUPCH20                                                          
         CLI   RECTYPE,RECTSTAP     staff approval                              
         JE    DUPCH40                                                          
         CLI   RECTYPE,RECTIOAP     invoice order approval                      
         JE    DUPCH60                                                          
         USING JOBPASD,R2                                                       
         MVI   JOBPTYP,JOBPTYPQ     client approval                             
         MVI   JOBPSUB,JOBPSUBQ                                                 
         MVC   JOBPCPY,CUXCPY                                                   
         MVC   JOBPAPPL,BYTE1                                                   
         MVC   JOBPPIDB,CCTPID                                                  
         MVC   JOBPJOB,TEMP2        account code                                
         MVC   JOBPMED,QA_MEDCO                                                 
         MVC   JOBPOFFC,QA_OFFCO                                                
         MVI   JOBPSEQ,0                                                        
         MVC   CSVKEY1,JOBPAS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    DUPCH12                                                          
         DC    H'0'                                                             
DUPCH11  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
DUPCH12  CLC   CSVKEY1(JOBPPIDB-JOBPASD),JOBPAS                                 
         JNE   EXITY               all done - duplicate not found               
         CLC   JOBPPIDB,QA_PIN                                                  
         JE    DUPCH11                                                          
         CLI   BYTE2,YESQ                                                       
         JE    DUPCH15             checking default                             
         TM    JOBPSTA,JOBPDFLT                                                 
         JNZ   DUPCH11             don't want default - so ignore               
         J     EXITN               duplicate found                              
DUPCH15  TM    JOBPSTA,JOBPDFLT                                                 
         JNZ   EXITN               this is default - so error                   
         J     DUPCH11             keep looking                                 
         DROP  R2                                                               
*                                                                               
         USING NCTPASD,R2                                                       
DUPCH20  MVI   NCTPTYP,NCTPTYPQ                                                 
         MVI   NCTPSUB,NCTPSUBQ                                                 
         MVC   NCTPCPY,CUXCPY                                                   
         MVC   NCTPAPPL,BYTE1                                                   
         MVC   NCTPNCC,QA_NCLCO                                                 
         MVI   NCTPSEQ,0                                                        
         MVC   CSVKEY1,NCTPAS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    DUPCH22                                                          
         DC    H'0'                                                             
DUPCH21  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
DUPCH22  CLC   CSVKEY1(NCTPPIDB-NCTPASD),NCTPAS                                 
         JNE   EXITY               all done - duplicate not found               
         CLC   NCTPPIDB,QA_PIN                                                  
         JE    DUPCH21                                                          
         TM    NCTPSTAT,NCTPDFLT                                                
         JNZ   DUPCH21             don't want default - so ignore               
         J     EXITN               duplicate found                              
         DROP  R2                                                               
         USING DPAPASD,R2                                                       
DUPCH40  MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,CUXCPY                                                   
         MVC   DPAPAPPL,BYTE1                                                   
         MVC   DPAP1RAC,TEMP2                                                   
         MVC   CSVKEY1,DPAPAS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    DUPCH42                                                          
         DC    H'0'                                                             
DUPCH41  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
DUPCH42  CLC   CSVKEY1(DPAPXVAL-DPAPASD),DPAPAS                                 
         JNE   EXITY               all done - duplicate not found               
         CLC   DPAPPIDB,QA_PIN                                                  
         JE    DUPCH41                                                          
         CLI   BYTE2,YESQ                                                       
         JE    DUPCH45             checking default                             
         TM    DPAPSTAT,DPAPDFLT                                                
         JNZ   DUPCH41             Don't want default -so ignore                
         J     EXITN               duplicate found                              
DUPCH45  TM    DPAPSTAT,DPAPDFLT                                                
         JNZ   EXITN               this is default - so error                   
         J     DUPCH41             keep looking                                 
         DROP  R2                                                               
         USING APPPASD,R2                                                       
DUPCH60  MVI   APPPTYP,APPPTYPQ                                                 
         MVI   APPPSUB,APPPSUBQ                                                 
         MVC   APPPCAT,BYTE1                                                    
         MVC   APPPCPY,CUXCPY                                                   
DUPCH62  MVC   CSVKEY1,APPPAS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    DUPCH65                                                          
         DC    H'0'                                                             
DUPCH63  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
DUPCH65  CLC   CSVKEY1(APPPVAL-APPPASD),APPPAS                                  
         JNE   EXITY               all done - duplicate not found               
         CLC   APPPPIDB,QA_PIN                                                  
         JE    DUPCH63                                                          
         CLI   QA_IOTYP,IOTDEF                                                  
         JNE   *+8                                                              
         MVI   BYTE3,APPPDFT                                                    
         CLI   QA_IOTYP,IOTCLI                                                  
         JNE   *+8                                                              
         MVI   BYTE3,APPPCLI                                                    
         CLI   QA_IOTYP,IOTNCL                                                  
         JNE   *+8                                                              
         MVI   BYTE3,APPPNCLI                                                   
         CLI   QA_IOTYP,IOTEXP                                                  
         JNE   *+8                                                              
         MVI   BYTE3,APPPEXP                                                    
         CLI   QA_IOTYP,IOTPRO                                                  
         JNE   *+8                                                              
         MVI   BYTE3,APPPPROD                                                   
         CLI   QA_IOTYP,IOTART                                                  
         JNE   *+8                                                              
         MVI   BYTE3,APPPART                                                    
         CLI   QA_IOTYP,IOTINT                                                  
         JNE   *+8                                                              
         MVI   BYTE3,APPPINT                                                    
         CLC   APPPSCAT,BYTE3                                                   
         JNE   DUPCH63             not this sub cat                             
         CLC   APPPACCD,TEMP2                                                   
         JNE   DUPCH63             wrong account                                
*                                                                               
         OC    QA_MEDCO,QA_MEDCO                                                
         JZ    DUPCH66                                                          
         CLC   QA_MEDCO,SPACES                                                  
         JE    DUPCH66                                                          
         CLC   APPPMED,QA_MEDCO                                                 
         JNE   DUPCH63             wrong media                                  
*                                                                               
DUPCH66  OC    QA_DEPCO,QA_DEPCO                                                
         JZ    DUPCH67                                                          
         CLC   QA_DEPCO,SPACES                                                  
         JE    DUPCH67                                                          
*???     CLC   APPPDEPT,QA_DEPCO     ????????????????                           
*??      JNE   DUPCH63             wrong department                             
*                                                                               
DUPCH67  OC    QA_OFFCO,QA_OFFCO                                                
         JZ    DUPCH69                                                          
         CLC   QA_OFFCO,SPACES                                                  
         JE    DUPCH69                                                          
         CLC   APPPOFFC,QA_OFFCO                                                
         JNE   DUPCH63             wrong office                                 
DUPCH69  TM    APPPSTAT,APPPDFLT                                                
         JZ    DUPCH63             keep looking                                 
         J     EXITN               this is default - so error                   
         DROP  R2                                                               
         EJECT                                                                  
*********************************************                                   
*Sort the person access work maps           *                                   
*  on entry byte1 = length of data          *                                   
*           full1= a(array) in WMP          *                                   
*********************************************                                   
         SPACE 1                                                                
SORTMAP NTR1   LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SRTMAP*'                                                      
*                                                                               
         L     R4,FULL1                                                         
         XR    R3,R3                                                            
         IC    R3,BYTE1                                                         
         USING LW_D,R4                                                          
         LA    RF,LW_DATA2                                                      
         XR    R2,R2                                                            
         ICM   R2,3,LW_NUMN        Number of entries                            
         CHI   R2,1                                                             
         JNH   EXITY               no need to sort                              
         DROP  R4                                                               
         GOTO1 VXSORT,DMCB,(RF),(R2),(R3),(R3),0                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Create LIDELS from TSAR recs and an RCSTEL if reqd                  *         
* on exit asvelst points to start of last element in genarea          *         
***********************************************************************         
         SPACE 1                                                                
LIMNEW   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*LIMNEW*'                                                      
         USING LIDELD,R6                                                        
         USING BPELTABD,R5                                                      
         L     R0,AGENAREA         clear buffer to store elements               
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO7 use to store auditab                   
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO8 use to store auditab                   
         L     RF,AIO7                                                          
         ST    RF,AAUDITAB          next audit entry                            
                                                                                
         L     R6,AGENAREA                                                      
         ST    R6,ASVELST           start of an element                         
         XC    LASTSTYP,LASTSTYP                                                
         LA    R2,TSARREC                                                       
         USING TSAR_D,R2                                                        
LNEW20   GOTOR GOTSAR,DMCB,('TSAGET',0)                                         
         JNE   LNEW300                                                          
         CLC   TSARTYPE,LASTSTYP                                                
         JNE   LNEW30                                                           
         L     RF,ASVELST          will another item fit in element ?           
         SR    RE,RE                                                            
         IC    RE,LIDLN-LIDELD(RF)                                              
         SR    RF,RF                                                            
         IC    RF,BPELLEN                                                       
         AR    RE,RF                                                            
         CHI   RE,X'FE'                                                         
         JNH   LNEW70              yes - fits in                                
         SR    RF,RF               no - start new elemnt                        
         IC    RF,BYTE1            bump sequence number                         
         AHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         J     LNEW60                                                           
*                                  start a new element                          
LNEW30   MVC   LASTSTYP,TSARTYPE                                                
         MVI   BYTE1,0             new element sequence number                  
         LA    R5,LIMTAB                                                        
LNEW40   CLC   TSARTYPE,BPELID     find type in table                           
         JE    LNEW60                                                           
         LA    R5,BPELLNQ(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         JNE   LNEW40                                                           
         J     LNEW300             all limit list done                          
         USING LIDELD,R6                                                        
LNEW60   L     R6,ASVELST                                                       
         SR    RE,RE                                                            
         IC    RE,LIDLN                                                         
         AR    R6,RE                                                            
         ST    R6,ASVELST          start of next element                        
                                                                                
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDLN,LIDLNDQ                                                    
         MVC   LIDITLN,BPELLEN                                                  
         MVC   LIDTYPE,BPELTYPE                                                 
         MVC   LIDSEQ,BYTE1                                                     
*                                  add an item                                  
LNEW70   CLI   BPELID,RECTPCLI                                                  
         JNE   LNEW80                                                           
         LA    R4,TSARDATA                                                      
         USING MA1D,R4                                                          
         MVC   TEMP2(L'MA1CLI+L'MA1PROD+L'MA1JOBC),SPACES bld accnt cde         
         MVC   TEMP2(L'MA1CLI),MA1CLI                                           
         SR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         LA    RE,TEMP2                                                         
         AR    RE,RF                                                            
         MVC   0(L'MA1PROD,RE),MA1PROD                                          
         SR    R1,R1                                                            
         IC    R1,PPROLEN                                                       
         SR    R1,RF                                                            
         AR    RE,R1                                                            
         MVC   0(L'MA1JOBC,RE),MA1JOBC                                          
         OC    TEMP2(L'MA1CLI+L'MA1PROD+L'MA1JOBC),SPACES                       
*                                                                               
         MVC   LIDLACT,TEMP2                                                    
         MVC   LIDLOFF,MA1OFF                                                   
         CLI   MA1EST,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLESTM                                                
         CLI   MA1EXP,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLEXPN                                                
         CLI   MA1INV,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLINVC                                                
         CLI   MA1JOB,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLJOBS                                                
         CLI   MA1ORD,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLORDS                                                
         CLI   MA1RES,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLRESC                                                
         CLI   MA1TIM,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLTIME                                                
         J     LNEW200                                                          
         DROP  R4                                                               
*                                                                               
LNEW80   CLI   BPELID,RECTPMED                                                  
         JNE   LNEW90                                                           
         LA    R4,TSARDATA                                                      
         USING MA2D,R4                                                          
         MVC   LIDLMED,MA2MED                                                   
         CLI   MA2EST,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLESTM                                                
         CLI   MA2EXP,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLEXPN                                                
         CLI   MA2INV,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLINVC                                                
         CLI   MA2JOB,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLJOBS                                                
         CLI   MA2ORD,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLORDS                                                
         CLI   MA2RES,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLRESC                                                
         CLI   MA2TIM,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLTIME                                                
         SR    RF,RF                                                            
         J     LNEW200                                                          
         DROP  R4                                                               
LNEW90   CLI   BPELID,RECTPEXP                                                  
         JNE   LNEW100                                                          
         LA    R4,TSARDATA                                                      
         USING MA3D,R4                                                          
         MVC   LIDLETY,MA3EXTC                                                  
         CLI   MA3EXP,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLEXPN                                                
         CLI   MA3INV,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLINVC                                                
         CLI   MA3ORD,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLORDS                                                
         J     LNEW200                                                          
         DROP  R4                                                               
LNEW100  CLI   BPELID,RECTPNCL                                                  
         JNE   LNEW110                                                          
         LA    R4,TSARDATA                                                      
         USING MA4D,R4                                                          
         MVC   LIDLACT,MA4NCC                                                   
         CLI   MA4TIM,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLTIME                                                
         J     LNEW200                                                          
         DROP  R4                                                               
LNEW110  CLI   BPELID,RECTPSTF                                                  
         JNE   LNEW120                                                          
         LA    R4,TSARDATA                                                      
         USING MA5D,R4                                                          
         MVC   LIDLACT,SPACES                                                   
         LLC   RE,ONERL1L          For each location build 1R account           
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   LIDLACT(0),MA5OFF                                                
*                                                                               
         AHI   RE,1                                                             
         LA    R3,LIDLACT                                                       
         AR    R3,RE                                                            
         LLC   RF,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),MA5DEPT                                                  
*                                                                               
         AHI   RF,1                                                             
         AR    R3,RF               R3=A(end of dept code on 1R account)         
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),MA5SUBD                                                  
*                                                                               
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         LLC   RF,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),MA5PER                                                   
         OC    LIDLACT,SPACES                                                   
*                                                                               
         CLI   MA5EXP,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLEXPN                                                
         CLI   MA5TIM,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLTIME                                                
         J     LNEW200                                                          
         DROP  R4                                                               
LNEW120  CLI   BPELID,RECTPWC                                                   
         JNE   LNEW130                                                          
         LA    R4,TSARDATA                                                      
         USING MA6D,R4                                                          
         MVC   LIDLWC,MA6WORK                                                   
         CLI   MA6TIM,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLTIME                                                
         J     LNEW200                                                          
         DROP  R4                                                               
LNEW130  CLI   BPELID,RECTPREP                                                  
         JNE   LNEW140                                                          
         LA    R4,TSARDATA                                                      
         USING MA7D,R4                                                          
         MVC   LIDLREP,MA7FORM                                                  
         CLI   MA7REP,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLREPT                                                
         J     LNEW200                                                          
         DROP  R4                                                               
LNEW140  CLI   BPELID,RECTPEST                                                  
         JNE   LNEW150                                                          
         LA    R4,TSARDATA                                                      
         USING MA8D,R4                                                          
         MVC   LIDLSCH,MA8SCH                                                   
         CLI   MA8EST,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLESTM                                                
         J     LNEW200                                                          
         DROP  R4                                                               
LNEW150  CLI   BPELID,RECTPSUP                                                  
         JE    *+6                                                              
         DC    H'0'               what is it then ????                          
         LA    R4,TSARDATA                                                      
         USING MA9D,R4                                                          
         MVI   LIDLSUNT,C'S'                                                    
         MVC   LIDLSLDG,MA9SLED                                                 
         MVC   LIDLSACT,MA9SACC                                                 
         CLI   MA9INV,YESQ                                                      
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLINVC                                                
         J     LNEW200                                                          
         DROP  R4                                                               
*                                  add to audit table                           
LNEW200  L     RF,AAUDITAB                                                      
         USING AUDITABD,RF                                                      
         MVI   AUDIFLAG,STCSADD    guess add                                    
         MVC   AUDITYPE,BPELTYPE                                                
         MVC   AUDIAPPL,LIDLAPPL                                                
         MVC   AUDIAPP2,LIDLAPP2                                                
         SR    RE,RE                                                            
         IC    RE,BPELLEN                                                       
                                                                                
         LR    R0,RE               save                                         
         AHI   RE,L'AUDIFLAG+L'AUDITYPE+L'AUDITLEN                              
         STC   RE,AUDITLEN                                                      
         LR    RE,R0               restore                                      
                                                                                
         SHI   RE,3                exec len + l'lidlappl & lidlapp2             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   AUDIDATA(0),LIDLACT                                              
         ICM   R1,15,AAUDITAB                                                   
         SR    RE,RE                                                            
         IC    RE,AUDITLEN                                                      
         AR    R1,RE                                                            
         STCM  R1,15,AAUDITAB                                                   
         DROP  RF                                                               
*                                                                               
         L     RF,ASVELST          start of the element                         
         SR    RE,RE                                                            
         IC    RE,LIDLN-LIDELD(RF)                                              
         SR    R1,R1                                                            
         IC    R1,BPELLEN                                                       
         AR    RE,R1                                                            
         STC   RE,LIDLN-LIDELD(RF)                                              
         AR    R6,R1               bump along genarea                           
         J     LNEW20                                                           
         DROP  R2,R6                                                            
*                                  any rstel needed ?                           
LNEW300  XC    HALF2,HALF2                                                      
         CLI   QA_DEFA1,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2,RSTAJOBS                                                   
         CLI   QA_DEFA2,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2,RSTAMED                                                    
         CLI   QA_DEFA3,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2,RSTAETYP                                                   
         CLI   QA_DEFA4,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2,RSTA1NAC                                                   
         CLI   QA_DEFA5,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2,RSTASTAF                                                   
         CLI   QA_DEFA6,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2,RSTAWC                                                     
         CLI   QA_DEFA7,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2,RSTAREPF                                                   
         CLI   QA_DEFA8,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2,RSTASCHM                                                   
         CLI   QA_DEFA9,DEFNONEQ                                                
         JNE   *+8                                                              
         OI    HALF2+1,RSTASUPP                                                 
         J     EXITY                                                            
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* look at existing record(s)                                          *         
* any element which is not a lidel or rstel should be added to genarea*         
* lidels are being replaced with new input in genarea                 *         
* rstel is being updated new input in genarea                         *         
* add any list rows not already in audit table flagged as delete      *         
* set AUPAGOLD flags to all/none/list for the 9 types a1-a9           *         
* on entry half2 = rstel brand ocean status (rstacst1&2)              *         
***********************************************************************         
LIMOLD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*LIMOLD*'                                                      
         L     R3,AIO1                                                          
         L     R2,ASVELST                                                       
         SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         AR    R2,RE                                                            
         ST    R2,ASVELST          start of next element in genarea             
                                                                                
         MVI   AUPAGOLD,DEFALLQ    old details for audit - guess ALL            
         MVC   AUPAGOLD+1(L'AUPAGOLD-1),AUPAGOLD                                
         USING LLSRECD,R3          R3=A(Limit list record)                      
K        USING LLSKEY,IOKEY                                                     
         XC    K.LLSKEY,K.LLSKEY   Build key of limit list record               
         MVI   K.LLSKTYP,LLSKTYPQ                                               
         MVI   K.LLSKSUB,LLSKSUBQ                                               
         MVC   K.LLSKCPY,CUXCPY                                                 
         MVC   K.LLSKPIDB,QA_PIN                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    LIMOL40                                                          
         MVC   AUPAGOLD,SPACES     no existing data                             
         J     LIMOL900                                                         
LIMOL40  L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,LLSRFST                                                       
LIMOL60  CLI   0(R3),0                                                          
         JE    LIMOL270                                                         
         CLI   0(R3),LIDELQ                                                     
         JE    LIMOL100                                                         
         CLI   0(R3),RSTELQ                                                     
         JE    LIMOL250                                                         
LIMOL80  SR    RE,RE               don't know what this is but keep!            
         IC    RE,1(R3)            element length                               
         SHI   RE,1                exec len                                     
         BASR  RF,0                                                             
         MVC   0(0,R2),0(R3)                                                    
         EX    RE,0(RF)                                                         
                                                                                
         AHI   RE,1                back to element len                          
         AR    R2,RE                                                            
         ST    R2,ASVELST                                                       
         J     LIMOL260                                                         
***                                                                             
         USING LIDELD,R3                                                        
LIMOL100 LA    RF,QA_DEFA1                                                      
         CLI   LIDTYPE,LIDTCPJL    a1                                           
         JNE   *+12                                                             
         MVI   AUPAGOLD,DEFLISTQ                                                
         J     LIMOL110                                                         
         LA    RF,QA_DEFA2                                                      
         CLI   LIDTYPE,LIDTMEDL    a2                                           
         JNE   *+12                                                             
         MVI   AUPAGOLD+1,DEFLISTQ                                              
         J     LIMOL110                                                         
         LA    RF,QA_DEFA3                                                      
         CLI   LIDTYPE,LIDTEXPL    a3                                           
         JNE   *+12                                                             
         MVI   AUPAGOLD+2,DEFLISTQ                                              
         J     LIMOL110                                                         
         LA    RF,QA_DEFA4                                                      
         CLI   LIDTYPE,LIDTNCLL    a4                                           
         JNE   *+12                                                             
         MVI   AUPAGOLD+3,DEFLISTQ                                              
         J     LIMOL110                                                         
         LA    RF,QA_DEFA5                                                      
         CLI   LIDTYPE,LIDT1RAC    a5                                           
         JNE   *+12                                                             
         MVI   AUPAGOLD+4,DEFLISTQ                                              
         J     LIMOL110                                                         
         LA    RF,QA_DEFA6                                                      
         CLI   LIDTYPE,LIDTWCL     a6                                           
         JNE   *+12                                                             
         MVI   AUPAGOLD+5,DEFLISTQ                                              
         J     LIMOL110                                                         
         LA    RF,QA_DEFA7                                                      
         CLI   LIDTYPE,LIDTSCRB    a7                                           
         JNE   *+12                                                             
         MVI   AUPAGOLD+6,DEFLISTQ                                              
         J     LIMOL110                                                         
         LA    RF,QA_DEFA8                                                      
         CLI   LIDTYPE,LIDTESCH    a8                                           
         JNE   *+12                                                             
         MVI   AUPAGOLD+7,DEFLISTQ                                              
         J     LIMOL110                                                         
         LA    RF,QA_DEFA9                                                      
         CLI   LIDTYPE,LIDTSUPP    a9                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   AUPAGOLD+8,DEFLISTQ                                              
                                                                                
LIMOL110 CLI   0(RF),DEFLISTQ                                                   
         JNE   LIMOL260                                                         
         GOTOR CHEKAUDT            was a list-still a list-upd aud tab          
         J     LIMOL260                                                         
         DROP  R3                                                               
         USING RSTELD,R3           found a RSTEL in old rec                     
LIMOL250 TM    RSTACST1,RSTAJOBS   save details for audit                       
         JZ    *+8                                                              
         MVI   AUPAGOLD,DEFNONEQ                                                
         TM    RSTACST1,RSTAMED                                                 
         JZ    *+8                                                              
         MVI   AUPAGOLD+1,DEFNONEQ                                              
         TM    RSTACST1,RSTAETYP                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+2,DEFNONEQ                                              
         TM    RSTACST1,RSTA1NAC                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+3,DEFNONEQ                                              
         TM    RSTACST1,RSTASTAF                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+4,DEFNONEQ                                              
         TM    RSTACST1,RSTAWC                                                  
         JZ    *+8                                                              
         MVI   AUPAGOLD+5,DEFNONEQ                                              
         TM    RSTACST1,RSTAREPF                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+6,DEFNONEQ                                              
         TM    RSTACST1,RSTASCHM                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+7,DEFNONEQ                                              
         TM    RSTACST1+1,RSTASUPP                                              
         JZ    *+8                                                              
         MVI   AUPAGOLD+8,DEFNONEQ                                              
         MVC   RSTACST1(L'RSTACST1+L'RSTACST2),HALF2  replace with new          
         XC    HALF2,HALF2                                                      
         J     LIMOL80                                                          
         DROP  R3                                                               
LIMOL260 SR    RF,RF               bump to next element in record               
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         J     LIMOL60                                                          
LIMOL270 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         CLC   K.LLSKEY(LLSKSEQ-LLSRECD),IOKEYSAV                               
         JE    LIMOL40             PROCESS NEXT RECORD                          
LIMOL900 OC    HALF2,HALF2         new rstel needed?                            
         JZ    EXITY               no-either no all/non or added to old         
         L     R2,ASVELST                                                       
         USING RSTELD,R2                                                        
*&&UK*&& XC    RSTEL(RSTLN4Q),RSTEL                                             
*&&US*&& XC    RSTEL(RSTLN3Q),RSTEL                                             
         MVI   RSTEL,RSTELQ                                                     
*&&UK*&& MVI   RSTLN,RSTLN4Q                                                    
*&&US*&& MVI   RSTLN,RSTLN3Q                                                    
         MVC   RSTACST1,HALF2                                                   
         MVC   RSTACST2,HALF2+1                                                 
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Update limit list record - rebuild record with data held in GENAREA *         
* buffer  - add new records when there isn't enough room to store the *         
* data                                                                *         
***********************************************************************         
                                                                                
LIMUPD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*LIMUPD*'                                                      
                                                                                
         L     R4,ABLOCK                                                        
         USING CPTRD,R4                                                         
         L     RF,AGENAREA                                                      
         ST    RF,ASVELST          start of next element to add                 
         MVI   BYTE1,0             Sequence number                              
         MVI   BYTE2,0             Indicators                                   
                                                                                
         L     R3,AIO1                                                          
         USING LLSRECD,R3          R3=A(Limit list record)                      
                                                                                
K        USING LLSKEY,IOKEY                                                     
LIMUPD02 XC    K.LLSKEY,K.LLSKEY   Build key of limit list record               
         MVI   K.LLSKTYP,LLSKTYPQ                                               
         MVI   K.LLSKSUB,LLSKSUBQ                                               
         MVC   K.LLSKCPY,CUXCPY                                                 
         MVC   K.LLSKPIDB,QA_PIN                                                
         L     R1,=AL4(IOHIUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   LIMUPD24            not found                                    
         CLC   K.LLSKEY(LLSKSEQ-LLSRECD),IOKEYSAV                               
         JNE   LIMUPD24                                                         
LIMUPD04 MVC   CSVKEY2,IOKEY                                                    
         CLI   ALLDONE,YESQ        All elements in table processed?             
         JNE   *+8                 No                                           
         OI    BYTE2,X'80'         Yes - delete original records                
                                                                                
LIMUPD08 MVC   BYTE1,K.LLSKSEQ     Save current sequence number                 
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
         TM    BYTE2,X'80'         Are we deleting records ?                    
         JNZ   LIMUPD12                                                         
         LHI   R0,LLSRFST-LLSRECD                                               
         STCM  R0,3,LLSRLEN                                                     
         GOTOR FILLREC             Fill a record with elements                  
         J     LIMUPD14                                                         
                                                                                
***********************************************************************         
* Here to write back current limit list rec and get next one          *         
***********************************************************************         
                                                                                
LIMUPD12 OI    LLSRSTAT,LLSSDELT                                                
LIMUPD14 GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,CSVKEY2                                                    
         LA    RE,IOKEY                                                         
         MVC   LLSKSTAT-LLSRECD(L'LLSKSTAT,RE),LLSRSTAT                         
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1' UPDATE DIR STATUS          
                                                                                
         L     R1,=AL4(IOSQUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   K.LLSKEY(LLSKSEQ-LLSRECD),IOKEYSAV                               
         JE    LIMUPD04            Process next record                          
***********************************************************************         
* Here when all current limit records have been processed - set       *         
* sequence number of next limit record (to be added) and test if we   *         
* we have further elements to add                                     *         
***********************************************************************         
                                                                                
                                                                                
LIMUPD22 LLC   RE,BYTE1            Bump key sequence number                     
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
                                                                                
LIMUPD24 CLI   ALLDONE,YESQ        Any more limit access elements?              
         JE    EXITY               no                                           
***********************************************************************         
* Here if we have run out of space for elements -                     *         
*  create a new record and set flag so that we issue an ADDREC        *         
***********************************************************************         
                                                                                
LIMUPD28 XC    LLSRECD(256),LLSRECD                                             
         MVC   LLSKEY,IOKEYSAV     Build a new alimit list record               
         MVC   LLSKSEQ,BYTE1                                                    
         LHI   R0,LLSRFST-LLSRECD                                               
         STCM  R0,3,LLSRLEN                                                     
         OI    BYTE2,X'40'         Set adding new records                       
                                                                                
         GOTOR FILLREC             Add elements from Genarea to Aio1            
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,CSVKEY2                                                    
         LA    RE,IOKEY                                                         
         MVC   LLSKSTAT-LLSRECD(L'LLSKSTAT,RE),LLSRSTAT                         
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1' UPDATE DIR STATUS          
                                                                                
         DROP  R3,R4                                                            
         EJECT                                                                  
******************************************************************              
* create page and row type audit stcsels                         *              
* firstly page type using ascold and qa_defa(n) flags            *              
* then row type(s) creating an stcsel for all of the list changes*              
*   then for all of the adds then for all of the deletes         *              
*   for each type i.e. A1 changes/A1 adds/A1 dels/A2changes...etc*              
******************************************************************              
LIMAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*LIMAUD*'                                                      
         L     R0,AGENAXTN         clear buffer to store audit elements         
         LH    R1,=Y(GENAREAX-GENAEXTN)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LAY   RF,GENAEXTN                                                      
         ST    RF,AAUDITAB         start of next element to add                 
                                                                                
*                                  page type audit elements                     
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING STCELD,R4                                                        
         MVI   STCEL,STCELQ                                                     
         MVI   STCLN,STCSLN2Q                                                   
         MVI   STCIND,STCIACCS                                                  
         MVC   STCSPID,CUUSER                                                   
         MVC   STCSUSR,QA_PIN                                                   
         GOTOR VDATCON,DMCB,(5,0),(1,STCSDTE)                                   
         MVC   STCSTIM,CTIME                                                    
         MVI   STCSSTAT,STCSPAGE                                                
         LA    R0,9                a1-a9                                        
         LA    R2,1                                                             
         LA    RE,AUPAGOLD                                                      
         LA    RF,QA_DEFA1                                                      
LIMAUD10 CLC   0(1,RE),0(RF)                                                    
         JE    LIMAUD40              no change at page level                    
         MVI   STCSTYPE,STCSSCPJ                                                
         CHI   R2,1                                                             
         JE    LIMAUD20                                                         
         MVI   STCSTYPE,STCSSMED                                                
         CHI   R2,2                                                             
         JE    LIMAUD20                                                         
         MVI   STCSTYPE,STCSSETY                                                
         CHI   R2,3                                                             
         JE    LIMAUD20                                                         
         MVI   STCSTYPE,STCSSNON                                                
         CHI   R2,4                                                             
         JE    LIMAUD20                                                         
         MVI   STCSTYPE,STCSSSTF                                                
         CHI   R2,5                                                             
         JE    LIMAUD20                                                         
         MVI   STCSTYPE,STCSSWC                                                 
         CHI   R2,6                                                             
         JE    LIMAUD20                                                         
         MVI   STCSTYPE,STCSSREP                                                
         CHI   R2,7                                                             
         JE    LIMAUD20                                                         
         MVI   STCSTYPE,STCSSSCH                                                
         CHI   R2,8                                                             
         JE    LIMAUD20                                                         
         MVI   STCSTYPE,STCSSSUP                                                
                                                                                
LIMAUD20 MVI   STCSSTA2,STCSCHG                                                 
         CLI   0(RE),C' '                                                       
         JNE   *+8                                                              
         MVI   STCSSTA2,STCSADD                                                 
                                                                                
         CLI   0(RF),DEFNONEQ                                                   
         JNE   *+12                                                             
         MVI   STCSPSTA,STCSPNON                                                
         J     LIMAUD30                                                         
         CLI   0(RF),DEFALLQ                                                    
         JNE   *+12                                                             
         MVI   STCSPSTA,STCSPALL                                                
         J     LIMAUD30                                                         
         MVI   STCSPSTA,STCSPLST                                                
LIMAUD30 L     R1,AAUDITAB             add page level elemnt to genarea         
         MVC   0(STCSLN2Q,R1),ELEMENT                                           
         AHI   R1,STCSLN2Q                                                      
         ST    R1,AAUDITAB                                                      
LIMAUD40 LA    RE,1(RE)            bump to next                                 
         LA    RF,1(RF)                                                         
         LA    R2,1(R2)                                                         
         JCT   R0,LIMAUD10                                                      
                                                                                
*                                  row type audit elements                      
         MVI   STCLN,STCSLN1Q                                                   
         MVI   STCSSTAT,STCSROW                                                 
         XC    ELEMENT+STCSLN1Q(L'ELEMENT-STCSLN1Q),ELEMENT+STCSLN1Q            
         LA    R3,LIMTAB                                                        
         USING BPELTABD,R3                                                      
LIMAUD50 CLI   BPELID,X'FF'                                                     
         JE    LIMAUD90                                                         
                                                                                
         MVI   BYTE3,STCSCHG                                                    
         GOTOR CSTCEL              create stcsel of changes for type            
         MVI   BYTE3,STCSADD                                                    
         GOTOR CSTCEL              create stcsel of adds for type               
         MVI   BYTE3,STCSDEL                                                    
         GOTOR CSTCEL              create stcsel of deletes for type            
                                                                                
LIMAUD60 AHI   R3,BPELLNQ                                                       
         J     LIMAUD50                                                         
LIMAUD90 MVI   BYTE2,AUDKACCS                                                   
         GOTOR PERAUD              Uses STCELS held in GENAEXTN                 
         J     EXITY                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* Create LIDELS from TSAR recs                                        *         
* on exit asvelst points to start of last element in genarea          *         
***********************************************************************         
         SPACE 1                                                                
APPNEW   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*APPNEW*'                                                      
         L     R0,AGENAREA         clear buffer to store elements               
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO7 use to store auditab                   
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO8 use to store auditab                   
         L     RF,AIO7                                                          
         ST    RF,AAUDITAB          next audit entry                            
         LA    R5,APPTAB                                                        
         USING BPELTABD,R5                                                      
                                                                                
         L     R6,AGENAREA                                                      
         USING LIDELD,R6                                                        
         ST    R6,ASVELST           start of an element                         
         XC    LASTSTYP,LASTSTYP                                                
         LA    R2,TSARREC                                                       
         USING TSAR_D,R2                                                        
APPN20   GOTOR GOTSAR,DMCB,('TSAGET',0)                                         
         JNE   EXITY                no data - all done                          
         CLC   TSARTYPE,LASTSTYP                                                
         JNE   APPN30                                                           
         L     RF,ASVELST          will another item fit in element ?           
         SR    RE,RE                                                            
         IC    RE,LIDLN-LIDELD(RF)                                              
         SR    RF,RF                                                            
         IC    RF,BPELLEN                                                       
         AR    RE,RF                                                            
         CHI   RE,X'FE'                                                         
         JNH   APPN70              yes - fits in                                
         SR    RF,RF               no - start new elemnt                        
         IC    RF,BYTE1            bump sequence number                         
         AHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         J     APPN60                                                           
*                                  start a new element                          
APPN30   MVC   LASTSTYP,TSARTYPE                                                
         MVI   BYTE1,0             new element sequence number                  
         LA    R5,APPTAB                                                        
APPN40   CLC   TSARTYPE,BPELID     find type in table                           
         JE    APPN60                                                           
         LA    R5,BPELLNQ(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         JNE   APPN40                                                           
         J     EXITY               tsar type not in table                       
         USING LIDELD,R6                                                        
APPN60   L     R6,ASVELST                                                       
         SR    RE,RE                                                            
         IC    RE,LIDLN                                                         
         AR    R6,RE                                                            
         ST    R6,ASVELST          start of next element                        
                                                                                
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDLN,LIDLNDQ                                                    
         MVC   LIDITLN,BPELLEN                                                  
         MVC   LIDTYPE,BPELTYPE                                                 
         MVC   LIDSEQ,BYTE1                                                     
*                                                                               
APPN70   CLI   BPELID,RECTCLAP     b1                                           
         JNE   APPN80                                                           
         MVC   TEMP2(L'TSB1_CLI+L'TSB1_PRO+L'TSB1_JBC),SPACES                   
         MVC   TEMP2(L'TSB1_CLI),TSB1_CLI                                       
         SR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         LA    RE,TEMP2                                                         
         AR    RE,RF                                                            
         MVC   0(L'TSB1_PRO,RE),TSB1_PRO                                        
         SR    R1,R1                                                            
         IC    R1,PPROLEN                                                       
         SR    R1,RF                                                            
         AR    RE,R1                                                            
         MVC   0(L'TSB1_JBC,RE),TSB1_JBC                                        
         OC    TEMP2(L'TSB1_CLI+L'TSB1_PRO+L'TSB1_JBC),SPACES                   
*                                                                               
         MVC   LIDASJAC,TEMP2                                                   
         MVC   LIDASJOF,TSB1_OFF                                                
         MVC   LIDASJME,TSB1_MED                                                
         CLI   TSB1_EST,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTAT,LIDAESTY                                                
         CLI   TSB1_ESD,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTAT,LIDAESTD                                                
         CLI   TSB1_JBA,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTAT,LIDAJOBY                                                
         CLI   TSB1_JDA,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTAT,LIDAJOBD                                                
         CLI   TSB1_TIM,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTAT,LIDATIME                                                
         CLI   TSB1_INA,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTA2,LIDAESI                                                 
         CLI   TSB1_IDA,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTA2,LIDAESID                                                
         CLI   TSB1_X1B,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTA2,LIDAEL1B                                                
         CLI   TSB1_1NB,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTA2,LIDAEL1N                                                
         CLI   TSB1_X2B,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTA2,LIDAEL2B                                                
         CLI   TSB1_2NB,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTA2,LIDAEL2N                                                
         J     APPN200                                                          
*                                                                               
APPN80   CLI   BPELID,RECTNCAP     b2                                           
         JNE   APPN90                                                           
         MVC   LIDA1NAC,TSB2_NCC                                                
         CLI   TSB2_TIM,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDASTAT,LIDATIME                                                
         J     APPN200                                                          
*                                                                               
APPN90   CLI   BPELID,RECTSTAP     b3                                           
         JNE   APPN120                                                          
         MVC   LIDAPACC,SPACES                                                  
         LLC   RE,ONERL1L          For each location build 1R account           
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   LIDAPACC(0),TSB3_OFF                                             
*                                                                               
         AHI   RE,1                                                             
         LA    R3,LIDAPACC                                                      
         AR    R3,RE                                                            
         LLC   RF,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),TSB3_DEP                                                 
*                                                                               
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         LLC   RF,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),TSB3_PER                                                 
*                                                                               
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         LLC   RF,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   0(0,R3),TSB3_PER                                                 
         OC    LIDAPACC,SPACES                                                  
         ZAP   LIDAPEXV,TSB3_XMA                                                
         TM    SCPXEL+CPXSTAT6-CPXELD,CPX2LAEI                                  
         JNZ   APPN100                                                          
         CLI   TSB3_XMG,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPDTY,LIDAPDEX                                                
         J     APPN110                                                          
APPN100  CLI   TSB3_XMG,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPDTY,LIDAPDE1                                                
         CLI   TSB3_XM2,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPDTY,LIDAPDE2                                                
APPN110  CLI   TSB3_FIN,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPDTY,LIDAPDEF                                                
         CLI   TSB3_DFN,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPDTY,LIDAPDED                                                
         CLI   TSB3_TIM,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPDTY,LIDAPDTI                                                
         J     APPN200                                                          
*                                                                               
APPN120  CLI   BPELID,RECTIOAP     b4                                           
         JNE   APPN180                                                          
         CLI   TSB4_OA,YESQ                                                     
         JNE   *+8                                                              
         OI    LIDAPTYP,LIDAPOR                                                 
         CLI   TSB4_OAD,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPTYP,LIDAPORD                                                
         CLI   TSB4_OF,YESQ                                                     
         JNE   *+8                                                              
         OI    LIDAPTYP,LIDAPOF                                                 
         CLI   TSB4_OFD,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPTYP,LIDAPOFD                                                
         CLI   TSB4_IA,YESQ                                                     
         JNE   *+8                                                              
         OI    LIDAPTYP,LIDAPIN                                                 
         CLI   TSB4_IAD,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDAPTYP,LIDAPIND                                                
*                                                                               
         CLI   TSB4_IOT,IOTDEF                                                  
         JNE   *+12                                                             
         MVI   LIDAPSCT,LIDADFT                                                 
         J     APPN130                                                          
         CLI   TSB4_IOT,IOTCLI                                                  
         JNE   *+12                                                             
         MVI   LIDAPSCT,LIDACLI                                                 
         J     APPN130                                                          
         CLI   TSB4_IOT,IOTNCL                                                  
         JNE   *+12                                                             
         MVI   LIDAPSCT,LIDANCLI                                                
         J     APPN130                                                          
         CLI   TSB4_IOT,IOTEXP                                                  
         JNE   *+12                                                             
         MVI   LIDAPSCT,LIDAEXP                                                 
         J     APPN130                                                          
         CLI   TSB4_IOT,IOTPRO                                                  
         JNE   *+12                                                             
         MVI   LIDAPSCT,LIDAPROD                                                
         J     APPN130                                                          
         CLI   TSB4_IOT,IOTART                                                  
         JNE   *+12                                                             
         MVI   LIDAPSCT,LIDAART                                                 
         J     APPN130                                                          
         CLI   TSB4_IOT,IOTINT                                                  
         JNE   *+8                                                              
         MVI   LIDAPSCT,LIDAINT                                                 
*                                                                               
APPN130  MVC   LIDAPOFF,TSB4_OFF                                                
         MVC   LIDAPDPT,TSB4_DEP                                                
         MVC   LIDAPACL,TSB4_SLC                                                
         CLC   TSB4_SAC,SPACES                                                  
         JE    APPN140                                                          
         OC    TSB4_SAC,TSB4_SAC                                                
         JZ    APPN140                                                          
         MVC   LIDAPACA,TSB4_SAC                                                
         J     APPN170                                                          
APPN140  CLC   TSB4_2PC,SPACES                                                  
         JE    APPN150                                                          
         OC    TSB4_2PC,TSB4_2PC                                                
         JZ    APPN150                                                          
         MVC   LIDAPACA,TSB4_2PC                                                
         J     APPN170                                                          
APPN150  MVC   TEMP2(L'TSB4_CLI+L'TSB4_PRO+L'TSB4_JBC),SPACES                   
         MVC   TEMP2(L'TSB4_CLI),TSB4_CLI                                       
         SR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         LA    RE,TEMP2                                                         
         AR    RE,RF                                                            
         MVC   0(L'TSB4_PRO,RE),TSB4_PRO                                        
         SR    R1,R1                                                            
         IC    R1,PPROLEN                                                       
         SR    R1,RF                                                            
         AR    RE,R1                                                            
         MVC   0(L'TSB4_JBC,RE),TSB4_JBC                                        
         OC    TEMP2(L'TSB4_CLI+L'TSB4_PRO+L'TSB4_JBC),SPACES                   
         MVC   LIDAPACA,TEMP2                                                   
APPN170  MVC   LIDAPMED,TSB4_MED                                                
         MVC   LIDAPETY,TSB4_EXP                                                
         ZAP   LIDAPSEL,TSB4_SLF                                                
         ZAP   LIDAPVAL,TSB4_APP                                                
         J     APPN200                                                          
*                                                                               
APPN180  CLI   BPELID,RECTBUAP     b5                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   LIDLPID,TSB5_PIN                                                 
         CLI   TSB5_TIM,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLTIME                                                
         CLI   TSB5_EXP,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLEXPN                                                
         CLI   TSB5_ORD,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLORDS                                                
         CLI   TSB5_INV,YESQ                                                    
         JNE   *+8                                                              
         OI    LIDLAPPL,LIDLINVC                                                
                                                                                
APPN200  L     RF,AAUDITAB                                                      
         USING AUDITABD,RF                                                      
         MVI   AUDIFLAG,STCSADD    guess add                                    
         MVC   AUDITYPE,BPELTYPE                                                
         MVC   AUDIAPPL,LIDLAPPL                                                
         MVC   AUDIAPP2,LIDLAPP2                                                
         SR    RE,RE                                                            
         IC    RE,BPELLEN                                                       
                                                                                
         LR    R0,RE               save                                         
         AHI   RE,L'AUDIFLAG+L'AUDITYPE+L'AUDITLEN                              
         STC   RE,AUDITLEN                                                      
         LR    RE,R0               restore                                      
                                                                                
         SHI   RE,3                exec len+l'lidlappl+l'lidlapp2               
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   AUDIDATA(0),LIDDATA+L'LIDLAPPL+L'LIDLAPP2                        
         ICM   R1,15,AAUDITAB                                                   
         SR    RE,RE                                                            
         IC    RE,AUDITLEN                                                      
         AR    R1,RE                                                            
         STCM  R1,15,AAUDITAB                                                   
         DROP  RF                                                               
*                                                                               
         L     RF,ASVELST          start of the element                         
         SR    RE,RE                                                            
         IC    RE,LIDLN-LIDELD(RF)                                              
         SR    R1,R1                                                            
         IC    R1,BPELLEN                                                       
         AR    RE,R1                                                            
         STC   RE,LIDLN-LIDELD(RF)                                              
         AR    R6,R1               bump along genarea                           
         J     APPN20                                                           
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* compare old lidels to new for audit                                 *         
* set AUPAGOLD flags to all/none/list/default all for the 4 b0 types  *         
***********************************************************************         
APPOLD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*APPOLD*'                                                      
         MVI   AUPAGOLD,DEFNONEQ   old details for page - guess NONE            
         MVC   AUPAGOLD+1(L'AUPAGOLD-1),AUPAGOLD                                
         L     R2,ASVELST                                                       
         SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         AR    R2,RE                                                            
         ST    R2,ASVELST          start of next element                        
         L     R3,AIO1                                                          
         USING APPRECD,R3          R3=A(approver record)                        
K        USING APPKEY,IOKEY                                                     
         XC    K.APPKEY,K.APPKEY   Build key of approver record                 
         MVI   K.APPKTYP,APPKTYPQ                                               
         MVI   K.APPKSUB,APPKSUBQ                                               
         MVC   K.APPKCPY,CUXCPY                                                 
         MVC   K.APPKPIDB,QA_PIN                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    APPOL10                                                          
         MVC   AUPAGOLD,SPACES     no existing data                             
         J     EXITY                                                            
APPOL10  L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    APPRSTA2,APPSESTQ                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD,DEFALLQ                                                 
         TM    APPRSTA2,APPSESIQ                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+1,DEFALLQ                                               
         TM    APPRSTA2,APPSRJAQ                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+2,DEFALLQ                                               
         TM    APPRSTA2,APPSDJAQ                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+2,DEFDALLQ                                              
                                                                                
         TM    APPRSTAT,APPSFINA                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+3,DEFALLQ                                               
         TM    APPRSTAT,APPSFIND                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+3,DEFDALLQ                                              
*                                                                               
         LA    R3,APPRFST                                                       
APPOL20  CLI   0(R3),0                                                          
         JE    APPOL400                                                         
         CLI   0(R3),LIDELQ                                                     
         JE    APPOL100            lidels only needed for audit                 
         SR    RE,RE               save unknown element                         
         IC    RE,1(R3)                                                         
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R2),0(R3)                                                    
         EX    RE,0(RF)                                                         
                                                                                
         SR    RE,RE                                                            
         IC    RE,1(R3)                                                         
         AR    R2,RE                                                            
         ST    R2,ASVELST                                                       
         J     APPOL300                                                         
         USING LIDELD,R3                                                        
APPOL100 CLI   LIDTYPE,LIDTAPSJ                                                 
         JNE   APPOL105                                                         
         TM    LIDASTAT,LIDAESTY   estimate approver                            
         JNZ   *+12                                                             
         TM    LIDASTAT,LIDAESTD                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD,DEFLISTQ                                                
                                                                                
         TM    LIDASTA2,LIDAESI    internal estimate approver                   
         JNZ   *+12                                                             
         TM    LIDASTA2,LIDAESTD                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+1,DEFLISTQ                                              
                                                                                
         TM    LIDASTAT,LIDAJOBY   job approver                                 
         JNZ   *+12                                                             
         TM    LIDASTAT,LIDAJOBD                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+2,DEFLISTQ                                              
         J     APPOL110                                                         
                                                                                
APPOL105 CLI   LIDTYPE,LIDTAP1R                                                 
         JNE   APPOL110                                                         
         TM    LIDAPDTY,LIDAPDEF   expense finance approver                     
         JNZ   *+12                                                             
         TM    LIDAPDTY,LIDAPDED                                                
         JZ    *+8                                                              
         MVI   AUPAGOLD+3,DEFLISTQ                                              
                                                                                
APPOL110 GOTOR CHEKAUDT            use old lidel to update audit table          
APPOL300 SR    RF,RF               bump to next element in record               
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         J     APPOL20                                                          
APPOL400 L     R1,=AL4(IOSQUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   K.APPKEY(APPKSEQ-APPRECD),IOKEYSAV                               
         JE    APPOL10             Process next record                          
APPOL900 J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
*******************************************************************             
* compare old lidel item to audit table                           *             
* if not there add as delete                                      *             
* if there and changed flag as change                             *             
* if there and unchanged flag as unchanged                        *             
* on entry r3 points to lidel in old record                       *             
*******************************************************************             
CHEKAUDT NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHEKAT*'                                                      
         USING LIDELD,R3                                                        
CHAUD100 SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,LIDLN            element length                               
         SHI   RF,LIDLNDQ          fixed part                                   
         SR    R1,R1                                                            
         IC    R1,LIDITLN                                                       
         STC   R1,BYTE2            save len of sub element                      
         DR    RE,R1                                                            
         LR    R1,RF               r1 = #entries in element                     
         LA    R5,LIDDATA          start at first sub element                   
                                                                                
* check lidel entries against audit table-flag as change/nochange/del           
CHAUD120 L     R4,AIO7             auditab in io7 & 8                           
         USING AUDITABD,R4                                                      
CHAUD130 CLI   AUDITYPE,0                                                       
         JE    CHAUD170                                                         
         CLC   AUDITYPE,LIDTYPE                                                 
         JNE   CHAUD140                                                         
         SR    RE,RE                                                            
         IC    RE,BYTE2            len of a sub entry of this type              
         SHI   RE,3                exec len+ L'LIDLAPPL+L'LIDLAPP2              
         BASR  RF,0                                                             
         CLC   AUDIDATA(0),L'LIDLAPPL+L'LIDLAPP2(R5)                            
         EX    RE,0(RF)                                                         
         JE    CHAUD150            found it                                     
CHAUD140 SR    RE,RE               bump to next audit table entry               
         IC    RE,AUDITLEN                                                      
         AR    R4,RE                                                            
         J     CHAUD130                                                         
                                                                                
CHAUD150 CLC   AUDIAPPL,0(R5)      found - but has it changed ?                 
         JNE   CHAUD160                                                         
         CLC   AUDIAPP2,L'LIDLAPPL(R5)                                          
         JNE   CHAUD160                                                         
         MVI   AUDIFLAG,0          no change                                    
         J     CHAUD180                                                         
CHAUD160 MVI   AUDIFLAG,STCSCHG    changed                                      
         J     CHAUD180                                                         
CHAUD170 MVI   AUDIFLAG,STCSDEL    add to table flagged as delete               
         MVC   AUDITYPE,LIDTYPE                                                 
         MVC   AUDIAPPL,0(R5)                                                   
         MVC   AUDIAPP2,L'LIDLAPPL(R5)                                          
         SR    RE,RE                                                            
         IC    RE,BYTE2            len of a sub entry of this type              
                                                                                
         LR    R0,RE               save                                         
         AHI   RE,L'AUDIFLAG+L'AUDITYPE+L'AUDITLEN                              
         STC   RE,AUDITLEN                                                      
         LR    RE,R0               restore                                      
                                                                                
         SHI   RE,3                exec len+l'lidlappl+l'lidapp2                
         BASR  RF,0                                                             
         MVC   AUDIDATA(0),L'LIDLAPPL+L'LIDLAPP2(R5)                            
         EX    RE,0(RF)                                                         
         DROP  R4                                                               
CHAUD180 SR    RE,RE                                                            
         IC    RE,BYTE2                                                         
         AR    R5,RE               bump to next sub element                     
         JCT   R1,CHAUD120                                                      
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* Update approver record - rebuild record with data held in GENAREA  *          
* buffer  - add new records when there isn't enough room to store the*          
* data                                                               *          
**********************************************************************          
                                                                                
APPUPD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*APPUPD*'                                                      
                                                                                
         L     R4,ABLOCK                                                        
         USING CPTRD,R4                                                         
         L     RF,AGENAREA                                                      
         ST    RF,ASVELST          start of next element to add                 
         MVI   BYTE1,0             Sequence number                              
         MVI   BYTE2,0             Indicators                                   
         MVI   ALLDONE,NOQ                                                      
                                                                                
         L     R3,AIO1                                                          
         USING APPRECD,R3          R3=A(Approver record)                        
                                                                                
K        USING APPKEY,IOKEY                                                     
APPUPD02 XC    K.APPKEY,K.APPKEY   Build key of approver record                 
         MVI   K.APPKTYP,APPKTYPQ                                               
         MVI   K.APPKSUB,APPKSUBQ                                               
         MVC   K.APPKCPY,CUXCPY                                                 
         MVC   K.APPKPIDB,QA_PIN                                                
         L     R1,=AL4(IOHIUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   APPUPD24            Rec not there - need to add                  
         CLC   K.APPKEY(APPKSEQ-APPRECD),IOKEYSAV                               
         JNE   APPUPD04            Need to add                                  
APPUPD04 MVC   CSVKEY2,IOKEY                                                    
         CLI   ALLDONE,YESQ        All elements in table processed?             
         JNE   *+8                 No                                           
         OI    BYTE2,X'80'         Yes - delete original record                 
                                                                                
APPUPD08 MVC   BYTE1,K.APPKSEQ     Save current sequence number                 
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
         TM    BYTE2,X'80'         Are we deleting records ?                    
         JNZ   APPUPD12                                                         
         LHI   R0,APPRFST-APPRECD                                               
         STCM  R0,3,APPRLEN                                                     
         GOTOR FILLREC             Fill a record with elements                  
         MVC   APPRSTAT(L'APPRSTAT+L'APPRSTA2),HALF1                            
         J     APPUPD14                                                         
                                                                                
***********************************************************************         
* Here to write back current approver rec and get next one            *         
***********************************************************************         
                                                                                
APPUPD12 OI    APPRSTAT,APPSDELT                                                
APPUPD14 GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,CSVKEY2                                                    
         LA    RE,IOKEY                                                         
         MVC   APPKSTAT-APPRECD(L'APPKSTAT+L'APPKSTA2,RE),APPRSTAT              
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1' UPDATE DIR STATUS          
                                                                                
         L     R1,=AL4(IOSQUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   K.APPKEY(APPKSEQ-APPRECD),IOKEYSAV                               
         JE    APPUPD04            Process next record                          
***********************************************************************         
* Here when all current approver records have been processed -set     *         
* sequence number of next approver record (to be added) and test if   *         
* we have further elements to add                                     *         
***********************************************************************         
                                                                                
                                                                                
APPUPD22 LLC   RE,BYTE1            Bump key sequence number                     
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
                                                                                
APPUPD24 CLI   ALLDONE,YESQ        Any more approver elements?                  
         JE    EXITY               No                                           
                                                                                
***********************************************************************         
* Here if we have run out of space for elements -                     *         
*  create a new record and set flag so that we issue an ADDREC        *         
***********************************************************************         
                                                                                
         XC    APPRECD(256),APPRECD                                             
         MVC   APPKEY,IOKEYSAV     Build a new approver record                  
         MVC   APPKSEQ,BYTE1                                                    
         LHI   R0,APPRFST-APPRECD                                               
         STCM  R0,3,APPRLEN                                                     
         OI    BYTE2,X'40'         Set adding new records                       
                                                                                
         GOTOR FILLREC             Add elements from Genarea to Aio1            
         MVC   APPRSTAT(L'APPRSTAT+L'APPRSTA2),HALF1                            
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,CSVKEY2                                                    
         LA    RE,IOKEY                                                         
         MVC   APPKSTAT-APPRECD(L'APPKSTAT+L'APPKSTA2,RE),APPRSTAT              
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IODIR+IO1' UPDATE DIR STATUS            
         JE    APPUPD22                                                         
         DC    H'0'                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
******************************************************************              
* audit trail for approver                                       *              
* create page type stcel for b0                                                 
* create row type audit stcsels  for b1-b5                       *              
* create an stcsel for all of the list changes                   *              
*   then for all of the adds then for all of the deletes         *              
*   for each type i.e. B1 changes/B1 adds/B1 dels/B2changes...etc*              
******************************************************************              
APPAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*APPAUD*'                                                      
         L     R0,AGENAXTN         clear buffer to store audit elements         
         LH    R1,=Y(GENAREAX-GENAEXTN)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LAY   RF,GENAEXTN                                                      
         ST    RF,AAUDITAB         start of next element to add                 
                                                                                
         MVC   WORK(1),QA_GLECA                                                 
         MVC   WORK+1(1),QA_GLICA                                               
         MVC   WORK+2(1),QA_GLJA                                                
         MVC   WORK+3(1),QA_GLEFA                                               
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING STCELD,R4                                                        
         MVI   STCEL,STCELQ                                                     
         MVI   STCLN,STCSLN2Q                                                   
         MVI   STCIND,STCIACCS                                                  
         MVC   STCSPID,CUUSER                                                   
         MVC   STCSUSR,QA_PIN                                                   
         GOTOR VDATCON,DMCB,(5,0),(1,STCSDTE)                                   
         MVC   STCSTIM,CTIME                                                    
         MVI   STCSSTAT,STCSPAGE                                                
         LA    RE,AUPAGOLD                                                      
         LA    RF,WORK                                                          
         LA    R0,4                                                             
         LA    R2,1                                                             
*                                  page audit stcel                             
APPAUD10 CLC   0(1,RE),0(RF)                                                    
         JE    APPAUD30            no change at page level                      
         MVI   STCSTYPE,STCSASTF                                                
         CHI   R2,4                                                             
         JE    *+8                                                              
         MVI   STCSTYPE,STCSACPJ                                                
APPAUD20 MVI   STCSSTA2,STCSCHG                                                 
         CLI   0(RE),C' '                                                       
         JNE   *+8                                                              
         MVI   STCSSTA2,STCSADD                                                 
         CHI   R2,1                                                             
         JNE   APPAUD25                                                         
         CLI   0(RF),DEFNONEQ                                                   
         JNE   *+12                                                             
         OI    STCSPSTA,STCSSCNO                                                
         J     APPAUD30                                                         
         CLI   0(RF),DEFALLQ                                                    
         JNE   *+12                                                             
         OI    STCSPSTA,STCSSCAL                                                
         J     APPAUD30                                                         
         OI    STCSPSTA,STCSSCLS                                                
         J     APPAUD30                                                         
APPAUD25 CHI   R2,2                                                             
         JNE   APPAUD26                                                         
         CLI   0(RF),DEFNONEQ                                                   
         JNE   *+12                                                             
         OI    STCSPSTA,STCSSINO                                                
         J     APPAUD30                                                         
         CLI   0(RF),DEFALLQ                                                    
         JNE   *+12                                                             
         OI    STCSPSTA,STCSSIAL                                                
         J     APPAUD30                                                         
         OI    STCSPSTA,STCSSILS                                                
         J     APPAUD30                                                         
APPAUD26 CHI   R2,3                                                             
         JNE   APPAUD27                                                         
         CLI   0(RF),DEFNONEQ                                                   
         JNE   *+12                                                             
         OI    STCSPSTA,STCS1RNO                                                
         J     APPAUD30                                                         
         CLI   0(RF),DEFALLQ                                                    
         JNE   *+12                                                             
         OI    STCSPSTA,STCS1RAL                                                
         J     APPAUD30                                                         
         CLI   0(RF),DEFLISTQ                                                   
         JNE   *+12                                                             
         OI    STCSPSTA,STCS1RLS                                                
         J     APPAUD30                                                         
         OI    STCSPSTA,STCS1RAD                                                
         J     APPAUD30                                                         
APPAUD27 CHI   R2,4                                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(RF),DEFNONEQ                                                   
         JNE   *+12                                                             
         OI    STCSPSTA,STCSJBNO                                                
         J     APPAUD30                                                         
         CLI   0(RF),DEFALLQ                                                    
         JNE   *+12                                                             
         OI    STCSPSTA,STCSJBAL                                                
         J     APPAUD30                                                         
         CLI   0(RF),DEFLISTQ                                                   
         JNE   *+12                                                             
         OI    STCSPSTA,STCSJBLS                                                
         J     APPAUD30                                                         
         OI    STCSPSTA,STCSJBAD                                                
         J     APPAUD30                                                         
APPAUD30 CHI   R2,3                                                             
         JL    APPAUD40                                                         
         OC    STCSPSTA(L'STCSPSTA+L'STCSPST2),STCSPSTA                         
         JZ    APPAUD40                                                         
         L     R1,AAUDITAB             add page level elemnt to genarea         
         MVC   0(STCSLN2Q,R1),ELEMENT                                           
         AHI   R1,STCSLN2Q                                                      
         ST    R1,AAUDITAB                                                      
         XC    ELEMENT+STCSLN1Q(L'ELEMENT-STCSLN1Q),ELEMENT+STCSLN1Q            
APPAUD40 LA    RE,1(RE)            bump to next                                 
         LA    RF,1(RF)                                                         
         LA    R2,1(R2)                                                         
         JCT   R0,APPAUD10                                                      
*                                  row audit stcels                             
         MVI   STCLN,STCSLN1Q                                                   
         MVI   STCSSTAT,STCSROW                                                 
         XC    ELEMENT+STCSLN1Q(L'ELEMENT-STCSLN1Q),ELEMENT+STCSLN1Q            
         LA    R3,APPTAB                                                        
         USING BPELTABD,R3                                                      
APPAUD50 CLI   BPELID,X'FF'                                                     
         JE    APPAUD90                                                         
                                                                                
         MVI   BYTE3,STCSCHG                                                    
         GOTOR CSTCEL              create stcsel of changes for type            
         MVI   BYTE3,STCSADD                                                    
         GOTOR CSTCEL              create stcsel of adds for type               
         MVI   BYTE3,STCSDEL                                                    
         GOTOR CSTCEL              create stcsel of deletes for type            
                                                                                
APPAUD60 AHI   R3,BPELLNQ                                                       
         J     APPAUD50                                                         
APPAUD90 MVI   BYTE2,AUDKAPPR                                                   
         GOTOR PERAUD              Uses STCELS held in GENAEXTN                 
         J     EXITY                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
********************************************************************            
* add an stcel audit element to genarea                            *            
*        for limit list or approver lidels                         *            
* on entry R3 points to limtab or apptab/byte3 is action           *            
* on entry R4 points to stcel element                              *            
********************************************************************            
CSTCEL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CSTCEL*'                                                      
         L     R5,AIO7                                                          
         USING AUDITABD,R5                                                      
         USING BPELTABD,R3                                                      
         USING STCELD,R4                                                        
CSTCEL10 CLI   AUDITYPE,0                                                       
         JE    CSTCEL60            all done - add the element                   
         CLC   AUDITYPE,BPELTYPE   match type - a1 a2 etc                       
         JNE   CSTCEL20                                                         
         CLC   AUDIFLAG,BYTE3      match action -add/change/del                 
         JNE   CSTCEL20                                                         
         MVC   STCSSTA2,BYTE3      action                                       
         MVC   STCSTYPE,BPSTCEL                                                 
         SR    R1,R1                                                            
         IC    R1,BPELLEN                                                       
         SR    RE,RE                                                            
         IC    RE,STCLN            existing element length                      
         LR    R2,R4                                                            
         AR    R2,RE               R2 points to end of element                  
         AR    RE,R1               add l'liddata to element length              
         STC   RE,STCLN            new element length                           
         MVC   0(L'AUDIAPPL,R2),AUDIAPPL                                        
         MVC   L'AUDIAPPL(L'AUDIAPP2,R2),AUDIAPP2                               
                                                                                
         SHI   R1,3                exec len+l'lidlappl&l'lidlapp2               
         BASR  RF,0                                                             
         MVC   L'LIDLAPPL+L'LIDLAPP2(0,R2),AUDIDATA                             
         EX    R1,0(RF)                                                         
CSTCEL20 SR    RE,RE               bump to next audit table entry               
         IC    RE,AUDITLEN                                                      
         AR    R5,RE                                                            
         J     CSTCEL10                                                         
                                                                                
CSTCEL60 L     R1,AAUDITAB         add row level stcel to genaextn              
         SR    RE,RE                                                            
         IC    RE,STCLN            l'element                                    
         CHI   RE,STCSLN1Q                                                      
         JE    CSTCELX             no row levels for this type/action           
         LR    RF,R1                                                            
         AR    RF,RE                                                            
         ST    RF,AAUDITAB         ready for next one                           
         SHI   RE,1                exec len                                     
         BASR  RF,0                                                             
         MVC   0(0,R1),ELEMENT                                                  
         EX    RE,0(RF)                                                         
         MVI   STCLN,STCSLN1Q      reset stcel work area for next               
         XC    ELEMENT+STCSLN1Q(L'ELEMENT-STCSLN1Q),ELEMENT+STCSLN1Q            
CSTCELX  J     EXITY                                                            
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* Update role records                                                 *         
***********************************************************************         
                                                                                
ROLEUPD  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDROL*'                                                      
                                                                                
         L     R0,AGENAREA                                                      
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R6,AGENAREA                                                      
*                                 build table of role numbers from TSAR         
         LA    R2,TSARREC                                                       
         USING TSAR_D,R2                                                        
         J     ROLU20             first one already read                        
ROLU10   GOTOR GOTSAR,DMCB,('TSAGET',0)                                         
         JNE   ROLU30              no more roles                                
ROLU20   CLI   TSARTYPE,RECTROLE                                                
         JNE   ROLU30              no more roles                                
         MVC   0(L'TSB6_ROL,R6),TSB6_ROL                                        
         LA    R6,L'TSB6_ROL+1(R6)  +1 for the action flag                      
         J     ROLU10                                                           
ROLU30   MVC   0(L'FFS4,R6),FFS4         flag end of table                      
         DROP R2                                                                
*                       has user deleted any roles ? if so add to table         
ROLU40   LA    R3,IOKEY                                                         
         USING PIDRECD,R3                                                       
         XC    PIDKEY,PIDKEY   Build key of passive                             
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,QA_PIN                                                   
         MVI   PIDKSTYP,PIDKROLQ                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   ROLU70            no passives for this person                    
         J     ROLU52                                                           
                                                                                
ROLU50   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   ROLU70            no more passives for this person               
ROLU52   CLC   PIDKEY(PIDKPER-PIDRECD),IOKEYSAV                                 
         JNE   ROLU70            no more passives for this person               
                                                                                
         L     R6,AGENAREA       look thru TSAR table                           
ROLU54   CLC   0(2,R6),FFS4                                                     
         JE    ROLU58            role must have been deleted by user            
         CLC   0(L'TSB6_ROL,R6),PIDKPER  is role in list from tsar?             
         JE    ROLU56                                                           
         LA    R6,L'TSB6_ROL+1(R6)                                              
         J     ROLU54                                                           
                                                                                
ROLU56   MVI   L'TSB6_ROL(R6),NOCHANGQ -passive and TSAR                        
         J     ROLU50            get next passive                               
ROLU58   MVC   0(L'TSB6_ROL,R6),PIDKPER                                         
         MVI   L'TSB6_ROL(R6),DELMEQ   -passive but no TSAR                     
         MVC   L'TSB6_ROL+1(L'FFS4,R6),FFS4                                     
         J     ROLU50            get next passive                               
*now we are left with a list of roles in genarea- flagged in 3 ways             
*flagged as nochangq - just ignore these                                        
*flagged as delmeq - delete passive and remove role from role record            
*flagged as addmeq - add a new passive and add to/add a role record             
*now go down list and action as necessary                                       
ROLU70   L     R6,AGENAREA                                                      
ROLU72   CLC   0(2,R6),FFS4                                                     
         JE    ROLU900            all done                                      
         CLI   2(R6),NOCHANGQ                                                   
         JE    ROLU200            nothing to do                                 
                                                                                
         L     R4,ABLOCK                                                        
         USING CPTRD,R4                                                         
         LA    R2,IOKEY                                                         
         USING ROLRECD,R2                                                       
         XC    ROLKEY,ROLKEY                                                    
         MVI   ROLKTYP,ROLKTYPQ                                                 
         MVI   ROLKSUB,ROLKSUBQ                                                 
         MVC   ROLKCPY,CUXCPY                                                   
         MVC   ROLKOFF,SPACES       are office codes used ????                  
         MVC   ROLKNUM,0(R6)                                                    
         L     R1,=AL4(IORDUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    ROLU76                                                           
         CLI   2(R6),DELMEQ                                                     
         JNE   *+6                                                              
         DC    H'0'                must be there if deleting                    
         L     R5,AGENAXTN                                                      
         MVC   0(2,R5),QA_PIN                                                   
         J     ROLU92                                                           
ROLU76   MVC   CSVKEY2,IOKEY                                                    
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
                                                                                
*find lidtpid(s)-put pids into a table                                          
         MVI   BYTE1,NOQ                                                        
         L     R5,AGENAXTN                                                      
         L     R1,AIO1                                                          
         LA    R1,ROLRFST-ROLRECD(R1)                                           
         USING LIDELD,R1                                                        
ROLU80   CLI   LIDEL,0                                                          
         JE    ROLU89M                                                          
         CLI   LIDEL,LIDELQ                                                     
         JNE   ROLU89                                                           
         CLI   LIDTYPE,LIDTPID                                                  
         JNE   ROLU89                                                           
         MVI   LIDEL,X'FF'         these elements to be replaced                
*                                  how many pids in element ?                   
         LLC   RF,LIDLN            total len                                    
         SHI   RF,LIDLNDQ          now RF =len pid data                         
         SRL   RF,1                                                             
         LR    R0,RF               now r0=number of pids                        
*                                                                               
         LA    RF,LIDLNDQ(R1)                                                   
ROLU81   CLI   2(R6),DELMEQ                                                     
         JNE   ROLU84                                                           
         CLC   0(2,RF),QA_PIN                                                   
         JE    ROLU87              deleting so don't save                       
         J     ROLU86                                                           
ROLU84   CLI   BYTE1,NOQ                                                        
         JNE   ROLU86                                                           
         CLC   0(2,RF),QA_PIN                                                   
         JH    ROLU86                                                           
         MVC   0(2,R5),QA_PIN                                                   
         LA    R5,2(R5)                                                         
         MVI   BYTE1,YESQ                                                       
ROLU86   MVC   0(2,R5),0(RF)                                                    
         LA    R5,2(R5)                                                         
ROLU87   LA    RF,2(RF)                                                         
         JCT   R0,ROLU81                                                        
ROLU89   LLC   RE,LIDLN                                                         
         AR    R1,RE                                                            
         J     ROLU80                                                           
ROLU89M  CLI   2(R6),ADDMEQ                                                     
         JNE   ROLU89T             not adding                                   
         CLI   BYTE1,YESQ                                                       
         JE    ROLU89T             already added                                
         MVC   0(2,R5),QA_PIN                                                   
ROLU89T  GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',AIO1),0                         
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*now all LIDTPIDS have gone& the updated list of PIDs is in genaext             
ROLU92   L     R5,AGENAXTN                                                      
         OC    0(2,R5),0(R5)                                                    
         JZ    ROLU110             the only PID has been deleted                
         MVI   BYTE1,0             element sequence number                      
*                                                                               
ROLU94   XC    ELEMENT,ELEMENT                                                  
         USING LIDELD,R1                                                        
         LA    R1,ELEMENT                                                       
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDLN,LIDLNDQ                                                    
         MVI   LIDTYPE,LIDTPID                                                  
         MVI   LIDITLN,2                                                        
         MVC   LIDSEQ,BYTE1                                                     
         LA    R2,LIDLNDQ(R1)                                                   
*                                  adjust element len for PID add               
ROLU96   LLC   RF,LIDLN            RF=Existing element length                   
         LLC   RE,LIDITLN                                                       
         AR    RF,RE               len PID being added                          
         CHI   RF,255                                                           
         JNL   ROLU100                                                          
         STC   RF,LIDLN                                                         
                                                                                
         DROP  R1                                                               
         MVC   0(2,R2),0(R5)                                                    
         LA    R2,2(R2)                                                         
         LA    R5,2(R5)                                                         
         OC    0(2,R5),0(R5)                                                    
         JNZ   ROLU96                                                           
*                                                                               
ROLU100  GOTO1 VHELLO,DMCB,(C'P',ACCMST),AIO1,ELEMENT                           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         OC    0(2,R5),0(R5)                                                    
         JZ    ROLU110                                                          
         SR    RF,RF                                                            
         IC    RF,BYTE1            bump sequence number                         
         AHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         J     ROLU94                                                           
*                                                                               
ROLU110  GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
*                                                                               
         LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING RACELD,R2           add activity element                         
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTCHA                                                  
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CUPASS                                                   
         MVC   RACTERM,CUTERM                                                   
         GOTOR VDATCON,DMCB,(5,0),(1,RACDATE)                                   
         MVC   RACTIME,CTIME                                                    
         AHI   R2,RACLNQ                                                        
         MVI   0(R2),0             add delimiter                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AIO1,ELEMENT                           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE2,1                                                          
         JE    ROLU112             adding                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,CSVKEY2                                                    
         LA    RE,IOKEY                                                         
         L     R1,AIO1                                                          
         MVC   ROLKSTAT-ROLRECD(L'ROLKSTAT,RE),ROLRSTAT-ROLRECD(R1)             
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1' UPDATE DIR                 
         JE    ROLU200                                                          
         DC    H'0'                                                             
ROLU112  GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,CSVKEY2                                                    
         LA    RE,IOKEY                                                         
         L     R1,AIO1                                                          
         MVC   ROLKSTAT-ROLRECD(L'ROLKSTAT,RE),ROLRSTAT-ROLRECD(R1)             
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IODIR+IO1' UPDATE DIR STATUS            
         JE    ROLU200                                                          
         DC    H'0'                                                             
                                                                                
ROLU200  LA    R6,3(R6)            get next role                                
         J     ROLU72                                                           
ROLU900  J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Update group records                                                *         
* read TSAR and move all groups to genarea flagged as ADD's           *         
* read old groups - if in table flag as NOCHANGE                      *         
*                   if not in table then add to table and flag as DEL *         
* create audit STCELS from table                                                
* update records from table                                                     
***********************************************************************         
                                                                                
GRPUPD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GRPUPD*'                                                      
         L     R0,AGENAREA                                                      
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R6,AGENAREA                                                      
                                                                                
         LA    R2,TSARREC         build table group numbers from TSAR           
         USING TSAR_D,R2                                                        
         J     GRPU20             first one already read                        
GRPU10   GOTOR GOTSAR,DMCB,('TSAGET',0)                                         
         JNE   GRPU30              no more groups                               
GRPU20   CLI   TSARTYPE,RECTPGR2                                                
         JNE   GRPU30              no more groups                               
         MVC   0(L'MAAGLC,R6),TSARDATA                                          
         MVI   L'MAAGLC(R6),ADDMEQ                                              
         LA    R6,L'MAAGLC+1(R6)    +1 for the action flag                      
         J     GRPU10                                                           
GRPU30   MVC   0(L'FFS4,R6),FFS4         flag end of table                      
         DROP  R2                                                               
*                       has user deleted any groups? if so add to table         
GRPU40   LA    R3,IOKEY                                                         
         USING LLSRECD,R3                                                       
         XC    LLSKEY,LLSKEY   Build key                                        
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,QA_PIN                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   GRPU100           nothing for this person                        
         J     GRPU52                                                           
                                                                                
GRPU50   L     R1,=AL4(IOSQUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   GRPU100           no more groups for this person                 
GRPU52   CLC   LLSKEY(LLSKGRP-LLSRECD),IOKEYSAV                                 
         JNE   GRPU100           no more for this person                        
         OC    LLSKGRP,LLSKGRP                                                  
         JZ    GRPU50                                                           
                                                                                
         L     R6,AGENAREA       look thru TSAR table                           
GRPU54   CLC   0(L'FFS4,R6),FFS4                                                
         JE    GRPU58            grp must have been deleted by user             
         CLC   0(L'MAAGLC,R6),LLSKGRP is group in list from tsar?               
         JE    GRPU56                                                           
         LA    R6,L'MAAGLC+1(R6)                                                
         J     GRPU54                                                           
                                                                                
GRPU56   MVI   L'MAAGLC(R6),NOCHANGQ -passive and TSAR                          
         J     GRPU50            get next group                                 
GRPU58   MVC   0(L'MAAGLC,R6),LLSKGRP                                           
         MVI   L'MAAGLC(R6),DELMEQ     -llsrec but no TSAR-2b deleted           
         MVC   L'MAAGLC+1(L'FFS4,R6),FFS4                                       
         J     GRPU50            get next llsrec                                
                                                                                
*now we are left with a list of groups in genarea- flagged in 3 ways            
*flagged as nochangq - just ignore these                                        
*flagged as delmeq - delete llsrec  and remove pid from group record            
*flagged as addmeq - add a new llsrrec and add to/add a group record            
*  now go down genarea list and action as nec.                                  
                                                                                
                                                                                
GRPU100  L     R6,AGENAREA                                                      
         L     R4,ABLOCK                                                        
         USING CPTRD,R4                                                         
GRPU110  CLC   0(L'FFS4,R6),FFS4                                                
         JE    GRPU900            all done                                      
         CLI   L'MAAGLC(R6),NOCHANGQ                                            
         JE    GRPU800            nothing to do-bump to next                    
         CLI   L'MAAGLC(R6),ADDMEQ                                              
         JE    GRPU400            add a PID                                     
*                                 remove pid from group rec                     
         LA    R2,IOKEY                                                         
         USING GLSRECD,R2                                                       
         XC    GLSKEY,GLSKEY                                                    
         MVI   GLSKTYP,GLSKTYPQ                                                 
         MVI   GLSKSUB,GLSKSUBQ                                                 
         MVC   GLSKCPY,CUXCPY                                                   
         MVC   GLSKGRP,0(R6)                                                    
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GRPU130                                                          
         DC    H'0'                must be there if deleting                    
GRPU120  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JE    GRPU130                                                          
         DC    H'0'                must be there if deleting                    
GRPU130  CLC   GLSKEY(GLSKGRP-GLSKEY+L'GLSKGRP),IOKEYSAV                        
         JE    *+6                                                              
         DC    H'0'                must be there if deleting                    
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
*                                  does it contain a lidtpid ?                  
         L     R1,AIO1                                                          
         LA    R1,GLSRFST-GLSRECD(R1)                                           
         USING LIDELD,R1                                                        
GRPU150  CLI   LIDEL,0                                                          
         JE    GRPU120             read next record                             
         CLI   LIDEL,LIDELQ                                                     
         JNE   GRPU160                                                          
         CLI   LIDTYPE,LIDTPID                                                  
         JE    GRPU170                                                          
GRPU160  LLC   RE,LIDLN                                                         
         AR    R1,RE                                                            
         J     GRPU150                                                          
*                                  now look for PID in element                  
GRPU170  XC    ELEMENT,ELEMENT                                                  
         LLC   RF,LIDLN                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ELEMENT(0),LIDEL    save element data                            
         EX    RF,0(RE)                                                         
         ST    R1,SAVER1           a(LIDEL in io area )                         
*                                  how many pids in element ?                   
         LLC   RF,LIDLN            total len                                    
         SHI   RF,LIDLNDQ          now RF =len pid data                         
         SRL   RF,1                now rf=number pids                           
*                                  look for the PID                             
GRPU180  LA    RE,ELEMENT                                                       
         AHI   RE,LIDLNDQ                                                       
         CLC   0(2,RE),QA_PIN                                                   
         JE    GRPU185                                                          
         LA    RE,2(RE)                                                         
         JCT   RF,GRPU180                                                       
         J     GRPU160             pid not in this element-look at next         
*                                  remove PID                                   
GRPU185  JCT   RF,*+8                                                           
         J     GRPU186                                                          
         MVC   0(2,RE),2(RE)                                                    
         LA    RE,2(RE)                                                         
         JCT   RF,GRPU185                                                       
GRPU186  XC    0(2,RE),0(RE)                                                    
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
         L     R1,SAVER1                                                        
         MVI   LIDEL,X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',AIO1),0                         
         OC    ELEMENT+LIDLNDQ(2),ELEMENT+LIDLNDQ                               
         JZ    GRPU190             was only 1 pid and now it has gone           
*                                  adjust element len for PID removal           
         LA    R1,ELEMENT                                                       
         LLC   RF,LIDLN                                                         
         LLC   RE,LIDITLN                                                       
         SR    RF,RE                                                            
         STC   RF,LIDLN                                                         
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),GLSRECD,ELEMENT                        
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GRPU190  GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    GRPU800                                                          
         DC    H'0'                                                             
*                                 add a pid to a group rec                      
GRPU400  MVI   BYTE1,0            sequence number of LIDTPID                    
         LA    R2,IOKEY                                                         
         USING GLSRECD,R2                                                       
         XC    GLSKEY,GLSKEY                                                    
         MVI   GLSKTYP,GLSKTYPQ                                                 
         MVI   GLSKSUB,GLSKSUBQ                                                 
         MVC   GLSKCPY,CUXCPY                                                   
         MVC   GLSKGRP,0(R6)                                                    
         L     R1,=AL4(IOHIUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   GRPU600             no records for this grp-create new           
         J     GRPU430                                                          
GRPU420  L     R1,=AL4(IOSQUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
GRPU430  MVC   CSVKEY2,IOKEY                                                    
         CLC   IOKEY(GLSKGRP-GLSKEY+L'GLSKGRP),IOKEYSAV                         
         JNE   GRPU600             add to last rec read if possible             
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         L     R1,AIO1                                                          
         LA    R1,GLSRFST-GLSRECD(R1)                                           
         USING LIDELD,R1                                                        
GRPU450  CLI   LIDEL,0                                                          
         JE    GRPU420                                                          
         CLI   LIDEL,LIDELQ                                                     
         JNE   GRPU460                                                          
         CLI   LIDTYPE,LIDTPID                                                  
         JE    GRPU470                                                          
GRPU460  LLC   RE,LIDLN                                                         
         AR    R1,RE                                                            
         J     GRPU450                                                          
                                                                                
*                                  add PID to element if there is space         
GRPU470  LLC   RF,LIDSEQ                                                        
         AHI   RF,1                                                             
         STC   RF,BYTE1            save sequence number of next LIDTPID         
         L     RF,AIO1                                                          
         CLC   GLSRLEN-GLSRECD(2,RF),=AL2(MAXRECLN-2) is rec full               
         JNL   GRPU460             still need to get lidtpids seq nums          
         LLC   RF,LIDLN                                                         
         CHI   RF,254              is element full                              
         JNL   GRPU460             wont fit into element-look 4 another         
                                                                                
         ST    R1,SAVER1                                                        
         ST    RF,SAVERF                                                        
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
         L     RF,SAVERF                                                        
         L     R1,SAVER1                                                        
         XC    ELEMENT,ELEMENT                                                  
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ELEMENT(0),LIDEL    save element data                            
         EX    RF,0(RE)                                                         
*                                                                               
         MVI   LIDEL,X'FF'                                                      
         DROP  R1                                                               
         L     R2,AIO1                                                          
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',GLSRECD),0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING LIDELD,R1                                                        
         LA    R1,ELEMENT                                                       
         LLC   RF,LIDLN            element length                               
         AR    R1,RF                                                            
         MVC   0(2,R1),QA_PIN                                                   
         AHI   RF,2                                                             
         LA    R1,ELEMENT                                                       
         STC   RF,LIDLN            increment element lenth                      
                                                                                
GRPU490  GOTO1 VHELLO,DMCB,(C'P',ACCMST),GLSRECD,ELEMENT                        
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,CSVKEY2                                                    
         LA    RE,IOKEY                                                         
         MVC   GLSKSTAT-GLSRECD(L'GLSKSTAT,RE),GLSRSTAT                         
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1' UPDATE DIR STATUS          
         JE    GRPU800                                                          
         DC    H'0'                                                             
*                                                                               
GRPU600  XC    ELEMENT,ELEMENT     need to build new element for 1 PID          
         USING LIDELD,R1                                                        
         LA    R1,ELEMENT                                                       
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDLN,LIDLNDQ+2                                                  
         MVI   LIDITLN,2                                                        
         MVI   LIDTYPE,LIDTPID                                                  
         MVC   LIDSEQ,BYTE1                                                     
         MVC   LIDDATA(2),QA_PIN                                                
                                                                                
         L     R2,AIO1                                                          
         CLC   0(GLSKGRP-GLSKEY+L'GLSKGRP,R2),IOKEYSAV                          
         JNE   GRPU610             no recs for this group                       
         CLC   GLSRLEN,=AL2(MAXRECLN-LIDLNDQ-2)                                 
         JH    GRPU610             last rec full                                
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
         J     GRPU490                                                          
GRPU610  GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         L     R2,AIO1                                                          
         MVC   GLSKEY,IOKEYSAV                                                  
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),GLSRECD,ELEMENT                        
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,IODA,0,ACOMFACS                 
         MVC   IOKEY,CSVKEY2                                                    
         LA    RE,IOKEY                                                         
         MVC   GLSKSTAT-GLSRECD(L'GLSKSTAT,RE),GLSRSTAT                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IODIR+IO1' UPDATE DIR STATUS            
         JE    GRPU800                                                          
         DC    H'0'                                                             
*                                                                               
GRPU800  LA    R6,L'MAAGLC+1(R6)                                                
         J     GRPU110                                                          
GRPU900  GOTOR GRPAUD             use table in genarea to create stcels         
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* create stcels for group changes                                     *         
***********************************************************************         
GRPAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GRPAUD*'                                                      
         L     R0,AGENAXTN         clear buffer to store audit elements         
         LH    R1,=Y(GENAREAX-GENAEXTN)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LAY   RF,GENAEXTN                                                      
         ST    RF,AAUDITAB         start of next element stcel                  
                                                                                
         MVI   BYTE1,STCSADD                                                    
GRPAUD10 XC    ELEMENT,ELEMENT                                                  
         USING STCELD,R4                                                        
         LAY   R4,ELEMENT                                                       
         L     R3,AGENAREA                                                      
         MVI   STCEL,STCELQ                                                     
         MVI   STCLN,STCSLN1Q                                                   
         MVI   STCIND,STCIACCS                                                  
         MVC   STCSPID,CUUSER                                                   
         MVC   STCSUSR,QA_PIN                                                   
         GOTOR VDATCON,DMCB,(5,0),(1,STCSDTE)                                   
         MVC   STCSTIM,CTIME                                                    
         MVI   STCSSTAT,STCSROW                                                 
         MVC   STCSSTA2,BYTE1                                                   
         MVI   STCSTYPE,STCSSGRP                                                
                                                                                
GRPAUD30 CLC   0(L'FFS4,R3),FFS4                                                
         JE    GRPAUD70            end of table                                 
         CLI   BYTE1,STCSADD                                                    
         JNE   GRPAUD32                                                         
         CLI   L'MAAGLC(R3),ADDMEQ                                              
         JNE   GRPAUD50                                                         
         J     GRPAUD34                                                         
GRPAUD32 CLI   L'MAAGLC(R3),DELMEQ                                              
         JNE   GRPAUD50                                                         
                                                                                
GRPAUD34 SR    R1,R1                                                            
         LA    R1,L'MAAGLC                                                      
         SR    RE,RE                                                            
         IC    RE,STCLN            existing element length                      
         LR    R2,R4                                                            
         AR    R2,RE               R2 points to end of element                  
         AR    RE,R1               add l'liddata to element length              
         STC   RE,STCLN            new element length                           
         MVC   0(L'MAAGLC,R2),0(R3)                                             
                                                                                
GRPAUD50 LA    R3,L'MAAGLC+1(R3)   bump to next audit table entry               
         J     GRPAUD30                                                         
                                                                                
GRPAUD70 L     R1,AAUDITAB                                                      
         SR    RE,RE                                                            
         IC    RE,STCLN            l'element                                    
         CHI   RE,STCSLN1Q                                                      
         JE    GRPAUD80            no row levels for this action                
         LR    RF,R1                                                            
         AR    RF,RE                                                            
         ST    RF,AAUDITAB         ready for next one                           
         SHI   RE,1                exec len                                     
         BASR  RF,0                                                             
         MVC   0(0,R1),ELEMENT                                                  
         EX    RE,0(RF)                                                         
         DROP  R4                                                               
                                                                                
GRPAUD80 CLI   BYTE1,STCSDEL                                                    
         JE    GRPAUD90                                                         
         MVI   BYTE1,STCSDEL                                                    
         J     GRPAUD10                                                         
GRPAUD90 MVI   BYTE2,AUDKACCS                                                   
         GOTOR PERAUD              Uses STCELS held in GENAEXTN                 
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Update or add new audit record for person record                    *         
* entry - STCELS in GENAEXTN                                          *         
*         BYTE2 = audit type                                          *         
***********************************************************************         
                                                                                
PERAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PERAUD*'                                                      
         XR    RE,RE                                                            
         L     R4,AGENAXTN                                                      
NEW      USING STCELD,R4                                                        
         XC    IOKEY,IOKEY                                                      
         XC    BYTE1,BYTE1                                                      
         LA    R2,IOKEY                                                         
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3                                        
         USING AUDRECD,R2                                                       
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVC   AUDKAUDT,BYTE2                                                   
         MVC   AUDKPID,QA_PIN                                                   
         MVI   AUDKSEQ,0                                                        
         MVC   CSVKEY1,AUDKEY                                                   
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   PERAUD56                                                         
PERAUD10 MVC   BYTE1,AUDKSEQ                                                    
                                                                                
PERAUD20 MVC   CSVKEY2,AUDKEY                                                   
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
PERAUD26 CLI   NEW.STCEL,0                                                      
         JE    PERAUD40                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,AUDRLEN                                                     
         IC    RF,NEW.STCLN                                                     
         ST    R4,SVSTCELS                                                      
         AR    RE,RF                                                            
         CH    RE,=H'2000'                                                      
         JH    PERAUD40                                                         
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,NEW.STCELD,=C'ADD=END'         
         CLI   12(R1),0                                                         
         JE    PERAUD38                                                         
         CLI   12(R1),5                                                         
         JE    PERAUD40                                                         
         DC    H'0'                                                             
PERAUD38 SR    RE,RE                                                            
         IC    RE,NEW.STCLN                                                     
         AR    R4,RE                                                            
         J     PERAUD26                                                         
*                                                                               
PERAUD40 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    PERAUD44                                                         
         DC    H'0'                                                             
PERAUD44 MVC   IOKEY,CSVKEY2                                                    
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(IOSQUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   CSVKEY1(AUDKSEQ-AUDKEY),AUDKEY DO ANY RECORDS EXIST              
         JE    PERAUD10            YES                                          
PERAUD54 SR    RE,RE               NO - BUILD NEW RECORD                        
         IC    RE,BYTE1                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
PERAUD56 CLI   NEW.STCEL,0                                                      
         JE    PERDAUDY                                                         
         L     R2,AIO2                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO2                                        
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVC   AUDKAUDT,BYTE2                                                   
         MVC   AUDKPID,QA_PIN                                                   
         MVC   AUDKSEQ,BYTE1                                                    
         MVC   AUDRLEN,=Y(RAURFST-RAUKEY)                                       
PERAUD58 CLI   NEW.STCEL,0                                                      
         JE    PERAUD62                                                         
         SR    RE,RE                                                            
         ICM   RE,3,AUDRLEN                                                     
         SR    RF,RF                                                            
         IC    RF,NEW.STCLN                                                     
         ST    R4,SVSTCELS                                                      
         AR    RE,RF                                                            
         CH    RE,=H'2000'                                                      
         JH    PERAUD62                                                         
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,NEW.STCELD,=C'ADD=END'         
         CLI   12(R1),0                                                         
         JE    PERAUD60                                                         
         CLI   12(R1),5                                                         
         JE    PERAUD62                                                         
         DC    H'0'                                                             
*                                                                               
PERAUD60 SR    RE,RE                                                            
         IC    RE,NEW.STCLN                                                     
         AR    R4,RE                                                            
         J     PERAUD58                                                         
*                                                                               
PERAUD62 MVC   IOKEY,AUDKEY                                                     
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    PERAUD64                                                         
         CLI   IOERR,IOEDEL                                                     
         JE    PERAUD64                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    PERAUD54                                                         
         DC    H'0'                                                             
*                                                                               
PERAUD64 DS    0H                  Update existing records                      
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO7                                        
         L     R0,AIO7             copy new record to IO7                       
         LA    R1,IOLENQ                                                        
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    PERAUD66                                                         
         DC    H'0'                                                             
*                                                                               
PERAUD66 L     R0,AIO2             copy new record to IO2                       
         LA    R1,2000                                                          
         L     RE,AIO7                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    PERAUD68                                                         
         DC    H'0'                                                             
PERAUD68 LA    RE,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    PERAUD54                                                         
         DC    H'0'                                                             
         DROP  NEW,R2                                                           
*                                                                               
PERDAUDY J     EXITY                                                            
                                                                                
PERDAUDN J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Add elements from Genarea to record in AIO1                         *         
* on entry asvelst is start of next element to add from genarea       *         
* on exit  asvelst is start of next element to add from genarea       *         
*          alldone set to Y if no more elements to be added           *         
* firstly find any element lower than 1F - pass1                      *         
* then process the 1F's - pass2                                       *         
* then process any element greater than 1F - pass3                    *         
* if approver rec update HALF1 with status                            *         
***********************************************************************         
         SPACE 1                                                                
FILLREC  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*AGENEL*'                                                      
         L     R3,AIO1                                                          
         USING LLSRECD,R3                                                       
         MVC   TEMP(L'LLSKEY),LLSKEY                                            
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         MVC   LLSKEY,TEMP                                                      
         MVI   PASSFLAG,1                                                       
         CLI   1(R3),LLSKSUBQ                                                   
         JNE   FILLR10                                                          
         MVC   LLSRLEN,=Y(LLSRFST-LLSKEY+1)                                     
         XC    LLSRSTA,LLSRSTA                                                  
         XC    LLSRLNK,LLSRLNK                                                  
         MVI   LLSRFST,0                                                        
         J     FILLR25                                                          
         DROP  R3                                                               
FILLR10  CLI   1(R3),APPKSUBQ                                                   
         JNE   FILLR20                                                          
         USING APPRECD,R3                                                       
         MVC   APPRLEN,=Y(APPRFST-APPKEY+1)                                     
         XC    APPRSTA,APPRSTA                                                  
         XC    APPRLNK,APPRLNK                                                  
         MVI   APPRFST,0                                                        
         J     FILLR25                                                          
         DROP  R3                                                               
         USING LLSRECD,R3                                                       
FILLR20  DC    H'0'                                                             
FILLR25  L     R4,ASVELST       start of next element in genarea to add         
         CLI   PASSFLAG,1                                                       
         JNE   FILLR52                                                          
         CLI   0(R4),LIDELQ                                                     
         JNL   FILLR60                                                          
         J     FILLR56                                                          
FILLR52  CLI   PASSFLAG,2                                                       
         JNE   FILLR54                                                          
         CLI   0(R4),LIDELQ                                                     
         JNE   FILLR60                                                          
         J     FILLR56                                                          
FILLR54  CLI   0(R4),LIDELQ                                                     
         JNH   FILLR60                                                          
FILLR56  SR    R2,R2                                                            
         IC    R2,1(R4)         element length                                  
         SR    RF,RF                                                            
         ICM   RF,3,LLSRLEN                                                     
         AR    RF,R2                                                            
         CHI   RF,IOLENQ                                                        
         JH    EXITY            record full                                     
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO1,ASVELST,=CL8'ADD=CODE'            
FILLR60  SR    RF,RF            bump to next element in genarea                 
         L     RF,ASVELST                                                       
         SR    RE,RE                                                            
         IC    RE,1(R4)         element length                                  
         AR    RF,RE                                                            
         ST    RF,ASVELST                                                       
         CLI   0(RF),0                                                          
         JNE   FILLR25                                                          
         L     RF,AGENAREA                                                      
         ST    RF,ASVELST                                                       
         CLI   PASSFLAG,1                                                       
         JNE   *+12                                                             
         MVI   PASSFLAG,2                                                       
         J     FILLR25                                                          
         CLI   PASSFLAG,2                                                       
         JNE   *+12                                                             
         MVI   PASSFLAG,3                                                       
         J     FILLR25                                                          
         MVI   ALLDONE,YESQ                                                     
         CLI   1(R3),APPKSUBQ                                                   
         JNE   EXITY                                                            
         XC    HALF1,HALF1                                                      
         CLI   QA_GLECA,C'2'                                                    
         JNE   *+8                                                              
         OI    HALF1+1,APPSESTQ                                                 
         CLI   QA_GLICA,C'2'                                                    
         JNE   *+8                                                              
         OI    HALF1+1,APPSESIQ                                                 
         CLI   QA_GLJA,C'2'                                                     
         JNE   *+8                                                              
         OI    HALF1+1,APPSRJAQ                                                 
         CLI   QA_GLEFA,C'2'                                                    
         JNE   *+8                                                              
         OI    HALF1,APPSFINA                                                   
         CLI   QA_GLJA,C'4'                                                     
         JNE   *+8                                                              
         OI    HALF1+1,APPSDJAQ                                                 
         CLI   QA_GLEFA,C'4'                                                    
         JNE   *+8                                                              
         OI    HALF1,APPSFIND                                                   
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Save error messages in an area so we can send all errors together   *         
***********************************************************************         
                                                                                
SAVERR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SAVERR*'                                                      
                                                                                
         L     RF,ANXTERR          Address of next error in table               
         LAY   RE,ERRTAB                                                        
         CLI   0(RE),ET_EOTQ      If table is empty                             
         JNE   SAVERR10                                                         
         LAY   RF,ERRTAB           Point to start of table                      
         XC    ALSTERR,ALSTERR                                                  
         USING ET_D,RF                                                          
SAVERR10 LLC   R0,4(R1)                                                         
         AHI   R0,ET_LN1Q          R0=Length of new entry                       
         AR    R0,RF                                                            
         LAY   RE,ERRTAB+L'ERRTAB                                               
         CR    R0,RE                                                            
         JH    SAVERR30            No room left                                 
         SR    R0,RF                                                            
         STC   R0,0(RF)            Set length of new entry                      
         OI    TWAMODE,TWAMERP     Set we have validation error                 
         LM    R2,R4,0(R1)                                                      
         MVC   ET_ERRNO,0(R2)                                                   
         MVC   ET_ROWNM,0(R4)                                                   
         SHI   R0,ET_LN1Q                                                       
         LTR   R1,R0               Test any extra text to be added              
         JZ    SAVERR20            No                                           
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   ET_EXTRA(0),0(R3)   Move extra text into entry                   
         EX    R1,0(RE)                                                         
SAVERR20 ST    RF,ALSTERR          REMEMBER PREVIOUS ERROR SLOT                 
         LA    RF,ET_D+ET_LN1Q     Point to next error slot                     
         AR    RF,R0               Add length of extra text                     
         ST    RF,ANXTERR          Set A(Next entry to be added)                
         MVI   ET_D,ET_EOTQ        Set new end of table                         
         J     EXITY                                                            
                                                                                
SAVERR30 DS    0H                  Too many errors for DDLINKIO                 
         L     RF,ALSTERR          Truncate it                                  
         LA    R0,ET_LN1Q                                                       
         AR    R0,RF                                                            
         LAY   RE,ERRTAB+L'ERRTAB                                               
         CR    R0,RE                                                            
         JNH   *+6                 Room left to warn user                       
         DC    H'0'                table still too long                         
         SR    R0,RF                                                            
         STC   R0,0(RF)            Set length of new entry                      
         OI    TWAMODE,TWAMEDP     Give up now                                  
         MVC   ET_ERRNO,=AL2(AE$TMERR)                                          
         MVI   ET_LN1Q(RF),ET_EOTQ                                              
         J     EXITY                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON HEADER UPLOAD - A0                           *         
***********************************************************************         
                                                                                
PERHED   LKREQ H,A#PHDR,NEWREC=Y                                                
PIN      LKREQ F,01,(D,B#SAVED,QA_PIN),HEXD,TEXT=(*,PINLIT)                     
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON CLIENT ACCESS UPLOAD -A1                     *         
***********************************************************************         
                                                                                
PERCLI   LKREQ H,A#PCLI,NEWREC=Y                                                
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA1),CHAR,TEXT=(*,DEFACLIT)                 
OffCode  LKREQ F,02,(I,B#SAVED,QA_CLACI),CHAR,TEXT=AC#OFFC,            +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA1OFF,ARRAY=S                    
CliCode  LKREQ F,03,,CHAR,TEXT=AC#CLIC,OLEN=L'MA1CLI,DELIM=COMMA                
ProdCode LKREQ F,04,,CHAR,TEXT=AC#PROC,OLEN=L'MA1PROD,DELIM=COMMA               
JobCode  LKREQ F,05,,CHAR,TEXT=AC#JOBC,OLEN=L'MA1JOBC,DELIM=COMMA               
Estimate LKREQ F,06,,CHAR,TEXT=(*,ESTSLIT),OLEN=L'MA1EST,DELIM=COMMA            
Expenses LKREQ F,07,,CHAR,TEXT=AC#EXPEN,OLEN=L'MA1EXP,DELIM=COMMA               
Invoices LKREQ F,08,,CHAR,TEXT=AC#INVS,OLEN=L'MA1INV,DELIM=COMMA                
Jobs     LKREQ F,09,,CHAR,TEXT=AC#JOBS,OLEN=L'MA1JOB,DELIM=COMMA                
Orders   LKREQ F,10,,CHAR,TEXT=AC#ORDS,OLEN=L'MA1ORD,DELIM=COMMA                
Resource LKREQ F,11,,CHAR,TEXT=AC#BORES,OLEN=L'MA1RES,DELIM=COMMA               
Time     LKREQ F,12,,CHAR,TEXT=AC#TIME,OLEN=L'MA1TIM,ARRAY=E,          +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA1D     DSECT                                                                  
MA1OFF   DS    XL2                                                              
MA1CLI   DS    XL6                                                              
MA1PROD  DS    XL6                                                              
MA1JOBC  DS    XL6                                                              
MA1EST   DS    XL1                                                              
MA1EXP   DS    XL1                                                              
MA1INV   DS    XL1                                                              
MA1JOB   DS    XL1                                                              
MA1ORD   DS    XL1                                                              
MA1RES   DS    XL1                                                              
MA1TIM   DS    XL1                                                              
MA1LENQ  EQU   *-MA1D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON MEDIA ACCESS UPLOAD - A2                     *         
***********************************************************************         
                                                                                
PERMED   LKREQ H,A#PMED,NEWREC=Y                                                
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA2),CHAR,TEXT=(*,DEFACLIT)                 
MedCode  LKREQ F,02,(I,B#SAVED,QA_MEACI),CHAR,TEXT=AC#MEDC,            +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA2MED,ARRAY=S                    
Estimate LKREQ F,03,,CHAR,TEXT=(*,ESTSLIT),OLEN=L'MA2EST,DELIM=COMMA            
Expenses LKREQ F,04,,CHAR,TEXT=AC#EXPEN,OLEN=L'MA2EXP,DELIM=COMMA               
Invoices LKREQ F,05,,CHAR,TEXT=AC#INVS,OLEN=L'MA2INV,DELIM=COMMA                
Jobs     LKREQ F,06,,CHAR,TEXT=AC#JOBS,OLEN=L'MA2JOB,DELIM=COMMA                
Orders   LKREQ F,07,,CHAR,TEXT=AC#ORDS,OLEN=L'MA2ORD,DELIM=COMMA                
Resource LKREQ F,08,,CHAR,TEXT=AC#BORES,OLEN=L'MA2RES,DELIM=COMMA               
Time     LKREQ F,09,,CHAR,TEXT=AC#TIME,OLEN=L'MA2TIM,ARRAY=E,          +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA2D     DSECT                                                                  
MA2MED   DS    XL1                                                              
MA2EST   DS    XL1                                                              
MA2EXP   DS    XL1                                                              
MA2INV   DS    XL1                                                              
MA2JOB   DS    XL1                                                              
MA2ORD   DS    XL1                                                              
MA2RES   DS    XL1                                                              
MA2TIM   DS    XL1                                                              
MA2LENQ  EQU   *-MA2D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON EXP TYPE ACCESS UPLOAD - A3                  *         
***********************************************************************         
                                                                                
PEREXP   LKREQ H,A#PEXP,NEWREC=Y                                                
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA3),CHAR,TEXT=(*,DEFACLIT)                 
ExpTypCd LKREQ F,02,(I,B#SAVED,QA_EXACI),CHAR,TEXT=(*,EXTCDLIT),       +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA3EXTC,ARRAY=S                   
Orders   LKREQ F,03,,CHAR,TEXT=AC#ORDS,OLEN=L'MA3ORD,DELIM=COMMA                
Invoices LKREQ F,04,,CHAR,TEXT=AC#INVS,OLEN=L'MA3INV,DELIM=COMMA                
Expenses LKREQ F,05,,CHAR,TEXT=AC#EXPEN,OLEN=L'MA3EXP,ARRAY=E,         +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA3D     DSECT                                                                  
MA3EXTC  DS    XL3                                                              
MA3ORD   DS    XL1                                                              
MA3INV   DS    XL1                                                              
MA3EXP   DS    XL1                                                              
MA3LENQ  EQU   *-MA3D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON NON CLIENT ACCESS UPLOAD - A4                *         
***********************************************************************         
                                                                                
PERNCL   LKREQ H,A#PNCL,NEWREC=Y                                                
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA4),CHAR,TEXT=(*,DEFACLIT)                 
NonClCde LKREQ F,02,(I,B#SAVED,QA_NCACI),CHAR,TEXT=AC#NCCOD,           +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA4NCC,ARRAY=S                    
Time     LKREQ F,03,,CHAR,TEXT=AC#TIME,OLEN=L'MA4TIM,ARRAY=E,          +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA4D     DSECT                                                                  
MA4NCC   DS    XL12                                                             
MA4TIM   DS    XL1                                                              
MA4LENQ  EQU   *-MA4D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON STAFF ACCOUNT ACCESS UPLOAD - A5             *         
***********************************************************************         
                                                                                
PERSTAF  LKREQ H,A#PSTF,NEWREC=Y                                                
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA5),CHAR,TEXT=(*,DEFACLIT)                 
Office   LKREQ F,02,(I,B#SAVED,QA_STACI),CHAR,TEXT=AC#OFFC,            +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA5OFF,ARRAY=S                    
DeptCode LKREQ F,03,,CHAR,TEXT=(*,DEPCDLIT),OLEN=L'MA5DEPT,DELIM=COMMA          
SubDepCd LKREQ F,04,,CHAR,TEXT=(*,SUBDCLIT),OLEN=L'MA5SUBD,DELIM=COMMA          
PersCode LKREQ F,05,,CHAR,TEXT=AC#RSPSC,OLEN=L'MA5PER,DELIM=COMMA               
Expenses LKREQ F,06,,CHAR,TEXT=AC#EXPEN,OLEN=L'MA5EXP,DELIM=COMMA               
Time     LKREQ F,07,,CHAR,TEXT=AC#TIME,OLEN=L'MA5TIM,ARRAY=E,          +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA5D     DSECT                                                                  
MA5OFF   DS    XL2                                                              
MA5DEPT  DS    XL6                                                              
MA5SUBD  DS    XL6                                                              
MA5PER   DS    XL8                                                              
MA5EXP   DS    XL1                                                              
MA5TIM   DS    XL1                                                              
MA5LENQ  EQU   *-MA5D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON WORK CODE ACCESS UPLOAD - A6                 *         
***********************************************************************         
                                                                                
PERWCOD  LKREQ H,A#PWC,NEWREC=Y                                                 
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA6),CHAR,TEXT=(*,DEFACLIT)                 
WorkCode LKREQ F,02,(I,B#SAVED,QA_WKACI),CHAR,TEXT=AC#WC,              +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA6WORK,ARRAY=S                   
Time     LKREQ F,03,,CHAR,TEXT=AC#TIME,OLEN=L'MA6TIM,ARRAY=E,          +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA6D     DSECT                                                                  
MA6WORK  DS    XL2                                                              
MA6TIM   DS    XL1                                                              
MA6LENQ  EQU   *-MA6D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON REPORT FORMAT ACCESS UPLOAD - A7             *         
***********************************************************************         
                                                                                
PERREPF  LKREQ H,A#PREP,NEWREC=Y                                                
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA7),CHAR,TEXT=(*,DEFACLIT)                 
RepForcd LKREQ F,02,(I,B#SAVED,QA_RPACI),CHAR,TEXT=(*,RPFCDLIT),       +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA7FORM,ARRAY=S                   
Reports  LKREQ F,03,,CHAR,TEXT=(*,REPRTLIT),OLEN=L'MA7REP,ARRAY=E,     +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA7D     DSECT                                                                  
MA7FORM  DS    XL8                                                              
MA7REP   DS    XL1                                                              
MA7LENQ  EQU   *-MA7D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON ESTIMATE SCHEME ACCESS UPLOAD - A8           *         
***********************************************************************         
                                                                                
PERESTS  LKREQ H,A#PESCH,NEWREC=Y                                               
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA8),CHAR,TEXT=(*,DEFACLIT)                 
RepScCd  LKREQ F,02,(I,B#SAVED,QA_ESTAI),CHAR,TEXT=(*,REPSCLIT),       +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA8SCH,ARRAY=S                    
Estimate LKREQ F,03,,CHAR,TEXT=(*,ESTSLIT),OLEN=L'MA8EST,ARRAY=E,      +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA8D     DSECT                                                                  
MA8SCH   DS    XL8                                                              
MA8EST   DS    XL1                                                              
MA8LENQ  EQU   *-MA8D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON SUPPLIER ACCESS UPLOAD - A9                  *         
***********************************************************************         
                                                                                
PERSUP   LKREQ H,A#PSUP,NEWREC=Y                                                
DefAcc   LKREQ F,01,(D,B#SAVED,QA_DEFA9),CHAR,TEXT=(*,DEFACLIT)                 
SupLeCd  LKREQ F,02,(I,B#SAVED,QA_SUACI),CHAR,TEXT=(*,SUPLCLIT),       +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MA9SLED,ARRAY=S                   
SupAcCd  LKREQ F,03,,CHAR,TEXT=(*,SUPACLIT),OLEN=L'MA9SACC,DELIM=COMMA          
Invoices LKREQ F,04,,CHAR,TEXT=AC#INVS,OLEN=L'MA9INV,ARRAY=E,          +        
               DELIM=COMMA                                                      
         LKREQ E                                                                
MA9D     DSECT                                                                  
MA9SLED  DS    XL1                                                              
MA9SACC  DS    XL12                                                             
MA9INV   DS    XL1                                                              
MA9LENQ  EQU   *-MA9D                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON GROUP LIST ACCESS UPLOAD - AA                *         
***********************************************************************         
                                                                                
PERGRP   LKREQ H,A#PGRP,NEWREC=Y                                                
GrpLiCd  LKREQ F,01,(I,B#SAVED,QA_GRPLI),CHAR,TEXT=(*,GRLICLIT),       +        
               LIST=NOSORT,DELIM=COMMA,OLEN=L'MAAGLC,ARRAY=*                    
         LKREQ E                                                                
MAAD     DSECT                                                                  
MAAGLC   DS    XL8                                                              
MAALENQ  EQU   *-MAAD                                                           
SVRDEF   CSECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PERSON GLOBAL APPROVAL UPLOAD - B0                  *         
***********************************************************************         
                                                                                
PERGLOB  LKREQ H,A#PGLOAP,NEWREC=Y                                              
EstClApp LKREQ F,01,(D,B#SAVED,QA_GLECA),CHAR,TEXT=(*,GLESCLIT)                 
IntEstCA LKREQ F,02,(D,B#SAVED,QA_GLICA),CHAR,TEXT=(*,GLISCLIT)                 
JobApp   LKREQ F,03,(D,B#SAVED,QA_GLJA),CHAR,TEXT=(*,GLJOBLIT)                  
ExpFinAp LKREQ F,04,(D,B#SAVED,QA_GLEFA),CHAR,TEXT=(*,GLEXFLIT)                 
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON CLIENT APPROVAL UPLOAD - B1                  *         
***********************************************************************         
                                                                                
PERCLAP  LKREQ H,A#PCLIAP,NEWREC=Y                                              
OffCode  LKREQ F,01,(D,B#SAVED,QA_OFFCO),CHAR,TEXT=AC#OFFC                      
CliCode  LKREQ F,02,(D,B#SAVED,QA_CLICO),CHAR,TEXT=AC#CLIC                      
ProdCode LKREQ F,03,(D,B#SAVED,QA_PROCO),CHAR,TEXT=AC#PROC                      
JobCode  LKREQ F,04,(D,B#SAVED,QA_JOBCO),CHAR,TEXT=AC#JOBC                      
MedCode  LKREQ F,05,(D,B#SAVED,QA_MEDCO),CHAR,TEXT=AC#MEDC                      
EstApp   LKREQ F,06,(D,B#SAVED,QA_ESTAP),CHAR,TEXT=AC#ESAPR                     
EstDefAp LKREQ F,07,(D,B#SAVED,QA_ESDFA),CHAR,TEXT=(*,ESDALIT)                  
IntEstAp LKREQ F,08,(D,B#SAVED,QA_INTEA),CHAR,TEXT=AC#IESAP                     
InEsDefA LKREQ F,09,(D,B#SAVED,QA_INEDA),CHAR,TEXT=(*,IEDALIT)                  
Exp1ApBi LKREQ F,10,(D,B#SAVED,QA_E1AB),CHAR,TEXT=(*,EX1ABLIT)                  
Exp1ApNB LKREQ F,11,(D,B#SAVED,QA_E1ANB),CHAR,TEXT=(*,EX1NBLIT)                 
Exp2ApBi LKREQ F,12,(D,B#SAVED,QA_E2AB),CHAR,TEXT=(*,EX2ABLIT)                  
Exp2ApNB LKREQ F,13,(D,B#SAVED,QA_E2ANB),CHAR,TEXT=(*,EX2NBLIT)                 
JobApp   LKREQ F,14,(D,B#SAVED,QA_JOBAP),CHAR,TEXT=AC#JOBAP                     
JobDefAp LKREQ F,15,(D,B#SAVED,QA_JOBDA),CHAR,TEXT=(*,JOBDFLIT)                 
TimeApp  LKREQ F,16,(D,B#SAVED,QA_TIMAP),CHAR,TEXT=AC#TIME                      
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON NON-CLIENT APPROVAL UPLOAD - B2              *         
***********************************************************************         
                                                                                
PERNCLP  LKREQ H,A#PNCLAP,NEWREC=Y                                              
NonCliCo LKREQ F,01,(D,B#SAVED,QA_NCLCO),CHAR,TEXT=AC#NCCOD                     
Time     LKREQ F,02,(D,B#SAVED,QA_TIME),CHAR,TEXT=AC#TIME                       
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON STAFF APPROVAL UPLOAD - B3                   *         
***********************************************************************         
                                                                                
PERSTAP  LKREQ H,A#PSTFAP,NEWREC=Y                                              
OffCode  LKREQ F,01,(D,B#SAVED,QA_OFFCO),CHAR,TEXT=AC#OFFC                      
DeptCode LKREQ F,02,(D,B#SAVED,QA_DEPCO),CHAR,TEXT=AC#DEPC                      
SubDpCod LKREQ F,03,(D,B#SAVED,QA_SUBDC),CHAR,TEXT=AC#CP123                     
PersCode LKREQ F,04,(D,B#SAVED,QA_PERCO),CHAR,TEXT=AC#RSPSC                     
ExpLinAp LKREQ F,05,(D,B#SAVED,QA_EXLMA),CHAR,TEXT=(*,EXLINLIT)                 
Ex2LinAp LKREQ F,06,(D,B#SAVED,QA_E2LMA),CHAR,TEXT=(*,EX2LNLIT)                 
ExpFinAp LKREQ F,07,(D,B#SAVED,QA_EXFAP),CHAR,TEXT=(*,EXFINLIT)                 
ExDfFiAp LKREQ F,08,(D,B#SAVED,QA_EXDFA),CHAR,TEXT=(*,EXDFNLIT)                 
Time     LKREQ F,09,(D,B#SAVED,QA_TIME),CHAR,TEXT=AC#TIME                       
ExpLimit LKREQ F,10,(D,B#SAVED,QA_EXAPL),SPAK,TEXT=(*,EXLIMLIT)                 
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON INVOICES ORDERS APPROVAL UPLOAD - B4         *         
***********************************************************************         
                                                                                
PERINVO  LKREQ H,A#PI_OAP,NEWREC=Y                                              
OrdApp   LKREQ F,01,(D,B#SAVED,QA_OA),CHAR,TEXT=(*,ORDAPLIT)                    
OrdDefAp LKREQ F,02,(D,B#SAVED,QA_OAD),CHAR,TEXT=(*,ORDDALIT)                   
OFinAp   LKREQ F,03,(D,B#SAVED,QA_OF),CHAR,TEXT=(*,FINAPLIT)                    
OFinDefA LKREQ F,04,(D,B#SAVED,QA_OFD),CHAR,TEXT=(*,FINDALIT)                   
InvAp    LKREQ F,05,(D,B#SAVED,QA_IA),CHAR,TEXT=(*,INVAPLIT)                    
InvDefAp LKREQ F,06,(D,B#SAVED,QA_IAD),CHAR,TEXT=(*,INVDALIT)                   
InOrType LKREQ F,07,(D,B#SAVED,QA_IOTYP),CHAR,TEXT=(*,IOTYPLIT)                 
CliCode  LKREQ F,08,(D,B#SAVED,QA_CLICO),CHAR,TEXT=AC#CLIC                      
ProdCode LKREQ F,09,(D,B#SAVED,QA_PROCO),CHAR,TEXT=AC#PROC                      
JobCode  LKREQ F,10,(D,B#SAVED,QA_JOBCO),CHAR,TEXT=AC#JOBC                      
OffCode  LKREQ F,11,(D,B#SAVED,QA_OFFCO),CHAR,TEXT=AC#OFFC                      
DeptCode LKREQ F,12,(D,B#SAVED,QA_DEPCO),CHAR,TEXT=AC#DEPC                      
ExpdCode LKREQ F,13,(D,B#SAVED,QA_EXPCO),CHAR,TEXT=(*,EXPCDLIT)                 
SupLeCde LKREQ F,14,(D,B#SAVED,QA_SUPLC),CHAR,TEXT=(*,SULECLIT)                 
SupAcCde LKREQ F,15,(D,B#SAVED,QA_SUPAC),CHAR,TEXT=(*,SUACCLIT)                 
2PAccCde LKREQ F,16,(D,B#SAVED,QA_2PAC),CHAR,TEXT=(*,ACC2PLIT)                  
MedCode  LKREQ F,17,(D,B#SAVED,QA_MEDCO),CHAR,TEXT=AC#MEDC                      
SlfApAmt LKREQ F,18,(D,B#SAVED,QA_SLFAA),SPAK,TEXT=(*,SELFALIT)                 
AppAmt   LKREQ F,19,(D,B#SAVED,QA_APPAM),SPAK,TEXT=(*,APPAMLIT)                 
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON BACK UP APPROVAL UPLOAD - B5                 *         
***********************************************************************         
                                                                                
PERBUPA  LKREQ H,A#PBUAP,NEWREC=Y                                               
PIN      LKREQ F,01,(D,B#SAVED,QA_BUPIN),HEXD,TEXT=(*,PINLIT)                   
Time     LKREQ F,02,(D,B#SAVED,QA_TIME),CHAR,TEXT=AC#TIME                       
Expense  LKREQ F,03,(D,B#SAVED,QA_EXPEN),CHAR,TEXT=AC#EXP                       
Orders   LKREQ F,04,(D,B#SAVED,QA_ORDER),CHAR,TEXT=AC#ORDS                      
Invoices LKREQ F,05,(D,B#SAVED,QA_INVCE),CHAR,TEXT=AC#INVS                      
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON ROLE UPLOAD - B6                             *         
***********************************************************************         
                                                                                
PEROLE   LKREQ H,A#PROL,NEWREC=Y                                                
Role     LKREQ F,01,(D,B#SAVED,QA_ROLE),LBIN,TEXT=AC#ROLES                      
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON TRAILER UPLOAD - BF                          *         
***********************************************************************         
                                                                                
PERTRLR  LKREQ H,A#PTRL,NEWREC=Y                                                
Empty    LKREQ F,01,(D,B#SAVED,QA_DEFAC),CHAR,TEXT=(*,EMPTLIT)                  
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR PERSON ADD/DELETE TO JOB UPLOAD - 0004              *         
***********************************************************************         
                                                                                
PERACTU  LKREQ H,A#PRAC,NEWREC=Y                                                
Actn     LKREQ F,01,(D,B#SAVED,QA_ACTN),LBIN,TEXT=(*,ACTNLIT)                   
Unit     LKREQ F,02,(D,B#SAVED,QA_UNIT),CHAR,TEXT=(*,UNITLIT)                   
Ledgr    LKREQ F,03,(D,B#SAVED,QA_LEDG),CHAR,TEXT=(*,LEDGLIT)                   
Acc      LKREQ F,04,(D,B#SAVED,QA_ACCT),CHAR,TEXT=(*,ACCTLIT)                   
Indx     LKREQ F,05,(D,B#SAVED,QA_INDX),HEXD,TEXT=(*,INDXLIT)                   
PIN      LKREQ F,06,(D,B#SAVED,QA_APIN),HEXD,TEXT=(*,BPINLIT)                   
         LKREQ E                                                                
***********************************************************************         
* END OF REQUEST MAP TABLES                                          *          
***********************************************************************         
                                                                                
         LKMAP X                                                                
                                                                                
*** GENERAL UPLOAD RESPONSE DATA MAP NUMBERS                                    
                                                                                
* general equates *                                                             
D#UPLERR EQU   255                                                              
D#UPLNOD EQU   254                                                              
DELMEQ   EQU   C'D'                                                             
ADDMEQ   EQU   0                                                                
NOCHANGQ EQU   C'L'                                                             
COMMA    EQU   C','                                                             
***********************************************************************         
* Validate text fields                                                *         
***********************************************************************         
                                                                                
VALTXT   LM    R2,R4,LP_AINP-LP_D(R5)                                           
         STC   R3,0(R4)            Returns L'Text,Text string                   
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         MVC   1(0,R4),0(R2)                                                    
         EX    R3,0(RE)                                                         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* GENERAL EXITS HERE                                                  *         
***********************************************************************         
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
D#ID#    EQU   1                                                                
D#ERR    EQU   2                   Error                                        
                                                                                
CANALLIT DC    C'Client analysis'                                               
SANALLIT DC    C'Staff analysis'                                                
PEXECLIT DC    C'Person is executive'                                           
PROCNLIT DC    C'Project control'                                               
FUSRLIT  DC    C'Foreign User'                                                  
FNAMLIT  DC    C'Foreign name'                                                  
BALNCLIT DC    C'Balance sheet'                                                 
PRLSSLIT DC    C'Profit and loss'                                               
JANALLIT DC    C'Job analysis'                                                  
GLOFLIT  DC    C'General ledger office'                                         
GLACLIT  DC    C'General ledger account'                                        
VATLIT   DC    C'VAT number'                                                    
TAXLIT   DC    C'Tax/VAT registration number'                                   
SECLIT   DC    C'Security'                                                      
NICPLIT  DC    C'National Insurance profile'                                    
CLCSTLIT DC    C'Client cost'                                                   
MASTLIT  DC    C'Is master job'                                                 
ESTILIT  DC    C'Is brandocean estimate'                                        
ESTSLIT  DC    C'Estimates'                                                     
ESDALIT  DC    C'Estimate default approver'                                     
IEDALIT  DC    C'Internal estimate default approver'                            
EX1ABLIT DC    C'Expense level 1 approver billable'                             
EX1NBLIT DC    C'Expense level 1 approver non-billable'                         
EX2ABLIT DC    C'Expense level 2 approver billable'                             
EX2NBLIT DC    C'Expense level 2 approver non-billable'                         
JOBDFLIT DC    C'Jobs default approver'                                         
EXLINLIT DC    C'Expenses line manager approver'                                
EX2LNLIT DC    C'Expenses level 2 line manager approver'                        
EXFINLIT DC    C'Expenses finance approver'                                     
EXDFNLIT DC    C'Expenses default finance approver'                             
EXLIMLIT DC    C'Expense approval limit'                                        
PINLIT   DC    C'PIN'                                                           
ORDAPLIT DC    C'Order approver'                                                
ORDDALIT DC    C'Order default approver'                                        
FINAPLIT DC    C'Order finance approver'                                        
FINDALIT DC    C'Order finance default approver'                                
INVAPLIT DC    C'Invoice approver'                                              
INVDALIT DC    C'Invoice default approver'                                      
IOTYPLIT DC    C'Invoice/order type'                                            
EXPCDLIT DC    C'Expenditure code'                                              
SULECLIT DC    C'Supplier ledger code'                                          
SUACCLIT DC    C'Supplier account code'                                         
ACC2PLIT DC    C'2P account code'                                               
SELFALIT DC    C'Self approval amount'                                          
APPAMLIT DC    C'Approval amount'                                               
GRLICLIT DC    C'Group list code'                                               
GLESCLIT DC    C'Global estimate client approver'                               
GLISCLIT DC    C'Global internal estimate client approver'                      
GLJOBLIT DC    C'Global job approver'                                           
GLEXFLIT DC    C'Global expense finance approver'                               
DEFACLIT DC    C'Default access'                                                
EXTCDLIT DC    C'Expenditure type code'                                         
DEPCDLIT DC    C'Dept code'                                                     
SUBDCLIT DC    C'Sub-dept code'                                                 
RPFCDLIT DC    C'Report format code'                                            
REPRTLIT DC    C'Reports'                                                       
REPSCLIT DC    C'Report scheme code'                                            
SUPLCLIT DC    C'Supplier ledger code'                                          
SUPACLIT DC    C'Supplier account code'                                         
MCLILIT  DC    C'Master client'                                                 
MPROLIT  DC    C'Master product'                                                
MJOBLIT  DC    C'Master Job'                                                    
JLESTLIT DC    C'Job lock estimates'                                            
JLORDLIT DC    C'Job lock orders'                                               
JLBILLIT DC    C'Job lock billing'                                              
JLTIMLIT DC    C'Job lock timesheets'                                           
JLADJLIT DC    C'Job lock adjustments'                                          
JLEXTLIT DC    C'Job lock externals'                                            
LNDESLIT DC    C'Long description'                                              
DSKCLIT  DC    C'Desktop comments'                                              
DSKPLIT  DC    C'Desktop priority'                                              
TWOPLIT  DC    C'2P account code'                                               
TWODLIT  DC    C'2D account code'                                               
MADRLIT  DC    C'Media address'                                                 
MCOMLIT  DC    C'Media commission rate'                                         
MVATTLIT DC    C'Media VAT type'                                                
NEWBLIT  DC    C'Prevent new bookings'                                          
CMPRLIT  DC    C'Add campaign ref'                                              
BLCOMLIT DC    C'Billing comments compulsory'                                   
ORCOMLIT DC    C'Order comments compulsory'                                     
GEN1BLIT DC    C'Generate ME1B for booking'                                     
FRMCDLIT DC    C'Formula record code'                                           
MXAMTLIT DC    C'Max amount for media bill'                                     
SURCHLIT DC    C'Surcharge %'                                                   
BDGCULIT DC    C'Budget currency'                                               
DRNMLIT  DC    C'Debtors account name'                                          
DRLVLIT  DC    C'Debtors level'                                                 
CSNMLIT  DC    C'Costing account name'                                          
CSLVLIT  DC    C'Costing level'                                                 
BCHNMLIT DC    C'Bank charges name'                                             
EXCNMLIT DC    C'Exchange differences name'                                     
ANANMLIT DC    C'Analysis name'                                                 
DISNMLIT DC    C'Discount name'                                                 
TWOPNLIT DC    C'2P Account name'                                               
TWODNLIT DC    C'2D Account name'                                               
INCOLIT  DC    C'Income account code'                                           
INCONLIT DC    C'Income name'                                                   
WRITLIT  DC    C'Write off account code'                                        
WRITNLIT DC    C'Write off name'                                                
ONAMLIT  DC    C'Override name'                                                 
PLOKLIT  DC    C'Payee Locked'                                                  
PRCRLIT  DC    C'Peel/Close RecCRs'                                             
PRDRLIT  DC    C'Peel/Close RecDRs'                                             
B13VLIT  DC    C'13b Vat code'                                                  
EMPTLIT  DC    C'Empty - available for future use'                              
ACTNLIT  DC    C'Action - 1 for add, 2 for delete'                              
UNITLIT  DC    C'Unit code'                                                     
LEDGLIT  DC    C'Ledger code'                                                   
ACCTLIT  DC    C'Account code'                                                  
INDXLIT  DC    C'Index number from account'                                     
BPINLIT  DC    C'Binary PIN of person'                                          
                                                                                
MAXRECLN EQU   IOLENQ-(L'IODA+L'IOWORK)                                         
                                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
ACCMST   DC    C'ACCMST  '                                                      
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
LSPACES  DC    255C' '                                                          
FFS4     DC    X'FFFFFFFF'                                                      
ADDEND   DC    C'ADD=END '                                                      
ADDCODE  DC    C'ADD=CODE'                                                      
                                                                                
DMCOMMIT DC    C'COMMIT  '         Recovery checkpoint command                  
                                                                                
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(A#PHDR),AL1(RECTPHDR)                                        
         DC    AL2(A#PCLI),AL1(RECTPCLI)                                        
         DC    AL2(A#PMED),AL1(RECTPMED)                                        
         DC    AL2(A#PEXP),AL1(RECTPEXP)                                        
         DC    AL2(A#PNCL),AL1(RECTPNCL)                                        
         DC    AL2(A#PSTF),AL1(RECTPSTF)                                        
         DC    AL2(A#PWC),AL1(RECTPWC)                                          
         DC    AL2(A#PREP),AL1(RECTPREP)                                        
         DC    AL2(A#PESCH),AL1(RECTPEST)                                       
         DC    AL2(A#PSUP),AL1(RECTPSUP)                                        
         DC    AL2(A#PGRP),AL1(RECTPGRP)                                        
         DC    AL2(A#PGLOAP),AL1(RECTPGLO)                                      
         DC    AL2(A#PCLIAP),AL1(RECTCLAP)                                      
         DC    AL2(A#PNCLAP),AL1(RECTNCAP)                                      
         DC    AL2(A#PSTFAP),AL1(RECTSTAP)                                      
         DC    AL2(A#PI_OAP),AL1(RECTIOAP)                                      
         DC    AL2(A#PBUAP),AL1(RECTBUAP)                                       
         DC    AL2(A#PROL),AL1(RECTROLE)                                        
         DC    AL2(A#PTRL),AL1(RECTPTRL)                                        
         DC    AL2(A#PRAC),AL1(RECTPRAC)                                        
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
*                      Build person person elements from TSAR                   
LIMTAB   DS    0F                                                               
         DC    AL1(RECTPCLI),AL1(LIDTCPJL),AL1(LIDLLN9Q),AL1(STCSSCPJ)          
         DC    AL1(RECTPMED),AL1(LIDTMEDL),AL1(LIDLLN3Q),AL1(STCSSMED)          
         DC    AL1(RECTPEXP),AL1(LIDTEXPL),AL1(LIDLLN4Q),AL1(STCSSETY)          
         DC    AL1(RECTPNCL),AL1(LIDTNCLL),AL1(LIDLLN1Q),AL1(STCSSNON)          
         DC    AL1(RECTPSTF),AL1(LIDT1RAC),AL1(LIDLLN1Q),AL1(STCSSSTF)          
         DC    AL1(RECTPWC),AL1(LIDTWCL),AL1(LIDLLN2Q),AL1(STCSSWC)             
         DC    AL1(RECTPREP),AL1(LIDTSCRB),AL1(LIDLLN5Q),AL1(STCSSREP)          
         DC    AL1(RECTPEST),AL1(LIDTESCH),AL1(LIDLLN5Q),AL1(STCSSSCH)          
         DC    AL1(RECTPSUP),AL1(LIDTSUPP),AL1(LIDLLN8Q),AL1(STCSSSUP)          
         DC    X'FF'                                                            
APPTAB   DS    0F                                                               
         DC    AL1(RECTCLAP),AL1(LIDTAPSJ),AL1(LIDSJLNQ),AL1(STCSACPJ)          
         DC    AL1(RECTNCAP),AL1(LIDTAP1N),AL1(LID1NLNQ),AL1(STCSANON)          
         DC    AL1(RECTSTAP),AL1(LIDTAP1R),AL1(LID1RLNQ),AL1(STCSASTF)          
         DC    AL1(RECTIOAP),AL1(LIDTINOR),AL1(LIDAPLNQ),AL1(STCSAINO)          
         DC    AL1(RECTBUAP),AL1(LIDTBACK),AL1(LIDLLN6Q),AL1(STCSBACK)          
         DC    X'FF'                                                            
                                                                                
BPELTABD DSECT                     ** DSECT TO COVER BPELTAB **                 
BPELID   DS    XL1                                                              
BPELTYPE DS    XL1                                                              
BPELLEN  DS    XL1                 length of item within element                
BPSTCEL  DS    XL1                                                              
BPELLNQ  EQU   *-BPELTABD                                                       
                                                                                
AUDITABD DSECT                     ** DSECT TO COVER AUDITAB **                 
AUDIFLAG DS    XL1                                                              
AUDITYPE DS    XL1                                                              
AUDITLEN DS    XL1                                                              
AUDIAPPL DS    XL1                                                              
AUDIAPP2 DS    XL1                                                              
AUDIDATA DS    0X                                                               
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTPHDR EQU   1                   Person header                                
RECTPCLI EQU   2                   Client access                                
RECTPMED EQU   3                   Media access                                 
RECTPEXP EQU   4                   Expenditure types access                     
RECTPNCL EQU   5                   Non-client access                            
RECTPSTF EQU   6                   Staff access                                 
RECTPWC  EQU   7                   Workcode access                              
RECTPREP EQU   8                   Report format access                         
RECTPEST EQU   9                   Estimate scheme access                       
RECTPSUP EQU   10                  Supplier access                              
RECTPGRP EQU   11                  Group list access                            
RECTPGLO EQU   12                  Global approver                              
RECTCLAP EQU   13                  Client approver                              
RECTNCAP EQU   14                  Non client approver                          
RECTSTAP EQU   15                  Staff approver                               
RECTIOAP EQU   16                  Invoice and orders approver                  
RECTBUAP EQU   17                  Back up approver                             
RECTROLE EQU   18                  Client team roles                            
RECTPTRL EQU   19                  Person trailer                               
RECTPRAC EQU   20                  Person add/delete to account                 
RECTABL  EQU   *-RECTABD                                                        
RECTPGR2 EQU   99                  Dummy group list to get to end TSAR          
         EJECT                                                                  
                                                                                
SAVED    DSECT                                                                  
VACCEMU  DS    A                   Emulator converter                           
SVSTCELS DS    A                   Current address of stc elements              
SAVER1   DS    A                   Save R1                                      
SAVER4   DS    A                   Save R4                                      
SAVER6   DS    A                   Save R6                                      
SAVERF   DS    A                   Save RF                                      
SAVEPY   DS    A                   Save address of year position in job         
ASVELST  DS    A                   A(element start)                             
AAUDITAB DS    A                   A(next auditab entry)                        
ANXTERR  DS    A                   A(Next error entry)                          
ALSTERR  DS    A                   A(Previous error entry in errtab)            
*                                                                               
AGENCY   DS    XL1                                                              
USRID    DS    XL2                 USER ID TO BE USED                           
GLOBID   DS    XL2                 GLOBAL ID ACCORDING TO COMPANY REC           
OPEN     DS    CL6                 Open data in character                       
TSARREC  DS    XL(TSPXTNL)         TSAR block for record buffer                 
TSARRBLK DS    XL(TSPXTNL)         Tsar Block                                   
TSARNUMB DS    XL2                                                              
LASTSTYP DS    XL(L'TSARTYPE)                                                   
ALLDONE  DS    XL1                                                              
AUPAGOLD DS    XL9                 old audit page settings limit/apprvr         
PASSFLAG DS    XL1                                                              
SAVEVAR  DS    0F                  ** Variables **                              
                                                                                
***********************************************************************         
* Request values                                                      *         
***********************************************************************         
*                                                                               
QA_VALS  DS    0X                                                               
QA_PIN   DS    XL2                 PIN                                          
QA_DEFAC DS    CL1                 Default access for BF                        
DEFLISTQ EQU   C'1'                                                             
DEFALLQ  EQU   C'2'                                                             
DEFNONEQ EQU   C'3'                                                             
DEFDALLQ EQU   C'4'                                                             
                                                                                
QA_DEFS  DS    0CL9                defaults                                     
QA_DEFA1 DS    CL1                 Default access for A1                        
QA_DEFA2 DS    CL1                 Default access for A2                        
QA_DEFA3 DS    CL1                 Default access for A3                        
QA_DEFA4 DS    CL1                 Default access for A4                        
QA_DEFA5 DS    CL1                 Default access for A5                        
QA_DEFA6 DS    CL1                 Default access for A6                        
QA_DEFA7 DS    CL1                 Default access for A7                        
QA_DEFA8 DS    CL1                 Default access for A8                        
QA_DEFA9 DS    CL1                 Default access for A9                        
                                                                                
QA_CLACI DS    XL1                 Client access list indicator                 
QA_ACLAC DS    AL3   a1            Address of client access list                
QA_MEACI DS    XL1                 Media access list indicator                  
QA_AMEAC DS    AL3   a2            Address of client access list                
QA_EXACI DS    XL1                 Expend access list indicator                 
QA_AEXAC DS    AL3   a3            Address of expend access list                
QA_NCACI DS    XL1                 Non client access list indicator             
QA_ANCAC DS    AL3   a4            Address of non client access list            
QA_STACI DS    XL1                 Staff access list indicator                  
QA_ASTAC DS    AL3   a5            Address of staff access list                 
QA_WKACI DS    XL1                 Workcode access list indicator               
QA_AWKAC DS    AL3   a6            Address of workcode access list              
QA_RPACI DS    XL1                 Report access list indicator                 
QA_ARPAC DS    AL3   a7            Address of report access list                
QA_ESTAI DS    XL1                 Estimate access list indicator               
QA_AESTA DS    AL3   a8            Address of estimate access list              
QA_SUACI DS    XL1                 Supplier access list indicator               
QA_ASUAC DS    AL3   a9            Address of supplier access list              
QA_GRPLI DS    XL1                 Group list code indicator                    
QA_AGRPL DS    AL3   aa            Address of group list                        
*                                                                               
QA_GRPLC DS    CL8                 Group list code                              
QA_GLECA DS    CL1                 Global Estimate client approver              
QA_GLICA DS    CL1                 Global Internal Est. client approver         
QA_GLJA  DS    CL1                 Global Job approver                          
QA_GLEFA DS    CL1                 Global Expense Finance approver              
QA_OFFCO DS    CL2                 Office Code                                  
QA_CLICO DS    CL6                 Client Code                                  
QA_PROCO DS    CL6                 Product Code                                 
QA_JOBCO DS    CL6                 Job code                                     
QA_MEDCO DS    CL1                 Media Code                                   
QA_ESTAP DS    CL1                 Estimate Approver                            
QA_ESDFA DS    CL1                 Estimate Default Approver                    
QA_INTEA DS    CL1                 Internal Estimate Approver                   
QA_INEDA DS    CL1                 Internal Est. Default Approver               
QA_E1AB  DS    CL1                 Expense Lev 1 Approver Billable              
QA_E1ANB DS    CL1                 Expense Lev 1 Approver Non-billable          
QA_E2AB  DS    CL1                 Expense Lev 2 Approver Billable              
QA_E2ANB DS    CL1                 Expense Lev 2 Approver Nonn-billable         
QA_JOBAP DS    CL1                 Jobs Approver                                
QA_JOBDA DS    CL1                 Jobs Default Approver                        
QA_TIMAP DS    CL1                 Time Approver                                
QA_NCLCO DS    CL12                NON CLIENT CODE                              
QA_TIME  DS    CL1                 Time                                         
QA_DEPCO DS    CL6                 Dept code                                    
QA_SUBDC DS    CL6                 Sub-Dept code                                
QA_PERCO DS    CL8                 Person code                                  
QA_EXLMA DS    CL1                 Expenses Line Manager Approver               
QA_E2LMA DS    CL1                 Expenses Lev2 Line Manager Approver          
QA_EXFAP DS    CL1                 Expenses Finance Approver                    
QA_EXDFA DS    CL1                 Expenses Default Finance Approver            
QA_EXAPL DS    PL6                 Expense approval limit                       
QA_OA    DS    CL1                 Order approver                               
QA_OAD   DS    CL1                 Order default approver                       
QA_OF    DS    CL1                 Order finance approver                       
QA_OFD   DS    CL1                 Order finance default approver               
QA_IA    DS    CL1                 Invoice approver                             
QA_IAD   DS    CL1                 Invoice default approver                     
QA_IOTYP DS    CL1                 Invoice/Order type                           
IOTDEF   EQU   C'1'                Default                                      
IOTCLI   EQU   C'2'                Client                                       
IOTNCL   EQU   C'3'                Non-client                                   
IOTEXP   EQU   C'4'                Expense                                      
IOTPRO   EQU   C'5'                Production                                   
IOTART   EQU   C'6'                Artist                                       
IOTINT   EQU   C'7'                Internal                                     
QA_EXPCO DS    CL3                 Expenditure code                             
QA_SUPLC DS    CL1                 Supplier ledger code                         
QA_SUPAC DS    CL12                Supplier account code                        
QA_2PAC  DS    CL12                2P account code                              
QA_SLFAA DS    PL6                 Self Approval amount                         
QA_APPAM DS    PL6                 Approval amount                              
QA_BUPIN DS    XL2                 PIN of backup                                
QA_EXPEN DS    CL1                 Expense                                      
QA_ORDER DS    CL1                 Order                                        
QA_INVCE DS    CL1                 Invoice                                      
QA_ROLE  DS    CL2                 Role                                         
         ORG   QA_VALS                                                          
QA_ACTN  DS    CL1                 Action                                       
QA_ADDQ  EQU   1                   - Add                                        
QA_DELQ  EQU   2                   - Delete                                     
QA_UNIT  DS    CL1                 Unit                                         
QA_LEDG  DS    CL1                 Ledger                                       
QA_ACCT  DS    CL12                Account                                      
QA_INDX  DS    XL2                 Unique id to stop concurrent updates         
QA_APIN  DS    XL2                 Person binary PIN                            
         ORG                                                                    
QA_VALSL EQU   *-QA_VALS                                                        
                                                                                
OA_VALS  DS    0F                                                               
RECTYPE  DS    AL(L'RECTTYPE)      Record type                                  
OA_INDX  DS    XL2                 Unique id to stop concurrent updates         
OA_VALSL EQU   *-OA_VALS                                                        
                                                                                
DR_VALS  DS    0F                  Derived variables                            
AC_OFF   DS    CL2                 Office code                                  
DR_VALSL EQU   *-DR_VALS                                                        
                                                                                
ERRTAB   DS    XL500               Saved error messages and data                
* ERRTAB is currently limited by the record size you've asked DDLINKIO          
* to use (LIOBRECL or 1K default).                                              
SAVEL    EQU   *-SAVED                                                          
***********************************************************************         
* Error table                                                         *         
***********************************************************************         
                                                                                
ET_D     DSECT                                                                  
ET_EOTQ  EQU   FF                  End of table indiciator                      
ET_LN    DS    X                   Length of this entry                         
ET_ERRNO DS    XL(L'ROUERRV)       Error number                                 
ET_ROWNM DS    XL1                 Row number error applies                     
ET_LN1Q  EQU   *-ET_D                                                           
ET_EXTRA DS    0C                  Extra error text                             
                                                                                
***********************************************************************         
* TSAR Dsect                                                          *         
***********************************************************************         
                                                                                
TSAR_D   DSECT                     TSAR RECORD DSECT                            
TSARTYPE DS    XL1                 MAP TYPE                                     
TSARSEQ  DS    XL2                                                              
TSARKYLQ EQU   *-TSAR_D                                                         
TSARDATA DS    0X                  DATA                                         
TSB1_REC DS    0X                  Client approval rights                       
TSB1_OFF DS    XL2                                                              
TSB1_CLI DS    XL6                                                              
TSB1_PRO DS    XL6                                                              
TSB1_JBC DS    XL6                                                              
TSB1_MED DS    X                                                                
TSB1_EST DS    X                                                                
TSB1_ESD DS    X                                                                
TSB1_INA DS    X                                                                
TSB1_IDA DS    X                                                                
TSB1_X1B DS    X                                                                
TSB1_1NB DS    X                                                                
TSB1_X2B DS    X                                                                
TSB1_2NB DS    X                                                                
TSB1_JBA DS    X                                                                
TSB1_JDA DS    X                                                                
TSB1_TIM DS    X                                                                
         ORG   TSARDATA                                                         
TSB2_REC DS    0X                  Non-client approval                          
TSB2_NCC DS    XL12                                                             
TSB2_TIM DS    X                                                                
         ORG   TSARDATA                                                         
TSB3_REC DS    0X                  Staff approval                               
TSB3_OFF DS    XL2                                                              
TSB3_DEP DS    XL6                                                              
TSB3_SUB DS    XL6                                                              
TSB3_PER DS    XL8                                                              
TSB3_XMG DS    X                                                                
TSB3_XM2 DS    X                                                                
TSB3_FIN DS    X                                                                
TSB3_DFN DS    X                                                                
TSB3_TIM DS    X                                                                
TSB3_XMA DS    PL6                                                              
         ORG   TSARDATA                                                         
TSB4_REC DS    0X                  Invoice orders approval                      
TSB4_OA  DS    X                   Order approver                               
TSB4_OAD DS    X                   Default order approver                       
TSB4_OF  DS    X                   Order finance approver                       
TSB4_OFD DS    X                   Default order finance approver               
TSB4_IA  DS    X                   Invoice approver                             
TSB4_IAD DS    X                   Default invoice approver                     
TSB4_IOT DS    X                   Invoice /order type                          
TSB4_CLI DS    XL6                                                              
TSB4_PRO DS    XL6                                                              
TSB4_JBC DS    XL6                                                              
TSB4_OFF DS    XL2                                                              
TSB4_DEP DS    XL6                                                              
TSB4_EXP DS    XL3                                                              
TSB4_SLC DS    X                                                                
TSB4_SAC DS    XL12                                                             
TSB4_2PC DS    XL12                                                             
TSB4_MED DS    X                                                                
TSB4_DEF DS    X                                                                
TSB4_SLF DS    PL6                                                              
TSB4_APP DS    PL6                                                              
         ORG   TSARDATA                                                         
TSB5_REC DS    0X                  Back-up approval                             
TSB5_PIN DS    XL2                                                              
TSB5_TIM DS    XL1                                                              
TSB5_EXP DS    XL1                                                              
TSB5_ORD DS    XL1                                                              
TSB5_INV DS    XL1                                                              
         ORG   TSARDATA                                                         
TSB6_REC DS    0X                  Role                                         
TSB6_ROL DS    XL2                                                              
         ORG                                                                    
TSARLENQ EQU   *-TSAR_D                                                         
*                                                                               
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
         PRINT OFF                                                              
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACBRA2A   03/12/20'                                      
         END                                                                    
