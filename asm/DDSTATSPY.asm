*          DATA SET DDSTATSPY  AT LEVEL 068 AS OF 07/19/05                      
*PHASE STATSPYA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE TWANG                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE FATABOFF                                                               
         SPACE 2                                                                
         TITLE 'STATSPY - PRINT TWA''S OF HEAVY USERS   '                       
         PRINT NOGEN                                                            
STATSPY  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKX-WORKD,STATSPY*,=V(REGSAVE),RA,CLEAR=YES                    
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         L     R1,=A(TBLOCK-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,ATBLOCK                                                       
         L     R1,=A(ADRBUFF-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AADRBUFF                                                      
*                                                                               
         BAS   RE,INIT             INIT + READ CARDS                            
         BAS   RE,OPENFLS          OPEN FILES                                   
*                                                                               
         CLI   MODE,C'T'           TWA MODE                                     
         BNE   *+12                                                             
         BAS   RE,MAINTWA                                                       
         B     STATXXX                                                          
*                                                                               
         CLI   MODE,C'C'           CSV MODE                                     
         BE    STAT010                                                          
         CLI   MODE,C'A'           ADR MODE                                     
         BE    STAT010                                                          
         CLI   MODE,C'P'           PRINT MODE                                   
         BE    STAT010                                                          
         B     STATXXX                                                          
*                                                                               
STAT010  BAS   RE,MAINADR                                                       
         B     STATXXX                                                          
*                                                                               
STATXXX  BAS   RE,CLOSE            CLOSE FILES                                  
XBASE    XBASE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         LA    R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         L     R3,AADRBUFF                                                      
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT050                                                          
         MVC   PRL(80),0(R3)                                                    
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   ERR1                NEQ MEANS INVALID KEYWORD                    
         B     INIT010                                                          
*                                                                               
INIT050  CLI   INPUT,C'T'          TEST INPUT=TAPE                              
         BE    *+12                                                             
         CLI   INPUT,C'D'          TEST INPUT=DISK                              
         BNE   ERR2                INVALID PARAMETER                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        OPEN FILES                                         *                   
*************************************************************                   
         SPACE 1                                                                
OPENFLS  NTR1                                                                   
         CLI   INPUT,C'D'          OPEN ADRFILE FOR INPUT                       
         BNE   OPEN020                                                          
         L     R3,AADRBUFF                                                      
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,SERVICE,FILELIST,(R3)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
OPEN020  CLI   INPUT,C'T'          OPEN ADRTAPE FOR INPUT                       
         BNE   OPEN030                                                          
         OPEN  ADRIN                                                            
*                                                                               
OPEN030  CLI   MODE,C'T'           MODE = TWA                                   
         BE    OPEN040                                                          
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         B     OPEN050                                                          
*                                  INITIALISE SORTER FOR TWA                    
OPEN040  GOTO1 =V(SORTER),DMCB,SCARD1,SCARD2                                    
         B     OPENX                                                            
*                                  INITIALISE SORTER FOR TAPEOUT                
OPEN050  GOTO1 =V(SORTER),DMCB,SCARD3,SCARD4                                    
         B     OPENX                                                            
*                                                                               
OPENX    B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN TWA                                           *                   
*************************************************************                   
         SPACE 1                                                                
MAINTWA  NTR1                                                                   
         L     R3,AADRBUFF         R3=A(BUFFER)                                 
*                                                                               
MAINT10  BAS   RE,ADRGET           GET AN ADR RECORD                            
*                                                                               
         CLI   0(R3),X'FF'         TEST FOR EOF                                 
         BE    MAINT30                                                          
         CLC   0(4,R3),=C'**TW'    TEST FOR **TWA                               
         BE    MAINT20                                                          
         CLI   0(R3),C'*'          IGNORE OTHER SPECIALS                        
         BE    MAINT10                                                          
*                                                                               
         B     MAINT10             IGNORE NORMAL ADR RECS                       
*                                                                               
MAINT20  LA    R2,ADRHDR           PUT CPU INTO ADRHDR AND SORT                 
         MVC   0(4,R2),ADRSTTM-ADRREC+24(R2)                                    
         GOTO1 =V(SORTER),DMCB,PUT,(R2)                                         
         B     MAINT10                                                          
*                                                                               
MAINT30  LA    R1,TITLE2           PRINT TWA TITLE                              
         BAS   RE,PRINTT                                                        
*                                                                               
MAINT40  GOTO1 =V(SORTER),DMCB,GET GET TWAS BACK FROM SORT                      
         ICM   R3,15,4(R1)                                                      
         BZ    MAINT90             END OF FILE                                  
*                                                                               
         SR    R1,R1               PAGE EJECT ON NEW TWA                        
         BAS   RE,PRINTT                                                        
         BAS   RE,PRINTL                                                        
*                                                                               
         LA    R2,12(R3)           OUTPUT TRANSACTION DETAIL                    
         LR    R1,R2                                                            
         BAS   RE,PRINTTRN                                                      
         MVC   PRL(132),IOSORT                                                  
         BAS   RE,PRINTL                                                        
*                                                                               
         LA    R2,8(R2)                                                         
         CLC   9(2,R2),=X'0105'    TEST FOR SRTOP                               
         BNE   MAINT60                                                          
         LA    R2,44(R2)                                                        
         GOTO1 =V(PRNTBL),DMCB,(8,TWAT),(R2),C'DUMP',1900,=C'1D  ',0            
         BAS   RE,PRINTL                                                        
         B     MAINT40                                                          
*                                                                               
MAINT60  LA    R2,48(R2)                                                        
         GOTO1 =V(TWANG),DMCB,(R2),ATBLOCK                                      
*                                                                               
         MVC   PRL(82),BOXTOP      BOX IT                                       
         BAS   RE,PRINTL                                                        
         L     R2,ATBLOCK                                                       
         LA    R0,24                                                            
MAINT70  MVI   PRL+00,VBAR         BOX IT                                       
         MVC   PRL+1(80),0(R2)     PRINT TWA LINE                               
         MVI   PRL+81,VBAR         BOX IT                                       
         BAS   RE,PRINTL                                                        
         LA    R2,80(R2)                                                        
         BCT   R0,MAINT70                                                       
         MVC   PRL(82),BOXBOT      BOX IT                                       
         BAS   RE,PRINTL                                                        
         B     MAINT40             BACK FOR NEXT                                
*                                                                               
MAINT90  GOTO1 =V(SORTER),DMCB,END                                              
*                                                                               
MAINTWAX B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN ADR                                           *                   
*************************************************************                   
         SPACE 1                                                                
MAINADR  NTR1                                                                   
         XC    FULL,FULL                                                        
*                                                                               
MAINA10  L     R3,AADRBUFF         R3=A(BUFFER)                                 
         BAS   RE,ADRGET           GET AN ADR RECORD                            
*                                                                               
         CLI   0(R3),X'FF'         TEST FOR EOF                                 
         BE    MAINA30                                                          
         CLC   0(4,R3),=C'**TW'    IGNORE **TWA                                 
         BE    MAINA10                                                          
         CLI   0(R3),C'*'          IGNORE OTHER SPECIALS                        
         BE    MAINA10                                                          
*                                                                               
         LA    R2,100              100 ADR RECORDS                              
         MVC   IOL,=X'00480000'                                                 
MAINA20  CLI   0(R3),C'$'          IGNORE SPECIALS                              
         BE    MAINA21                                                          
         MVC   IOAREA(64),0(R3)                                                 
         MVC   IOSORT,12(R3)       SORT ON SIN                                  
         GOTO1 =V(SORTER),DMCB,PUT,IOL                                          
MAINA21  LA    R3,64(R3)                                                        
         BCT   R2,MAINA20                                                       
*                                                                               
         B     MAINA10                                                          
*                                                                               
MAINA30  XC    CPUT,CPUT                                                        
*                                                                               
MAINA40  GOTO1 =V(SORTER),DMCB,GET GET RECORDS BACK FROM SORT                   
         ICM   R3,15,4(R1)                                                      
         BZ    MAINAX              END OF FILE                                  
*                                                                               
         L     R1,X'34'(R3)        ACCUMULATE CPU                               
         A     R1,CPUT                                                          
         ST    R1,CPUT                                                          
*                                                                               
         CLI   MODE,C'C'           CONVERT TO CSV                               
         BNE   MAINA50                                                          
         LR    R1,R3                                                            
         BAS   RE,CONVERT                                                       
         LA    R3,IOL                                                           
*                                                                               
MAINA50  CLI   MODE,C'P'           CONVERT TO PRINTABLE                         
         BNE   MAINA90                                                          
         LR    R1,R3                                                            
         BAS   RE,PRINT                                                         
         LA    R3,IOL                                                           
*                                                                               
MAINA90  PUT   TAPEOUT,(R3)        PUT TO TAPEOUT                               
         B     MAINA40                                                          
*                                                                               
MAINAX   MVC   PRL,SPACES1                                                      
         MVC   PRL(10),=C'CPU TOTAL='                                           
         EDIT  (B4,CPUT),(10,PRL+10)                                            
         BAS   RE,PRINTL                                                        
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN CSV                                           *                   
*************************************************************                   
         SPACE 1                                                                
MAINCSV  NTR1                                                                   
         XC    FULL,FULL                                                        
*                                                                               
MAINC10  L     R3,AADRBUFF         R3=A(BUFFER)                                 
         BAS   RE,ADRGET           GET AN ADR RECORD                            
*                                                                               
         CLI   0(R3),X'FF'         TEST FOR EOF                                 
         BE    MAINC30                                                          
         CLC   0(4,R3),=C'**TW'    IGNORE **TWA                                 
         BE    MAINC10                                                          
         CLI   0(R3),C'*'          IGNORE OTHER SPECIALS                        
         BE    MAINC10                                                          
*                                                                               
         LA    R2,100              100 ADR RECORDS                              
         MVC   IOL,=X'00440000'                                                 
MAINC20  CLI   0(R3),C'$'          IGNORE SPECIALS                              
         BE    MAINC21                                                          
         MVC   ADRWORK(64),0(R3)                                                
         BAS   RE,CONVERT                                                       
         PUT   TAPEOUT,IOL         PUT TO TAPEOUT                               
MAINC21  LA    R3,64(R3)                                                        
         BCT   R2,MAINC20                                                       
*                                                                               
         B     MAINC10                                                          
*                                                                               
MAINC30  EQU   *                                                                
*                                                                               
MAINCX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CONVERT ADRFILE TO CSV - WORK TO IOAREA            *                   
*************************************************************                   
         SPACE 1                                                                
CONVERT  NTR1                                                                   
*                                                                               
         LR    R2,R1                                                            
         LA    R2,8(R2)                                                         
         USING ADRRECD,R2                                                       
         LA    R3,IOSORT                                                        
         MVC   0(8,R3),ADRSYM      LUIDLUID                                     
         MVI   8(R3),C','                                                       
         LA    R3,9(R3)                                                         
*                                                                               
         MVC   BYTE,ADROVSYS       OV SYS                                       
         BAS   RE,GTOVSYS                                                       
         MVC   0(2,R3),WORK                                                     
         MVI   2(R3),C','                                                       
         LA    R3,3(R3)                                                         
*                                                                               
         MVC   BYTE,ADRSYSNO       SE SYS                                       
         BAS   RE,GTSESYS                                                       
         MVC   0(4,R3),WORK                                                     
         MVI   4(R3),C','                                                       
         LA    R3,5(R3)                                                         
*                                                                               
         MVC   BYTE,ADRPRGNO       PROG                                         
         BAS   RE,GTPROG                                                        
         MVC   0(4,R3),WORK                                                     
         MVI   4(R3),C','                                                       
         LA    R3,5(R3)                                                         
*                                                                               
         MVC   0(1,R3),ADRTASK     TASK                                         
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
*                                                                               
         EDIT  (B4,ADRSIN),(7,0(R3)),ALIGN=LEFT                                 
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
*NOP     MVC   FULL,ADRINTM        IN TIME                                      
*NOP     BAS   RE,TIMEOUT                                                       
*NOP     MVC   0(11,R3),WORK1                                                   
*NOP     MVI   11(R3),C','                                                      
*NOP     LA    R3,12(R3)                                                        
*                                                                               
         ICM   RF,15,ADRINTM                                                    
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(7,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         ICM   RF,15,ADRSTTM       COMPUTE START-INTIME                         
         ICM   R1,15,ADRINTM                                                    
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
*NOP     MVC   FULL,ADRSTTM                                                     
*NOP     BAS   RE,TIMEOUT                                                       
*NOP     MVC   0(11,R3),WORK1                                                   
*NOP     MVI   11(R3),C','                                                      
*NOP     LA    R3,12(R3)                                                        
*                                                                               
         ICM   RF,15,ADRNDTM       COMPUTE END TIME-START TIME                  
         ICM   R1,15,ADRSTTM                                                    
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
*NOP     MVC   FULL,ADRNDTM                                                     
*NOP     BAS   RE,TIMEOUT                                                       
*NOP     MVC   0(11,R3),WORK1                                                   
*NOP     MVI   11(R3),C','                                                      
*NOP     LA    R3,12(R3)                                                        
*                                                                               
         L     RF,ADRCPUTM         CPU TIME IN 100TH                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
*NOP     MVC   FULL,ADRCPUTM                                                    
*NOP     BAS   RE,TIMEOUT                                                       
*NOP     MVC   0(11,R3),WORK1                                                   
*NOP     MVI   11(R3),C','                                                      
*NOP     LA    R3,12(R3)                                                        
*                                                                               
         EDIT  (B2,ADRMSGI),(5,0(R3)),ALIGN=LEFT                                
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (B2,ADRMSGO),(5,0(R3)),ALIGN=LEFT                                
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (B1,ADROVCNT),(3,0(R3)),ALIGN=LEFT                               
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (B3,ADRIOCNT),(6,0(R3)),ALIGN=LEFT                               
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,ADRFLAG1,0(R3),1                                 
         MVI   2(R3),C','                                                       
         LA    R3,3(R3)                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,ADRFLAG2,0(R3),1                                 
         MVI   2(R3),C','                                                       
         LA    R3,3(R3)                                                         
*                                                                               
         EDIT  (B4,ADRUDATA+2),(6,0(R3)),ALIGN=LEFT                             
         AR    R3,R0                                                            
*NOP     MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
                                                                                
*                                                                               
         LA    R1,IOL                                                           
         SR    R3,R1                                                            
         STH   R3,IOL                                                           
*                                                                               
CONVERTX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        CONVERT TO PRINTABLE OUTPUT                        *                   
*************************************************************                   
         SPACE 1                                                                
PRINT    NTR1                                                                   
*                                                                               
         MVC   IOSORT(132),SPACES1                                              
*                                                                               
         LR    R2,R1                                                            
         LA    R2,8(R2)                                                         
         USING ADRRECD,R2                                                       
         LA    R3,IOSORT                                                        
         MVC   0(8,R3),ADRSYM      LUIDLUID                                     
         LA    R3,9(R3)                                                         
*                                                                               
         MVC   BYTE,ADROVSYS       OV SYS                                       
         BAS   RE,GTOVSYS                                                       
         MVC   0(2,R3),WORK                                                     
         LA    R3,3(R3)                                                         
*                                                                               
         MVC   BYTE,ADRSYSNO       SE SYS                                       
         BAS   RE,GTSESYS                                                       
         MVC   0(4,R3),WORK                                                     
         LA    R3,5(R3)                                                         
*                                                                               
         MVC   BYTE,ADRPRGNO       PROG                                         
         BAS   RE,GTPROG                                                        
         MVC   0(4,R3),WORK                                                     
         LA    R3,5(R3)                                                         
*                                                                               
         MVC   0(1,R3),ADRTASK     TASK                                         
         LA    R3,2(R3)                                                         
*                                                                               
         EDIT  (B4,ADRSIN),(7,0(R3)),ZERO=NOBLANK                               
         LA    R3,8(R3)                                                         
*                                                                               
*NOP     MVC   FULL,ADRINTM        IN TIME                                      
*NOP     BAS   RE,TIMEOUT                                                       
*NOP     MVC   0(11,R3),WORK1                                                   
*NOP     MVI   11(R3),C','                                                      
*NOP     LA    R3,12(R3)                                                        
*                                                                               
         ICM   RF,15,ADRINTM                                                    
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(7,0(R3)),ZERO=NOBLANK                                      
         LA    R3,8(R3)                                                         
*                                                                               
         ICM   RF,15,ADRSTTM       COMPUTE START-INTIME                         
         ICM   R1,15,ADRINTM                                                    
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ZERO=NOBLANK                                      
         LA    R3,6(R3)                                                         
*                                                                               
*NOP     MVC   FULL,ADRSTTM                                                     
*NOP     BAS   RE,TIMEOUT                                                       
*NOP     MVC   0(11,R3),WORK1                                                   
*NOP     MVI   11(R3),C','                                                      
*NOP     LA    R3,12(R3)                                                        
*                                                                               
         ICM   RF,15,ADRNDTM       COMPUTE END TIME-START TIME                  
         ICM   R1,15,ADRSTTM                                                    
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ZERO=NOBLANK                                      
         LA    R3,6(R3)                                                         
*                                                                               
*NOP     MVC   FULL,ADRNDTM                                                     
*NOP     BAS   RE,TIMEOUT                                                       
*NOP     MVC   0(11,R3),WORK1                                                   
*NOP     MVI   11(R3),C','                                                      
*NOP     LA    R3,12(R3)                                                        
*                                                                               
         L     RF,ADRCPUTM         CPU TIME IN 100TH                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ZERO=NOBLANK                                      
         LA    R3,6(R3)                                                         
*                                                                               
*NOP     MVC   FULL,ADRCPUTM                                                    
*NOP     BAS   RE,TIMEOUT                                                       
*NOP     MVC   0(11,R3),WORK1                                                   
*NOP     MVI   11(R3),C','                                                      
*NOP     LA    R3,12(R3)                                                        
*                                                                               
         EDIT  (B2,ADRMSGI),(5,0(R3)),ZERO=NOBLANK                              
         LA    R3,6(R3)                                                         
*                                                                               
         EDIT  (B2,ADRMSGO),(5,0(R3)),ZERO=NOBLANK                              
         LA    R3,6(R3)                                                         
*                                                                               
         EDIT  (B1,ADROVCNT),(3,0(R3)),ZERO=NOBLANK                             
         LA    R3,4(R3)                                                         
*                                                                               
         EDIT  (B3,ADRIOCNT),(6,0(R3)),ZERO=NOBLANK                             
         LA    R3,7(R3)                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,ADRFLAG1,0(R3),1                                 
         LA    R3,3(R3)                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,ADRFLAG2,0(R3),1                                 
         LA    R3,3(R3)                                                         
*                                                                               
         EDIT  (B4,ADRUDATA+2),(6,0(R3)),ZERO=NOBLANK                           
         LA    R3,7(R3)                                                         
                                                                                
*                                                                               
         L     RF,ADRCPUTM         CPU TIME IN 38400                            
         EDIT  (RF),(6,0(R3)),ZERO=NOBLANK                                      
         LA    R3,7(R3)                                                         
*                                                                               
         ICM   RF,15,ADRCPUTM                                                   
         SR    RE,RE                                                            
         M     RE,=F'1000'                                                      
         D     RE,=F'384'          DIVIDE BY 384                                
         EDIT  (RF),(6,0(R3)),ZERO=NOBLANK                                      
         LA    R3,7(R3)                                                         
*                                                                               
         LA    R1,IOL                                                           
         SR    R3,R1                                                            
         STH   R3,IOL                                                           
*                                                                               
PRINTX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        SHORT READABLE PRINTABLE OUTPUT                    *                   
*************************************************************                   
         SPACE 1                                                                
PRINTTRN NTR1                                                                   
*                                                                               
         MVC   IOSORT(132),SPACES1                                              
*                                                                               
         LR    R2,R1                                                            
         LA    R2,8(R2)                                                         
         USING ADRRECD,R2                                                       
         LA    R3,IOSORT                                                        
         MVC   0(8,R3),ADRSYM      LUIDLUID                                     
         LA    R3,9(R3)                                                         
*                                                                               
         MVC   BYTE,ADRSYSNO       SE SYS                                       
         BAS   RE,GTSESYS                                                       
         MVC   0(4,R3),WORK                                                     
         MVI   4(R3),C'/'                                                       
         LA    R3,5(R3)                                                         
*                                                                               
         MVC   BYTE,ADRPRGNO       PROG                                         
         BAS   RE,GTPROG                                                        
         MVC   0(4,R3),WORK                                                     
         LA    R3,5(R3)                                                         
*                                                                               
         MVC   FULL,ADRINTM        IN TIME                                      
         BAS   RE,TIMEOUT                                                       
         MVC   0(11,R3),WORK1                                                   
         MVI   11(R3),C' '                                                      
         LA    R3,12(R3)                                                        
*                                                                               
         MVC   0(3,R3),=C'ET='                                                  
         LA    R3,3(R3)                                                         
         ICM   RF,15,ADRNDTM       COMPUTE END TIME-START TIME                  
         ICM   R1,15,ADRSTTM                                                    
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ZERO=NOBLANK,ALIGN=LEFT                           
         LA    R3,6(R3)                                                         
*                                                                               
         MVC   0(4,R3),=C'CPU='                                                 
         LA    R3,4(R3)                                                         
         L     RF,ADRCPUTM         CPU TIME IN 100TH                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ZERO=NOBLANK,ALIGN=LEFT                           
         LA    R3,6(R3)                                                         
*                                                                               
         MVC   0(3,R3),=C'IO='                                                  
         LA    R3,3(R3)                                                         
         EDIT  (B3,ADRIOCNT),(6,0(R3)),ZERO=NOBLANK,ALIGN=LEFT                  
         LA    R3,7(R3)                                                         
*                                                                               
         MVC   0(5,R3),=C'REAL='                                                
         LA    R3,5(R3)                                                         
         EDIT  (B4,ADRUDATA+2),(6,0(R3)),ZERO=NOBLANK,ALIGN=LEFT                
         LA    R3,7(R3)                                                         
*                                                                               
         MVC   0(2,R3),=C'Q='                                                   
         LA    R3,2(R3)                                                         
         ICM   RF,15,ADRSTTM       COMPUTE START-INTIME                         
         ICM   R1,15,ADRINTM                                                    
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(5,0(R3)),ZERO=NOBLANK,ALIGN=LEFT                           
         LA    R3,6(R3)                                                         
*                                                                               
         MVC   0(3,R3),=C'OV='                                                  
         LA    R3,3(R3)                                                         
         MVC   BYTE,ADROVSYS       OV SYS                                       
         BAS   RE,GTOVSYS                                                       
         MVC   0(2,R3),WORK                                                     
         LA    R3,3(R3)                                                         
*                                                                               
         MVC   0(2,R3),=C'S='                                                   
         LA    R3,2(R3)                                                         
         EDIT  (B4,ADRSIN),(7,0(R3)),ZERO=NOBLANK,ALIGN=LEFT                    
         LA    R3,8(R3)                                                         
*                                                                               
         MVC   0(2,R3),=C'T=#'                                                  
         LA    R3,3(R3)                                                         
         MVC   0(1,R3),ADRTASK     TASK                                         
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R1,IOL                                                           
         SR    R3,R1                                                            
         STH   R3,IOL                                                           
*                                                                               
PRINTTX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        CLOSE                                              *                   
*************************************************************                   
         SPACE 1                                                                
CLOSE    NTR1                                                                   
         CLI   INPUT,C'T'          CLOSE TAPEIN IF USED                         
         BNE   CLOSEX                                                           
         CLOSE ADRIN                                                            
CLOSEX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        ROUTINES FOR READING ADR RECORDS                   *                   
*************************************************************                   
         SPACE 1                                                                
ADRGET   NTR1                                                                   
         L     R3,AADRBUFF                                                      
*                                                                               
         CLI   INPUT,C'D'          TEST DISK INPUT                              
         BNE   AGET020                                                          
         OC    ADRADR,ADRADR       TEST FIRST TIME                              
         BNZ   *+10                                                             
         MVC   ADRADR,=X'00000000'                                              
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,ADRFILE,ADRADR,(R3)                      
         TM    8(R1),X'80'                                                      
         BO    ADREND              END-OF-FILE                                  
         CLI   8(R1),0                                                          
         BE    AGET050             GOT A RECORD                                 
         DC    H'0'                                                             
*                                                                               
AGET020  CLI   INPUT,C'T'          TEST TAPE INPUT                              
         BNE   AGET030                                                          
*                                                                               
         GET   ADRIN,(R3)          GET TAPE RECORD                              
         B     AGET050                                                          
*                                                                               
AGET030  DC    H'0'                INPUT=(UNKNOWN)                              
*                                                                               
AGET050  B     EXIT                EXIT REC IN ADRBUFF                          
*                                                                               
ADREND   MVC   0(16,R3),FFS        EXIT FFS IN ADRBUFF                          
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT TRANSACTION DETAIL                           *                   
*************************************************************                   
         SPACE 1                                                                
TRANSOUT NTR1                                                                   
         LR    R2,R1                                                            
         MVC   PRL(8),0(R2)                                                     
*                                                                               
         MVC   BYTE,8(R2)          PRINT SYSTEM NAME                            
         BAS   RE,GTOVSYS                                                       
         MVC   PRL+10(3),WORK                                                   
*                                                                               
         MVC   BYTE,9(R2)          PRINT SE NAME                                
         BAS   RE,GTSESYS                                                       
         MVC   PRL+15(7),WORK                                                   
*                                                                               
         MVC   BYTE,10(R2)         PRINT PROG NAME                              
         BAS   RE,GTPROG                                                        
         MVC   PRL+24(7),WORK                                                   
*                                                                               
         MVC   FULL,16(R2)         END TIME                                     
         L     RF,20(R2)                                                        
         S     RF,FULL             RF=ELAPSED TIME                              
         SR    RE,RE                                                            
         D     RE,=F'6000'         RF=MINS RE=SECS*100                          
*                                                                               
         MVC   PRL+34(3),=C'ET='                                                
         EDIT  (RF),(3,PRL+37),ZERO=NOBLANK,TRAIL=C'm'                          
         EDIT  (RE),(6,PRL+41),2,ZERO=NOBLANK,TRAIL=C's'                        
*                                                                               
         MVC   PRL+48(4),=C'from'                                               
*                                                                               
         MVC   FULL,16(R2)                                                      
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+53(8),WORK1                                                  
         MVC   PRL+62(2),=C'to'                                                 
         MVC   FULL,20(R2)                                                      
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+65(8),WORK1                                                  
*                                                                               
         MVC   PRL+76(4),=C'CPU='                                               
         EDIT  (B4,24(R2)),(6,PRL+80),2,ALIGN=LEFT                              
         MVC   PRL+88(4),=C'I/O='                                               
         EDIT  (B2,34(R2)),(6,PRL+92),ALIGN=LEFT                                
*                                                                               
         MVC   PRL+99(4),=C'SIN='                                               
         GOTO1 =V(HEXOUT),DMCB,12(R2),PRL+103,4                                 
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINTER ROUTINES                                   *                   
*************************************************************                   
         SPACE 1                                                                
PRINTT   ST    RE,SAVERE           SET TITLES AND FORCE PAGE THROW              
         LTR   RF,R1                                                            
         L     R1,=V(CPRINT)                                                    
         USING DPRINT,R1                                                        
         BZ    PRINTT1             IF R1 WAS ZERO JUST FORCE NEW PAGE           
         MVC   TITLE,SPACES        CLEAR TITLE TO SPACES                        
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              EX TITLES INTO TITLE                         
         B     *+10                                                             
         MVC   TITLE(0),1(RF)                                                   
PRINTT1  ZAP   LINE,=P'99'         ZAP LINE TO FORCE NEW PAGE                   
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         SPACE 1                                                                
PRINTL   ST    RE,SAVERE           PRINT A LINE                                 
         L     R1,=V(CPRINT)                                                    
         MVC   P,PRL                                                            
         GOTO1 =V(PRINTER)         GOTO PRINTER                                 
         MVC   PRL,SPACES1                                                      
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         DROP  R1                                                               
         EJECT                                                                  
*************************************************************                   
*        MISC SUBROUTINES                                   *                   
*************************************************************                   
         SPACE 1                                                                
GTOVSYS  ST    RE,SAVERE           GET SYS NAME INTO WORK                       
         SR    R1,R1                                                            
         LA    R1,SYSLST                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   0(1,R1),BYTE        SYSTEM NUMBER IN BYTE                        
         BE    GTOV010                                                          
         BXLE  R1,RE,*-10                                                       
         GOTO1 =V(HEXOUT),DMCB,BYTE,WORK,1                                      
         B     *+10                                                             
GTOV010  MVC   WORK(8),9(R1)       OV SYSTEM                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GTSESYS  ST    RE,SAVERE           GET SE NAME INTO WORK                        
         SR    R1,R1               AND A(PGMS) TAB INTO FULL                    
         L     R1,=V(SELIST)                                                    
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   7(1,R1),BYTE        SE NUMBER IN BYTE                            
         BE    GTSE010                                                          
         BXLE  R1,RE,*-10                                                       
         GOTO1 =V(HEXOUT),DMCB,BYTE,WORK,1                                      
         B     *+10                                                             
GTSE010  MVC   WORK(8),0(R1)       SE SYSTEM                                    
         MVC   FULL,24(R1)         A(PROG TABLE)                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GTPROG   ST    RE,SAVERE           GET PROGNAME INTO WORK                       
         L     R1,FULL             FULL MUST BE A(PGMS)                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   8(1,R1),BYTE        BYTE=PROG NUMBER                             
         BE    GTP010                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   FULL(1),BYTE                                                     
         MVC   WORK(4),=C'P=??'                                                 
         GOTO1 =V(HEXOUT),DMCB,FULL,WORK+2,1                                    
         B     *+10                                                             
GTP010   MVC   WORK,0(R1)          PROGRAM                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***************************************************                             
*        TIME EDIT SUBROUTINE FULL TO WORK1       *                             
***************************************************                             
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,TUHOUR                                                        
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUMINUTE                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUSECOND                                                      
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
TUHOUR   DC    F'138240000'        60*60*38400                                  
TUMINUTE DC    F'2304000'          60*38400                                     
TUSECOND DC    F'38400'            38400                                        
TUMSEC   DC    F'384'              384                                          
         EJECT                                                                  
*************************************************************                   
*        ALLOCATE STORAGE AREA                              *                   
*        ENTRY R1=LENGTH OF AREA IN K.                      *                   
*************************************************************                   
         SPACE 1                                                                
GETMAIN  ST    RE,SAVERE                                                        
*                                                                               
         SLL   R1,10               MULTIPLY R1 BY 1K                            
         LR    R0,R1                                                            
         GETMAIN RU,LV=(0),LOC=(BELOW,ANY)                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        ERRORS                                             *                   
*************************************************************                   
         SPACE 1                                                                
ERR1     MVC   PRL(32),=C'PARAMETER CARD - INVALID KEYWORD'                     
         B     ERRX                                                             
ERR2     MVC   PRL(34),=C'PARAMETER CARD - INVALID PARAMETER'                   
         B     ERRX                                                             
ERRX     L     RD,SAVERD                                                        
         BAS   RE,PRINTL                                                        
         B     XBASE                                                            
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1),AL1(OP LEN-1),AL3(OUTPUT)               
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'INPUT  ',AL1(4),AL1(0),AL3(INPUT)                              
         DC    C'MODE   ',AL1(3),AL1(0),AL3(MODE)                               
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
INPUT    DC    C'D'                INPUT=DISK/TAPE                              
MODE     DC    C'T'                MODE=TWA/ADR                                 
         EJECT                                                                  
VALCARD  NTR1                                                                   
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,FULL             SAVE LAST CHR ADDR IN FULL                   
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    VALCEXIT                                                         
VALC001  LA    R4,CARDTAB                                                       
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,12(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     VALCERR             ERROR IF ALL DONE                            
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
         CLI   0(R2),C'='                                                       
         BNE   VALCERR                                                          
         IC    R1,8(R4)            GET LEN FOR MOVE                             
         SR    RF,RF                                                            
         ICM   RF,7,9(R4)          GET ADDRESS FOR MOVE                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC030  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,FULL             TEST FOR END OF CARD                         
         BL    VALC030                                                          
*                                                                               
VALCEXIT CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
VALCERR  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
TITLE1   DC    AL1(L'TITLE1T)                                                   
TITLE1T  DC    C'PARAMETER CARDS'                                               
TITLE2   DC    AL1(L'TITLE2T)                                                   
TITLE2T  DC    C'TWA''S OF HEAVY CPU USERS'                                     
*                                                                               
BOXTOP   DC    X'4B',80X'60',X'4B'                                              
VBAR     EQU   X'4F'                                                            
BOXBOT   DC    X'4B',80X'60',X'4B'                                              
*                                                                               
*OXTOP   DC    X'AC',80X'BF',X'BC'                                              
*BAR     EQU   X'FA'                                                            
*OXBOT   DC    X'AB',80X'BF',X'BB'                                              
*                                                                               
ADRIN    DCB   DDNAME=ADRIN,DSORG=PS,MACRF=(GM),EODAD=ADREND,          X        
               BLKSIZE=9500                                                     
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
*                                                                               
SCARD1   DC    C'SORT FIELDS=(1,4,BI,A) '                                       
SCARD2   DC    C'RECORD TYPE=F,LENGTH=1908 '                                    
SCARD3   DC    C'SORT FIELDS=(5,4,BI,A) '                                       
SCARD4   DC    C'RECORD TYPE=V,LENGTH=4004 '                                    
GET      DC    C'GET '                                                          
PUT      DC    C'PUT '                                                          
END      DC    C'END '                                                          
*                                                                               
SPACES1  DC    CL132' '                                                         
FFS      DC    16X'FF'                                                          
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMRDIR   DC    CL8'DMRDIR'                                                      
GETREC   DC    CL8'GETREC'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
ADRFILE  DC    CL8'ADRFILE'                                                     
DMOPEN   DC    CL8'DMOPEN'                                                      
STATS    DC    CL8'STATS'                                                       
TWAT     DC    CL8'TWA DUMP'                                                    
*                                                                               
SERVICE  DC    CL8'SERVICE'                                                     
FILELIST DC    C'NADRFIL X'                                                     
       ++INCLUDE FASYSLST                                                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
DMCB     DS    6F                                                               
TEMP     DS    CL80                                                             
WORK     DS    CL80                                                             
WORK1    DS    CL80                                                             
ADRWORK  DS    CL80                                                             
PRL      DS    CL132                                                            
*                                                                               
ACTION   DS    CL8                                                              
*                                                                               
CPUT     DS    F                                                                
*                                                                               
ADRADR   DS    F                                                                
SAVERE   DS    A                                                                
SAVERD   DS    A                                                                
*                                                                               
IOL      DS    F                                                                
IOSORT   DS    F                                                                
IOAREA   DS    1000C                                                            
*                                                                               
ATBLOCK  DS    A                                                                
AADRBUFF DS    A                                                                
ABUFFER  DS    A                                                                
*                                                                               
ADRHDR   DS    CL4                 SORT HEADER                                  
ADRBUFF  DS    6144C               ADR BLOCK                                    
TBLOCK   DS    0D                                                               
         DS    1920C                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
SSB      CSECT                                                                  
         DS    0D                                                               
         DC    X'00000002'                                                      
         SPACE 2                                                                
UTL      CSECT                                                                  
         DS    0D                                                               
         DC    F'0',X'01'                                                       
         SPACE 2                                                                
* FAADRREC                                                                      
       ++INCLUDE FAADRREC                                                       
*                                                                               
         ORG   ADRRECD             INTERNAL VERSION OF 1ST 16 BYTES             
ADRTYPE  DS    C                                                                
ADRTYPE1 DS    C                                                                
ADRNAME1 DS    CL7                                                              
ADRNAME2 DS    CL7                                                              
         EJECT                                                                  
       ++INCLUDE DDSTATREC                                                      
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068DDSTATSPY 07/19/05'                                      
         END                                                                    
