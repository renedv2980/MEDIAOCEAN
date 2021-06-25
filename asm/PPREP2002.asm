*          DATA SET PPREP2002  AT LEVEL 030 AS OF 05/01/02                      
*PHASE PP2002C,+0                  **** NOTE "C" PHASE                          
         TITLE 'PP2002  INVOICE CHECKING LIST'                                  
* CHANGE LOG                                                                    
* BPLA 5/9/91   ADD GNOPT AND SET IT FROM PAGYPROF OR A0 PROFILE                
*                                                                               
* SMYE 12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
PP2002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP2002                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LR    R5,RC                                                            
         A     R5,=F'4096'                                                      
         USING PPFILED,RC,R5                                                    
         LA    R9,SPACEND                                                       
         USING PP20WRKD,R9                                                      
         EJECT                                                                  
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BNE   BRKS                                                             
         SPACE 2                                                                
*                                                                               
*                                  TEST IF BUY PAID (P) OR UNPAID (U)           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
         BAS   R8,NEXTEL                                                        
         BNE   CMPGRS                                                           
         OC    2(3,R2),2(R2)                                                    
         BNZ   CMPGRS                                                           
         MVI   PAYSW,C'U'                                                       
         B     DTES                                                             
*                                                                               
*                                       COMPARE GROSS & PAID GROSS              
CMPGRS   MVI   PAYSW,C'P'                                                       
         CLC   GROSS,PGROSS                                                     
         BE    DTES                                                             
         MVI   PAYSW,C'U'                                                       
         SPACE 2                                                                
*                                                                               
*                                  COMPARE DATES                                
DTES     CLC   PBUYKEY+16(3),BSTART                                             
         BNL   DTES02                                                           
         CLI   PAYSW,C'U'                                                       
         BNE   PP20EX                                                           
         MVI   PUSW,C'Y'                PREVIOUSLY UNPAID                       
         B     PP20EX                                                           
DTES02   CLC   PBUYKEY+16(3),BEND                                               
         BNH   PROC                                                             
         CLI   PAYSW,C'P'                                                       
         BNE   PP20EX                                                           
         MVI   SPSW,C'Y'                SUBSEQUENTLY PAID                       
         B     PP20EX                                                           
         EJECT                                                                  
*                                                                               
PROC     MVI   BUYSW,C'Y'                                                       
         CLI   PAYSW,C'P'                                                       
         BE    PP20EX                                                           
*                                  BUY IS WITHIN REQUEST DATES                  
*                                       & UNPAID - SET UP PRINT LINE            
*                                            INSERT DATE                        
*                                            EST-LINE                           
*                                        (N) LINES & RATE                       
*                                        (M) SPACE & PAYABLE DATE               
*                                            PREMIUM                            
*                                            GROSS                              
*                                            GROSS LESS COMM                    
*                                            CASH DISC                          
*                                            NET PAYABLE                        
         MVI   PRNTSW,C'Y'                                                      
*                                                                               
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(4,P)                                    
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(7,P)                                   
*                                                                               
         CLI   PBUYKLIN,X'01'                                                   
         BNH   PROC02                                                           
         EDIT  (1,PBUYKLIN),(2,P+6),FILL=0                                      
         MVI   P+5,C'-'                                                         
*                                                                               
PROC02   EDIT  (2,PBUYKEST),(3,P+13),FILL=0                                     
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         MVC   PBYODTCN,DATCON                                                  
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
*                                                                               
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   PROC8                                                            
         MVC   P+17(20),PBYOSPC                                                 
         MVC   PSECOND+17(20),PBYOSPC2                                          
         CLI   PBYOSPC,C' '        TEST SPACE BUY                               
         BH    PROC6               YES                                          
         MVC   P+17(7),PBYOUNTS                                                 
PROC6    MVC   P+27(8),PBYOUR    OK - SINCE NEWS SPACES ONLY 7 CHARS            
         DS    0H                ALSO SOME SPACES HAVE RATES                    
         MVC   P+37(11),PBYOPRM                                                 
         B     PROC22                                                           
*                                                                               
PROC8    DS    0H                                                               
*                                                                               
         MVC   P+17(20),PBYOSPC                                                 
         MVC   PSECOND+17(20),PBYOSPC2                                          
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(5,P+37)                                
         DROP  R6                                                               
*                                                                               
*                                                                               
PROC22   L     R2,GROSS                                                         
         S     R2,PGROSS                                                        
         LA    R3,P+56                                                          
         LR    R4,R2                                                            
         A     R4,PUBTOTS                                                       
         ST    R4,PUBTOTS                                                       
         BAS   R8,EDT                                                           
         A     R2,PAGYCOM                                                       
         S     R2,AGYCOM                                                        
         LA    R3,P+69                                                          
         BAS   R8,EDT                                                           
         A     R2,PUBTOTS+4                                                     
         ST    R2,PUBTOTS+4                                                     
         L     R2,CSHDSC                                                        
         S     R2,PCSHDSC                                                       
         LA    R3,P+82                                                          
         BAS   R8,EDT                                                           
         A     R2,PUBTOTS+8                                                     
         ST    R2,PUBTOTS+8                                                     
         L     R2,PYABLE                                                        
         S     R2,PAID                                                          
         LA    R3,P+95                                                          
         BAS   R8,EDT                                                           
         A     R2,PUBTOTS+12                                                    
         ST    R2,PUBTOTS+12                                                    
*                                                                               
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,REPT                                                          
*                                                                               
*                                                                               
PP20EX   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*                                  EDIT                                         
*                                                                               
EDT      EDIT  (R2),(13,(R3)),2,MINUS=YES,COMMAS=YES                            
         BR    R8                                                               
         SPACE 3                                                                
*                                                                               
*                                  REPORT PRINT                                 
REPT     NTR                                                                    
         MVC   HEAD6+9(89),STPUB                                                
         CLI   QOPT7,C'T'                                                       
         BNE   REPT01                                                           
         MVC   HEAD1+62(3),=C'-TA'                                              
*                                                                               
*                                  PRODUCT                                      
REPT01   MVC   HEAD4(9),=C'PRODUCT  '                                           
         CLC   QPRODUCT(3),=C'ALL'                                              
         BNE   REPT01A                                                          
         MVC   HEAD4+9(16),=C'ALL ALL PRODUCTS'                                 
         MVC   HEAD12+14(4),=C'PRD-'                                            
         MVC   HEAD13+14(4),=C'----'                                            
         B     REPT02                                                           
REPT01A  MVC   HEAD4+9(3),PPRDKPRD                                              
         MVC   HEAD4+13(20),PPRDNAME                                            
*                                  DIVISIONS                                    
REPT02   CLC   QDIV(3),=3C' '                                                   
         BE    REPT04                                                           
         MVC   HEAD3(8),=C'DIVISION'                                            
         MVC   HEAD3+11(3),PDIVKDIV                                             
         MVC   HEAD3+15(20),PDIVNAME                                            
*                                  REGIONS                                      
REPT04   CLI   FORCEMID,C'Y'                                                    
         BNE   REPT08                                                           
         CLC   QREGION(3),=3C' '                                                
         BE    REPT06                                                           
         MVC   MID1(6),=C'REGION'                                               
         MVC   MID1+8(3),PREGKREG                                               
         MVC   MID1+12(20),PREGNAME                                             
*                                  DISTRICTS                                    
REPT06   CLC   QDIST(3),=3C' '                                                  
         BE    REPT08                                                           
         LA    R2,MID2                                                          
         CLI   MID1,C' '                                                        
         BNE   *+8                                                              
         LA    R2,MID1                                                          
         MVC   0(8,R2),=C'DISTRICT'                                             
         MVC   11(3,R2),PDSTKDST                                                
         MVC   15(20,R2),PDSTNAME                                               
*                                                                               
REPT08   GOTO1 REPORT                                                           
*                                                                               
         XIT                                                                    
         SPACE 3                                                                
*                                  GET NEXT BUY ELEMENT                         
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BCR   8,R8                                                             
         CLI   0(R2),0                                                          
         BNE   *-18                                                             
         LTR   R2,R2                                                            
         BR    R8                                                               
         SPACE 2                                                                
*                                  ROUTINE TO ELIMINATE RIGHT-MOST              
*                                       BLANKS.  AT END R7 POINTS TO            
*                                       LAST NON-BL ANK POSITION                
ESFLOAT  OI    0(R7),C' '                                                       
         CLI   0(R7),C' '                                                       
         BNE   EFEX                                                             
         BCT   R7,ESFLOAT                                                       
EFEX     BR    RE                                                               
           EJECT                                                                
*                                                                               
*                                  BREAK ROUTINES                               
*                                                                               
*              FIRST BUY FOR A REQUEST                                          
BRKS     CLI   MODE,FBUYREQ                                                     
         BNE   BRKS02                                                           
         MVI   RCSUBPRG,0                                                       
         CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*        GOTO1 DTCNV,(R1),QEND,(1,BEND)                                         
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
         XC    PUBTOTS(22),PUBTOTS                                              
         XC    P(132),P                                                         
         XC    PSECOND(132),PSECOND                                             
         MVC   PAGE,=X'0001'                                                    
         B     PP20EX                                                           
         EJECT                                                                  
BRKS02   CLI   MODE,CLIFRST           FIRST BUY FOR CLIENT                      
         BNE   BRKS04                                                           
         MVC   GNOPT,PAGYPROF         SET GNOPT FROM AGY PROFILE                
*                                     READ A0 PROFILE                           
*                                     FOR GROSS/NET PAY OPTION                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P0A0'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),QCLIENT                                                
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,A0PROF,DATAMGR                                 
         OC    A0PROF,A0PROF                                                    
         BZ    *+10                                                             
         MVC   GNOPT,A0PROF                                                     
         B     PP20EX                                                           
         EJECT                                                                  
*                                                                               
*              FIRST BUY FOR A PUB                                              
BRKS04   CLI   MODE,FBUYPUB                                                     
         BNE   BRKS06                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
         MVI   PUBACT,C'Y'         SET PUB ACTIVITY                             
*                                  SET UP PUB NAME, ZONE, CITY, STATE           
*                                       AND PUB NO IN WORK AREA                 
         XC    PUBTOTS(21),PUBTOTS                                              
         XC    STPUB(89),STPUB                                                  
         LA    R7,STPUB                                                         
         MVC   0(20,R7),PUBNAME                                                 
         LA    R7,20(R7)                                                        
         BAS   RE,ESFLOAT                                                       
         MVI   2(R7),C'-'                                                       
         MVC   4(20,R7),PUBZNAME                                                
         LA    R7,24(R7)                                                        
         BAS   RE,ESFLOAT                                                       
         SPACE 2                                                                
         CLI   QMEDIA,C'N'                                                      
         BNE   PUBNO                                                            
         MVI   2(R7),C'-'                                                       
         MVC   4(16,R7),PUBCITY                                                 
         LA    R7,20(R7)                                                        
         BAS   RE,ESFLOAT                                                       
         MVI   1(R7),C','                                                       
         MVC   2(2,R7),PUBSTATE                                                 
         LA    R7,3(R7)                                                         
         SPACE 2                                                                
PUBNO    MVI   2(R7),C'-'                                                       
         IC    R4,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R4),PBUYKPUB),4(R7)                               
         B     PP20EX                                                           
         SPACE 2                                                                
*              LAST BUY FOR A TURNAROUND REQUEST (21)                           
BRKS06   CLI   MODE,LBUYREQ                                                     
         BNE   BRKS08                                                           
         CLI   QOPT7,C'T'                                                       
         BNE   PP20EX                                                           
         CLI   BUYSW,C'Y'                                                       
         BE    PP20EX                                                           
         MVC   P+10(13),=C'NO BUYS FOUND'                                       
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,REPT                                                          
         B     PP20EX                                                           
         SPACE 2                                                                
*              LAST BUY FOR A PUB                                               
BRKS08   CLI   MODE,LBUYPUB                                                     
         BNE   PP20EX                                                           
*                                                                               
*                                  PUBLICATION BREAK                            
*                                       PRINT TOTALS                            
*                                                                               
         CLI   PUBACT,C'Y'                                                      
         BNE   BRKSEX                                                           
         CLI   PRNTSW,C'Y'                                                      
         BE    BK8A                                                             
         MVC   P+10(17),=C'ALL BUYS ARE PAID'                                   
         BAS   RE,REPT                                                          
         B     BRKSEX                                                           
BK8A     MVC   P+48(5),=C'TOTAL'                                                
         LA    R6,PUBTOTS                                                       
         LA    R3,P+56                                                          
         LA    R4,4                                                             
BK8C     L     R2,0(R6)                                                         
         BAS   R8,EDT                                                           
         LA    R6,4(R6)                                                         
         LA    R3,13(R3)                                                        
         BCT   R4,BK8C                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPT                                                          
*                                                                               
*                                                                               
*                                  IF NOT TURNAROUND  -  DO NOT                 
*                                       PRINT INVOICE TOTALS AND                
*                                       DIFFERENCES.                            
         CLC   QOPT7,C'T'                                                       
         BE    BRKSEX                                                           
         CLC   QPROG,=C'21'                                                     
         BNE   BRKSEX                                                           
         MVC   P+40(13),=C'INVOICE TOTAL'                                       
         LA    R2,P+56                                                          
         L     R4,PUBTOTS                                                       
         CLI   GNOPT,C'G'                                                       
         BE    *+12                                                             
         L     R4,PUBTOTS+4                                                     
         LA    R2,P+69                                                          
         EDIT  (4,QPAY),(13,(R2)),2,MINUS=YES,COMMAS=YES                        
         BAS   RE,REPT                                                          
*                                                                               
*                                                                               
         MVC   P+43(10),=C'DIFFERENCE'                                          
         S     R4,QPAY                                                          
         EDIT  (R4),(13,(R2)),2,MINUS=YES,COMMAS=YES                            
         BAS   RE,REPT                                                          
*                                                                               
*                                                                               
BRKSEX   MVI   PUBACT,0                                                         
         B     PP20EX                                                           
         EJECT                                                                  
         LTORG                                                                  
         DC    2500X'00'        JUST TO MAKE PHASE BIGGER                       
         SPACE 2                                                                
*                                                                               
*                                                                               
       ++INCLUDE PP20WRKD                                                       
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE PPNEWFILE                                                      
           EJECT                                                                
       ++INCLUDE PPREPWORK                                                      
           EJECT                                                                
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030PPREP2002 05/01/02'                                      
         END                                                                    
