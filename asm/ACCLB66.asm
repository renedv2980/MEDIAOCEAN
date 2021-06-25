*          DATA SET ACCLB66    AT LEVEL 064 AS OF 08/16/00                      
*PHASE T62166A                                                                  
*&&      SET   NOP=N                                                            
CLB66    TITLE '- PC BILLING - PRINT BILLS'                                     
***********************************************************************         
* NTRY: CSBILNUM = BILL NUMBER                                        *         
***********************************************************************         
CLB66    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB66**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     R7,AREP                                                          
         USING REPD,R7                                                          
         L     RC,AOVERWRK                                                      
         USING PTWORKD,RC                                                       
         ST    RE,RELO                                                          
*                                                                               
         LA    R0,AROUTN           R0=(NUMBER OF ROUTINES)                      
         LA    RF,AREROUT          RF=A(RELOCATED ADDRESSES)                    
         XR    RE,RE                                                            
         LA    R1,ROUT                                                          
PRINT02  ST    R1,0(RF)                                                         
         STC   RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,PRINT02                                                       
*                                                                               
         GOTO1 AGETBLH             GET BILL HEADER DATA                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BNE   EXIT                                                             
         GOTO1 AGETFMT             GET FORMAT DETAILS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         BNE   EXIT                                                             
         GOTO1 APRCRECS            PROCESS RECORDS                              
         B     EXITY                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* BRANCH TO ROUTINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
ROUT     DS    0H                                                               
         NTR1  ,                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         L     R8,AROUT(RF)                                                     
         A     R8,RELO                                                          
         BR    R8                                                               
         SPACE 1                                                                
AROUT    DS    0F                  ROUTINES                                     
         DC    A(GETBLH)           GET BILL HEADER DETAILS                      
         DC    A(GETFMT)           GET FORMAT DETAILS                           
         DC    A(PRCRECS)          PROCESS RECORDS                              
         DC    A(RPTFRST)          FIRST FOR REPORT                             
         DC    A(LVLFRST)          FIRST FOR LEVEL                              
         DC    A(PAGFRST)          FIRST FOR PAGE                               
         DC    A(SECFRST)          FIRST FOR BODY SECTION                       
         DC    A(PRTPAR)           PRINT A PARAGRAPH                            
         DC    A(PAGBRK)           PAGE BREAK                                   
         DC    A(SECLAST)          LAST FOR BODY SECTION                        
         DC    A(PAGLAST)          LAST FOR PAGE                                
         DC    A(LVLLAST)          LAST FOR LEVEL                               
         DC    A(RPTLAST)          LAST FOR REPORT                              
         DC    A(CPYREC)           COPY RECORD                                  
         DC    A(GETLINE)          GET CURRENT LINE NUMBER                      
         DC    A(SETLINE)          SET REPORT LINE NUMBER                       
AROUTN   EQU   (*-AROUT)/L'AROUT                                                
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR LITERAL POOL                                              *         
***********************************************************************         
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   LTORG+X'0100'                                                    
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET BILL HEADER DETAILS                                  *         
*                                                                     *         
* EXIT: BILL HEADER RECORD SAVED IN IOA                               *         
***********************************************************************         
         SPACE 1                                                                
GETBLH   DS    0H                                                               
         USING *,R8                                                             
K        USING BEDRECD,IOKEY                                                    
         XC    K.BEDPAS,K.BEDPAS                                                
         MVI   K.BEDPTYP,BEDPTYPQ                                               
         MVC   K.BEDPCPY,CUABIN                                                 
         MVI   K.BEDPSUB,BEDPSUBQ                                               
         MVC   K.BEDPBLNO,CSBILNUM                                              
         L     R1,=AL4(IOHIGH+IOACCDIR+IOBLH)                                   
         GOTO1 AIO                                                              
         BNE   EXITN                                                            
         CLC   K.BEDPAS(BEDPIND-BEDPAS),IOKEYSAV                                
         BNE   EXITN                                                            
         MVC   PTFMTNUM,K.BEDPFORM SAVE FORMAT NUMBER                           
         MVC   PTPRGID,=C'BL'                                                   
         CLI   K.BEDPIND,BEDPIDFT                                               
         BNE   *+10                                                             
         MVC   PTPRGID,=C'BD'                                                   
         DROP  K                                                                
*                                                                               
         L     R1,=AL4(IOGET+IOACCMST+IOBLH)                                    
         GOTO1 AIO                                                              
         BNE   EXITN                                                            
         L     RF,AIOBLH                                                        
         MVC   PTHDRKEY,0(RF)      SAVE BILL HEADER KEY                         
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET FORMAT DETAILS                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETFMT   DS    0H                                                               
         USING *,R8                                                             
K        USING BFMRECD,IOKEY                                                    
         XC    K.BFMKEY,K.BFMKEY                                                
         MVI   K.BFMKTYP,BFMKTYPQ                                               
         MVC   K.BFMKCPY,CUABIN                                                 
         MVI   K.BFMKSUB,BFMKSUBQ                                               
         MVC   K.BFMKFMT,PTFMTNUM                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   EXIT                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BNE   EXIT                                                             
         L     R3,AIO1                                                          
         LA    R3,BFMRFST-BFMRECD(R3)                                           
         USING BOFELD,R3                                                        
         XR    RF,RF                                                            
GFMT02   CLI   BOFEL,0                                                          
         BE    EXITN                                                            
         CLI   BOFEL,BOFELQ                                                     
         BE    *+12                                                             
         IC    RF,BOFLN                                                         
         BXH   R3,RF,GFMT02                                                     
         MVC   REPMAXL,BOFMAXLN                                                 
         DROP  R3                                                               
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS BILL RECORDS                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PRCRECS  DS    0H                                                               
         USING *,R8                                                             
         GOTO1 ARPTFRST            INITIALISE REPORT                            
*                                                                               
K        USING BEDRECD,IOKEY                                                    
         MVC   K.BEDKEY,PTHDRKEY                                                
         MVC   K.BEDKLVL,=AL2(BEDKLCPQ)                                         
*                                                                               
L        USING BEDKEY,PTLVLKEY                                                  
S        USING BEDKEY,PTSECKEY                                                  
P        USING BEDKEY,PTPARKEY                                                  
         XC    L.BEDKEY,L.BEDKEY                                                
         XC    S.BEDKEY,S.BEDKEY                                                
         XC    P.BEDKEY,P.BEDKEY                                                
*                                                                               
         LHI   R1,IOACCDIR+IOHIGH+IOPARA                                        
         B     *+8                                                              
PRECS02  LHI   R1,IOACCDIR+IOSEQ+IOPARA                                         
         GOTO1 AIO                                                              
         BNE   PRECS30                                                          
         CLC   K.BEDKEY(BEDKLVL-BEDKEY),IOKEYSAV                                
         BNE   PRECS30                                                          
*                                                                               
* TEST FOR CONTINUATION RECORDS ??                                              
*                                                                               
PRECS10  DS    0H                                                               
         GOTO1 AIO,IOGET+IOACCMST+IOPARA                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P.BEDKEY,K.BEDKEY                                                
         CLC   L.BEDKLVL,K.BEDKLVL TEST FOR CHANGE OF LEVEL                     
         BE    PRECS14                                                          
         TM    PTINDS,PTIPRT       TEST ANYTHING PRINTED                        
         BZ    PRECS12                                                          
         GOTO1 ALVLLAST            LAST FOR PREVIOUS LEVEL                      
PRECS12  GOTO1 ALVLFRST            FIRST FOR LEVEL                              
*                                                                               
PRECS14  DS    0H                                                               
         CLC   K.BEDKWHER,=AL2(BEDKWPHQ)                                        
         BE    PRECS28                                                          
         CLC   K.BEDKWHER,=AL2(BEDKWPFQ)                                        
         BE    PRECS28                                                          
         CLC   K.BEDKWHER,=AL2(BEDKWBDQ)                                        
         BE    PRECS16                                                          
         TM    PTINDS,PTIBODY      TEST PREVIOUS SECTION WAS BODY               
         BZ    PRECS20                                                          
         GOTO1 ASECLAST                                                         
         B     PRECS20                                                          
*                                                                               
PRECS16  DS    0H                                                               
         OC    K.BEDKPARA,K.BEDKPARA  IGNORE SECTION HEADER                     
         BZ    PRECS28                                                          
         TM    PTINDS,PTIBODY      TEST ALREADY IN BODY OF SECTION              
         BZ    PRECS18                                                          
         CLC   S.BEDKSSEQ,K.BEDKSSEQ YES - TEST CHANGE OF SECTION               
         BE    PRECS20                                                          
         GOTO1 ASECLAST                                                         
PRECS18  GOTO1 ASECFRST                                                         
         CLC   K.BEDKSLVL,=AL2(BEDKSHPQ)                                        
         BE    PRECS28                                                          
*                                                                               
PRECS20  DS    0H                                                               
         GOTO1 APRTPAR,BOPARM,AIOPARA                                           
*                                                                               
PRECS28  DS    0H                                                               
         CLC   K.BEDKEY,P.BEDKEY   TEST ANY OTHER IO DONE                       
         BE    PRECS02                                                          
         MVC   K.BEDKEY,P.BEDKEY   YES - RE-ESTABLISH READ SEQUENCE             
         GOTO1 AIO,IOREAD+IOACCDIR+IOPARA                                       
         B     PRECS02                                                          
         DROP  K                                                                
*                                                                               
PRECS30  DS    0H                                                               
         GOTO1 ALVLLAST                                                         
         GOTO1 ARPTLAST                                                         
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REPORT                                                    *         
***********************************************************************         
         SPACE 1                                                                
RPTFRST  DS    0H                                                               
         USING *,R8                                                             
         XC    REPAPHS,REPAPHS                                                  
         XC    REPSUBID,REPSUBID                                                
*                                                                               
         MVI   REPHEADN,0          SET NUMBER OF HEADLINES ETC. TO 0            
         MVI   REPMIDSN,0          (WILL HANDLE THESE OURSELVES)                
         MVI   REPFOOTN,0                                                       
         XC    REPAHEAD,REPAHEAD                                                
         XC    REPAMIDS,REPAMIDS                                                
         XC    REPAFOOT,REPAFOOT                                                
         OI    REPIND2,REPISKP                                                  
*                                                                               
*        MVI   REPPRNTN,0          INITIALLY                                    
         MVI   REPPRNSA,0                                                       
         MVI   REPPRNTI,REPPCLRA                                                
         LA    RE,REPH1                                                         
         ST    RE,REPAPRNT                                                      
         LHI   RF,REPX-REPH1                                                    
         XR    R1,R1 '                                                          
         ICM   R1,8,BCSPACES                                                    
         MVCL  RE,R0                                                            
*                                                                               
         OC    REPSUBID,CSREPID    SET REPORT ID FROM INPUT OVERRIDE            
         BNZ   RFRST02                                                          
         MVI   REPSUBID,C'A'       ELSE USE DEFAULT                             
         MVC   REPSUBID+1(L'PTPRGID),PTPRGID                                    
RFRST02  MVC   REPPRGID,PTPRGID                                                 
*&&UK*&& MVI   REPCLASS,C'B'                                                    
         XC    CSREPID,CSREPID                                                  
*                                                                               
         MVI   BCWORK,C'A'                                                      
         MVC   BCWORK+1(2),REPPRGID                                             
         MVC   BCWORK+3(2),REPUSRID                                             
         L     RF,ACOM                                                          
         L     RF,CPQPROF-COMFACSD(RF)                                          
         GOTO1 (RF),BODMCB,BCWORK,(2,REPBLK),ACOM                               
         MVC   REPDESC,BCSPACES                                                 
         MVC   REPDESC(2),=C'BL'                                                
         MVC   REPDESC+2(L'CSBILNUM),CSBILNUM                                   
*                                                                               
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTO1 (RF),(R1)                                                        
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   REPACTN,REPAPUT                                                  
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEVEL (COVER PAGE / MAIN BILL / SUMMARY)                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
LVLFRST  DS    0H                                                               
         USING *,R8                                                             
L        USING BEDKEY,PTLVLKEY                                                  
S        USING BEDKEY,PTSECKEY                                                  
P        USING BEDKEY,PTPARKEY                                                  
*                                                                               
         MVC   L.BEDKEY,PTHDRKEY                                                
         MVC   L.BEDKLVL,P.BEDKLVL                                              
         XC    S.BEDKEY,S.BEDKEY                                                
         NI    PTINDS,PTIPRT                                                    
*                                                                               
         CLC   P.BEDKWHER,=AL2(BEDKWPHQ)                                        
         BNE   LFRST02                                                          
         OI    PTINDS,PTIPAGH                                                   
         GOTO1 ACPYREC,BOPARM,AIOPAGH,AIOPARA                                   
*                                                                               
LFRST02  DS    0H                                                               
         XR    R4,R4               R4 = NUMBER OF LINES IN FOOTLINE             
K        USING BEDRECD,IOKEY       READ FOR PAGE FOOTER                         
         MVC   K.BEDKEY,L.BEDKEY                                                
         MVC   K.BEDKWHER,=AL2(BEDKWPFQ)                                        
         LHI   R1,IOREAD+IOACCDIR+IOPAGF                                        
         GOTO1 AIO                                                              
         BNE   LFRST10                                                          
         LHI   R1,IOGET+IOACCMST+IOPAGF                                         
         GOTO1 AIO                                                              
         BNE   LFRST10                                                          
         L     R3,AIOPAGF                                                       
         LA    R3,BEDRFST-BEDRECD(R3)                                           
         USING BFPELD,R3                                                        
         XR    RF,RF                                                            
LFRST04  CLI   BFPEL,0                                                          
         BE    LFRST10                                                          
         CLI   BFPEL,BFPELQ                                                     
         BE    *+12                                                             
         IC    RF,BFPLN                                                         
         BXH   R3,RF,LFRST04                                                    
         OI    PTINDS,PTIPAGF                                                   
         IC    R4,BFPHGT                                                        
         DROP  R3                                                               
*                                                                               
LFRST10  DS    0H                                                               
         XR    RE,RE               SET LAST LINE - FOOTER                       
         IC    RE,REPMAXL                                                       
         LA    RE,1(RE)                                                         
         SR    RE,R4                                                            
         STC   RE,PTPAGFLN                                                      
*                                                                               
LVLFRSTX DS    0H                                                               
         GOTO1 APAGFRST            DO FIRST FOR PAGE                            
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR PAGE                                                      *         
***********************************************************************         
         SPACE 1                                                                
PAGFRST  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         TM    PTINDS,PTIPAGH      PRINT PAGE HEADER                            
         BZ    PFRST02                                                          
* ?? NEED TO PROCESS RECORD (FOR PAGE NUMBER ETC.)                              
         GOTO1 APRTPAR,BOPARM,(1,AIOPAGH)                                       
*                                                                               
PFRST02  TM    PTINDS,PTISECPH     PRINT SECTION PAGE HEADINGS                  
         BZ    PAGFRSTX                                                         
         GOTO1 APRTPAR,BOPARM,(1,AIOSECPH)                                      
*                                                                               
PAGFRSTX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR BODY SECTION                                              *         
***********************************************************************         
         SPACE 1                                                                
SECFRST  DS    0H                                                               
         USING *,R8                                                             
S        USING BEDKEY,PTSECKEY                                                  
         L     RF,AIOPARA                                                       
         USING BEDRECD,RF                                                       
         MVC   S.BEDKEY,0(RF)                                                   
         XC    S.BEDKPARA,S.BEDKPARA                                            
         OI    PTINDS,PTIBODY                                                   
         CLC   BEDRSLVL,=AL2(BEDKSHPQ)                                          
         BNE   SFRST02                                                          
         DROP  RF,S                                                             
         GOTO1 ACPYREC,BOPARM,AIOSECPH,AIOPARA                                  
         OI    PTINDS,PTISECPH                                                  
*                                                                               
SFRST02  DS    0H                                                               
         TM    PTINDS,PTIPBODY     TEST PRINTED PREVIOUS BODY SECITION          
         BZ    SFRST10                                                          
* TEST WANT PAGE BREAK NOW                                                      
*        BNE   SFRST10                                                          
         GOTO1 APAGBRK                                                          
         B     SECFRSTX                                                         
*                                                                               
SFRST10  DS    0H                                                               
         TM    PTINDS,PTISECPH                                                  
         BZ    SECFRSTX                                                         
*        IF PARA FITS ON PAGE THEN   ??                                         
         GOTO1 APRTPAR,BOPARM,AIOSECPH                                          
*        PRINT IT                                                               
*                                                                               
SECFRSTX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT PARAGRAPH                                                     *         
*                                                                     *         
* NTRY: P1 BYTE 0 = NON-ZERO IF PRINTING PAGE HEADER/FOOTER           *         
*             1-3 = A(BEDRECD)                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PRTPAR   DS    0H                                                               
         USING *,R8                                                             
         XR    R5,R5                                                            
         IC    R5,0(R1)            R5 = NON-ZERO IF PAGE HEADER/FOOTER          
         L     R2,0(R1)                                                         
         USING BEDRECD,R2                                                       
         LA    R3,BEDRFST                                                       
         USING BFPELD,R3                                                        
         XR    RF,RF                                                            
PPAR02   CLI   BFPEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BFPEL,BFPELQ                                                     
         BE    *+12                                                             
         IC    RF,BFPLN                                                         
         BXH   R3,RF,PPAR02                                                     
*                                                                               
         XR    R4,R4                                                            
         ICM   R4,1,BFPHGT         R4 = HEIGHT OF PARAGRAPH                     
         BZ    EXIT                                                             
*                                                                               
PPAR04   DS    0H                                                               
         GOTO1 AGETLINE,BOPARM,(R4)                                             
         L     R6,8(R1)            R6 = LINE NUMBER AFTER PARAGRAPH             
*                                                                               
         LTR   R5,R5               TEST PAGE HEADER/FOOTER                      
         BNZ   PPAR10                                                           
         CLM   R6,1,PTPAGFLN       NO - TEST PARA FITS ON PAGE                  
         BNH   PPAR10                                                           
         GOTO1 APAGBRK                                                          
         LA    R5,1                                                             
         B     PPAR04                                                           
*                                                                               
PPAR10   DS    0H                                                               
         CLI   BFPHGTMN,0          0 MIN HEIGHT => NO ITEMS IN PARA             
         BNE   PPAR12                                                           
         GOTO1 ASETLINE,(R6)                                                    
         B     PRTPARX                                                          
PPAR12   DS    0H                                                               
         GOTO1 AFMTTXT,BOPARM,(C'P',BEDRECD),REPH2                              
         MVI   REPPRNTN,1                                                       
         XR    R0,R0                                                            
         ICM    R0,1,BFPHGT                                                     
         BZ    PPAR16                                                           
         LA    R3,REPH2                                                         
PPAR14   MVC   REPH1,0(R3)                                                      
         GOTO1 VREPORT,REPD                                                     
         MVC   0(L'REPH1,R3),BCSPACES                                           
         LA    R3,L'REPH1(R3)                                                   
         BCT   R0,PPAR14                                                        
PPAR16   CLM   R6,1,REPLINE                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRTPARX  DS    0H                                                               
         OI    PTINDS,PTIPRT       SET SOMETHING HAS BEEN PRINTED               
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PAGE BREAK                                                          *         
***********************************************************************         
         SPACE 1                                                                
PAGBRK   DS    0H                                                               
         USING *,R8                                                             
         TM    PTINDS,PTIPRT                                                    
         BZ    PBRK02                                                           
         GOTO1 APAGLAST                                                         
PBRK02   GOTO1 APAGFRST                                                         
PAGBRKX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST FOR BODY SECTION                                               *         
***********************************************************************         
         SPACE 1                                                                
SECLAST  DS    0H                                                               
         USING *,R8                                                             
         NI    PTINDS,FF-(PTIBODY+PTISECPH)                                     
         OI    PTINDS,PTIPBODY                                                  
SECLASTX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST FOR PAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
PAGLAST  DS    0H                                                               
         USING *,R8                                                             
         TM    PTINDS,PTIPAGF                                                   
         BZ    PAGLASTX                                                         
         XR    R1,R1                                                            
         IC    R1,PTPAGFLN                                                      
         GOTO1 ASETLINE,(R1)                                                    
* ?? NEED TO PROCESS RECORD (FOR PAGE NUMBER ETC.)                              
         GOTO1 APRTPAR,BOPARM,(1,AIOPAGF)                                       
*                                                                               
PAGLASTX DS    0H                                                               
         OI    REPHEADI,REPHFRCE   FORCE NEW PAGE NEXT TIME ??                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST FOR LEVEL (COVER PAGE / MAIN BILL / SUMMARY)                   *         
***********************************************************************         
         SPACE 1                                                                
LVLLAST  DS    0H                                                               
         USING *,R8                                                             
         GOTO1 APAGLAST                                                         
LVLLASTX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST FOR PRINTING                                                   *         
***********************************************************************         
         SPACE 1                                                                
RPTLAST  DS    0H                                                               
         USING *,R8                                                             
         MVI   REPACTN,REPACLO                                                  
         GOTO1 VREPORT,REPD                                                     
RPTLASTX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COPY RECORD                                              *         
*                                                                     *         
* NTRY: P1 = A(DESTINATION RECORD)                                    *         
*       P2 = A(SOURCE RECORD)                                         *         
***********************************************************************         
         SPACE 1                                                                
CPYREC   DS    0H                                                               
         USING *,R8                                                             
         L     RE,4(R1)                                                         
         USING BEDRECD,RE                                                       
         LH    RF,BEDRLEN                                                       
         L     R0,0(R1)                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET CURRENT LINE NUMBER                                  *         
*                                                                     *         
* NTRY: P1 = HEIGHT OF PARAGRPAH                                      *         
* EXIT: P2 = CURRENT LINE NUMBER                                      *         
*       P3 = CURRENT LINE NUMBER + HEIGHT OF PARAGRAPH                *         
***********************************************************************         
         SPACE 1                                                                
GETLINE  DS    0H                                                               
         USING *,R8                                                             
         L     R2,0(R1)                                                         
         LA    R3,1                                                             
         TM    REPIND1,REPIPUT     TEST NOTHING YET PRINTED                     
         BZ    GLINE02                                                          
         TM    REPHEADI,REPHFRCE   TEST NEW PAGE ALREADY FORCED                 
         BO    GLINE02                                                          
         CLC   REPLINE,REPMAXL     TEST ALREADY PAST END OF PAGE                
         BH    GLINE02                                                          
         IC    R3,REPLINE                                                       
*                                                                               
GLINE02  DS    0H                                                               
         LA    R4,0(R2,R3)                                                      
         STM   R3,R4,4(R1)                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET REPORT LINE NUMBER                                   *         
*                                                                     *         
* NTRY: R1 = REQUIRED LINE NUMBER                                     *         
***********************************************************************         
         SPACE 1                                                                
SETLINE  DS    0H                                                               
         USING *,R8                                                             
         LR    R3,R1               R3 = REQUIRED LINE NUMBER                    
         CLM   R3,1,REPMAXL        TEST IS ON CURRENT PAGE                      
         BL    *+12                                                             
         OI    REPHEADI,REPHFRCE                                                
         B     EXIT                                                             
*                                                                               
         GOTO1 AGETLINE,BOPARM,0                                                
         L     R2,4(R1)            R2 = CURRENT LINE NUMBER                     
*                                                                               
         LR    R4,R3               R4 = REQUIRED - CURRENT                      
         SR    R4,R2                                                            
         BNP   EXIT                                                             
*                                                                               
         MVI   REPPRNTN,1                                                       
         STC   R4,REPPRNSA                                                      
         GOTO1 VREPORT,REPD                                                     
         CLM   R3,1,REPLINE                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
ADDEND   DC    C'ADD=END'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
*                                                                               
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
WORKD    DSECT                                                                  
*                                                                               
IOBLH    EQU   IOA                                                              
         ORG   AIOA                                                             
AIOBLH   DS    A                   IO AREA FOR BILL HEADER RECORD               
*                                                                               
IOPAGH   EQU   IO8                                                              
         ORG   AIO8                                                             
AIOPAGH  DS    A                   IO AREA FOR PAGE HEADER                      
*                                                                               
IOPAGF   EQU   IO6                                                              
         ORG   AIO6                                                             
AIOPAGF  DS    A                   IO AREA FOR PAGE FOOTER                      
*                                                                               
IOSECPH  EQU   IO4                                                              
         ORG   AIO4                                                             
AIOSECPH DS    A                   IO AREA FOR SECTION PAGE HEADINGS            
*                                                                               
IOPARA   EQU   IO2                                                              
         ORG   AIO2                                                             
AIOPARA  DS    A                   IO AREA FOR PARAGRAPH                        
*                                                                               
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
PTWORKD  DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
AREROUT  DS    0A                                                               
AGETBLH  DS    A                   GET BILL HEADER DETAILS                      
AGETFMT  DS    A                   GET FORMAT DETAILS                           
APRCRECS DS    A                   PROCESS RECORDS                              
ARPTFRST DS    A                   INITIALIZE REPORT                            
ALVLFRST DS    A                   FIRST FOR LEVEL                              
APAGFRST DS    A                   FIRST FOR PAGE                               
ASECFRST DS    A                   FIRST FOR BODY SECTION                       
APRTPAR  DS    A                   PRINT A PARAGRAPH                            
APAGBRK  DS    A                   PAGE BREAK                                   
ASECLAST DS    A                   LAST FOR BODY SECTION                        
APAGLAST DS    A                   LAST FOR PAGE                                
ALVLLAST DS    A                   LAST FOR LEVEL                               
ARPTLAST DS    A                   LAST FOR REPORT                              
ACPYREC  DS    A                   COPY RECORD                                  
AGETLINE DS    A                   GET CURRENT LINE NUMBER                      
ASETLINE DS    A                   SET REPORT LINE NUMBER                       
AREROUTN EQU   (*-AREROUT)/L'AREROUT                                            
         DS    (AROUTN-AREROUTN)X ENSURE AROUTN=AREROUTN                        
         DS    (AREROUTN-AROUTN)X                                               
*                                                                               
PTPRGID  DS    CL2                 REPORT PROGRAM ID                            
PTFMTNUM DS    XL1                 FORMAT NUMBER                                
PTHDRKEY DS    XL42                BILL HEADER KEY                              
PTLVLKEY DS    XL42                CURRENT LEVEL HEADER KEY                     
PTSECKEY DS    XL42                CURRENT BODY SECTION KEY                     
PTPARKEY DS    XL42                CURRENT BILL PARAGRAPH KEY                   
*                                                                               
PTPAGFLN DS    XL1                 PAGE FOOTER LINE NUMBER                      
PTINDS   DS    XL1                 INDICATOR BYTE                               
PTIPRT   EQU   X'80'               HAVE PRINTED FIRST PARAGRAPH                 
PTIPAGH  EQU   X'40'               PAGE HEADER EXISTS                           
PTIPAGF  EQU   X'20'               PAGE FOOTER EXISTS                           
PTIPBODY EQU   X'10'               A BODY SECTION OF BILL PRINTED               
PTIBODY  EQU   X'08'               PROCESSING BODY OF BILL                      
PTISECPH EQU   X'04'               SECTION PAGE HEADINGS EXIST                  
         DS    (OVERWRKL-(*-PTWORKD))X                                          
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064ACCLB66   08/16/00'                                      
         END                                                                    
