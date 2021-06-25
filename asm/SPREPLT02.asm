*          DATA SET SPREPLT02  AT LEVEL 030 AS OF 01/20/15                      
*PHASE SPLT02A                                                                  
*INCLUDE SPBVAL                                                                 
*INCLUDE SPFMTINO                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE DLFLD                                                                  
*INCLUDE DDUCOM                                                                 
*INCLUDE GETUSER                                                                
*                                                                               
         TITLE 'SPLT02 - CHANGE LOG'                                            
*                                                                               
*                                                                               
         TITLE 'SPLT02 - SPOTPAK LABATT INTERFACE'                              
*                                                                               
*                                                                               
* QOPT4        ' ' = NO OUTPUT FILE, Y= PRODUCE OUTPUT FILE                     
*                                                                               
*                                                                               
SPLT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPLT02                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    R8,SPACEND                                                       
         USING SPLTWRKD,R8                                                      
         LA    R7,P                                                             
         USING BILLINED,R7                                                      
*                                                                               
         RELOC RELO                                                             
         SPACE 2                                                                
         CLI   MODE,PROCBILL                                                    
         BE    PRBIL                                                            
         CLI   MODE,REQFRST                                                     
         BE    FBLR                                                             
         CLI   MODE,REQLAST                                                     
         BE    LBLR                                                             
***OFF                                                                          
         CLI   MODE,OFCFRST                                                     
         BE    FBOFF                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBOFF                                                            
***OFF                                                                          
         CLI   MODE,CLTFRST        FRIST FOR CLIENT                             
         BE    FBC                                                              
         CLI   MODE,CLTLAST       LAST FOR CLIENT                               
         BE    LBCLI                                                            
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
RUNF     DS    0H                  RUN FIRST                                    
         MVI   ERRORSW,0                                                        
         MVI   DOWNACT,0           CLEAR DOWNLOAD ACTIVITY                      
*                                                                               
         B     EXIT                                                             
*                                                                               
FBLR     DS    0H                  FIRST FOR REQUEST                            
*                                                                               
         L     RF,=V(GETUSER)                                                   
         A     RF,RELO                                                          
         ST    RF,VGETUSER                                                      
*                                                                               
         L     RF,=A(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
*                                                                               
         L     RF,=V(DDUCOM)                                                    
         A     RF,RELO                                                          
         ST    RF,VDDUCOM                                                       
*                                                                               
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
         ST    RF,VDLFLD                                                        
*                                                                               
         L     RF,=V(SPBVAL)                                                    
         A     RF,RELO                                                          
         ST    RF,VSPBVAL                                                       
*                                                                               
         L     RF,=V(SPFMTINO)                                                  
         A     RF,RELO                                                          
         ST    RF,VSFMTINO                                                      
*                                                                               
         SR    R0,R0            SET INVOICE LIST BINSRCH PARS                   
         L     R1,=A(INVTAB)                                                    
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,4                                                             
         LA    R4,0                                                             
         L     R5,=A(INVMAX)    INVMAX IS 10,000 INVS PER CLT                   
         STM   R0,R5,INVPARS                                                    
*                                                                               
         CLI   FIRSTSW,0           FIRST TIME TEST                              
         BNE   FBLR1                                                            
         MVI   FIRSTSW,1                                                        
*                                                                               
         MVI   ENDRPTSW,0         CLEAR SWITCH USED IN END PROCESSES            
*                                                                               
         LA    R3,RUNTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         LA    R3,TRUNTOTS         TOTAL FOR TAPE                               
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         XC    RUNINVS,RUNINVS      RUN INVOICE TOTALS                          
*                                                                               
         XC    START(12),START                                                  
         XC    LSTBLKY,LSTBLKY                                                  
         MVC   SAVMAX,MAXLINES                                                  
*                                                                               
FBLR1    DS    0H                                                               
*                                                                               
         MVI   RACTSW,0              ZERO REQUEST ACTIVITY                      
         MVI   OACTSW,0              ZERO REQUEST ACTIVITY                      
*                                                                               
         MVC   DYNDDN,=CL8'SLTTAPE'                                             
         MVC   DYNDSN,=CL20'SPTTAPE.PP0LTXX1'                                   
         MVC   DYNDSN+13(2),QAGY                                                
**                                                                              
*              NO-OP MIX FOR TAPE SPECS CHK                                     
**                                                                              
*                                                                               
FBLR2    DS    0H                                                               
         CLI   QOPT4,C'N'          NO TAPE                                      
         BE    FBLR3                                                            
*                                                                               
         GOTO1 VDOWNLD,DMCB,(RA)   INITIALIZE DOWNLOAD                          
*                                                                               
FBLR3    DS    0H                                                               
         LA    R3,PRDTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         LA    R3,CLTTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         LA    R3,OFFTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         LA    R3,REQTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         XC    CLTINVS,CLTINVS   CLEAR CLIENT AND REQ INVOICE TOTALS            
         XC    REQINVS,REQINVS                                                  
*                                                                               
         XC    LASTKEY,LASTKEY                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   TOTSW,0                                                          
         OC    START(12),START                                                  
         BNZ   *+10                                                             
         MVC   START(12),QSTART    SET START-END FROM FRST REQ                  
         CLC   START(12),SPACES                                                 
         BNE   FBLR4                                                            
         MVC   START+00(2),RCDATE+06   YY                                       
         MVC   START+02(2),RCDATE+00   MM                                       
         MVC   START+04(2),RCDATE+03   DD       ***   CONVERT START             
         GOTO1 DATCON,DMCB,(0,START),(0,START)  *** TO NEW DATE FORMAT          
         MVC   END,START                                                        
FBLR4    DS    0H                                                               
         CLC   QSTART(12),SPACES                                                
         BNE   *+10                                                             
         MVC   QSTART(12),START                                                 
         B     EXIT                                                             
         SPACE 3                                                                
         EJECT                                                                  
FBC      DS    0H                  FIRST FOR CLIENT                             
*                                                                               
         XC    LASTCPE,LASTCPE                                                  
         XC    PPGRECNT,PPGRECNT     MUST CLEAR FOR BINSRCH                     
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SOB1'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
*     NOW ALWAYS PASS OFFICE DATA                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RF                                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         CLI   QCLT,C'*'          ONE OFFICE REQUEST ?                          
         BE    FBC1B               YES                                          
         CLI   QCLT,C'$'          ALL OFFICE REQUEST ?                          
         BE    FBC1B               YES                                          
         CLC   QCLT,=C'ALL'       ALL CLIENTS REQUEST ?                         
         BNE   FBC1D               NO - ONE CLIENT ONLY REQUEST                 
*                                                                               
FBC1B    DS    0H                                                               
*                                                                               
FBC1D    MVI   NEWCLT,C'Y'                                                      
         CLI   PROGPROF+0,C'Y'     NEW PAGE PER CLIENT?                         
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                  SET CLIENT DATA IN SVMID                     
         MVI   NEWCLT,C'Y'                                                      
         MVC   SVMID,SPACES                                                     
         MVC   SVMID(7),=C'CLIENT='                                             
         MVC   SVMID+8(3),CLT                                                   
         MVC   SVMID+12(24),CLTNM                                               
*                                                                               
         LA    RF,SVMID+36                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         L     RE,ADCLT                                                         
         CLI   CCLTIFC-CLTHDR(RE),C' '                                          
         BNH   FBILC9                                                           
         MVI   2(RF),C'('                                                       
         MVC   3(8,RF),CCLTIFC-CLTHDR(RE)                                       
         LA    RF,11(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
*                                                                               
FBILC9   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
FBCX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
LBCLI    DS    0H                  LAST FOR CLIENT                              
*                                  SEND ENTRIES TO DOWNLOAD                     
         L     R2,PPGRECNT         FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROCNT                                                          
*                                                                               
*        I HAVE SOMETHING TO SEND                                               
*                                                                               
*                                                                               
LBCLI3   L     R3,AOFPPGT                                                       
         LA    R3,L'PPGKEY(R3)     BUMP PAST PSUEDO KEY                         
LBCLI5   GOTO1 VDOWNLD,DMCB,(RA),(R3)                                           
         LA    R3,PPGTLEN(R3) TO NEXT RECORD IN TABLE                           
         BCT   R2,LBCLI5                                                        
*                                                                               
ZEROCNT  B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
LBLR     DS    0H                  LAST FOR REQUEST                             
         CLI   QCLT,C'$'                                                        
         BE    LBLR2                                                            
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   RACTSW,0            CHK FOR ACTIVITY                             
         BE    LBLR2               NO                                           
         MVI   ENDRPTSW,C'E'       INDICATES COMING FROM LBLR OR LBOFF          
         BRAS  RE,PRBILRTN           GO FINISH LAST PRD/CLT                     
         MVI   ENDRPTSW,0          CLEAR                                        
*                                                                               
LBLR2    DS    0H                                                               
         CLI   RACTSW,0                                                         
         BE    EXIT                                                             
         LA    R3,REQTOTS                                                       
         LA    R4,RUNTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
         CLI   QOPT4,C'N'                                                       
         BE    LBLR4                                                            
         LA    R3,REQTOTS                                                       
         LA    R4,TRUNTOTS         ALSO POST TO TAPE TOTALS                     
         BRAS  RE,ROLTOTS                                                       
         L     R0,TRUNINVS                                                      
         A     R0,REQINVS                                                       
         ST    R0,TRUNINVS                                                      
*                                                                               
LBLR4    BRAS  RE,PRNT                                                          
         MVC   BLINE(18),=C'**REQUEST TOTALS**'                                 
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,REQINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,REQTOTS                                                       
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         BRAS  RE,PRNT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                  RUN LAST                                     
         BRAS  RE,PRNT                                                          
         MVC   BLINE(14),=C'**RUN TOTALS**'                                     
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,RUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
         LA    R3,RUNTOTS                                                       
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         BRAS  RE,PRNT                                                          
         MVC   BLINE(15),=C'**FILE TOTALS**'                                    
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,TRUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT               
         LA    R3,TRUNTOTS                                                      
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         CLI   ERRORSW,0        ANY ERRORS?                                     
         BE    RUNL10              NO                                           
         MVC   P+1(36),=C'*** ERRORS HAVE BEEN ENCOUNTERED ***'                 
         BRAS  RE,PRNT                                                          
         TM    ERRORSW,X'01'     ESTIMATE USER1 INVALID ?                       
         BNO   RUNL3                                                            
         MVC   P+3(32),=C'*** ESTIMATE USER1 INCORRECT ***'                     
         BRAS  RE,PRNT                                                          
RUNL3    TM    ERRORSW,X'02'     REGION "INVALID" ?                             
         BNO   RUNL4                                                            
         MVC   P+3(33),=C'*** REGION NOT A VALID NUMBER ***'                    
         BRAS  RE,PRNT                                                          
RUNL4    DS    0H                                                               
*                                                                               
RUNL8    DS    0H                                                               
         MVC   P+1(35),=C'**WARNING -OUTPUT FILE HAS ERRORS**'                  
         BRAS  RE,PRNT                                                          
*                                                                               
*                       CLOSE DOWNLOAD FILE HERE                                
RUNL10   DS    0H                                                               
         CLI   DOWNACT,C'Y'       DOWNLOADING ACTIVITY?                         
         BNE   EXIT                                                             
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         B     EXIT                                                             
         SPACE 2                                                                
*                            ***** PROCESS BILL *****                           
         SPACE 2                                                                
PRBIL    DS    0H                                                               
         MVI   ENDRPTSW,0          CLEAR                                        
         BRAS  RE,PRBILRTN         BILL ROUTINE                                 
*                                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
***OFF                                                                          
LBOFF    DS    0H                                                               
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   OACTSW,0            ANY BILL FOR OFFICE PROCESSED                
         BE    LBOFF5              NO                                           
         MVI   ENDRPTSW,C'E'       INDICATES COMING FROM LBLR OR LBOFF          
         BRAS  RE,PRBILRTN           MUST DO PRD/CLT TOTALS FIRST               
         MVI   ENDRPTSW,0          CLEAR                                        
*                                                                               
LBOFF5   MVI   BLPRD,C'*'                                                       
         MVC   BLPRD+1(2),SAVCOFF                                               
         MVC   BLINE+5(17),=C'**OFFICE TOTALS**'                                
*                                                                               
         MVI   OACTSW,0                                                         
*                                                                               
         CLC   OFFTOTS(24),=4PL6'0'                                             
         BNE   LBOFF8                                                           
         MVC   BLINE+25(11),=C'NO ACTIVITY'                                     
         BRAS  RE,PRNT                                                          
         B     EXIT                                                             
*                                                                               
LBOFF8   DS    0H                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,OFFINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,OFFTOTS                                                       
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
         LA    R3,OFFTOTS                                                       
         LA    R4,REQTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         LA    R3,OFFTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         L     R0,REQINVS        ROLL OFFICE INVOICE TOTAL                      
         A     R0,OFFINVS        TO REQUEST                                     
         ST    R0,REQINVS                                                       
         XC    OFFINVS,OFFINVS                                                  
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
FBOFF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTKEY,LASTKEY                                                  
         MVI   OACTSW,0                                                         
         LA    R3,OFFTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
*                                                                               
         L     RF,ADCLT                                                         
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'S'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,SVAGY                                                
         MVC   OFFD.OFCOFC,COFFICE-CLTHDR(RF)                                   
*                                                                               
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         L     RF,VOFFICER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(2,WORK),(0,ACOMFACS)                                  
         CLI   0(R1),0                                                          
         BNE   FBOFFX                                                           
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
FBOFFX   B     EXIT                                                             
***OFF                                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                       POST BILL TO TABLE                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS     0D                                                              
POSTB    NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         LA     R3,PPGREC                                                       
         USING  PPGRECD,R3                                                      
         L      R2,ADBILL                                                       
         USING  BILLREC,R2                                                      
         MVC    PPGIDATE,BQDATE       DATE PRINTED ON BILL                      
         CLI    PPGIDATE,0            IS THERE ONE?                             
         BNE    *+10                                                            
         MVC    PPGIDATE,BDATE      ELSE USE RUN DATE                           
*                                                                               
*        MUST CONVERT DDS DATE FORMATS                                          
*                                                                               
         GOTO1  DATCON,DMCB,(0,PPGIDATE),(20,WORK)                              
         MVC    PPGIDATE(4),WORK+4     MMDD                                     
         MVC    PPGIDATE+4(2),WORK+2   YY  (LAST PART OF YYYY)                  
*                                                                               
         GOTO1  DATCON,DMCB,(0,BDATE),(20,WORK)                                 
*                                                                               
         MVC    PPGMOB(2),WORK+4      MM                                        
         MVC    PPGMOB+2(2),WORK+2    YY  (LAST PART OF YYYY)                   
*                                                                               
         MVC    PPGINVF,DINVFULL      FULL INVOICE NUMBER                       
         MVC    PPGINVNO,DINVNO       LAST 6 DIGITS OF INVOICE NUMBER           
*                                                                               
         L      RF,ADEST                                                        
*                                                                               
         MVC    PPGEU2,EUSER2-ESTHDR(RF)    BRAND EST                           
         MVC    PPGEU1,EUSER1-ESTHDR(RF)    ALSO FROM BRAND                     
         OC     PPGEU1,SPACES                                                   
         OC     PPGEU2,SPACES                                                   
*                                                                               
**TEST**                                                                        
*****    BRAS   RE,FINDMGP        FIND MARKET GROUP AND                         
*****                             USE PART OF GROUP NAME                        
*****                             RETURNED IN WORK                              
*****    MVC    PPGMGR1(3),WORK          WAS REGION FOR PRINT                   
**TEST**                                                                        
*                                                                               
         MVC    PPGMGR1(3),BLMGR+1    GROUP CODE                                
*                                                                               
         CLI    PPGEU1,C'.'           MUST BEGIN WITH DECIMAL                   
         BNE    POST1                   DISPLAY VALUE                           
         CLI    PPGEU1+30,C'0'        MUST HAVE 31 CHARACTERS                   
         BNL    POST2                 OK                                        
POST1    DS     0H                                                              
         MVC    P+1(43),=C'**ERROR-LNTH OR 1ST NOT DECIMAL - EST USER1'         
         MVC    P2+9(7),=C'VALUE ='                                             
         MVC    P2+16(31),PPGEU1                                                
         OI     ERRORSW,X'01'                                                   
         BRAS   RE,PRNT                                                         
POST2    DS     0H                                                              
*                                                                               
         CLI    PPGMGR1,C'0'                                                    
         BNL    POST3                                                           
         MVC    P+1(33),=C'**ERROR*  MISSING MARKET GRP CODE'                   
         MVC    P2+9(7),=C'VALUE ='                                             
         MVC    P2+16(3),PPGMGR1                                                
         OI     ERRORSW,X'02'                                                   
         BRAS   RE,PRNT                                                         
POST3    DS     0H                                                              
POST4    DS     0H                                                              
         CLI    QOPT4,C'Y'               SEE IF CREATING FILE                   
         BNE    POSTBX                                                          
*                                                                               
         ZAP    PPGBAMT,BILTOTS+24(6) AMOUNT DUE (INCLUDES TAXES)               
         ZAP    PPGGST,MYGST          BILLS GST                                 
         AP     PPGGST,MYHST          ADD HST                                   
         ZAP    PPGPST,MYPST          PST                                       
         SP     PPGPST,MYHST          EXCLUDE HST                               
*                                                                               
*  AT THIS POINT MUST ADD DATA RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         MVC    PPGKMED,QMED                                                    
         MVC    PPGKSMED,QMED       SUB MEDIA                                   
         CLI    NETPAKSW,C'Y'       SEE IF DOING NETPAK                         
         BNE    POSTB3                                                          
         CLI    BLMED,C' '          SUB MEDIA PRESENT                           
         BNH    *+10                                                            
         MVC    PPGKSMED,BLMED                                                  
POSTB3   MVC    PPGKCLI,CLT                                                     
         MVC    PPGCLI,CLT          CLIENT ALSO IN RECORD                       
         MVC    PPGMED,PPGKSMED     SUB-MEDIA ALSO IN RECORD                    
         L      RF,ADEST           RESET RF TO ESTIMATE                         
**NOPRD* MVC    PPGPRO,EKEYPRD-ESTHDR(RF)                                       
         MVC    PPGKEST+1(1),EKEYEST-ESTHDR(RF) +1 SPOT/NET 1 BYTE EST          
         MVC    PPGEST+1(1),EKEYEST-ESTHDR(RF) BOTH PLACES                      
         MVC    PPGINVMO,BKEYMBIL      BILLING MONTH                            
         NI     PPGINVMO,X'0F'        SET OFF YEAR HALF-BYTE                    
         MVC    PPGINVN,BKEYINV                                                 
         L      R2,AOFPPGT        ADDRESS OF PPGTAB                             
         PRINT  GEN                                                             
         GOTO1  =V(BINSRCH),BINVALS                                             
         PRINT  NOGEN                                                           
*                                                                               
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     GOTOXIT                                                         
         OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         L      RF,BINVALS               ADDRESS OF FOUND RECORD                
         LA     RF,L'PPGKEY(RF)          PAST KEY                               
*                                                                               
POSTB5   DS     0H                                                              
         AP     AMTDIS(6,RF),PPGBAMT                                            
         AP     GSTDIS(6,RF),PPGGST                                             
         AP     PSTDIS(6,RF),PPGPST                                             
GOTOXIT  DS     0H                                                              
         MVC    BINVALS,=A(PPGKMED)                                             
         MVI    BINVALS,1                                                       
POSTBX   DS     0H                                                              
         XIT1                                                                   
*                                                                               
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(PPGKMED)      RECORD TO BE ADDED                             
         DC    A(PPGTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
PPGRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    AL4(PPGTLEN)       LEN OF RECORD                                 
         DC    AL4(L'PPGKEY)      KEY SIZE                                      
         DC    F'4000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFPPGT  DC    A(PPGTABLE)                                                      
PPGKEY   DS    0XL13                                                            
PPGKMED  DS    CL1                                                              
PPGKCLI  DS    CL3                                                              
PPGKSMED DS    CL1           SUB-MEDIA                                          
PPGPRO   DS    CL3           **NOPRD*  THEY WILL DO PRDS TOGETHER               
PPGINUMB DS    0XL3                                                             
PPGINVMO DS    XL1           INVOICE MONTH                                      
PPGINVN  DS    XL2           INVOICE NUMBER                                     
PPGKEST  DS    XL2           EST IN LAST BYTE FOR SPOT/NET                      
*            ______                                                             
*              13                                                               
PPGREC   DS    0C                                                               
         ORG   *+PPGRECL                                                        
*                                                                               
ENDPPGR  DS    0C                                                               
*                                                                               
PPGTLEN  EQU   ENDPPGR-PPGKEY                                                   
*                                                                               
***********************                                                         
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R3                                                               
         DROP  R2                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        FIND MARKET GROUP AND RETURN FIRST 3 BYTES IN WORK           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
FINDMGP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADBILL                                                        
         USING BILLREC,R6                                                       
         MVC   WORK(3),=C'???'       DEFAULT IF NOT FOUND                       
         OC    BLMGR,BLMGR          CODE THERE                                  
         BZ    FINDMX                                                           
         MVC   SVMKEY,KEY                                                       
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING MKGRECD,RF                                                       
         MVC   MKGKTYP(2),=X'0D02'    MKT GROUP RECORD                          
         MVC   MKGKAGMD,BAGYMD                                                  
         MVC   MKGKCLT,BCLT                                                     
         MVC   MKGKMID(1),BLMGR       MKT GROUP ID AND CODE                     
         PACK  DUB,BLMGR+1(5)                                                   
         MVC   MKGKMGRP,DUB+5         PWOS                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(11),KEY                                                  
         BE    FINDM5                                                           
         MVC   KEY,KEYSAVE        TRY WITHOUT CLIENT                            
         XC    KEY+3(2),KEY+3                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(11),KEY                                                  
         BE    FINDM5                                                           
         BNE   FINDMX             LEAVE THE ??? IF NOT FOUND                    
*                                                                               
FINDM5   GOTO1 GETMKTGR                                                         
         L     RF,ADMKTGRP                                                      
         USING MKGRECD,RF                                                       
         LA    R6,MKGEL                                                         
         USING MKGEL10,R6                                                       
         LA    RE,MKGNAM3                                                       
         CLI   MKGNAM3,C' '       USE LOWEST NAME                               
         BH    FINDM20                                                          
         LA    RE,MKGNAM2                                                       
         CLI   MKGNAM2,C' '                                                     
         BH    FINDM20                                                          
         LA    RE,MKGNAM1                                                       
         CLI   MKGNAM1,C' '                                                     
         BH    FINDM20                                                          
*                                                                               
         DC    H'0'         BAD RECORD - NO NAMES                               
*                                                                               
FINDM20  MVC   WORK(3),0(RE)                                                    
*                                                                               
         MVC   KEY,SVMKEY                                                       
         GOTO1 HIGH                                                             
*                                                                               
FINDMX   XIT1                                                                   
*                                                                               
         DROP  RF                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        PROCESS BILL                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
PRBILRTN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADBILL                                                        
         USING BILLREC,R6                                                       
         CLI   ENDRPTSW,C'E'       COMING FROM LBLR OR LBOFF ?                  
         BE    PRB2B               YES                                          
*                                                                               
         CLC   BDATE,QSTART        DATES FILTER                                 
         BL    PRBRTNX                                                          
         CLI   QEND,C' '                                                        
         BE    PRB01                                                            
         CLC   BDATE,QEND                                                       
         BH    PRBRTNX                                                          
*                                                                               
PRB01    DS    0H                                                               
         CLI   BRETAIL,X'81'       CORP RETAIL?                                 
         BE    PRBRTNX                                                          
*                                  GET USER FIELDS                              
         CLC   LASTCPE(5),BILLREC+2  NEW PRODUCT                                
         BE    PRB02                                                            
*                                                                               
         XC    PPUSER1,PPUSER1     CLEAR                                        
         XC    PPUSER2,PPUSER2                                                  
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'P',ADPRD),(0,PPUSER1),    X        
               (0,PPUSER2)                                                      
*                                                                               
PRB02    CLC   LASTCPE,BILLREC+2  NEW ESTIMATE                                  
         BE    PRB05                                                            
*                                                                               
         MVC   LASTCPE,BILLREC+2    SAVE CLT/PRD/EST                            
*                                                                               
*        MUST READ ESTIMATE HERE                                                
*                                                                               
         MVC   SAVKEY,KEY          SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),BILLREC                                                   
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         CLC   ESTHDR(8),KEY       ALREADY THERE?                               
         BNE   PRB02A                                                           
         MVC   KEY,SAVKEY          RESTORE KEY                                  
         B     PRB02E                                                           
*                                                                               
PRB02A   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                ESTIMATE NOT ON FILE                         
         GOTO1 GETEST                                                           
         MVC   KEY,SAVKEY          RESTORE KEY                                  
         GOTO1 HIGH                RESTORE READ HIGH                            
*                                                                               
         DROP  RF                                                               
*                                                                               
PRB02E   DS    0H                                                               
         XC    EEUSER1,EEUSER1     CLEAR                                        
         XC    EEUSER2,EEUSER2                                                  
         XC    UCOMDATA,UCOMDATA                                                
*                                                                               
*        GET INTERNAL ORDER NUMBER FROM PRD/EST - USER 2                        
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'E',ADEST),0,(0,EEUSER2)            
*                                                                               
*                   NOW DEFAULT IS GET EEUSER1 FROM BRAND                       
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'E',ADEST),(0,EEUSER1),0            
*                                                                               
*                                                                               
*                                                                               
PRB05    DS    0H                                                               
         TM    BILSTAT,X'02'       IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT5+1,C'C'        NO, ARE WE SKIPPING OTHERS                   
         BE    PRBRTNX                                                          
PRB09    DS    0H                                                               
         CLI   QOPT5+1,C' '          CHECK AOR OPTION                           
         BE    PRB09D                                                           
         CLI   QOPT5+1,C'A'          ONLY AOR                                   
         BNE   PRB09B                                                           
         TM    BILSTAT,X'20'                                                    
         BNO   PRBRTNX                                                          
         B     PRB09D                                                           
*                                                                               
PRB09B   CLI   QOPT5+1,C'B'          AOR AND AOR/CLIENT                         
         BNE   PRB09C                                                           
         TM    BILSTAT,X'30'                                                    
         BNO   PRBRTNX                                                          
         B     PRB09D                                                           
*                                                                               
PRB09C   CLI   QOPT5+1,C'X'          SEE IF EXCLUDING AOR                       
         BNE   PRB09D                                                           
         TM    BILSTAT,X'20'                                                    
         BO    PRBRTNX                                                          
*                                                                               
PRB09D   DS    0H                                                               
         GOTO1 VSPBVAL,DMCB,(C'B',BILLREC),SPBVALD                              
*                                 SET MYGST AND MYPST AND MYHST                 
*****    ZAP   MYGST,=P'0'                                                      
*****    ZAP   MYPST,=P'0'                                                      
         L     R0,SPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,SPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,SPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYHST,DUB                                                        
*                                                                               
         CLC   BKEY(10),LSTBLKY      IF FIRST FOR EST/MOS                       
         BE    PRB1                                                             
         MVI   REVSW,C' '          SET NOT A REVISION                           
         MVC   LSTBLKY,BKEY                                                     
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                  ADD TO ESTIMATE AMTS                         
*                                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   BRETAIL,0                                                        
         BE    PRB2C                                                            
*                                  RETAIL BILL                                  
         CLI   BRETAIL,X'81'       IGNORE CORP BILLS                            
         BE    PRBRTNX                                                          
         MVI   RETAIL,C'Y'                                                      
*                                                                               
*                                                                               
PRB2C    DS    0H                                                               
*                                                                               
         CLC   BKEYCLT,LASTKCLT     SEE IF SAME CLIENT                          
         BE    PRB2C2                                                           
*                                                                               
         MVC   CLTINVS,INVPARS+8    SAVE LAST CLIENT'S TOTAL                    
*                                                                               
         SR    R0,R0            RESET INVOICE LIST BINSRCH PARS                 
         L     R1,=A(INVTAB)    FOR NEW CLIENT I REALLY PROCESS                 
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,4                                                             
         LA    R4,0                                                             
         L     R5,=A(INVMAX)     INVMAX IS 10,000 INVOICE PER CLT               
         STM   R0,R5,INVPARS                                                    
*                                                                               
*                                  USE BINSRCH TO ADD TO INVTAB                 
PRB2C2   DS    0H                                                               
         MVC   WORK(2),BKEYYSRV     YM MOS                                      
         MVC   WORK+2(2),BKEYINV                                                
         GOTO1 BINSRCH,INVPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   QOPT3,C'N'          NO PRINTING                                  
         BE    PRB26                                                            
*                                                                               
PRB2C5   CLC   BKEYCLT(5),LASTKCLT CHK FOR SAME CLT + PRD                       
         BE    PRB3                YES                                          
         CLC   BKEYCLT,LASTKCLT    CHK FOR CHANGE IN CLT                        
         BNE   PRB2A               FORCE CHG IN PRD ALSO                        
         CLC   BKEYPRD,LASTKPRD                                                 
         BE    PRB2D                                                            
PRB2A    CLI   LASTKPRD,0                                                       
         BE    PRB2D                                                            
PRB2B    DS    0H                                                               
         MVC   BLPRD,LASTKPRD                                                   
         MVC   BLINE+5(18),=C'**PRODUCT TOTALS**'                               
         LA    R3,PRDTOTS                                                       
         MVI   TOTSW,1                                                          
         BRAS  RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
         LA    R3,PRDTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         CLI   ENDRPTSW,C'E'       COMING FROM LBLR OR LBOFF ?                  
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
*                                                                               
         CLC   BKEYCLT,LASTKCLT    SEE IF NEW CLIENT ALSO                       
         BNE   PRB2D               IF YES THEN DON'T CHK PROFILE                
*                                                                               
         CLI   PROGPROF+1,C'Y'     CHK PAGE FOR PRODUCT                         
         BNE   PRB2D                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   NEWCLT,C'Y'         SO CLIENT WILL REPRINT                       
*                                  FOR EACH PRODUCT                             
*                                                                               
*                                                                               
PRB2D    CLC   BKEYCLT,LASTKCLT                                                 
         BE    PRB3                                                             
*                                                                               
         CLI   LASTKCLT,0          SEE IF FIRST TIME                            
         BNE   PRB2F                                                            
         MVC   LASTPROF,PROGPROF   MUST SAVE PROFILE                            
         B     PRB3                                                             
*                                                                               
PRB2F    MVC   BLPRD,CLT                                                        
         XC    LASTKEY,LASTKEY                                                  
         MVC   BLINE+5(17),=C'**CLIENT TOTALS**'                                
*                                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,CLTINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
*                                                                               
         LA    R3,CLTTOTS                                                       
         MVI   TOTSW,1                                                          
         BRAS  RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
*                                  FIRST CHECK PROFILE OF LAST CLIENT           
         CLI   LASTPROF+0,C'Y'     CHK NEW PAGE FOR CLIENT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   LASTPROF+1,C'Y'     OR NEW PAGE FOR PRODUCT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+0,C'Y'     CHK NEW PAGE FOR CLIENT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+1,C'Y'     OR NEW PAGE FOR PRODUCT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   LASTPROF,PROGPROF                                                
*                                                                               
***OFF                                                                          
         CLI   QCLT,C'$'             SEE IF DOING OFFILCE LIST                  
         BNE   PRB2M                 ROLL CLTTOTS TO OFFTOTS                    
         LA    R3,CLTTOTS                                                       
         LA    R4,OFFTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         L     R0,OFFINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,OFFINVS                                                       
         B     PRB2P                                                            
***OFF                                                                          
PRB2M    LA    R3,CLTTOTS                                                       
         LA    R4,REQTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         L     R0,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,REQINVS                                                       
PRB2P    LA    R3,CLTTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
*                                                                               
         CLI   ENDRPTSW,C'E'       COMING FROM LBLR OR LBOFF ?                  
         BE    PRBRTNX             YES - EXIT - WILL RETURN TO                  
*                                    REQTOTALS OR OFFICE TOTALS                 
PRB3     DS    0H                                                               
         CLI   NEWCLT,C'Y'         SEE IF NEW CLIENT                            
         BNE   PRB4                                                             
         MVC   P,SVMID                                                          
         MVC   P2,SVMID2                                                        
         MVI   ALLOWLIN,5                                                       
         MVI   NEWCLT,C'N'                                                      
         MVI   SPACING,2                                                        
         BRAS  RE,PRNT                                                          
*                                                                               
PRB4     DS    0H                                                               
         MVC   LASTKEY,BKEY                                                     
         SPACE 2                                                                
*                                  CREATE PRINT LINE(S)                         
         MVC   BLPRD,BKEYPRD            PRD                                     
*                                                                               
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         MVC   BLEST(3),WORK+2          EST                                     
*                                                                               
PRB8     DS    0H                                                               
PRB9     GOTO1 DATCON,DMCB,(3,BKEYYSRV),(9,BLPER)                               
*                                                                               
         B     PRB12                                                            
*                                                                               
PRB12    DS    0H                                                               
*                                                                               
         GOTO1 VSFMTINO,DMCB,BDATE,(2,BKEYINV),                        X        
               (QMED,B1PROF),B1XPROF                                            
*                                                                               
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)   SAVE FULL INVOICE NUMBERR                       
*                                                                               
         L     RF,DMCB+4        ADDRESS OF "SHORT" FORMAT                       
         MVC   DSINVNO(7),0(RF)     MAY INCLUDE DASH                            
******   MVC   BLINO(2),0(RF)                                                   
         MVC   BLINO+2(4),3(RF)     DON'T MOVE '-'                              
         L     RF,DMCB+8        ADDRESS OF Y/M                                  
         MVC   BLINO(2),0(RF)                                                   
         MVC   DINVNO(6),BLINO                                                  
*                                                                               
PRB13X   DS    0H                                                               
*            SEE IF RETAIL CORP OR DISTRIBUTOR BILL                             
         CLI   BRETAIL,X'02'                                                    
         BE    PRB13X2                                                          
         CLI   BRETAIL,X'01'                                                    
         BNE   PRB13X5                                                          
*                                                                               
PRB13X2  MVC   P2+BLRDATD(12),=C'RETAIL ACCT='                                  
         MVC   P2+BLRDATD+12(12),BRETACCT                                       
*                                                                               
PRB13X5  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(5,BLRDAT)            RUN DATE             
*                                                                               
         GOTO1 DATCON,(R1),(0,BQDATE),(7,BLBDAT)        BILL DATE               
*                                                      TYPE OF BILL             
         MVC   BLTYP(2),BTYPE                                                   
         LA    R3,BLTYP+2                                                       
PRB14    DS    0H                                                               
         MVI   0(R3),C'-'                                                       
*                                                                               
         TM    BILSTAT,X'02'           FOR COMMISSION ONLY BILL                 
         BZ    PRB15                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    BILSTAT,X'20'         SEE IF ALSO AOR                            
         BO    PRB14D                                                           
         MVC   1(3,R3),=C'COM'                                                  
*                                                                               
PRB14D   CLI   QOPT5+1,C'C'        TEST TO LEAVE NET                            
         BE    *+10                                                             
         ZAP   SPBVNETP,=P'0'      NO, SET NET = 0 (AC = RCVBL)                 
         B     PRB16                                                            
*                                                                               
PRB15    DS    0H                                                               
******   MVC   1(3,R3),=C'ADJ'                                                  
******   CLI   PBILSEP,C'A'                                                     
******   BE    PRB16                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    BILSTAT,X'20'         AOR BILL                                   
         BO    PRB16                                                            
         MVC   1(3,R3),=C'UFC'                                                  
         TM    BILSTAT,X'01'          UP FRONT COMMISSION                       
         BO    PRB16                                                            
         MVC   1(3,R3),=C'NET'                                                  
         TM    BILSTAT,X'08'          UP FRONT COMMISSION - NET                 
         BO    PRB16                                                            
*                                                                               
         MVC   1(3,R3),=C'REG'                                                  
*                                                                               
PRB16    DS    0H                                                               
*                                                                               
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
*                                                                               
**NEW 3/17/89                                                                   
         CLI   QOPT5+1,C'A'        SEE IF AOR ONLY                              
         BE    PRB16A              YES - THEN INCLUDE IN TOTALS                 
         TM    BILSTAT,X'20'       SEE IF AOR BILL                              
         BZ    PRB16B                                                           
PRB16A   ZAP   BILTOTS(6),=P'0'                                                 
         ZAP   BILTOTS+6(6),=P'0'                                               
         ZAP   BILTOTS+12(6),=P'0'                                              
         ZAP   BILTOTS+18(6),BACTP       SET AC TO RCVBL                        
         ZAP   BILTOTS+24(6),BACTP       ACTUAL                                 
         AP    BILTOTS+24(6),MYGST                                              
         AP    BILTOTS+24(6),MYPST                                              
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
         B     PRB16E                                                           
*                                                                               
*        SET MATS IN 5 FIELDS FOR ACCUMES                                       
*        (MATCH 5 PRINT COLUMNS)                                                
*                                                                               
PRB16B   ZAP   BILTOTS+0*BPLEQ(BPLEQ),SPBVGRSP     GROSS                        
         ZAP   BILTOTS+1*BPLEQ(BPLEQ),SPBVNETP     NET                          
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'                                     
         SP    BILTOTS+2*BPLEQ(BPLEQ),BACTP        CD IS -(RCVBL)               
*                                                                               
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'        CD IS 0                      
*                                                                               
PRB16C   ZAP   BILTOTS+4*BPLEQ(BPLEQ),BACTP        RCVBL                        
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYGST        ADD GST TO BILL AMT          
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYPST        ADD PST TO BILL AMT          
         ZAP   BILTOTS+5*BPLEQ(BPLEQ),MYGST        AND GST TOTALS               
         ZAP   BILTOTS+6*BPLEQ(BPLEQ),MYPST        AND PST TOTALS               
*                                                                               
         ZAP   DUB,BACTP                                                        
         TM    BILSTAT,X'02'       UNLESS COMMISSION ONLY BILL                  
         BNZ   PRB16D                                                           
*                                                                               
PRB16D   DS    0H                                                               
         ZAP   BILTOTS+3*BPLEQ(BPLEQ),DUB          FOR AC CALC                  
         SP    BILTOTS+3*BPLEQ(BPLEQ),SPBVNETP     -NET = TRUE AC               
*                                                                               
PRB16E   LA    R3,BILTOTS                                                       
         BRAS  RE,FMTAMTS               $ AMOUNTS                               
*                                                                               
         BRAS  RE,PRNT                                                          
*                                                                               
*                                                                               
*        DON'T BOTHER WITH FORMULA OPTION FOR NOW                               
*                                                                               
         B     PRB22                                                            
*                                                                               
*****    CLI   QOPT1,C'F'               FORMULA OPTION                          
*****    BNE   PRB22                    NO                                      
*****    OC    PBILBASA(5),PBILBASA                                             
*****    BZ    PRB22                    NO FORMULA                              
*****    CLI   PBILBASA,5          DONT PRINT IF G-CD +0 PCT                    
*****    BNE   *+14                                                             
*****    OC    PBILADJ,PBILADJ                                                  
*****    BZ    PRB22                                                            
*****    LA    R3,BLINO                                                         
*****    MVC   0(8,R3),=C'FORMULA='                                             
*****    SR    RF,RF                                                            
*****    IC    RF,PBILBASA                                                      
*****    SLL   RF,2                                                             
*****    LA    RF,ADJWRDS-4(RF)                                                 
*****    MVC   9(4,R3),0(RF)                                                    
*****    LA    R3,11(R3)                                                        
*****    CLI   1(RF),C' '                                                       
*****    BE    *+8                                                              
*****    LA    R3,3(R3)                                                         
*****    MVI   1(R3),C'0'                                                       
*****    LA    R0,1                                                             
*****    OC    PBILADJ,PBILADJ                                                  
*****    BZ    PRB18                                                            
*****    EDIT  (B3,PBILADJ),(8,1(R3)),5,ALIGN=LEFT                              
*****    LR    R4,R3                                                            
*****    AR    R4,R0               POINT TO END                                 
**B17    DS    0H                                                               
*****    LR    R5,R4                                                            
*****    CLI   0(R4),C'0'                                                       
*****    BE    PRB17D                                                           
*****    CLI   0(R4),C'.'                                                       
*****    BE    PRB17B                                                           
*****    B     PRB18                                                            
**B17B   LA    R4,1                END SCAN ON .                                
**B17D   BCTR  R0,R0               SHORTEN LENGTH                               
******   MVI   0(R5),C' '                                                       
**B17F   BCT   R4,PRB17                                                         
******                                                                          
**B18    DS    0H                                                               
*****    MVI   0(R3),C'+'                                                       
*****    TM    PBILADJ,X'80'                                                    
*****    BZ    *+8                                                              
*****    MVI   0(R3),C'-'                                                       
*****                                                                           
*****    AR    R3,R0                                                            
*****    MVC   2(6,R3),=C'PCT OF'                                               
*****    SR    RF,RF                                                            
*****    IC    RF,PBILBASB                                                      
*****    SLL   RF,2                                                             
*****    LA    RF,ADJWRDS-4(RF)                                                 
*****    MVC   9(4,R3),0(RF)                                                    
*****    MVI   MAXLINES,99                                                      
*****    BRAS  RE,PRNT                                                          
*                                                                               
PRB22    DS    0H                                                               
         CLI   QOPT2,C'R'               REVERSALS OPTION                        
         BNE   PRB26                    NO                                      
         B     PRB26                                                            
*                                                                               
PRB26    DS    0H                                                               
         LA    R3,BILTOTS                                                       
         LA    R4,PRDTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         LA    R3,BILTOTS                                                       
         LA    R4,CLTTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         MVI   RACTSW,1            SET REQUEST ACTIVITY                         
         MVI   OACTSW,1            SET REQUEST ACTIVITY                         
*                                                                               
PRB28    DS    0H                                                               
*******  CLI   QOPT4,C'Y'           SEE IF NOT PUTTING ON FILE                  
*******  BNE   PRBRTNX                                                          
         BRAS  RE,POSTB             POST BILL TO TABLE                          
PRBRTNX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         SPACE 3                                                                
ADJWRDS  DS    0C                                                               
         DC    C'G   '                                                          
         DC    C'N   '                                                          
         DC    C'    '                                                          
         DC    C'    '                                                          
         DC    C'G-CD'                                                          
         DC    C'N-CD'                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        FORMAT $ AMTS                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
FMTAMTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   STAGSW,0                                                         
         CP    0(6,R3),=P'9999999999'   COMPARE GROSS TO 100 MILLION            
         BL    *+8                                                              
         MVI   STAGSW,1            SET TO STAGGER TOTALS                        
*                                                                               
         LA    R5,0(R3)            GROSS                                        
         LA    R4,BLGRS                                                         
         LH    R1,=H'-1'          USED WHEN STAGGERING                          
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,6(R3)           NET                                           
         LA    R4,BLNET                                                         
         LA    R1,131              NEXT LINE -1                                 
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,12(R3)           CD                                           
         LA    R4,BLCD                                                          
         LH    R1,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,18(R3)           AC                                           
         LA    R4,BLAC                                                          
         LA    R1,131              NEXT LINE-1                                  
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,24(R3)           ACTUAL                                       
         LA    R4,BLBIL                                                         
         LH    R1,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,30(R3)           GST (PAST IS AT 36(R3))                      
         LA    R4,BLGST                                                         
         BAS   RE,FMTEDT8       PRINT PST UNDER GST                             
*                                                                               
FMTAX    MVI   TOTSW,0                                                          
*SMY*    B     EXIT                                                             
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         SPACE 2                                                                
FMTEDT   DS    0H                                                               
         CLI   STAGSW,1       SEE IF STAGGERING                                 
         BNE   FMTEDT5                                                          
         AR    R4,R1          ADJUST R4 FOR STAGGERING                          
         EDIT  (P6,0(R5)),(16,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   14(R4),C'C'                                                      
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   15(R4),C'*'                                                      
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
FMTEDT5  EDIT  (P6,0(R5)),(15,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   13(R4),C'C'                                                      
         BER   RE                                                               
         MVI   13(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         BR    RE                                                               
         SPACE 3                                                                
FMTEDT8  DS    0H               SPECIAL EDT FOR GST AND PST                     
         EDIT  (P6,0(R5)),(14,0(R4)),2,COMMAS=YES,CR=YES                        
         EDIT  (P6,6(R5)),(14,132(R4)),2,COMMAS=YES,CR=YES                      
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   12(R4),C'C'                                                      
         BE    FMTEDT8C                                                         
         MVI   12(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BE    FMTEDT8C                                                         
         MVI   13(R4),C'*'                                                      
*                                                                               
FMTEDT8C DS    0H                                                               
         CLI   12+132(R4),C'C'                                                  
         BER   RE                                                               
         MVI   12+132(R4),C'*'                                                  
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   13+132(R4),C'*'                                                  
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      PRINT ROUTINE                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
PRNT     NTR1  BASE=*,LABEL=*                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,0                                                       
******   CLI   PAGYNAT,C'C'       SEE IF CANADIAN                               
******   BNE   *+8                ALWAYS IS FOR LABATTS                         
         MVI   RCSUBPRG,50                                                      
*                                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    PRNT2                                                            
         ZIC   R1,LINE                                                          
         ZIC   R0,ALLOWLIN                                                      
         AR    R1,R0                                                            
         STC   R1,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    PRNT90                                                           
PRNT2    DS    0H                                                               
         CLI   MODE,REQLAST          REQUEST TOTALS                             
         BNL   PRNT90                                                           
         CLI   QCLT,C'$'             SEE IF DOING OFFICE LIST REQ               
         BNE   PRNT3                                                            
         MVC   HEAD5(8),=C'OFFICE X'                                            
         MVC   HEAD5+7(2),SAVCOFF                                               
         B     PRNT4                                                            
*                                                                               
PRNT3    CLI   QCLT,C'*'                                                        
         BNE   PRNT4                                                            
         MVC   HEAD5(8),=C'OFFICE X'                                            
         MVC   HEAD5+7(1),QCLT+1                                                
         B     PRNT4                                                            
*                                                                               
PRNT4    DS    0H                                                               
         CLI   QOPT5+1,C'A'       TEST AOR ONLY                                 
         BNE   PRNT5                                                            
         MVC   HEAD6(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
PRNT5    DS    0H                                                               
         CLI   QOPT5+1,C'B'       AOR AND AOR/CLIENT                            
         BNE   PRNT5B                                                           
         MVC   HEAD6(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
PRNT5B   DS    0H                                                               
         CLI   QOPT5+1,C'X'       NON=AOR BILLS ONLY                            
         BNE   PRNT6                                                            
         MVC   HEAD6(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
PRNT6    DS    0H                                                               
         CLI   QOPT5+1,C'C'       COMMISSION ONLY BILLS                         
         BNE   PRNT7                                                            
         MVC   HEAD6(25),=C'**COMMISSION ONLY BILLS**'                          
*                                                                               
PRNT7    DS    0H                                                               
PRNT90   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   MAXLINES,SAVMAX                                                  
PRNTXIT  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      ROLTOTS                                        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
ROLTOTS  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,7           FOR BCT                                           
ROLTOTS2 DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,ROLTOTS2                                                      
ROLTOTSX DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      CLRTOTS                                        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
CLRTOTS  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,7                                                             
         ZAP   0(6,R3),=PL6'0'                                                  
         LA    R3,6(R3)                                                         
         BCT   R0,*-10                                                          
CLRTOTSX DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
         L     RA,0(R1)                                                         
         USING SPWORKD,RA                                                       
         LA    R8,SPACEND                                                       
         USING SPLTWRKD,R8                                                      
         L     R4,4(R1)        ADDRESS OF PPGREC                                
         USING PPGRECD,R4                                                       
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVC   DLCBFLD,SPACES    CLEAR FIELD                                    
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
         CLI   MODE,RUNLAST       SEE IF END OF REPORT                          
         BE    DNRPTEND                                                         
*                                                                               
         CLI   MODE,REQFRST       SEE IF I NEED TO INTIALIZE                    
         BNE   DNMAIN                                                           
*                                                                               
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD                                                           
*                                                                               
*****    MAY DO "HEADER TITLES" HERE                                            
*                                                                               
         B     DNXIT                                                            
*                                                                               
*****                                                                           
*****    DOWNLOAD BILLING INFO HERE                                             
*****                                                                           
*****                                                                           
*                                                                               
DNMAIN   DS    0H                                                               
         MVI   DOWNACT,C'Y'        SET DOWN ACTIVITY                            
*                                  RECORD INDICATOR                             
         MVI   DLCBFLD,C'H'        HEADER                                       
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  INVOICE NUMBER                               
         MVC   DLCBFLD(L'PPGINVNO),PPGINVNO                                     
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  INVOICE DATE (MMDDYY)                        
         MVC   DLCBFLD(6),PPGIDATE                                              
*                                                                               
         MVI   DLCBFLD+6,X'4F'     SINCE NEXT FIELD IS EMPTY                    
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  VENDOR NUM                                   
         MVC   DLCBFLD(5),=C'38796'                                             
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  VENDOR SITE CODE                             
         MVC   DLCBFLD(8),=C'LTORONTO'                                          
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  INVOICE AMOUNT                               
*SMY*    ZAP   SVBILL,PPGBAMT      AMOUNT DUE (INCLUDES GST/PST/HST)            
*SMY*    NI    PPGBAMT+5,X'F0'                                                  
*SMY*    OI    PPGBAMT+5,X'0C'     MAKE POSITIVE                                
*                                                                               
         MVI   DLCBFLD,C'0'                                                     
         CP    PPGBAMT,=P'0'       SEE IF ZERO AMT DUE                          
         BE    DNM5                                                             
*                                                                               
         ST    R1,SAVER1           SAVE R1                                      
         EDIT  (P6,PPGBAMT),(11,WORK+20),FLOAT=-,ALIGN=LEFT                     
         L     R1,SAVER1           RESTORE R1                                   
*SMY*    ZAP   PPGBAMT,SVBILL      RESTORE FOR LINE ITEMS                       
         LR    RF,R0               LENGTH OF DATA OUTPUT                        
         BCTR  RF,0                FOR EXECUTED MOVE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),WORK+20                                               
*                                                                               
DNM5     MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  SOURCE                                       
         MVC   DLCBFLD(11),=C'M2U_EXT_INV'                                      
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  ORG ID                                       
         MVC   DLCBFLD(2),=C'85'                                                
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  CURRENCY                                     
         MVC   DLCBFLD(3),=C'CAD'                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  VOUCHER NUMBER                               
*                   J FOLLOWED BY LAST SIX DIGITS OF INVOICE NUMBER             
         MVI   DLCBFLD,C'J'                                                     
         MVC   DLCBFLD+1(6),PPGINVNO                                            
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  BATCH NUMBER                                 
*                                  J FOLLOWED BY BILLING MONTH (MMYY)           
         MVI   DLCBFLD,C'J'                                                     
         MVC   DLCBFLD+1(4),PPGMOB    MMYY                                      
         MVC   DLCBFLD+5(2),=X'4F4F'  SINCE NEXT 2 FIELDS ARE EMPTY             
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  TERMS NAME                                   
         MVC   DLCBFLD(06),=C'NET 60'                                           
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL     SEND END OF LINE                             
         GOTO1 VDLFLD                                                           
         EJECT                                                                  
*                                                                               
*        LINES RECORD(S)                                                        
*                                                                               
         MVI   SEQNUM,C'1'                                                      
*SMY*    MVI   TAXSW,0                                                          
         ZAP   MYPACK,PPGBAMT      START WITH BILL AMOUNT                       
         SP    MYPACK,PPGGST       SUBTRACT GST/HST/PST                         
*SMY*    SP    MYPACK,PPGPST                                                    
*SMY*    ZAP   MYPPOS,PPGBAMT                                                   
*SMY*    SP    MYPPOS,PPGGST                                                    
*SMY*    SP    MYPPOS,PPGPST                                                    
*SMY*    NI    MYPPOS+5,X'F0'      MAKE POSITIVE                                
*SMY*    OI    MYPPOS+5,X'0C'                                                   
*        ***NOTE* MAY WANT TO ELIMINATE LINE "2" IF GST ZERO *****              
DNPLINS  DS    0H                                                               
*                                  RECORD INDICATOR                             
         MVI   DLCBFLD,C'L'        LINE                                         
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  INVOICE NUMBER                               
         MVC   DLCBFLD(L'PPGINVNO),PPGINVNO                                     
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  LINE NUMBER                                  
         MVC   DLCBFLD(1),SEQNUM                                                
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  LINE TYPE LOOKUP CODE                        
         MVC   DLCBFLD(4),=C'ITEM'                                              
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  AMOUNT                                       
*                                  MYPACK=AMOUNT MINUS GST                      
         CLI   SEQNUM,C'1'         FIRST LINE ?                                 
         BNE   DNAMT20             NO                                           
*                                                                               
         MVI   DLCBFLD,C'0'                                                     
         CP    MYPACK,=P'0'                                                     
         BE    DNAMTX                                                           
*                                                                               
         ST    R1,SAVER1           SAVE R1                                      
         EDIT  (P6,MYPACK),(11,WORK+20),FLOAT=-,ALIGN=LEFT                      
         L     R1,SAVER1           RESTORE R1                                   
         LR    RF,R0               LENGTH OF DATA OUTPUT                        
         BCTR  RF,0                FOR EXECUTED MOVE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),WORK+20                                               
         B     DNAMTX                                                           
*                                                                               
DNAMT20  DS    0H                  GST AMOUNT ONLY                              
*                                                                               
         MVI   DLCBFLD,C'0'                                                     
         CP    PPGGST,=P'0'                                                     
         BE    DNAMTX                                                           
*                                                                               
         ST    R1,SAVER1           SAVE R1                                      
         EDIT  (P6,PPGGST),(11,WORK+20),FLOAT=-,ALIGN=LEFT                      
         L     R1,SAVER1           RESTORE R1                                   
         LR    RF,R0               LENGTH OF DATA OUTPUT                        
         BCTR  RF,0                FOR EXECUTED MOVE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),WORK+20                                               
DNAMTX   DS    0H                                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  DESCRIPTION                                  
         MVC   DLCBFLD(08),=C'M2U FEED'                                         
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  AMOUNT INCLUDES TAX FLAG                     
         MVI   DLCBFLD,C'Y'                                                     
         MVI   DLCBFLD+1,X'4F'     SINCE NEXT FIELD IS EMPTY                    
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  DIST CODE CONCATENATED (GL CODE)             
*  USES BRAND ESTIMATE USER FIELD 1 (PPGEU1) AS PART OF LINE "1"                
*                                                                               
         CLI   SEQNUM,C'1'         FIRST LINE ?                                 
         BNE   DNGL20              NO - "FIXED" GL CODE                         
*                                                                               
         MVC   DLCBFLD(3),=C'31.'                                               
         MVC   DLCBFLD+03(03),PPGMGR1                                           
         MVC   DLCBFLD+06(31),PPGEU1                                            
         MVC   DLCBFLD+37(03),=C'.00'                                           
         B     DNGLX                                                            
DNGL20   DS    0H                                                               
         MVC   DLCBFLD(L'GLCODE2),GLCODE2                                       
DNGLX    DS    0H                                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
* LOOP AND END LOGIC HERE                                                       
*                                                                               
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   SEQNUM,C'2'         WAS SEQNUM 2                                 
         BE    DNXIT               YES - END OF THIS SEGMENT                    
         MVI   SEQNUM,C'2'         ELSE SET IT TO 2                             
         B     DNPLINS             AND PROCESS LINE "2"                         
*                                                                               
DNRPTEND DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,DLCBEOR     SET END OF REPORT                            
         GOTO1 VDLFLD                                                           
*                                                                               
DNXIT    DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
DNPRINT  NTR1                                                                   
         MVC   SVLINE,LINE                                                      
         MVC   SVFORCEH,FORCEHED                                                
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
         MVC   LINE,SVLINE             RESTORE LINE                             
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
         MVI   RCWHATPR,1     RESET TO FIRST                                    
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
DMTHSW   DS    CL1         M IF PROCESSING MOS LINE                             
SAVER1   DS    F                                                                
MYWORK   DS    CL12                                                             
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
* NOTE - BELOW IS 40 CHARACTER GL CODE - MAY ONLY WANT 35  ******               
GLCODE2  DC    C'31.911.000.11284.000.0000.0000.000000.00'                      
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    X'4F'       FIELD DELIMITER - VERTICAL BAR                       
EOTCHR   DC    X'00'       END OF TEXT FIELD DELIMITER                          
EOTALT   DC    X'00'       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'00'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    X'00'       END OF REPORT CHR                                    
*                                                                               
*        NORMAL DOWNLOAD FIELDS ARE:                                            
*        C' ',C'"',C'''',X'5E',C':'     5E IS SEMI-COLON                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*                                                                               
         TITLE 'DSECTS AND WORK AREAS'                                          
SPLTWRKD DSECT                                                                  
*                                                                               
VDLFLD   DS    A                                                                
VDOWNLD  DS    A                                                                
VDDUCOM  DS    A                                                                
VGETUSER DS    A                                                                
VSPBVAL  DS    A                                                                
VSFMTINO DS    A                                                                
ASPECS   DS    A                                                                
ATOTS    DS    A                                                                
RELO     DS    A                                                                
LASTPROF DS    CL16                                                             
SVMKEY   DS    CL32                                                             
SVBAMT   DS    PL6                                                              
*                                                                               
*                                                                               
TAXSW    DS    XL1                                                              
ERRORSW  DS    XL1                                                              
SAVVEND  DS    CL09                                                             
SAVFILE  DS    CL13                                                             
SVMOS    DS    CL4                                                              
MYSVEST  DS    CL3                                                              
SEQNUM   DS    CL1                                                              
SAVCOFF  DS    CL2                                                              
SVLINE   DS    X                                                                
SVFORCEH DS    CL1                                                              
MYPPOS   DS    PL6                                                              
MYPACK   DS    PL6                                                              
*                                                                               
MYGST    DS    PL8                                                              
MYPST    DS    PL8                                                              
MYHST    DS    PL8                                                              
MYDUB    DS    PL8                                                              
*                                                                               
EATOTGRS DS    PL6                                                              
EATOTAC  DS    PL6                                                              
EATOTACT DS    PL6                                                              
EATOTNET DS    PL6                                                              
*                                                                               
SVKEY    DS    CL64         KEY AND KEYSAVE                                     
*                                                                               
RECLEN   DS    H                                                                
ELCODE   DS    X                                                                
DOWNACT  DS    C                                                                
RETAIL   DS    C                                                                
REVSW    DS    C                   REVISION SWITCH                              
BYTE2    DS    X                                                                
LSTBLKY  DS    XL13                LAST BILL KEY                                
B1XPROF  DS    CL16                                                             
B1PROF   DS    CL16                                                             
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                FULL FORMAT INVOICE NUMBER                   
DSINVNO  DS    CL7                  MN-NNNN (OR YMMNNNN)                        
*                                                                               
SAVKEY   DS    CL32                                                             
*                                                                               
CONTROLS DS    0XL4                                                             
CONTROL1 DS    X                                                                
CONTROL2 DS    X                                                                
CONTROL3 DS    X                                                                
CONTROL4 DS    X                                                                
OFFICE   DS    CL1                                                              
SVQST    DS    CL6                                                              
SVQEND   DS    CL6                                                              
STARTMOS DS    XL2                 START MOS FILTER (BINARY YM)                 
ENDMOS   DS    XL2                 END MOS FILTER (BINARY YM)                   
CNTRY    DS    C                                                                
SVMID    DS    CL132                                                            
SVMID2   DS    CL132                                                            
NEWCLT   DS    C                                                                
NETPAKSW DS    C                                                                
SKIPBILL DS    X                                                                
JWOASW   DS    C                                                                
YEARDIG  DS    XL1                 YEAR DIGIT                                   
DECADE   DS    XL1                                                              
WIAPLSW  DS    C                   WESTERN/APL SWITCH                           
WIQASW   DS    C                   WESTERN/APL QA SWITCH                        
WAPLBTSW DS    C                   YET ANOTHER MODE                             
*                                                                               
CLFILT   DS    CL3                                                              
MEFILT   DS    CL1                                                              
*                                                                               
EEUSER1  DS    CL53        OUTPUT OF GETUSER - ESTIMATE USER 1                  
EEUSER2  DS    CL37                          - ESTIMATE USER 2                  
PPUSER1  DS    CL53        OUTPUT OF GETUSER - PRODUCT USER 1                   
PPUSER2  DS    CL37                          - PRODUCT USER 2                   
*                                                                               
START    DS    CL6                                                              
END      DS    CL6                                                              
SAVMAX   DS    X                                                                
STAGSW   DS    X                                                                
TOTSW    DS    X                                                                
RACTSW   DS    X                                                                
OACTSW   DS    X                                                                
FIRSTSW  DS    X                                                                
ENDRPTSW DS    X                                                                
LASTKEY  DS    CL13                                                             
         ORG   LASTKEY                                                          
         DS    CL2                 00 AND AGY/MED                               
LASTKCLT DS    CL2                                                              
LASTKPRD DS    CL3                                                              
*                                                                               
LASTCPE  DS    CL6                 LAST CLT/PRD/EST                             
*                                                                               
INVPARS  DS    6F                  INVOICE BINSRCH PARS                         
*                                                                               
INVMAX   EQU   50000               MAX INVOICES PER CLT                         
*                                                                               
CLTINVS  DS    F                   CLIENT INVOICES                              
OFFINVS  DS    F                   OFFICE INVOICES                              
REQINVS  DS    F                   REQUEST INVOICES                             
RUNINVS  DS    F                   RUN INVOICES                                 
TRUNINVS DS    F                                                                
*                                                                               
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
*                                                                               
BILTOTS  DS    7PL6                                                             
PRDTOTS  DS    7PL6                                                             
CLTTOTS  DS    7PL6                                                             
OFFTOTS  DS    7PL6                                                             
REQTOTS  DS    7PL6                                                             
RUNTOTS  DS    7PL6                                                             
TRUNTOTS DS    7PL6                FOR TAPE REQS                                
*                                                                               
BPLEQ    EQU   6                   LENGTH OF PACKED ACCUMULATORS                
*                                                                               
BDEHSW   DS    XL1                                                              
BDEH1    DS    CL132                                                            
BDEH2    DS    CL132                                                            
BDEH3    DS    CL132                                                            
BDEH4    DS    CL132                                                            
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
SVBILL   DS    XL256                                                            
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
UCALL    EQU   *-UCTTLS                                                         
USAVKEY  DS    XL13             TO SAVE CURRENT READ SEQUENCE                   
UCOMQ    EQU   *-UCOMBLK                                                        
         SPACE 3                                                                
AMOUNTSD DSECT                                                                  
AMTGRS   DS    PL6                 GROSS                                        
AMTAC    DS    PL6                 ACTUAL COMMISSION                            
AMTACT   DS    PL6                 ACTUAL                                       
AMTNET   DS    PL6                 NET                                          
AMTCNT   DS    PL6                 COUNT                                        
AMTGST   DS    PL6                 GST                                          
AMTPST   DS    PL6                 PST                                          
AMTHST   DS    PL6                 HST                                          
NAMTS    EQU   (*-AMOUNTSD)/6                                                   
*                                                                               
         EJECT                                                                  
BILLINED DSECT                                                                  
BLINE    DS    0C                                                               
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLEST    DS    CL3                                                              
         DS    CL1                                                              
BLPER    DS    CL6                                                              
         DS    CL1                                                              
BLINO    DS    CL6                                                              
         DS    CL1                                                              
BLRDAT   DS    CL8                                                              
         DS    CL1                                                              
BLBDAT   DS    CL5                                                              
         DS    CL1                                                              
BLTYP    DS    CL6                                                              
BLGRS    DS    CL15                                                             
BLNET    DS    CL15                                                             
BLCD     DS    CL15                                                             
BLAC     DS    CL15                                                             
BLBIL    DS    CL15                                                             
BLGST    DS    CL13                                                             
         SPACE 3                                                                
BLESTD   EQU   BLEST-BLINE                                                      
BLPERD   EQU   BLPER-BLINE                                                      
BLRDATD  EQU   BLRDAT-BLINE                                                     
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         ORG   QAREA+49                                                         
QMOS     DS    0CL8                REQUESTED MONTH-OF-SERVICE RANGE             
QMOSSTRT DS    CL4                 START MOS (YYMM)                             
QMOSEND  DS    CL4                 END MOS (YYMM)                               
         ORG                                                                    
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAOR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDGETPROFD                                                     
         EJECT                                                                  
       ++INCLUDE DDUCOMD                                                        
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NECOMBLOK                                                      
         PRINT ON                                                               
         SPACE 3                                                                
         PRINT ON                                                               
*                                                                               
PPGRECD  DSECT                                                                  
PPGMED   DS    CL1           MEDIA (ALSO IN KEY)                                
PPGCLI   DS    CL3           CLIENT (ALSO IN KEY)                               
PPGEST   DS    XL2           ESTIMATE (ALSO IN KEY)                             
PPGIDATE DS    CL6           INVOICE DATE (YYMMDD)                              
PPGINVF  DS    CL10          FULL INVOICE NUMBER                                
PPGMOB   DS    CL4           CONVERTED FROM BDATE TO MMYY                       
PPGCDTYP DS    CL1           CD ITEMS INDICATOR                                 
PPGEU1   DS    CL32          EST USER 1                                         
PPGEU2   DS    CL16          EST USER 2                                         
PPGENAME DS    CL20          ESTIMATE NAME                                      
PPGPU1   DS    CL32          PRD USER 1                                         
PPGMGR1  DS    CL3           REGION FROM PNBMGR1 IN BILLREC                     
PPGINVNO DS    CL6           LAST 6 DIGITS OF INVOICE NUMBER                    
*        THE FIELDS WILL BE THE VALUES FOR THE FIRST BILL                       
*        FOR THE BINSRCH KEY                                                    
*                                                                               
*        THE FIELDS BELOW ARE THE TOTALS FOR THE INVOICE                        
*        (BINSRCH KEY)                                                          
PPGBAMT  DS    PL6                                                              
PPGGST   DS    PL6                                                              
PPGPST   DS    PL6                                                              
*                                                                               
PPGRECL  EQU   *-PPGMED                                                         
*                                                                               
LEDIS    EQU   PPGPU1-PPGMED                                                    
AMTDIS   EQU   PPGBAMT-PPGMED                                                   
GSTDIS   EQU   PPGGST-PPGMED                                                    
PSTDIS   EQU   PPGPST-PPGMED                                                    
*                                                                               
INVTAB   CSECT                                                                  
         ORG   *+(INVMAX*4)                                                     
         DC    X'00'                                                            
*                                                                               
PPGTABLE CSECT                                                                  
         ORG   *+(4000*PPGTLEN)                                                 
*                                                                               
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPREPLT02 01/20/15'                                      
         END                                                                    
