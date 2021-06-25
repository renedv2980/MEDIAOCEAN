*          DATA SET SPPAY01    AT LEVEL 032 AS OF 04/25/16                      
*PHASE T21301A                                                                  
*INCLUDE MEDGET                                                                 
         SPACE 1                                                                
*=============================================================*                 
*==========>                                          <=======*                 
* NOTE PATCH AT STA30 TO IGNORE EXCHANGE RATE LOOKUP ERRORS   *                 
*==========>                                          <=======*                 
*=============================================================*                 
         SPACE 2                                                                
*******************************************************************             
* 29AUG89 READ AND SAVE CANADIAN EXCHANGE RATES                   *             
*         AND REORGANIZE SUBROUTINES TO GET ADDRESSABILITY BACK   *             
* 16MAY90 INCREASE IOAREAS AND ERROR CHECK FOR ACC OFFICE CODE    *             
* 21NOV90 GST SUPPORT - DATES MAY NOT CROSS 12/30/90              *             
* 21APR92 SET UP RECORD DSECT ADDRESSABILITY                      *             
* 01NOV93 VALIDATE PST OPTIONS TO OVERRIDE BUY RECORD             *             
* 18APR96 IGNORE MISSING STATION ADDRESS RECORDS FOR AGENCY CK    *             
* 02AUG96 NOP TEST FOR 1/2 CHARACTER OFFICE CODES                 *             
* 07APR98 IF WESTERN PW OOWR, MAKE SURE ESTIMATE ENTERED          *             
* 08JUN98 MAKE A0 PROFILE Y2K COMPATIBLE                          *             
* 08NOV01 NEW OFFICER CALLS                                       *             
* 19JUL02 MORE CLIENT VALIDAION                                   *             
* 23OCT03 ALLOW PAYING REP IN OFFICE LEVEL STATION MASTER         *             
* 14APR04 PWE 005 - PAYING REP FOR CANADIAN NETWORK               *             
* 05FEB10 MHER - NEW PST/GST OPTIONS                              *             
* 25MAR11 AKAT - SUPPORT NEW AUTOPAY RECORD FORMAT ON XSPFIL      *             
* 19OCT11 AKAT - SET PAYREP TO A "Y" IF PAYING REP IS INPUT       *             
* 22MAR13 AKAT - ADD PE TO PSTLIST                                *             
*******************************************************************             
         TITLE 'T21301 - SPOTPAK PAY PROGRAM - HEADLINE EDITS'                  
T21301   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21301,RR=R8                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T21301+4096,RA                                                   
*                                                                               
         ST    R8,RELO                                                          
*                                                                               
         L     R3,VTWA                                                          
         USING T213FFD,R3                                                       
*                                                                               
         MVI   SVLINST,C'L'        DEFAULT TO DISPLAY BY BUYLINE                
         MVI   SVRANFL,1                                                        
*                                                                               
         OC    SVKEY,SVKEY                                                      
         BNZ   MED                                                              
* READ AGENCY HEADER                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGYALPHA                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYHDRD,R6                                                       
         MVC   SVAPROF,AGYPROF                                                  
         MVC   SVACCOFC,AGYOFC2                                                 
         MVC   SVAFLAG1,AGYFLAG1   X'20' = CTA ACTIVE                           
         MVC   SVAFLAG2,AGYFLAG2   X'40' = DIY TRADE                            
         B     PAY                                                              
         DROP  R6                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
MYEXIT   XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
PAY      DS    0H                                                               
*                                                                               
         LA    R2,PAYERH                                                        
         TM    4(R2),X'20'                                                      
         BO    MED                                                              
*                                                                               
         CLI   5(R2),0                                                          
         BNE   PAY1                                                             
         GOTO1 GLOBBER,DMCB,=C'GETF',(R2),,GLVSPREQ                             
*                                                                               
PAY1     OI    4(R2),X'20'                                                      
*                                                                               
PAY2     CLI   5(R2),0                                                          
         BE    MED                                                              
         GOTO1 GLOBBER,DMCB,=C'PUTF',(R2),,GLVSPREQ                             
         EJECT                                                                  
MED      DS    0H                                                               
         LA    R2,PAYMDH                                                        
         TM    4(R2),X'20'                                                      
         BO    CLT                                                              
*                                                                               
         BRAS  RE,CLRMD                                                         
*                                                                               
         CLI   5(R2),0                                                          
         BNE   MD1                                                              
         GOTO1 GLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                              
         GOTO1 GLOBBER,DMCB,=C'GETD',9(R2),9,GLVSPPAY                           
*                                                                               
MD1      MVI   SVAUTPAY,C'N'                                                    
         MVI   SVXATPAY,C'N'                                                    
         GOTO1 ANY                                                              
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),1                                                          
         BE    MD2                                                              
         CLI   5(R2),5                                                          
         BNE   PAYERR                                                           
         CLC   =C'TEST',9(R2)                                                   
         BE    MD2                                                              
         CLC   =C'!@#',9(R2)                                                    
         BNE   PAYERR                                                           
         MVI   SVAUTPAY,C'Y'                                                    
         CLC   =C'!@##',9(R2)      AUTOPAY OFF THE XSPFIL?                      
         BNE   MD2                 NO                                           
         MVI   SVXATPAY,C'Y'       YES                                          
*                                                                               
MD2      CLI   PAYMD,C'C'          DO NOT ALLOW CANAD 'C'                       
         BNE   *+12                                                             
         CLI   SVAPROF+7,C'C'                                                   
         BE    PAYERR                                                           
*                                                                               
         GOTO1 =V(MEDGET),DMCB,(PAYMD,AGYALPHA),VDATAMGR,WORK,RR=RB             
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BE    PAYERR                                                           
*                                                                               
         MVC   SVKEY(1),WORK                                                    
*                                                                               
         CLC   =C'MC',AGYALPHA                                                  
         BE    *+10                                                             
         MVC   PAYPADR(10),WORK+1                                               
*                                                                               
         OI    4(R2),X'20'                                                      
         GOTO1 GLOBBER,DMCB,=C'PUTD',8(R2),1,GLVSPMD                            
         CLI   5(R2),1                                                          
         BNH   AGYX                                                             
         LLC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         GOTO1 GLOBBER,DMCB,=C'PUTD',9(R2),(R4),GLVSPPAY                        
         SPACE 1                                                                
*=====================================================*                         
* INITIALIZE SECRET                                                             
*=====================================================*                         
         SPACE 1                                                                
AGYX     OC    T213FFD+4(2),T213FFD+4  TEST NEW SECURITY                        
         BNZ   *+14                                                             
         OC    T213FFD+6(2),T213FFD+6  TEST ANY LIMIT ACCESS                    
         BZ    CLT                                                              
* INITIALIZE SECRETD                                                            
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',SECBLK),0                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
CLT      LA    R2,PAYCLH                                                        
         TM    4(R2),X'20'                                                      
         BO    OPT                                                              
*                                                                               
         BRAS  RE,CLRCL                                                         
*                                                                               
         CLI   5(R2),0                                                          
         BNE   CLT1                                                             
         GOTO1 GLOBBER,DMCB,=C'GETF',(R2),,GLVSPCLT                             
*                                                                               
CLT1     GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
*                                                                               
         MVC   QCLT,WORK                                                        
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),2                                                          
         BL    PAYERR                                                           
         CLI   5(R2),3                                                          
         BH    PAYERR                                                           
* GET ADDRESS OF CLPACK                                                         
         GOTO1 VCALLOV,DMCB,0,X'D9000A14'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),QCLT,SVKEY+1                                           
*                                                                               
         CLI   0(R1),0                                                          
         BNE   PAYERR                                                           
* READ CLIENT                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   SVCLTDA,KEY+14                                                   
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         CLI   CPROF+6,C'Y'             DISPLAY CLIENT IN FORMAT "AAN"?         
         BNE   CLT1A                    NO                                      
*                                                                               
* GET ADDRESS OF CLUNPK                                                         
         GOTO1 VCALLOV,DMCB,0,X'D9000A15'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(C'Y',SVKEY+1),WORK                                    
         MVC   QCLT,WORK                CLIENT FORMAT "AAN"                     
*                                                                               
CLT1A    MVC   PAYCLXP(20),CNAME                                                
         XC    PAYCLINM,PAYCLINM                                                
         MVC   PAYCLINM(20),CNAME       MOVE TO ACC POSTING AREA                
         MVC   SVCACCS,CACCESS          SAVE LIMIT ACCESS CODES                 
         MVI   SVMACCS,X'FF'            SUPPRESS MKT LIMIT FOR NOW              
         MVC   SVCPROF,CPROF                                                    
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   SVPWPCT,CPWPCT                                                   
         MVI   SVEOWSD,0           RESET OOWR START DAY                         
         TM    COPT2,COP2TRAD      TEST NON-TBS TRADE CLIENT                    
         BZ    *+8                                                              
         OI    SVAFLAG1,X'02'      SIMULATE TRADE AGENCY FOR TRACY              
         MVC   SVOFFC,COFFICE      SAVE MEDIA OFFICE FOR AUTH VLDTN             
         MVC   SVCACCOF,CACCOFC    SAVE ACC OFFICE TOO                          
         MVC   SVCACCAG,CACCAGY    SAVE ACC AGENCY FROM CLIENT REC              
         MVC   SVCLOCK,CLOCKYM     SAVE CLT LOCK DATE                           
         MVC   SVCOPT2,COPT2                                                    
         MVC   SVCOPT4,COPT4                                                    
         DROP  R6                                                               
*                                                                               
CLT1X    CLI   T213FFD+6,C'+'           TEST MKT LOCKOUT                        
         BE    CLT10                                                            
         BRAS  RE,CALLOFCR                                                      
         EJECT                                                                  
* READ PROGRAM PROFILES *                                                       
         SPACE 1                                                                
CLT10    DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SDAR'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PAYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         XC    SVTRDREP,SVTRDREP                                                
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,WORK+30,VDATAMGR                                  
         MVC   SVTRDREP,WORK+36                                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SA0B'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PAYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         XC    SVPPROFB,SVPPROFB                                                
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVPPROFB,VDATAMGR                                 
         MVI   SVPPROFB+7,C'Y'  <=======  FORCE SUPERPAY OPTIONS                
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SA0A'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PAYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         XC    SVPPROFA,SVPPROFA                                                
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVPPROFA,VDATAMGR                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0A0'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   KEY(6),WORK         SAVE KEY                                     
         MVC   WORK+6(1),PAYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,WORK+24,VDATAMGR                                  
         MVC   SVPPROF,WORK+24     NOTE 16 BYTES SAVED                          
* CONVERT PROFILE DATES TO Y2K FORMAT                                           
         OC    SVPPROF+5(2),SVPPROF+5 SPECIAL OFFICE START                      
         BZ    CLT11A                                                           
         MVC   DUB(2),SVPPROF+5                                                 
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK    MAKE EBCDIC                         
         GOTO1 (RF),(R1),WORK,(3,DUB)       MAKE BINARY                         
         MVC   SVPPROF+5(2),DUB                                                 
*                                                                               
CLT11A   OC    SVPPROF+7(3),SVPPROF+7 MAKEGOOD AS MISSED                        
         BZ    CLT11X                                                           
         MVC   DUB(3),SVPPROF+7                                                 
         GOTO1 VDATCON,DMCB,(3,DUB),WORK                                        
         GOTO1 (RF),(R1),WORK,(3,SVPPROF+7)                                     
*                                                                               
CLT11X   DS    0H                                                               
         MVC   SVREQOPT,SVPPROF+2  SET DEFAULT REQ OPT                          
         SPACE 1                                                                
* GET GROSS/NET FROM AGENCY+OFFICE LEVEL PROFILE                                
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(6),KEY                                                      
**                                                                              
         MVC   WORK+7(3),=X'FFFFFF'     FAKE THE CLIENT OR GETPROF WILL         
         MVI   WORK+10,C'*'             RETURN THE MEDIA LEVEL THINKING         
         MVC   WORK+11(1),SVOFFC        IT MATCHES THE BLANK CLIENT             
**                                                                              
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,WORK2                                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVPPROF(1),WORK2                                                 
*                                                                               
         XC    SVMGDATE,SVMGDATE                                                
         CLI   SVCXTRA+7,C'Y'      TEST MKGD IN MSSD MONTH FEATURE              
         BNE   CLT12                                                            
         OC    SVPPROF+7(3),SVPPROF+7  TEST START DATE PRESENT                  
         BZ    CLT12                   NO                                       
         GOTO1 VDATCON,DMCB,(3,SVPPROF+7),(2,SVMGDATE)                          
*                                                                               
CLT12    CLI   SVPPROF+10,C'C'     TEST CCUSA INTERFACE                         
         BE    *+12                                                             
         CLI   SVPPROFA+3,C'Y'     TEST AUTO ACC POSTING                        
         BNE   CLT30                                                            
         BRAS  RE,GETACC                                                        
*                                                                               
CLT30    CLI   SVPPROFB+5,0        TEST OPTION TO PAY INVOICE AMTS              
         BE    CLT32               NO                                           
         CLI   SVPPROF,C'N'        MUST PAY NET                                 
         BE    CLT32                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPAYACT)                                              
         GOTO1 ERROR                                                            
*                                                                               
CLT32    OI    4(R2),X'20'                                                      
         GOTO1 GLOBBER,DMCB,=C'PUTF',(R2),,GLVSPCLT                             
         B     OPT                                                              
         EJECT                                                                  
OPT      XC    SVTST,SVTST                                                      
         XC    SVTSTSTA,SVTSTSTA                                                
         XC    NEWID,NEWID                                                      
         XC    SVPAYDT,SVPAYDT                                                  
         XC    SVSTALO,SVSTALO                                                  
         MVI   REVSW,C'N'                                                       
         MVI   SVPAYTYP,0                                                       
         MVI   SVXFROPT,0                                                       
         MVC   SVREQOPT,SVPPROF+2     SET DFLT REQ OPT FROM PROFILE             
         MVI   SVAUTOSW,0             RESET AUTO-SWAP OPTIONS                   
         MVI   SVGSTOPT,0                                                       
         MVI   SVGSTVAL,0                                                       
         MVI   SVUNPAY,0                                                        
         MVI   SVOPT31,0                                                        
         MVI   SVSRCCOM,C'Y'                                                    
         XC    SVMKTOV,SVMKTOV     MARKET OVERRIDE                              
         XC    SVOVRPST,SVOVRPST                                                
         XC    SVPSTEL,SVPSTEL                                                  
*                                                                               
         TM    PAYOPH+4,X'20'      TEST VALIDATED                               
         BO    OPT0                                                             
         NI    PAYPRH+4,X'DF'      FORCE PRD/EST-LIN VALIDATION                 
         XC    SVOPTRAN,SVOPTRAN   CLEAR OPTIONS BUYLINE RANGE                  
*                                                                               
OPT0     LA    R2,PAYOPH                                                        
         CLI   5(R2),0                                                          
         BNE   OPTD                                                             
         GOTO1 GLOBBER,DMCB,=C'GETF',(R2),,GLVSPOPT                             
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
OPTD     DS    0H                                                               
         CLC   =C'TEST',PAYMD+1                                                 
         BNE   *+14                                                             
         MVI   SVTSTEST,1                                                       
         MVC   SVTSTLIN,=H'1'                                                   
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
*                                                                               
OPT1     L     RE,AREC2            POINT TO SCANNER TABLE                       
         XC    0(256,RE),0(RE)     CLEAR                                        
         SPACE 1                                                                
*********************************************************                       
** NOTE -- SCANNER ENTRIES ARE NON-STANDARD *34* BYTES **                       
*********************************************************                       
         SPACE 1                                                                
         GOTO1 VSCANNER,DMCB,(12,(R2)),(5,AREC2)                                
         L     R4,AREC2                                                         
         MVI   ERRCD,INVERR                                                     
*                                                                               
OPT2     CLI   0(R4),0                                                          
         BE    OPTX                                                             
*                                                                               
         CLC   =C'GST',12(R4)     TEST TO INCLUDE GST IN AMOUNTS                
         BNE   OPT3                                                             
         CLI   1(R4),0            TEST OVERRIDE GST VALUE                       
         BNE   OPT2A                                                            
         MVI   SVGSTOPT,C'Y'                                                    
         B     OPT20                                                            
*                                                                               
OPT2A    CLI   1(R4),1            TEST ONE CHARACTER INPUT                      
         BNE   OPT2ERR                                                          
         MVC   SVGSTVAL,22(R4)                                                  
         CLI   SVGSTVAL,C'S'                                                    
         BE    OPT20                                                            
         CLI   SVGSTVAL,C'X'                                                    
         BE    OPT20                                                            
         CLI   SVGSTVAL,C'Z'                                                    
         BE    OPT20                                                            
OPT2ERR  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADGSTCD)                                              
         B     PAYERR                                                           
*                                                                               
OPT3     CLC   =C'PST ',12(R4)     INPUT LOOKS LIKE PST=X                       
         BNE   OPT3B                                                            
         OC    SVOVRPST,SVOVRPST   TEST ALREADY ENTERED                         
         BNZ   OPT3ERR                                                          
*                                                                               
         XC    PSTWORK,PSTWORK     BUILD A DUMMY FLDHDR FOR PSTVAL CALL         
         MVI   PSTWORK,48          SET FLDHDR LEN                               
         LA    R1,PSTWORK+8                                                     
         LA    RE,PSTLIST                                                       
         LA    R0,L'PSTLIST/2                                                   
*                                                                               
OPT3A    MVC   0(2,R1),0(RE)       MOVE PROVINCE CODE                           
         MVI   2(R1),C'='                                                       
         MVC   3(1,R1),22(R4)                                                   
         MVI   4(R1),C','                                                       
         LA    R1,5(R1)                                                         
         LA    RE,2(RE)                                                         
         BCT   R0,OPT3A                                                         
*                                                                               
         BCTR  R1,0                                                             
         MVI   0(R1),0             REMOVE TRAILING COMMA                        
         LA    R0,PSTWORK+8                                                     
         SR    R1,R0                                                            
         STC   R1,PSTWORK+5        SET INPUT FIELD LEN                          
         B     OPT3GO                                                           
PSTLIST  DC    C'PQNBNSNFONBCPE'                                                
*                                                                               
OPT3ERR  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(PSTTGTHR)                                              
         B     PAYERR                                                           
*                                                                               
OPT3B    CLI   0(R4),2             TEST 2 CHAR INPUT                            
         BNE   OPT4                                                             
         CLI   SVOVRPST,0                                                       
         BNE   OPT3ERR                                                          
*                                                                               
         LA    RE,PSTLIST          SET IF IT'S A PROVINCE CODE                  
         LA    R0,L'PSTLIST/2                                                   
*                                                                               
OPT3C    CLC   12(2,R4),0(RE)                                                   
         BE    OPT3D                                                            
         LA    RE,2(RE)                                                         
         BCT   R0,OPT3C                                                         
         B     OPT4                NOT A PST PROVINCE CODE                      
*                                                                               
* OK - THERE IS AT LEAST ONE PROVINCE                                           
*                                                                               
OPT3D    XC    PSTWORK,PSTWORK                                                  
         MVI   PSTWORK,8                                                        
         LA    R1,PSTWORK+8                                                     
*                                                                               
OPT3E    LA    RE,PSTLIST                                                       
         LA    R0,L'PSTLIST/2                                                   
         CLI   0(R4),2             TEST PROVINCE CODE LEN                       
         BNE   OPT3FX                                                           
*                                                                               
OPT3F    CLC   12(2,R4),0(RE)                                                   
         BE    OPT3G                                                            
         LA    RE,2(RE)                                                         
         BCT   R0,OPT3F                                                         
*                                                                               
OPT3FX   BCTR  R1,0                BACK UP TO LAST COMMA                        
         MVI   0(R1),C' '                                                       
         LA    R0,PSTWORK+8                                                     
         SR    R1,R0                                                            
         STC   R1,PSTWORK+5        SET INPUT LENGTH                             
         AHI   R4,-34              BACKUP POINTER SO CAN BUMP LATER             
         B     OPT3GO                                                           
*                                                                               
OPT3G    MVC   0(2,R1),12(R4)      MOVE PROVINCE CODE                           
         MVI   2(R1),C'='                                                       
         MVI   ERRCD,BADPSTVL                                                   
         CLI   1(R4),1             TEST LENGTH OF VALUE = 1                     
         BNE   PAYERR                                                           
         MVC   3(1,R1),22(R4)      MOVE PST VALUE                               
         MVI   4(R1),C','                                                       
*                                                                               
         LA    R1,5(R1)                                                         
         LA    R4,34(R4)                                                        
         B     OPT3E                                                            
*                                                                               
OPT3GO   BRAS  RE,VDT              NEED TO GET PERIOD DATES NOW                 
         NI    PAYDTH+4,X'DF'      SET UNVALIDATED SO DO AGAIN LATER            
*                                                                               
         XC    ELEM,ELEM           BUILD PSTBLK FOR PSTVAL CALL                 
         LA    R5,ELEM                                                          
         USING PSTBLKD,R5                                                       
*                                                                               
         LA    R1,PSTWORK                                                       
         ST    R1,PSTADIN                                                       
         LA    R1,ELEM+128         GET OUTPUT TO TEMP AREA                      
         ST    R1,PSTADOUT                                                      
         MVI   PSTACT,PSTVALQ                                                   
         MVC   PSTACOM,VCOMFACS                                                 
*                                                                               
         XC    DMCB,DMCB                                                        
         GOTO1 VCALLOV,DMCB,,X'D9000A6B'    PSTVAL                              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R5)                                                   
         CLI   PSTERR,0                                                         
         BE    OPT3X2                                                           
         MVI   ERRCD,BADPSTVL                                                   
         CLI   PSTERR,2                                                         
         BE    PAYERR                                                           
         MVI   ERRCD,BADPSTPR                                                   
         B     PAYERR                                                           
*                                                                               
OPT3X2   GOTO1 VDATCON,DMCB,SVSTART,(3,DUB)                                     
         CLC   DUB(3),=X'6E061C'   TEST PER STARTS AFTER JUN27/10               
         BL    OPT3X10                                                          
* FOR JUL/10+, CAN ONLY ENTER ONE PST VALUE AND IT OVERRIDES EVERYTHING         
         LA    R0,10                                                            
         LA    RE,ELEM+128                                                      
         SR    RF,RF                                                            
OPT3X4   CLI   0(RE),0                                                          
         BE    *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,OPT3X4                                                        
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(ONLY1PST)                                              
         CHI   RF,1                                                             
         BH    PAYERR                                                           
         XC    SVOVRPST,SVOVRPST   CLEAR PRIOR PST VALUES                       
         B     OPT3X20                                                          
                                                                                
OPT3X10  LA    R0,10               SET COUNTER                                  
         LA    RE,SVOVRPST                                                      
         LA    RF,ELEM+128                                                      
*                                                                               
* MAKE SURE DIDN'T DO SAME PROVINCE TWICE                                       
*                                                                               
OPT3X16  CLI   0(RE),0             TEST OVERRIDE SET ALREADY                    
         BE    OPT3X18             NO - DOESN'T MATTER                          
         CLI   0(RF),0             TEST OVERRIDE IN NEW                         
         BE    OPT3X20             NO - OK                                      
         MVI   ERRCD,BADPSTPR                                                   
         B     PAYERR                                                           
*                                                                               
OPT3X18  LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,OPT3X16                                                       
*                                                                               
OPT3X20  CLI   PAYMD,C'N'          TEST NETWORK                                 
         BE    OPT3X22                                                          
* FOR SELECT TV, CREATE NEW PST ELEMENT NOW                                     
         MVI   SVPSTEL,X'6B'       SET ELEMENT CODE                             
         MVI   SVPSTEL+1,12        SET ELEMENT LENGTH                           
         OC    SVPSTEL+2(10),ELEM+128 'OR' NEW VALUES TO SAVE OLD               
         B     OPT20                                                            
*                                                                               
OPT3X22  MVC   SVOVRPST,ELEM+128   FOR NETWORK,SAVE OVERRIDE INPUT!             
         B     OPT20                                                            
         DROP  R5                                                               
*                                                                               
OPT4     CLC   =C'PWOW',12(R4)     CLEAR AN OWPW INVOICE                        
         BNE   OPT5                                                             
         MVI   SVOWPW,C'Y'                                                      
         B     OPT20                                                            
*                                                                               
OPT5     CLC   =C'ES',12(R4)                                                    
         BNE   OPT6                                                             
         MVI   ERRCD,INVERR                                                     
         TM    3(R4),X'80'                                                      
         BZ    PAYERR                                                           
         L     R0,8(R4)                                                         
         LTR   R0,R0                                                            
         BZ    PAYERR                                                           
         CHI   R0,255                                                           
         BH    PAYERR                                                           
         STC   R0,SVTSTEST                                                      
         B     OPT20                                                            
*                                                                               
OPT6     CLC   =C'LI',12(R4)                                                    
         BNE   OPT7                                                             
         MVI   ERRCD,INVERR                                                     
         TM    3(R4),X'80'         TEST NUM                                     
         BZ    PAYERR                                                           
         L     R0,8(R4)                                                         
         LTR   R0,R0                                                            
         BZ    PAYERR                                                           
         CHI   R0,999                                                           
         BH    PAYERR                                                           
         STCM  R0,3,SVTSTLIN                                                    
         B     OPT20                                                            
*                                                                               
OPT7     CLC   =C'XFR',12(R4)                                                   
         BNE   OPT8                                                             
         CLC   =C'NO',22(R4)                                                    
         BNE   OPT8                                                             
         MVI   SVXFROPT,C'N'                                                    
         B     OPT20                                                            
*                                                                               
OPT8     CLC   =C'REQ',12(R4)                                                   
         BNE   OPT9                                                             
         MVC   SVREQOPT,22(R4)                                                  
         CLC   =C'NO',22(R4)                                                    
         BE    OPT20                                                            
         MVI   SVREQOPT,C'M'                                                    
         CLC   =C'IMR',22(R4)                                                   
         BE    OPT20                                                            
         MVI   SVREQOPT,C'C'                                                    
         CLC   =C'ICL',22(R4)                                                   
         BE    OPT20                                                            
*                                                                               
OPT9     CLC   =C'NOAFD',12(R4)                                                 
         BNE   OPT9A                                                            
         OI    SVTSTOPT,X'80'                                                   
         B     OPT20                                                            
*                                                                               
OPT9A    CLC   =C'NOINV',12(R4)    ALLOW PAYMENT WHEN NO INVOICE                
         BNE   OPT9AA                                                           
         OI    SVTSTOPT,X'08'      OVERRIDES A0A+15=D                           
         B     OPT20                                                            
*                                                                               
OPT9AA   CLI  SVAPROF+7,C'C'         TEST FOR CANADIAN AGENCY                   
         BNE  OPT10                                                             
         CLC  =C'BLI',12(R4)                                                    
         BNE  OPT9B                                                             
         MVI  SVLINST,C'L'                                                      
         B    OPT20                                                             
*                                                                               
OPT9B    CLC  =C'STA',12(R4)                                                    
         BNE  OPT9C                                                             
         MVI  ERRCD,INVERR                                                      
         OC   SVSTALO,SVSTALO                                                   
         BNZ  PAYERR                                                            
         MVI  SVLINST,C'S'                                                      
         CLI  1(R4),0                LENGTH                                     
         BE   OPT20                                                             
         CLI  1(R4),4                                                           
         BH   PAYERR                                                            
         TM   3(R4),X'40'            ALPHA                                      
         BZ   PAYERR                                                            
*                                                                               
         MVC  SVSTALO,22(R4)       SAVE FOR SPPAY15 TEST OPTION                 
         CLI  1(R4),4                                                           
         BE   *+10                                                              
         MVC  SVSTALO+3(1),1(R4)                                                
                                                                                
* THIS STUFF IS FOR SPPAY16 TEST OPTION                                         
                                                                                
         MVI   KEY,C'0'            NEED STATION REC FOR MKTNUM                  
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),PAYMD                                                   
         MVC   KEY+2(4),22(R4)                                                  
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   KEY+9(3),QCLT                                                    
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         GOTO1 RDSTA                                                            
*                                                                               
         USING STARECD,R6                                                       
         MVC   DUB,SPACES                                                       
         MVC   DUB(4),22(R4)                                                    
         MVI   DUB+4,C'N'                                                       
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,PAYMD                                                    
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPQMKT,SMKT       MARKET                                       
         MVC   STAPQSTA(8),DUB     STATION                                      
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         MVI   ERRCD,INVERR                                                     
         CLI   STAPERR,0                                                        
         BNE   PAYERR                                                           
         DROP  R6                                                               
         MVC   SVSUPMKST(4),STAPMKST    SAVE MKT(2)/STA(2) ONLY!                
         B     OPT20                                                            
*                                                                               
OPT9C    CLC  =C'BUY',12(R4)                                                    
         BNE  OPT9D                                                             
         MVI  ERRCD,INVERR                                                      
         OC   SVSTALO,SVSTALO                                                   
         BNZ  PAYERR                                                            
         MVI  SVLINST,C'L'                                                      
         CLI  1(R4),0                LENGTH                                     
         BE   PAYERR                                                            
         CLI  1(R4),3                                                           
         BH   PAYERR                                                            
         TM   3(R4),X'80'            NUMERIC                                    
         BZ   PAYERR                                                            
         L    R0,8(R4)               BINARY VALUE                               
         STC  R0,SVSTALO                                                        
         B    OPT20                                                             
*                                                                               
OPT9D    CLC  =C'RAN',12(R4)                                                    
         BNE  OPT10                                                             
         CLI  0(R4),3                LENGTH                                     
         BNE  OPT10                                                             
         BRAS RE,GETRANGE                                                       
         B    OPT20                                                             
         EJECT                                                                  
*                                                                               
OPT10    DS    0H                                                               
         LLC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
         MVI   BYTE,X'40'                                                       
         EX    RE,CLCGROSS                                                      
         BE    OPT10X                                                           
         MVI   BYTE,X'20'                                                       
         EX    RE,CLCNET                                                        
         BE    OPT10X                                                           
         B     OPT12                                                            
OPT10X   TM    SVTSTOPT,X'60'                                                   
         BNZ   OPTERR                                                           
         OC    SVTSTOPT(1),BYTE                                                 
         B     OPT20                                                            
*                                                                               
CLCGROSS CLC   12(0,R4),=C'GROSS'                                               
CLCNET   CLC   12(0,R4),=C'NET'                                                 
*                                                                               
OPT12    CLC   =C'ID',12(R4)                                                    
         BNE   OPT13                                                            
         CLI   1(R4),0                                                          
         BE    OPTERR                                                           
         MVC   NEWID,22(R4)                                                     
         OC    NEWID,SPACES                                                     
*                                                                               
         CLI   SVCXTRA+2,C'A'      TEST ID=MKTGRP                               
         BL    OPT20                                                            
         CLI   SVCXTRA+2,C'F'                                                   
         BH    OPT20                                                            
         CLC   NEWID(1),SVCXTRA+2  TEST SAME LETTER INPUT                       
         BNE   BADID                                                            
*                                                                               
         LA    R0,4                NOW MAKE SURE THERE ARE 4 DIGITS             
         LA    R1,NEWID+1                                                       
OPT12A   CLI   0(R1),C'0'                                                       
         BL    BADID                                                            
         CLI   0(R1),C'9'                                                       
         BH    BADID                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,OPT12A                                                        
         CLI   0(R1),C' '                                                       
         BH    BADID                                                            
         B     OPT20                                                            
*                                                                               
BADID    MVC   PAYMSG(33),=C'** ERROR - MKTGRP ID NOT VALID **'                 
BADID2   MVI   ERRAREA,X'FF'                                                    
         LA    R2,PAYOPH                                                        
         B     EXIT                                                             
*                                                                               
OPT13    CLC   =C'TRD',12(R4)                                                   
         BE    *+14                                                             
         CLC   =C'TRADE',12(R4)                                                 
         BNE   OPT14                                                            
         CLI   22(R4),C'Y'                                                      
         BNE   OPT13A                                                           
         OI    SVAFLAG1,X'02'                                                   
         B     OPT20                                                            
OPT13A   CLI   22(R4),C'N'                                                      
         BNE   OPT13ER                                                          
         NI    SVAFLAG1,X'FF'-X'02'                                             
         B     OPT20                                                            
*                                                                               
OPT13ER  MVC   PAYMSG(30),=C'NO WAY. SPECIFY TRD=Y OR TRD=N'                    
         B     BADID2                                                           
*                                                                               
OPT14    CLC   =C'PAID',12(R4)                                                  
         BE    OPT15X                                                           
*                                                                               
OPT15    CLC   =C'PD',12(R4)                                                    
         BE    OPT15C                                                           
         CLC   =C'MC',AGYALPHA                                                  
         B     OPT15A              <- NO UNCLEAR - 2/2/04                       
*******  BNE   OPT15A                                                           
         LA    RE,PAYER                                                         
         CLI   0(RE),C'*'                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         CLC   =C'UNCLEAR',0(RE)                                                
         BE    OPT15B                                                           
*                                                                               
OPT15A   CLI   T213FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   OPT16L                                                           
*=============> A SECRET PASSWORD MUST BE PRESENT                               
         LA    RE,PAYER                                                         
         CLI   PAYER,C'*'                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         CLC   =C'ELENA',0(RE)                                                  
         BE    OPT15B                                                           
         CLC   =C'KRISTINETY',0(RE)                                             
         BE    OPT15B                                                           
         CLC   =C'ROSEBUD',0(RE)                                                
         BE    OPT15B                                                           
         CLC   =C'REVCHECK',0(RE)                                               
         BE    OPT15B                                                           
         B     OPT16L                                                           
*                                                                               
OPT15B   CLC   =C'UNPD',12(R4)                                                  
         BNE   OPT16L                                                           
         MVI   SVUNPAY,C'Y'        SET UNPAY OPTION FLAG                        
*                                                                               
OPT15C   MVI   ERRCD,INVERR                                                     
         GOTO1 VDATVAL,DMCB,22(R4),WORK                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    PAYERR                                                           
         GOTO1 VDATCON,DMCB,WORK,(2,SVPAYDT)                                    
*                                                                               
OPT15X   CLC   =C'NOCHECK',PAYER                                                
         BE    *+8                                                              
         OI    SVTSTOPT,X'10'      SET PAID TEST BIT ON                         
         LA    R0,1                                                             
         OC    SVTST,SVTST                                                      
         BNZ   *+12                                                             
         STC   R0,SVTSTEST                                                      
         STCM  R0,3,SVTSTLIN                                                    
         B     OPT20                                                            
*                                                                               
OPT16L   LLC   RE,0(R4)            LENGTH OF FIELD                              
         BCTR  RE,0                                                             
         EX    RE,CLCREV                                                        
         BE    OPT16R                                                           
         B     OPT17                                                            
*                                                                               
CLCREV   CLC   12(0,R4),=C'REVERSE'                                             
*                                                                               
OPT16R   CLI   DMCB+4,1            NUMBER OF LINES (OPTIONS) USED               
         BNE   OPT16               REVERSE MUST BE USED ALONE                   
         MVI   REVSW,C'Y'                                                       
         B     OPTX                                                             
*                                                                               
OPT16    EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'$SW'                                                 
         BNE   OPT17                                                            
         MVC   SVAUTOSW(10),12(R4)     SAVE INPUT                               
         B     OPT20                                                            
*                                                                               
OPT17    DS    0H                                                               
         CLC   =C'MKT',12(R4)                                                   
         BNE   OPT18                                                            
         MVI   ERRCD,INVMKT                                                     
         TM    3(R4),X'80'         MARKET IS VALID NUMERIC?                     
         BNO   PAYERR                                                           
         MVC   SVMKTOV,10(R4)      SAVE BINARY MARKET OVERRIDE                  
         EDIT  SVMKTOV,QMKT,FILL=0                                              
         NI    PAYSTH+4,X'FF'-X'20'                                             
         B     OPT20                                                            
*                                                                               
OPT18    DS    0H                                                               
         CLI   T213FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   OPT19                                                            
         CLC   =C'CTA',12(R4)      TEST FORCE CTA                               
         BE    OPT18A                                                           
         CLC   =C'JKC',12(R4)      TEST JEANNE CLEMENTE                         
         BE    OPT18A                                                           
         B     OPT19                                                            
OPT18A   OI    SVAFLAG1,X'20'      SET CTA ACTIVE                               
         B     OPT20                                                            
*                                                                               
OPT19    CLC   =C'SRC',12(R4)                                                   
         BNE   OPTCN                                                            
         CLI   22(R4),C'N'                                                      
         BNE   PAYERR                                                           
         MVI   SVSRCCOM,C'N'                                                    
         B     OPT20                                                            
*                                                                               
OPTCN    CLI   PAYMD,C'N'          THESE OPTS ONLY VALID FOR CAN NET            
         BNE   PAYERR                                                           
         CLI   SVAPROF+7,C'C'                                                   
         BNE   PAYERR                                                           
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLC   =C'ST',12(R4)                                                    
         BNE   OPTCN2                                                           
         CLI   1(R4),0                                                          
         BE    PAYERR                                                           
         MVC   SVTSTSTA,22(R4)                                                  
         LA    R0,1                                                             
         STC   R0,SVTSTEST                                                      
         STCM  R0,3,SVTSTLIN                                                    
         B     OPT20                                                            
*                                                                               
OPTCN2   CLC   =C'NOAFD',12(R4)                                                 
         BNE   OPTERR                                                           
         OI    SVTSTOPT,X'80'                                                   
         B     OPT20                                                            
*                                                                               
OPT20    LA    R4,34(R4)           ** NOTE 34 BYTE SCANNER ENTRIES **           
         B     OPT2                                                             
*                                                                               
OPTX     OI    4(R2),X'20'                                                      
*                                                                               
         CLI   SVCXTRA+2,C'A'      TEST ID=MKTGRP                               
         BL    OPTX10                                                           
         CLI   SVCXTRA+2,C'F'                                                   
         BH    OPTX10                                                           
*                                                                               
         CLC   SVID,NEWID          TEST SAME ID AS PREVIOUS                     
         BE    OPTX10                                                           
         NI    PAYSTH+4,X'DF'      ELSE FORCE STATION EDIT                      
*                                                                               
OPTX10   MVC   SVID,NEWID          SAVE NEW ID                                  
         CLC   =C'FJWT$$',PAYER                                                 
         BNE   OPTX20                                                           
         OC    SVPAYDT,SVPAYDT     MAKE SURE JKC PUT IN THE DATE                
         BZ    OPTERR                                                           
         XC    SVTST,SVTST         UNSET ALL OTHER OPTIONS                      
*                                                                               
OPTX20   CLC   =C'NOCHECK',PAYER   IF NO CHECK SPECIFIED                        
         BNE   OPTX30                                                           
         OC    SVPAYDT,SVPAYDT     HAVE TO NAME A DATE                          
         BNZ   OPTX30                                                           
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOPAYDT)                                             
         B     PAYERR                                                           
*                                                                               
OPTX30   DS    0H                                                               
*                                                                               
OPTX100  B     PRD                                                              
*                                                                               
OPTERR   MVI   ERRCD,INVERR                                                     
         NI    4(R2),X'DF'         UNSET VALIDATED FLAG                         
         B     PAYERR                                                           
         EJECT                                                                  
PRD      LA    R2,PAYOPH                                                        
         TM    4(R2),X'80'                                                      
         LA    R2,PAYPRH                                                        
         BO    PRD00                                                            
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    ST                                                               
*                                                                               
PRD00    BRAS  RE,CLRPR                                                         
         XC    SVRANLO(4),SVRANLO  CLEAR BUYLINE RANGE                          
         XC    SVEUSER1,SVEUSER1                                                
         CLI   5(R2),0                                                          
         BNE   PRD0                                                             
         GOTO1 GLOBBER,DMCB,=C'GETF',(R2),,GLVSPPRD                             
*                                                                               
PRD0     GOTO1 ANY                                                              
*                                                                               
         L     RE,AREC2                                                         
         XC    0(64,RE),0(RE)                                                   
         GOTO1 VSCANNER,DMCB,(R2),(2,AREC2),C',=/-'                             
*                                                                               
         MVI   ERRCD,PRDERR                                                     
         L     R4,AREC2                                                         
         CLI   0(R4),2                                                          
         BL    PAYERR                                                           
         CLI   0(R4),3                                                          
         BH    PAYERR                                                           
         MVC   FULL(3),12(R4)                                                   
         BAS   RE,FINDPRD                                                       
*                                                                               
         L     R6,AREC                                                          
         USING PRDHDRD,R6                                                       
*                                                                               
         XC    PAYPRDNM,PAYPRDNM                                                
         MVC   PAYPRDNM(20),PNAME  MOVE TO ACC POSTING AREA                     
         MVC   PAYPRXP(20),PNAME                                                
         MVC   QPRD,FULL                                                        
         MVC   SVKEY+3(1),QPRD+3                                                
         MVI   SVKEY+9,0             RESET EST NUMBER                           
         MVI   SVKEY+10,0                                                       
         XC    SVKEY+11(2),SVKEY+11  RESET LINE NUMBER                          
*                                                                               
         CLI   SVPPROF+10,C'C'     TEST CCUSA INTERFACE ACTIVE                  
         BNE   PRD1                                                             
         CLC   =C'POL',QPRD                                                     
         BE    PAYERR                                                           
*                                                                               
PRD1     CLI   SVPPROFB+7,C'Y'     TEST SUPERPAY                                
         BE    PRD1X                                                            
         CLI   PAYMD,C'N'          TEST NTWK                                    
         BNE   *+12                                                             
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BE    PRD40                                                            
*                                                                               
PRD1X    CLC   =C'POL',12(R4)                                                   
         BE    PRD20                                                            
* NON-POL                                                                       
         CLI   1(R4),0             TEST DIVIDED FIELD                           
         BE    PRD10               NO                                           
*                                                                               
         MVC   FULL(3),22(R4)                                                   
         CLI   1(R4),2                                                          
         BL    PAYERR                                                           
         CLI   1(R4),3                                                          
         BH    PAYERR                                                           
         MVI   FULL+3,X'FF'        PRESET PRD CODE FOR 'ALL'                    
         CLC   =C'ALL',FULL                                                     
         BE    PRD2                                                             
         BAS   RE,FINDPRD                                                       
         MVI   PAYPRXP+20,C'-'                                                  
         MVC   PAYPRXP+21(19),PNAME                                             
*                                                                               
PRD2     MVC   QPRD2,FULL                                                       
*                                                                               
PRD10    DS    0H                                                               
         LA    R4,32(R4)                                                        
         CLI   0(R4),0                                                          
         BE    PRDX                                                             
         EJECT                                                                  
         TM    2(R4),X'80'         TEST NUMERIC                                 
         BO    PRDEST                                                           
*                                                                               
         CLI   0(R4),3                                                          
         BNE   PAYERR                                                           
         CLI   1(R4),0                                                          
         BNE   PAYERR                                                           
*                                                                               
         MVI   SVMODE,2                                                         
         CLC   =C'REG',12(R4)                                                   
         BE    PRDX                                                             
*                                                                               
         MVI   SVMODE,3                                                         
         CLC   =C'PIG',12(R4)                                                   
         BE    PRDX                                                             
*                                                                               
         MVI   ERRCD,BADMODE                                                    
         B     PAYERR                                                           
*                                                                               
PRD20    LA    R4,32(R4)                                                        
         CLI   0(R4),0                                                          
         BNE   PRD22                                                            
         CLI   1(R4),0                                                          
         BE    PRDX                                                             
         MVI   ERRCD,INVERR                                                     
         B     PAYERR                                                           
*                                                                               
PRD22    MVI   ERRCD,PRDERR                                                     
         TM    2(R4),X'80'         TEST NUMERIC                                 
         BO    PRDEST                                                           
         CLI   0(R4),2                                                          
         BL    PAYERR                                                           
         CLI   0(R4),3                                                          
         BH    PAYERR                                                           
         MVC   FULL(3),12(R4)                                                   
         BAS   RE,FINDPRD                                                       
         MVC   PAYPRXP(20),PNAME                                                
         MVC   QPRD,FULL           POL/XXX BECOMES XXX                          
         MVC   SVKEY+3(1),QPRD+3   SET PRD IN KEY                               
*                                                                               
         MVI   ERRCD,PRDERR                                                     
         CLI   1(R4),0                                                          
         BE    PRDX                                                             
         CLI   1(R4),2                                                          
         BL    PAYERR                                                           
         CLI   1(R4),3                                                          
         BH    PAYERR                                                           
         MVC   FULL(3),22(R4)                                                   
         BAS   RE,FINDPRD                                                       
         MVI   PAYPRXP+20,C'-'                                                  
         MVC   PAYPRXP+21(19),PNAME                                             
         MVC   QPRD2,FULL          POL/XXX BECOMES XXX-YYY                      
         B     PRDX                                                             
         EJECT                                                                  
*==================================================================             
* CANAD NTWK INPUT SHOULD BE POL/EST-LIN UNLESS RANGE ENTERED IN                
* OPTIONS FIELD (RAN=MMM-NNN)                                                   
*==================================================================             
         SPACE 1                                                                
PRD40    CLC   =C'POL',12(R4)                                                   
         BNE   PAYERR                                                           
         CLI   1(R4),0                                                          
         BNE   PAYERR                                                           
*                                                                               
         MVI   ERRCD,BADESLN                                                    
         LA    R4,32(R4)                                                        
         CLI   0(R4),0             EST                                          
         BE    PAYERR                                                           
*                                                                               
         CLI   0(R4),3                                                          
         BH    PAYERR                                                           
         TM    2(R4),X'80'         NUM                                          
         BZ    PAYERR                                                           
         L     R0,4(R4)                                                         
         LTR   R0,R0                                                            
         BZ    PAYERR                                                           
         CHI   R0,255                                                           
         BH    PAYERR                                                           
         STC   R0,SVKEY+9                                                       
*                                                                               
         MVI   ERRCD,ESTERR                                                     
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY                                                   
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),SVKEY+9                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   PAYERR                                                           
*                                                                               
         L     RE,AREC1                                                         
         ST    RE,AREC                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPAYEST)                                              
         L     RE,AREC                                                          
         CLI   ETYPE-ESTHDRD(RE),C'S'  TEST STEWARD EST                         
         BE    PAYERR                                                           
         CLI   ETYPE-ESTHDR(RE),C'B'   TEST BARTER ESTIMATE                     
         BE    PAYERR                                                           
*                                                                               
PRD43    CLI   1(R4),0             LIN                                          
         BE    PRDX                NO                                           
*                                                                               
PRD46    CLI   SVRANFL,C'S'                                                     
         BE    PAYERR                                                           
         CLI   1(R4),3                                                          
         BH    PAYERR                                                           
         TM    3(R4),X'80'                                                      
         BZ    PAYERR                                                           
         L     R0,8(R4)                                                         
         LTR   R0,R0                                                            
         BZ    PAYERR                                                           
         LA    R1,255                                                           
         CLI   SV1OR2,2                                                         
         BNE   *+8                                                              
         LA    R1,999                                                           
         CR    R0,R1                                                            
         BH    PAYERR                                                           
         STCM  R0,3,SVKEY+11                                                    
         STCM  R0,3,SVOPTRAN                                                    
         STCM  R0,3,SVOPTRAN+2                                                  
         MVI   SVRANFL,C'S'                                                     
*                                                                               
         MVC   PAYPRXP+20(20),=CL20'* BUY LINE 099-999 *'                       
         LLC   R0,SVKEY+9                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PAYPRXP+31(3),DUB                                                
         ICM   R0,3,SVKEY+11                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PAYPRXP+35(3),DUB                                                
*                                                                               
         B     PRDX                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE ESTIMATE                                                             
*                                                                               
PRDEST   MVI   ERRCD,ESTERR                                                     
         CLI   0(R4),3             MAX 3 DIGITS                                 
         BH    PAYERR                                                           
         L     R0,4(R4)                                                         
         LTR   R0,R0                                                            
         BZ    PAYERR                                                           
         CHI   R0,255                                                           
         BH    PAYERR                                                           
         STC   R0,SVKEY+9          SET ESTIMATE IN KEY                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY                                                   
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),SVKEY+9                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   PAYERR                                                           
         MVC   PAYPRXP+20(20),=CL20'** EST 999 ONLY **'                         
         LLC   R0,SVKEY+9                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PAYPRXP+27(3),DUB                                                
*                                                                               
         OC    SVPWPCT,SVPWPCT     TEST PW CLIENT                               
         BNZ   PRDEST2             YES - READ ESTHDR                            
         CLI   SVPPROFA+2,C'Y'     TEST ESTIMATE REQUIRED                       
         BE    PRDEST2                                                          
         CLI   SVPPROF+10,C'C'     TEST CCUSA INTERFACE ACTIVE                  
         BNE   PRDEST4                                                          
*                                                                               
PRDEST2  L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING ESTHDRD,R6                                                       
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPAYEST)                                              
         CLI   ETYPE,C'S'          TEST STEWARD EST                             
         BE    PAYERR                                                           
         CLI   ETYPE,C'B'          TEST BARTER ESTIMATE                         
         BE    PAYERR                                                           
         MVC   SVEUSER1,EUSER1                                                  
         MVC   SVESTART,ESTART     SAVE ESTIMATE START DATE                     
         MVI   SVEOWSD,0           NON-ZERO ONLY FOR PW EST                     
         TM    EFLAG1,EF1OOWPW     TEST TO DO OOWR PW CLEARANCES                
         BZ    PRDEST4             NO                                           
         OC    EPWPCT,EPWPCT       TEST PW ESTIMATE                             
         BZ    *+10                NO                                           
         MVC   SVEOWSD,EOWSDAY     SAVE OOWR START DAY (OR ZERO)                
         CLI   SVOWPW,C'Y'         DID THEY SAY THEY KNOW IT IS OWPW            
         BE    PRDEST4             YES                                          
* THEY NEED TO BE TOLD IT IS OWPW                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(OWPWERR)                                               
         B     PAYERR                                                           
         DROP  R6                                                               
*                                                                               
PRDEST4  CLI   1(R4),0             TEST FOR LINE NUMBER                         
         BE    PRDX                NO                                           
         MVI   ERRCD,INVERR                                                     
         CLI   PAYMD,C'N'          TEST NTWK                                    
         BNE   PAYERR                                                           
         B     PRD43               GO EDIT LINE NUMBER                          
*                                                                               
PRDX     DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,12,SVKEY+11                                                   
         ICM   R0,3,SVKEY+11                                                    
         LTR   R0,R0               TEST BUYLINE ENTERED                         
         BNZ   PRDX1                                                            
*                                                                               
         ICM   R0,15,SVOPTRAN      TEST RANGE ENTERED IN OPTIONS                
         BNZ   PRDX1                                                            
*                                                                               
         L     R0,=X'000103E7'                                                  
*                                                                               
PRDX1    STCM  R0,15,SVRANLO       SET LOW/HIGH RANGE OF BUYLINES               
                                                                                
         MVI   ERRCD,ESTERR                                                     
         OC    SVPWPCT,SVPWPCT     TEST PW CLIENT                               
         BNZ   PRDX2               YES - EST ALWAYS REQUIRED                    
         CLI   SVPPROFA+2,C'Y'     TEST ESTIMATE REQUIRED                       
         BNE   PRDX4                                                            
PRDX2    CLI   SVKEY+9,0           TEST ESTIMATE ENTERED                        
         BE    PAYERR                                                           
PRDX4    OI    PAYPRH+4,X'20'                                                   
         GOTO1 GLOBBER,DMCB,=C'PUTF',PAYPRH,,GLVSPPRD                           
         B     ST                                                               
         SPACE 2                                                                
FINDPRD  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY                                                   
         MVC   KEY+4(3),FULL                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING PRDHDRD,R6                                                       
         MVC   FULL+3(1),PCODE+1                                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* IF EITHER STATION OR PAYEE CHANGED, MUST RE-EDIT BOTH                         
*                                                                               
ST       LA    R2,PAYSTH                                                        
         TM    4(R2),X'20'                                                      
         BZ    STA2                                                             
         TM    PAYEEH+4,X'20'                                                   
         BZ    STA2                                                             
         CLI   SVCXTRA+2,C'*'      AND IF CCUSA                                 
         BNE   EE                                                               
         TM    PAYDTH+4,X'20'      RE-EDIT IF PAY PERIOD CHANGED                
         BNZ   EE                                                               
*                                                                               
STA2     BRAS  RE,CLRST                                                         
         CLI   5(R2),0                                                          
         BNE   STA4                                                             
         GOTO1 GLOBBER,DMCB,=C'GETF',(R2),,GLVSPSTA                             
*                                                                               
STA4     GOTO1 ANY                                                              
*                                                                               
         L     R4,AREC2            POINT TO STAVAL TABLE                        
         USING STABLKD,R4                                                       
         XC    0(STBLNQ,R4),0(R4)     CLEAR                                     
         ST    R2,STBADDR                                                       
         MVC   STBMED,PAYMD                                                     
         MVC   STBCTRY,SVAPROF+7                                                
         MVC   STBACOM,VCOMFACS                                                 
         GOTO1 VCALLOV,DMCB,0,X'D9000A68'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),STABLKD                                                
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   STBERR,0                                                         
         BNE   PAYERR                                                           
*                                                                               
         MVC   QSTA,STBSTA                                                      
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         MVC   QNET,STBNET                                                      
         DROP  R4                                                               
*                                                                               
* READ STATION RECORD                                                           
*                                                                               
STA10    MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),PAYMD                                                   
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   KEY+9(3),QCLT                                                    
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         GOTO1 RDSTA                                                            
*                                                                               
         USING STARECD,R6                                                       
*                                                                               
         MVI   SVSTASW,C'A'        ASSUME IT'S AN AGY LEVEL REC                 
         CLC   STAKCLT,=C'000'     TEST CLIENT SPECIFIC REC                     
         BE    *+8                                                              
         MVI   SVSTASW,C'C'        SET CLT SPECIFIC                             
*                                                                               
         MVC   QREP,SREP           SAVE REP                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   STA12                                                            
         CLI   PAYMD,C'N'          TEST NTWK                                    
         BNE   STA12                                                            
         MVC   SVNETPST,SPST       SAVE NETWORK PST VALUES                      
         B     STA50               GO READ NETDEF REC                           
*                                                                               
STA12    MVC   SVTAX,SNEWTAX       SAVE TAX RATE                                
         OC    SVMKTOV,SVMKTOV     MARKET OVERRIDE?                             
         BNZ   STA14                                                            
         MVC   QMKT,SMKT           SAVE MARKET                                  
         B     STA16                                                            
*                                                                               
STA14    DS    0H                  CHECK IF MKT IS IN STATION RECORD            
         CLC   =C'TEST',PAYER                                                   
         BE    STA16                                                            
         CLC   STOLDMK1,SVMKTOV                                                 
         BE    STA16                                                            
         CLC   STOLDMK2,SVMKTOV                                                 
         BE    STA16               EXIT WITH CC NE                              
         LA    R2,PAYOPH                                                        
         MVI   ERRCD,STANTMKT      STATION NOT IN MARKET RECORD                 
         B     PAYERR                                                           
*                                                                               
STA16    MVC   SVSTACNT,SCOUNTRY   SAVE COUNTRY CODE                            
         MVC   SVGSTCOD,SGSTCODE   SAVE GST CODE                                
         SPACE 1                                                                
*=============================================================*                 
* IF TRANSACTION FROM INCH, LOOK FOR A MARKET GLOBAL          *                 
* WHICH OVERRIDES DEFAULT MARKET NUMBER IN LIEU OF ACN NUMBER *                 
*=============================================================*                 
         SPACE 1                                                                
         CLI   INCHSW,C'Y'         TEST XFRCTL FROM INCH                        
         BNE   STA20                                                            
         GOTO1 GLOBBER,DMCB,=C'GETD',DUB,4,GLVSPMKT                             
         MVC   QMKT,DUB                                                         
         GOTO1 (RF),(R1),=C'DELE'  DELETE MARKET NUMBER GLOBAL                  
         EJECT                                                                  
***      LTORG                     WTF MEL????                                  
*=====================================================*                         
* MAKE SURE PROFILE COUNTRY CODES ARE VALID           *                         
*=====================================================*                         
         SPACE 1                                                                
STA20    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BE    *+8                                                              
         MVI   SVAPROF+7,C'U'      ELSE SET US                                  
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   STA34               NO - SKIP XCHG RATE PROCESSING               
*                                                                               
         CLI   SVCXTRA+9,C'C'      TEST CANADIAN CLIENT                         
         BE    STA22                                                            
         CLI   SVCXTRA+9,C'U'      TEST US CLIENT                               
         BE    STA22                                                            
         MVC   SVCXTRA+9(1),SVAPROF+7   ELSE SET TO SAME AS AGY                 
*                                                                               
STA22    CLI   SVSTACNT,C'C'       TEST CANADIAN STATION                        
         BE    STA24                                                            
         CLI   SVSTACNT,C'U'       TEST US STATION                              
         BE    STA24                                                            
         MVC   SVSTACNT(1),SVAPROF+7    ELSE SET TO SAME AS AGY                 
*                                                                               
STA24    CLC   SVAPROF+7(1),SVCXTRA+9  AGY = CLT ?                              
         BNE   STA30                                                            
         CLC   SVAPROF+7(1),SVSTACNT   AGY = STA ?                              
         BE    STA34                   ALL EQUAL, DON'T NEED XCH RATE           
         DROP  R6                                                               
*                                                                               
*                                                                               
STA30    BRAS  RE,GETXCH                                                        
*====>   BNE   PAYERR              CAN'T FIND EXCHANGE RATE                     
         EJECT                                                                  
STA34    CLI   SVPPROFA+13,C'Y'    TEST ALLOW PAYING REP BY OFFC                
         BNE   STA36                                                            
         CLI   SVSTASW,C'C'        TEST CLIENT LEVEL STATION FOUND              
         BE    STA36               YES - OVERRIDES OFFICE RECORD                
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),PAYMD                                                   
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,C'*'                                                       
         MVC   KEY+10(1),SVOFFC                                                 
         MVI   KEY+11,C' '                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AREC1                   
*                                                                               
         L     R6,AREC1                                                         
         USING STARECD,R6                                                       
*                                                                               
         CLC   KEY(12),STAKEY      TEST RECORD FOUND                            
         BNE   STA36               NO                                           
         CLI   SREP,C' '           TEST PAYING REP PRESENT                      
         BNH   STA36                                                            
         MVC   QREP,SREP           YES - IT RULES                               
         DROP  R6                                                               
*                                                                               
STA36    CLI   SVCXTRA+2,C'*'      TEST CCUSA                                   
         BNE   STA38                                                            
         BRAS  RE,VDT              MUST VALIDATE DATES FOR SPACNVAL             
         B     STA40                                                            
*                                                                               
STA38    CLI   SVCXTRA+2,C'A'      TEST ID=MKTGRP                               
         BL    STA45                                                            
         CLI   SVCXTRA+2,C'Z'                                                   
         BH    STA45                                                            
*                                                                               
STA40    BRAS  RE,GETSTEQ            GET STATION EQUIV REC                      
*                                                                               
STA45    DS    0H                                                               
         MVC   DUB(5),QSTA                                                      
         MVC   DUB+5(3),QNET                                                    
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,PAYMD                                                    
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPQMKT,QMKT       MARKET                                       
         MVC   STAPQSTA(8),DUB     STATION                                      
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         MVI   ERRCD,INVERR                                                     
         CLI   STAPERR,0                                                        
         BNE   PAYERR                                                           
         MVC   SVKEY+4(5),STAPMKST                                              
         DROP  R1                                                               
*                                                                               
         BRAS  RE,GETMKT                                                        
*                                                                               
STA46    CLI   PAYEEH+5,0          TEST REP INPUT                               
         BNE   STA60                                                            
*                                  OR HAVE VIA GLOBBER                          
         GOTO1 GLOBBER,DMCB,=C'GETF',PAYEEH,,GLVSPREP                           
         GOTO1 (RF),(R1),=C'DELE'  BEST TO DELETE SPECIAL REP GLOBAL            
         CLI   PAYEEH+5,0                                                       
         BNE   STA60                                                            
*                                                                               
         OI    PAYEEH+4,X'20'      INHIBIT PAYEE EDIT                           
*                                                                               
         CLC   QREP,=C'000'                                                     
         BE    STA48                                                            
         CLC   =C'CK',AGYALPHA                                                  
         BE    STA48                                                            
         MVC   PAYSTXP(23),=C'** PAYING  REP = 999 **'                          
         MVC   PAYSTXP+17(3),QREP                                               
         BRAS  RE,GETREP                                                        
         BNE   PAYERR                                                           
         B     STA60                                                            
*                                                                               
STA48    DS    0H                                                               
         BRAS  RE,GETADDR                                                       
         BNE   PAYERR                                                           
         B     STA60                                                            
         EJECT                                                                  
* READ NETDEF REC                                                               
*                                                                               
STA50    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),QSTA                                                    
         MVI   ERRCD,CNTWKPAY                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PAYERR                                                           
*                                                                               
*        MVC   QREP,=C'000'        CLEAR REP STUPID                             
         XC    SVSPREP,SVSPREP                                                  
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,PAYMD                                                    
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPQMKT,=C'0000'   MARKET                                       
         MVC   STAPQSTA,QSTA       STATION                                      
         MVC   STAPQNET,QNET                                                    
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         MVI   ERRCD,INVERR                                                     
         CLI   STAPERR,0                                                        
         BNE   PAYERR                                                           
         MVC   SVKEY+4(5),STAPMKST                                              
         DROP  R1                                                               
*                                                                               
         BRAS  RE,GETADDR                                                       
         BNE   PAYERR                                                           
*                                                                               
         CLI   PAYEEH+5,0          TEST REP INPUT                               
         BNE   STA60                                                            
*                                  OR HAVE VIA GLOBBER                          
         GOTO1 GLOBBER,DMCB,=C'GETF',PAYEEH,,GLVSPREP                           
         GOTO1 (RF),(R1),=C'DELE'  BEST TO DELETE SPECIAL REP GLOBAL            
         CLI   PAYEEH+5,0                                                       
         BNE   STA60                                                            
*                                                                               
         OI    PAYEEH+4,X'20'      INHIBIT PAYEE EDIT                           
*                                                                               
         CLC   QREP,=C'000'                                                     
         BE    STA60                                                            
         MVC   PAYSTXP,SPACES                                                   
         MVC   PAYSTXP(23),=C'** PAYING  REP = 999 **'                          
         MVC   PAYSTXP+17(3),QREP                                               
         BRAS  RE,GETREP                                                        
         BNE   PAYERR                                                           
*                                                                               
         EJECT                                                                  
STA60    DS    0H                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',(R2),,GLVSPSTA                             
*                                                                               
         CLI   SVAPROF+14,C'Y'     TEST TO SAVE PAYER NAME                      
         BNE   STAX                                                             
         B     STAX        ******* NOP ********                                 
* CALL PAYER NAME INTERFACE MODULE *                                            
         LA    R2,PAYERH                                                        
         XC    SVPKGEL,SVPKGEL                                                  
         LA    R4,SVPKGEL                                                       
         USING GETBUBLD,R4                                                      
*                                                                               
         MVC   GBCOMFAC,VCOMFACS                                                
         MVC   GBIOA,AREC1                                                      
         ST    R2,GBNAMFLD                                                      
         MVC   GBAGY,AGYALPHA                                                   
         MVC   GBMEDEBC,PAYMD                                                   
         MVC   GBCLTEBC,QCLT                                                    
         MVC   GBOFFICE,SVOFFC                                                  
         MVC   GBAGYMD,SVKEY                                                    
         MVC   GBCLT,SVKEY+1                                                    
         MVC   GBPRD,SVKEY+3                                                    
         MVC   GBEST,SVKEY+9                                                    
         MVC   GBMKT(5),SVKEY+4                                                 
         MVI   GBTYPE,C'I'         SET FOR BILLER/PAYER                         
* GET ADDRESS OF GETBUBL                                                        
         GOTO1 VCALLOV,DMCB,0,X'D9000A77'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R4)                                                   
         CLI   GBERR,0                                                          
         BE    STAX                                                             
         MVI   ERRCD,1                                                          
         CLI   GBERR,X'81'                                                      
         BE    PAYERR                                                           
         DC    H'0'                DIE FOR NOW ON DMGR ERROR                    
         DROP  R4                                                               
*                                                                               
STAX     LA    R2,PAYSTH           RESTORE R2                                   
*                                                                               
         CLI   SVAUTPAY,C'Y'       IF AUTOPAY                                   
         BNE   STAXX                                                            
         CLI   SVPPROFB+1,C'N'     DO NOT AUTOPAY IF TAX ON MAST REC            
         BNE   STAXX                                                            
         OC    SVTAX,SVTAX                                                      
         BZ    STAXX                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPAYTAX)                                              
         B     PAYERR                                                           
*                                                                               
STAXX    OI    4(R2),X'20'         SET STATION VALIDATED                        
         EJECT                                                                  
*                                                                               
EE       MVI   PAYREP,C'N'         NO PAY REP ENTERED ON SCREEN                 
         LA    R2,PAYEEH                                                        
         TM    4(R2),X'20'                                                      
         BO    DT                                                               
         BRAS  RE,VALEE                                                         
         BNE   PAYERR                                                           
         OI    4(R2),X'20'                                                      
         B     DT                                                               
         SPACE 2                                                                
         GETEL (R6),24,ELCODE                                                   
         EJECT                                                                  
DT       BRAS  RE,VDT                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,SVSTART,(3,SVSTARTB)                                
         GOTO1 (RF),(R1),SVEND,(3,SVENDB)                                       
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   DT2                                                              
         CLI   PAYMD,C'N'          TEST NETWORK                                 
         BNE   DT2                                                              
         CLC   SVENDB,=X'6E061C'   PERIOD END PRIOR TO JUL/10                   
         BNL   DT2                                                              
* NO SUPERPAY FEATURES PRIOR TO JUL/10                                          
         CLI   SVKEY+3,X'FF'       TEST PRD=POL                                 
         BNE   *+12                                                             
         CLI   SVKEY+9,0           TEST ESTIMATE ENTERED                        
         BNE   DT2                                                              
         MVI   ERRCD,BADESLN                                                    
         LA    R2,PAYPRH                                                        
         B     PAYERR                                                           
*                                                                               
DT2      OC    SVNETPST,SVNETPST                                                
         BNZ   DT4                                                              
         OC    SVOVRPST,SVOVRPST                                                
         BZ    DT10                                                             
*                                                                               
DT4      CLC   DUB(3),=X'74061B'   PERIOD START BEFORE 6/27/16                  
         BNL   DT10                NO                                           
         CLC   DUB+3(3),=X'74061B' PERIOD END BEFORE 6/28/10                    
         BL    DT10                YES - OK                                     
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(X100628)                                               
         B     PAYERR                                                           
*                                                                               
DT10     CLI   QPRD+2,C'#'         TEST TRADE CLEARANCE                         
         BNE   DT12                                                             
         TM    SVREPOP1,X'80'      TEST VALID TRADE PAYEE                       
         BO    DT12                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADPAYEE)                                              
         B     PAYERR                                                           
*                                                                               
DT12     LA    R4,SVCLOCK                                                       
         CLI   SVPPROFA+12,C'Y'    OBSERVE CLIENT LOCKS                         
         BNE   DT14                                                             
         BRAS  RE,CHKLOCK          CHECK CLIENT LOCK                            
*                                                                               
DT14     OI    PAYDTH+4,X'20'      SET DATE VALID                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',PAYDTH,,GLVSPPER                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
PAYERR   GOTO1 ERROR                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLRMD    NI    PAYMDH+4,X'DF'                                                   
         XC    SVTST,SVTST                                                      
         BRAS  R1,*+6                                                           
         DC    C'MC'                                                            
         CLC   0(2,R1),AGYALPHA                                                 
         JE    CLRCL                                                            
         XC    PAYPADR,PAYPADR                                                  
         FOUT  PAYPADRH                                                         
*                                                                               
CLRCL    NI    PAYCLH+4,X'DF'                                                   
         XC    PAYCLXP,PAYCLXP                                                  
         FOUT  PAYCLXPH                                                         
         XC    SVID,SVID                                                        
*                                                                               
CLRPR    NI    PAYPRH+4,X'DF'                                                   
         XC    PAYPRXP,PAYPRXP                                                  
         FOUT  PAYPRXPH                                                         
*                                                                               
         XC    QPRD(15),QPRD                                                    
*                                                                               
CLRST    NI    PAYSTH+4,X'DF'                                                   
         XC    PAYSTXP,PAYSTXP                                                  
         FOUT  PAYSTXPH                                                         
*                                                                               
         BRAS  R1,*+6                                                           
         DC    C'MC'                                                            
         CLC   0(2,R1),AGYALPHA                                                 
         JNE   CLRST10                                                          
         XC    PAYPADR,PAYPADR                                                  
         FOUT  PAYPADRH                                                         
*                                                                               
CLRST10  XC    SVXRATE,SVXRATE     CLEAR EXCHANGE RATE                          
         MVI   SVSTACNT,0          AND COUNTRY CODE                             
*                                                                               
CLREE    NI    PAYEEH+4,X'DF'                                                   
         XC    PAYEEXP,PAYEEXP                                                  
         FOUT  PAYEEXPH                                                         
         FOUT  PAYSTXPH                                                         
*                                                                               
         XC    SVSPREP,SVSPREP                                                  
         MVI   SVREPOP1,0                                                       
*                                                                               
CLRDT    NI    PAYDTH+4,X'DF'                                                   
         XC    PAYDTXP,PAYDTXP                                                  
         FOUT  PAYDTXPH                                                         
         FOUT  PAYDTH                                                           
*                                                                               
         XC    SVSTART(12),SVSTART                                              
         XC    SVENDP(4),SVENDP                                                 
         MVI   SVSPOT,0                                                         
         MVI   SVOFFCSW,0                                                       
*                                                                               
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* VALIDATE PAYEE                                                                
*==========================================================                     
                                                                                
VALEE    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   SVREPTYP,0          CLEAR REP TYPE                               
         BRAS  RE,CLREE                                                         
         CLI   5(R2),0                                                          
         BNE   EE0                                                              
         GOTO1 GLOBBER,DMCB,=C'GETF',(R2),,GLVSPREP                             
         GOTO1 (RF),(R1),=C'DELE'  BEST TO DELETE SPECIAL REP GLOBAL            
*                                                                               
EE0      DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    EEX                                                              
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'P'                                                       
         BNE   *+12                                                             
         MVI   PAYREP,C'Y'                                                      
         B     EE1                                                              
         CLI   8(R2),C'S'                                                       
         BE    EE1                                                              
*                                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    EEXERR                                                           
         CLI   5(R2),3             NO MORE THAN 3 DIGITS                        
         BH    EEXERR                                                           
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QREP,DUB                                                         
         CLC   QREP,=C'000'        TEST PAYING DIRECT                           
         BE    EE2                                                              
         B     EE20                                                             
*                                                                               
EE1      LA    R4,9(R2)                                                         
         LLC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNP   EEXERR                                                           
*                                                                               
         MVC   FULL(3),0(R4)                                                    
         OC    FULL(3),=C'   '                                                  
         GOTO1 VRCPACK,DMCB,(C'P',FULL),SVSPREP                                 
         BNZ   EEXERR                                                           
         GOTO1 (RF),(R1),(C'U',SVSPREP),QREP  3 CHAR REP TO QREP                
*                                                                               
* PAYING REP GOES IN QREP, SPECIAL REP GOES IN SVSPREP                          
*                                                                               
         CLI   8(R2),C'P'          TEST PAYING REP                              
         BNE   EE10                NO                                           
         OC    SVSPREP,SVSPREP     TEST P000                                    
         BZ    EE2                                                              
         XC    SVSPREP,SVSPREP     CLEAR SPECIAL REP CODE                       
         B     EE20                AND GET REP ADDRESS                          
*                                                                               
EE2      BRAS  RE,GETADDR          GET STATION ADDRESS                          
         BNE   EEXERR                                                           
         B     EEX                                                              
*                                                                               
* SPECIAL REP                                                                   
*                                                                               
EE10     OC   SVSPREP,SVSPREP      SPECIAL REP CANNOT BE 0                      
         BZ   EEXERR                                                            
*                                                                               
EE20     MVC   PAYSTXP,SPACES                                                   
         MVC   PAYSTXP(23),=C'** PAYING  REP = 999 **'                          
         MVC   PAYSTXP+17(3),QREP                                               
         OC    SVSPREP,SVSPREP                                                  
         BZ    *+10                                                             
         MVC   PAYSTXP+3(7),=C'SPECIAL'                                         
         FOUT  PAYSTXPH                                                         
*                                                                               
         BRAS  RE,GETREP           READ REP RECORD                              
         BNE   EEXERR                                                           
*                                                                               
EEX      MVI   WORK,C'*'                                                        
         MVC   WORK+1(9),WORK                                                   
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK *EXECUTED*                                          
         FOUT  (R2)                                                             
         J     EQXIT                                                            
*                                                                               
EEXERR   J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
VDT      NTR1  BASE=*,LABEL=*                                                   
         LA    R2,PAYDTH                                                        
         TM    4(R2),X'20'                                                      
         BO    DTX                                                              
*                                                                               
         BRAS  RE,CLRDT                                                         
         CLI   5(R2),0                                                          
         BNE   VDT2                                                             
         GOTO1 GLOBBER,DMCB,=C'GETF',(R2),,GLVSPPER                             
*                                                                               
VDT2     GOTO1 ANY                                                              
* TEST FOR M/D/Y-M/D/Y                                                          
         MVI   ERRCD,INVERR                                                     
         LA    R4,8(R2)                                                         
         GOTO1 VDATVAL,DMCB,(R4),SVSTART                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    VDT10                                                            
         MVC   SVEND,SVSTART       SET DEFAULT END = START                      
         A     R4,0(R1)                                                         
         LLC   R0,5(R2)                                                         
         C     R0,0(R1)            TEST ALL INPUT PROCESSED                     
         BE    VDT20               YES                                          
         CLI   0(R4),C'-'                                                       
         BNE   PAYERR                                                           
         LA    R4,1(R4)                                                         
         GOTO1 (RF),(R1),(R4),SVEND                                             
         OC    0(4,R1),0(R1)                                                    
         BZ    PAYERR                                                           
         MVI   ERRCD,STENDERR                                                   
         CLC   SVSTART,SVEND                                                    
         BH    PAYERR                                                           
         B     VDT20                                                            
         EJECT                                                                  
*                                                                               
* VALIDATE FOR M/Y                                                              
*                                                                               
VDT10    GOTO1 (RF),(R1),(2,(R4)),SVSTART                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    PAYERR                                                           
         LLC   R0,5(R2)                                                         
         C     R0,0(R1)            TEST ALL DATA PROCESSED                      
         BNE   PAYERR                                                           
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BE    *+12                YES - USE BRDCST MNTHS                       
         CLI   PAYMD,C'N'          TEST NTWK                                    
         BE    VDT12                                                            
* GET BRDCST MONTH DATES                                                        
         GOTO1 VCALLOV,DMCB,0,X'D9000A1D'  GET A(GETBROAD)                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(1,SVSTART),WORK,VGETDAY,VADDAY                        
         MVC   SVSTART(12),WORK                                                 
         B     VDT18                                                            
*                                                                               
* NTWK DEFAULTS TO CALENDAR MONTH                                               
*                                                                               
VDT12    MVC   SVSTART+4(2),=C'01'                                              
         GOTO1 VADDAY,DMCB,SVSTART,WORK,27                                      
*                                                                               
VDT14    GOTO1 (RF),(R1),WORK,WORK+6,1                                          
         CLC   WORK(4),WORK+6                                                   
         BNE   VDT16                                                            
         MVC   WORK(6),WORK+6                                                   
         B     VDT14                                                            
VDT16    MVC   SVEND,WORK                                                       
*                                                                               
VDT18    GOTO1 VDATCON,DMCB,SVSTART,(5,WORK)                                    
         GOTO1 VDATCON,DMCB,SVEND,(5,WORK+9)                                    
         MVI   WORK+8,C'-'                                                      
         MVC   PAYDTXP(17),WORK                                                 
*                                                                               
VDT20    GOTO1 VDATCON,DMCB,SVSTART,(2,SVSTARTP)                                
         GOTO1 VDATCON,DMCB,SVEND,(2,SVENDP)                                    
*                                                                               
         MVI   SVNETPAK,0                                                       
         CLI   PAYMD,C'N'                                                       
         BNE   VDT30                                                            
**NOP**  B     DTX                 ACTIVATE FOR ALL MEDIA MH 12/24/92           
*                                                                               
* TEST FOR SPECIAL OFFICE FEATURE                                               
*                                                                               
VDT30    DS    0H                                                               
         CLI   SVPPROF+4,C'*'      * IS NOT AN OFFICE YOU IDIOT                 
         BE    DTX                                                              
         CLI   SVPPROF+4,C'('      NOR IS ANYTHING LESS THAN '('                
         BL    DTX                                                              
*                                                                               
         MVI   ERRCD,BADOFCDT                                                   
         MVC   DUB(2),SVPPROF+5    MOVE START YEAR/MONTH                        
         OC    DUB(2),DUB          TEST THEY ENTERED A DATE                     
         BZ    PAYERR              IF NOT, MAKE THEM DO IT                      
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK                                        
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A1D'  GET A(GETBROAD)                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(1,WORK),WORK+6,VGETDAY,VADDAY                         
         CLC   SVEND,WORK+6        TEST REQ ENDS BEFORE OFFC START              
         BL    DTX                 YES - OK                                     
         CLC   SVSTART,WORK+6      TEST REQ STARTS AFTER OFFC START             
         BL    PAYERR              NO - ERROR                                   
         MVI   SVOFFCSW,C'Y'                                                    
*                                                                               
DTX      CLC   =C'MC',AGYALPHA                                                  
         BNE   DTXX                                                             
         CLC   SVSTART,=X'FAF2F0F4F0F1'                                         
         BL    PAYERR                                                           
DTXX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================*                         
* READ STATION ADDRESS RECORD                         *                         
*=====================================================*                         
         SPACE 1                                                                
GETADDR  NTR1  BASE=*,LABEL=*                                                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'A'                                                         
         MVC   KEY+1(1),PAYMD                                                   
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         USING ADDRECD,R6                                                       
         GOTO1 STA                                                              
         MVI   ERRCD,NOADDR                                                     
         CLC   KEY(15),0(R6)                                                    
         BE    GETADDR2                                                         
         CLC   =C'CK',AGYALPHA                                                  
         BNE   GETADDRX                                                         
         XC    ANAME,ANAME                                                      
         XC    A1LINE,A1LINE                                                    
         XC    A2LINE,A2LINE                                                    
         XC    A3LINE,A3LINE                                                    
         XC    ABIGZIP,ABIGZIP                                                  
         MVC   ANAME(20),=CL20'** NO ADDRESS **'                                
*                                                                               
GETADDR2 L     RE,AREC2            POINT TO SQUASHER BUILD AREA                 
         XC    0(84,RE),0(RE)                                                   
         MVC   0(20,RE),ANAME                                                   
         MVC   21(24,RE),A1LINE                                                 
         MVC   46(24,RE),A2LINE                                                 
         MVC   71(2,RE),A3LINE                                                  
         MVC   74(10,RE),ABIGZIP                                                
         OC    1(80,RE),SPACES                                                  
         OC    81(80,RE),SPACES                                                 
         GOTO1 SQUASHER,DMCB,AREC2,84                                           
         L     RE,AREC2                                                         
         MVC   PAYSTXP,0(RE)                                                    
*                                                                               
         MVI   ERRCD,0             CLEAR ERROR CODE                             
         CLI   SVPPROFA+3,C'Y'     TEST AUTO ACC POSTING                        
         BNE   GETADDRX                                                         
         MVC   PAYNME,SPACES                                                    
         MVC   PAYNME,ANAME                                                     
         LA    R4,PAYSTADD                                                      
         MVC   0(26,R4),SPACES                                                  
         MVC   0(24,R4),A1LINE                                                  
         LA    R4,26(R4)                                                        
         MVC   0(78,R4),SPACES                                                  
         MVC   0(24,R4),A2LINE                                                  
         MVC   26(2,R4),A3LINE                                                  
         MVC   52(10,R4),ABIGZIP                                                
         MVI   PAYCNTRY,C'U'                                                    
         CLC   =C'CANAD',AZIP                                                   
         BNE   *+8                                                              
         MVI   PAYCNTRY,C'C'                                                    
*                                                                               
GETADDRX CLI   ERRCD,0             SET CC FOR EXIT                              
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*========================================================*                      
* READ REP RECORD                                        *                      
*========================================================*                      
         SPACE 1                                                                
GETREP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),PAYMD                                                   
         MVC   KEY+2(3),QREP                                                    
         MVC   KEY+5(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         USING REPRECD,R6                                                       
*                                                                               
         GOTO1 STA                                                              
         MVI   ERRCD,NOREP                                                      
         CLC   KEY(15),0(R6)                                                    
         BNE   GETREPX                                                          
         OC    SVSPREP,SVSPREP     TEST SPECIAL REP ACTIVE                      
         BZ    *+10                                                             
         MVC   SVREPTYP,RSYND      SAVE SYNDICATION FLAG                        
         MVC   SVREPOP1,RPOPT1                                                  
*                                                                               
         L     RE,AREC2            POINT TO SQUASHER BUILD AREA                 
         XC    0(86,RE),0(RE)                                                   
         MVC   0(22,RE),RNAME                                                   
         MVC   23(24,RE),R1LINE                                                 
         MVC   48(24,RE),R2LINE                                                 
         MVC   73(2,RE),R3LINE                                                  
         MVC   76(10,RE),RBIGZIP                                                
         OC    1(80,RE),SPACES                                                  
         OC    81(80,RE),SPACES                                                 
         GOTO1 SQUASHER,DMCB,AREC2,86                                           
         L     RE,AREC2                                                         
         MVC   PAYEEXP,0(RE)                                                    
         EJECT                                                                  
         MVI   ERRCD,0             CLEAR ERROR CODE                             
         CLI   SVPPROFA+3,C'Y'     TEST AUTO ACC POSTING                        
         BNE   GETREPX                                                          
*                                                                               
         MVC   PAYNME,RNAME                                                     
         LA    R4,PAYSTADD                                                      
         MVC   0(26,R4),SPACES                                                  
         MVC   0(24,R4),R1LINE                                                  
         LA    R4,26(R4)                                                        
         MVC   0(78,R4),SPACES                                                  
         MVC   0(24,R4),R2LINE                                                  
         MVC   26(2,R4),R3LINE                                                  
         MVC   52(10,R4),RBIGZIP                                                
         MVI   PAYCNTRY,C'U'                                                    
         CLC   =C'CANAD',RZIP                                                   
         BNE   *+8                                                              
         MVI   PAYCNTRY,C'C'                                                    
         MVC   PAYREPST,RUNWNET   MOVE UNWIRED REP FLAG                         
         CLI   PAYREPST,C'Y'       IF NOT A Y, SET TO N                         
         BE    *+8                                                              
         MVI   PAYREPST,C'N'                                                    
*                                                                               
GETREPX  CLI   ERRCD,0             SET CC FOR EXIT                              
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*=======================================================*                       
* READ MKT REC FOR LOCKOUT CHECK                                                
*=======================================================*                       
         SPACE 1                                                                
GETMKT   NTR1  BASE=*,LABEL=*                                                   
         XC    SVMACCS,SVMACCS                                                  
*                                                                               
         MVI   ERRCD,0                   CLEAR ERROR CODE                       
         OC    T213FFD+6(2),T213FFD+6    TEST ANY LIMIT ACCESS                  
         BZ    GETMKTX                                                          
*                                                                               
GETMKT2  XC    KEY,KEY             READ MARKET RECORD                           
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),PAYMD                                                   
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         GOTO1 STA                                                              
         CLC   KEY(8),0(R6)                                                     
         BE    GETMKT4                                                          
         MVI   ERRCD,INVMKT                                                     
         LA    R2,PAYSTH                                                        
         GOTO1 ERROR                                                            
*                                                                               
         USING MKTRECD,R6                                                       
GETMKT4  MVC   SVMACCS,MKTLTACC                                                 
         BRAS  RE,CALLOFCR                                                      
*                                                                               
GETMKTX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
GETRANGE NTR1 BASE=*,LABEL=*                                                    
         MVI  SVRANFL,1                                                         
         LA   R1,22(R4)              PREPARE TO VALIDATE 1ST HALF OF            
         LA   RE,0                   RANGE (R1 AT FIELD, RE STORES              
         B    GETRAN2                LENGTH, RF MAXIMUM LENGTH)                 
*                                                                               
GETRAN1  MVI  SVRANFL,2                                                         
         LA   R1,1(R1)               PREPARE TO VALIDATE 2ND HALF OF            
         LR   R2,R1                  RANGE (R1 AND R2 AT FIELD, RE              
         LA   RE,0                   STORES LENGTH, RF MAXIMUM LENGTH)          
GETRAN2  LA   RF,3                                                              
*                                                                               
GETRAN3  CLI  0(R1),C'-'                                                        
         BE   GETRAN4                                                           
         CLI  0(R1),C'0'             VALUES OF RANGE MUST FIT BETWEEN           
         BL   GETRAN5                0 AND 9                                    
         CLI  0(R1),C'9'                                                        
         BH   GETRAN5                                                           
         CR   RE,RF                  EACH HALF OF RANGE CAN ONLY BE 3           
         BH   GETRERR                LONG                                       
         LA   R1,1(R1)                                                          
         LA   RE,1(RE)                                                          
         B    GETRAN3                                                           
*                                                                               
GETRAN4  CLI  SVRANFL,2              2ND HALF OF RANGE CANNOT END IN -          
         BE   GETRERR                                                           
         LA   RF,0                   EACH HALF OF RANGE MUST BE AT              
         CR   RE,RF                  LEAST 1 LONG                               
         BE   GETRERR                                                           
         BCTR RE,0                                                              
         EX   RE,*+8                                                            
         B    *+10                                                              
         PACK DUB,22(0,R4)                                                      
         CVB  RE,DUB                                                            
         STCM RE,3,SVOPTRAN                                                     
         B    GETRAN1                                                           
*                                                                               
GETRAN5  CLI  0(R1),C' '                                                        
         BNE  GETRERR                                                           
         CLI  SVRANFL,2                                                         
         BE   GETRAN6                                                           
         LTR  RE,RE                                                             
         BZ   GETRERR                                                           
         BCTR RE,0                                                              
         EX   RE,*+8                 IF ONLY ONE BUYLINE ENTERED (NO            
         B    *+10                   RANGE) ... DEFAULT RANGEHI TO              
         PACK DUB,22(0,R4)           RANGELOW                                   
         CVB  RE,DUB                                                            
         STCM RE,3,SVOPTRAN                                                     
         STCM RE,3,SVOPTRAN+2                                                   
         B    GETRAN7                                                           
*                                                                               
GETRAN6  LTR   RE,RE                                                            
         BNP   GETRERR                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,SVOPTRAN+2                                                  
         CLC   SVOPTRAN+2(2),SVOPTRAN                                           
         BNH   GETRERR                                                          
*                                                                               
GETRAN7  MVI   SVRANFL,C'S'                                                     
         XIT1                                                                   
*                                                                               
GETRERR  LA    R2,PAYOPH            POINT R2                                    
         B     PAYERR                                                           
*======================================================*                        
* READ A STATION EQUIVALENCE RECORD                    *                        
*======================================================*                        
         SPACE 1                                                                
GETSTEQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVCXTRA+2,C'*'      TEST CCUSA                                   
         BNE   STEQ02                                                           
*                                                                               
         LA    R4,ELEM             SET UP SPACNVAL INTERFACE BLOCK              
         USING SPAVBLKD,R4                                                      
         LA    RF,SPAVBLKL                                                      
         XCEF  SPAVBLK,(RF)                                                     
         LA    R1,T213FFD                                                       
         ST    R1,SPAVATWA                                                      
         MVC   SPAVACMF,VCOMFACS                                                
         LA    R1,SVID                                                          
         ST    R1,SPAVAACN                                                      
         MVC   SPAVAGY,AGYALPHA                                                 
         MVC   SPAVMED,PAYMD                                                    
         MVC   SPAVSTA,QSTA                                                     
         MVC   SPAVCIFC,SVCLTIFC                                                
         GOTO1 VDATCON,DMCB,SVSTART,(3,SPAVSDT)                                 
         GOTO1 VDATCON,DMCB,SVEND,(3,SPAVEDT)                                   
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A44'   GET A(SPACNVAL)                     
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,SPAVBLK   CALL IT                                      
         MVC   SVID,SPACES                                                      
         MVC   SVID(5),SPAVDACN       SAVE DEFAULT ACN                          
*                                                                               
STEQ02   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D44'                                                  
         MVC   KEY+2(1),SVKEY      A/M                                          
         MVC   KEY+3(2),SVKEY+1    CLT                                          
         MVC   KEY+5(5),QSTA                                                    
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    *+8                                                              
         MVI   KEY+9,C' '          FORCE MEDIA TO ' '                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    STEQ03                                                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+3(2),=X'FFFF'   READ FOR ALL CLT RECORD                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         BNE   STEQX                                                            
*                                                                               
STEQ03   MVC   AREC,AREC1                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVCXTRA+2,C'*'      TEST CCUSA                                   
         BE    STEQ10                                                           
*                                                                               
         MVI   ELCODE,3                                                         
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         USING STEEL03,R6                                                       
*                                                                               
         BAS   RE,STEQEL2                                                       
         BNE   STEQX                                                            
*                                                                               
STEQ04   CLC   STEMGID,SVCXTRA+2   TEST MKTGRP ID                               
         BNE   STEQ06                                                           
         PACK  WORK(3),SVID+1(5)                                                
         CLC   STEMGRP,WORK        TEST SAME MKTGRP NUM                         
         BNE   STEQ06                                                           
         SR    R0,R0                                                            
         ICM   R0,3,STEMGMKT       GET MKT NUM                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            OVERRIDE DEFAULT MARKET NUMBER               
         B     STEQX                                                            
*                                                                               
STEQ06   BAS   RE,STEQEL                                                        
         BE    STEQ04                                                           
         B     STEQX                                                            
*                                                                               
STEQ10   MVI   ELCODE,2            LOOK FOR ACN NO EQUIVALENCE ELEMENTS         
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         USING STEEL02,R6                                                       
         BAS   RE,STEQEL2                                                       
         B     *+8                                                              
*                                                                               
STEQ12   BAS   RE,STEQEL           LOOK AT NEXT ELEMENT                         
         BNE   STEQX                                                            
         USING STEEL02,R6                                                       
         CLI   INCHSW,C'Y'         TEST CALLED BY INCH                          
         BE STEQ20                                                              
         CLC   STEACN,SVID         TEST ACN NUMBER                              
         BNE   STEQ12                                                           
         SR    R1,R1               MATCH - GET THE MARKET NUMBER                
         ICM   R1,3,STEACNMK                                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         B     STEQX                                                            
         SPACE 1                                                                
*================================================================*              
*CALLED BY INCH. MATCH MARKET NUMBERS TO DETERMINE ACN NUMBER                   
*================================================================*              
         SPACE 1                                                                
STEQ20   PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         CLM   R0,3,STEACNMK                                                    
         BNE   STEQ12                                                           
         MVC   SVID,SPACES                                                      
         MVC   SVID(5),STEACN      IF MARKETS MATCH, USE ACN                    
*                                                                               
STEQX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
STEQEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
STEQEL2  CLI   0(R6),0                                                          
         BE    STEQELX                                                          
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     STEQEL                                                           
*                                                                               
STEQELX  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*======================================================*                        
* READ AN EXCHANGE RATE RECORD                         *                        
*======================================================*                        
         SPACE 1                                                                
GETXCH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVPPROFA+4,C'N'     TEST SUPPRESS AUTO XCH RATE                  
         BE    GETXCH10            YES - FORGET IT                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D4B'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         GOTO1 VDATCON,DMCB,(5,0),(3,DUB)                                       
         MVC   KEY+4(1),DUB                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOXCHRT)                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   GETXCHX                                                          
*                                                                               
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,24(R6)                                                        
         USING MXREL,R6                                                         
GETXCH2  CLI   0(R6),MXRELCDQ                                                   
         BE    GETXCH4                                                          
         CLI   0(R6),0                                                          
         BE    GETXCHX                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETXCH2                                                          
*                                                                               
GETXCH4  PACK  DUB,TODAY+2(2)      GET CURRENT MONTH                            
         CVB   RE,DUB                                                           
         BCTR  RE,0                                                             
         AR    RE,RE               X 2                                          
         LA    RE,MXRMNTHS(RE)                                                  
         MVC   SVXRATE,0(RE)       AND SAVE EXCHANGE RATE                       
GETXCH10 MVI   ERRCD,0             SET NO ERROR FLAG                            
*                                                                               
GETXCHX  CLI    ERRCD,0            SET CC ON EXIT                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*=========================================================*                     
* READ CONTROL FILE TO LOCATE CCUSA ACCTG FILE            *                     
*=========================================================*                     
         SPACE 1                                                                
GETACC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'                                                        
         MVC   WORK+23(2),T213FFD+10   MOVE ID NUMBER TO KEY                    
*                                                                               
         CLC   SVPPROF+14(2),=C'AA'  TEST ALT ACC AGENCY                        
         BNH   GETACC2                                                          
         CLC   SVPPROF+14(2),=C'00'  00 DOESN'T COUNT                           
         BE    GETACC2                                                          
         MVI   WORK,C'5'           THEN READ ACCESS RECORD                      
         MVC   WORK+23(2),SVPPROF+14                                            
*                                                                               
GETACC2  GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC2                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AREC2                                                         
         CLC   WORK(25),0(R1)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,28(R1)                                                        
GETACC4  CLI   0(R1),X'21'         TEST SYSTEM ELEMENT                          
         BNE   *+12                                                             
         CLI   2(R1),X'06'         TEST ACCOUNTING                              
         BE    GETACC10                                                         
*                                                                               
         LLC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   GETACC4                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOACCACC)                                            
         B     GETACCERR                                                        
*                                                                               
GETACC10 MVC   SVXFRACC,3(R1)      SAVE ACC SYSTEM NUMBER                       
         MVC   SVXFRCOM,4(R1)      AND COMPANY CODE                             
*                                                                               
         CLC   T213FFD+10(2),=AL2(CCUSAID)  TEST ID = CCUSA                     
         BE    *+14                                                             
         CLC   T213FFD+10(2),=AL2(COKEAT)   TEST ID = COKEAT                    
         BNE   GETACCX                                                          
*                                                                               
         MVI   ERRCD,BADINTR                                                    
         L     R1,AREC1            POINT TO CLTHDR                              
         LA    R1,CCLTIFC-CLTHDRD(R1)                                           
         LA    R0,8                COUNT DIGITS IN INTERFACE CODE               
*                                                                               
GETACC12 CLI   0(R1),C' '                                                       
         BNH   GETACC14                                                         
         CLI   0(R1),C'0'                                                       
         BL    GETACCERR                                                        
         CLI   0(R1),C'9'                                                       
         BH    GETACCERR                                                        
         LA    R1,1(R1)                                                         
         BCT   R0,GETACC12                                                      
*                                                                               
GETACC14 LA    RE,8                                                             
         SR    RE,R0                                                            
         BZ    GETACCERR                                                        
         BCTR  RE,0                                                             
         L     R1,AREC1                                                         
         LA    R1,CCLTIFC-CLTHDRD(R1)                                           
         MVC   SVCLTIFC,0(R1)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)    *EXECUTED*                                        
         CP    DUB,=P'0'                                                        
         BNE   GETACC20                                                         
         CLC   =C'BB',QCLT         ALLOW 0 INTERFACE CODE FOR BB                
         BNE   GETACCERR                                                        
*                                                                               
GETACC20 OI    DUB+7,X'0F'                                                      
         UNPK  SVXFRAGN,DUB                                                     
*                                                                               
GETACCX  XIT1                                                                   
*                                                                               
GETACCERR GOTO1 ERROR                                                           
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T213FFD+6                                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,SVKEY                                                   
         MVC   OFCLMT(4),T213FFD+6                                              
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD                                                       
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK2),VCOMFACS                                  
         MVI   ERRCD,SCRTYERR                                                   
         CLI   0(R1),0                                                          
         JE    EXIT                                                             
         GOTO1 ERROR                                                            
         LTORG                                                                  
         SPACE 1                                                                
         EJECT                                                                  
*===============================================================*               
* SET LOCK PERIOD DATES IN DUB AND COMPARE TO SVSTART/SVEND                     
*===============================================================*               
                                                                                
CHKLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    SVTSTOPT,X'10'      TEST PAID OPTION ACTIVE                      
         BO    CHKLX               YES - NO LOCK REQD                           
*                                                                               
         TM    SVCOPT2,X'08'       TEST CLIENT FROZEN                           
         BO    CHKLERR                                                          
*                                                                               
         OC    0(2,R4),0(R4)       TEST LOCK DATE PRESENT                       
         BZ    CHKLX                                                            
                                                                                
         MVC   DUB(2),0(R4)                                                     
         NI    DUB+1,X'3F'         DROP PRIOR/SUBSEQ FLAGS                      
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK                                        
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A1D'  GET A(GETBROAD)                      
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(1,WORK),WORK+6,VGETDAY,VADDAY                         
         GOTO1 VDATCON,DMCB,WORK+6,(2,DUB)                                      
         GOTO1 (RF),(R1),WORK+12,(2,DUB+2)                                      
*                                                                               
         TM    1(R4),X'80'         TEST MONTH AND PRIOR                         
         BZ    *+10                                                             
         XC    DUB(2),DUB          CLEAR START DATE                             
*                                                                               
         TM    1(R4),X'40'         TEST MONTH AND SUBSEQUENT                    
         BZ    *+12                                                             
         LHI   R0,-1                                                            
         STCM  R0,3,DUB+2          SET HIGH END DATE                            
*                                                                               
         CLC   SVSTARTP,DUB+2      PERIOD START AFTER LOCK ENDS                 
         BH    CHKLX                                                            
         CLC   SVENDP,DUB          PERIOD END BEFORE LOCK STARTS                
         BL    CHKLX                                                            
*                                                                               
CHKLERR  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CLFROZEN)                                              
         LA    R2,PAYERH                                                        
         GOTO1 ERROR                                                            
*                                                                               
CHKLX    XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPPAYWORK                                                      
         EJECT                                                                  
GENOLD   DSECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
         SPACE 2                                                                
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         SPACE 2                                                                
STEQRECD DSECT                                                                  
       ++INCLUDE SPGENSTEQ                                                      
         EJECT                                                                  
       ++INCLUDE SPGENXRT                                                       
         SPACE 2                                                                
       ++INCLUDE DDOFFICED                                                      
         SPACE 2                                                                
       ++INCLUDE FASECRETD                                                      
         SPACE 2                                                                
       ++INCLUDE DDGLOBEQUS                                                     
         SPACE 2                                                                
       ++INCLUDE SPGETBUBLD                                                     
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
SPAVBLKD DSECT                                                                  
       ++INCLUDE SPACNVALD                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPPAY01   04/25/16'                                      
         END                                                                    
