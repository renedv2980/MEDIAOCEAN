*          DATA SET SPMAT00S   AT LEVEL 012 AS OF 05/01/02                      
*PHASE T21500A,+0                                                               
*INCLUDE MEDGET                                                                 
*INCLUDE BINSRCH                                                                
         TITLE 'T21500 - I.C.S. MATCHING BASE MODULE'                           
         PRINT NOGEN                                                            
T21500   CSECT                                                                  
         NMOD1 MATWKL,T21500,R8,R7                                              
         USING GENOLD,RC                                                        
         BAS   RE,INITL                                                         
         USING T215FFD,RA                                                       
         SPACE 1                                                                
         L     RF,0(R1)            IF PFKEY NUMBER 3 PRESSED                    
         USING TIOBD,RF                                                         
         CLI   TIOBAID,3                                                        
         BE    AUTOSWAP                                                         
         CLI   TIOBAID,3+12                                                     
         BNE   START                                                            
AUTOSWAP XC    ICSSRV,ICSSRV       THEN AUTOSWAP TO $INVOICE                    
         MVC   ICSSRV(4),=C'+INV'                                               
         XIT1                                                                   
         SPACE 1                                                                
START    L     R4,16(R1)           A(COMFACS)                                   
         ST    R4,VCOMFACS                                                      
         MVC   VDATCON,CDATCON-COMFACSD(R4)                                     
         MVC   VSCANNER,CSCANNER-COMFACSD(R4)                                   
         MVC   VXSORT,CXSORT-COMFACSD(R4)                                       
         MVC   VGETPROF,CGETPROF-COMFACSD(R4)                                   
         MVC   VGLOBBER,CGLOBBER-COMFACSD(R4)                                   
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'MAT',3,GLVPGM                          
*                                                                               
        GOTO1 (RF),DMCB,=C'GETD',WORK,24,GLVXCTL                                
        CLI   8(R1),0             IF TRANSFER RECORD FOUND                      
        BNE   STRT10                                                            
        GOTO1 (RF),(R1),=C'DELE'                    DELETE IT                   
*                                                                               
STRT10   DS    0H                                                               
         XC    DMCB(8),DMCB        GET A(STAPACK)                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSTAPACK                                                  
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAPACK,0(R1)                                                   
*                                                                               
         XC    DMCB(8),DMCB        GET A(CLPACK)                                
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QCLPACK                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VCLPACK,0(R1)                                                    
*                                                                               
         XC    DMCB(8),DMCB        GET A(CODAY) = DAYUNPK                       
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QUNDAY                                                    
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VCODAY,0(R1)                                                     
*                                                                               
         XC    DMCB(8),DMCB        GET A(GETRATE)                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QGETRATE                                                  
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETRATE,0(R1)                                                   
*                                                                               
         RELOC RELO00                                                           
*                                                                               
         L     RF,=V(BINSRCH)                                                   
         A     RF,RELO00                                                        
         ST    RF,VBINSRCH                                                      
*                                                                               
         LA    R9,IOAREA                                                        
         LA    R1,GENOLD                                                        
         A     R1,=A(BUFFER-GENOLD)                                             
         ST    R1,ABUFFER                                                       
         A     R1,=A(BUFFX-BUFFER)                                              
         ST    R1,ABUFFX           A(END OF BUFFER)                             
         LA    R1,REPLST                                                        
         ST    R1,AFREP                                                         
*                                                                               
         LA    R1,FLMTAB           SET FLMTAB ADDR IN BINSRCH PARS              
         ST    R1,AFLMTAB          (=FLMTPARS+4)                                
*                                                                               
         MVC   TWPAGSIZ,=Y(TWPAGSZQ)  SET TWA PAGE SIZE (SEE SPMATWK)           
         MVC   BUFFRL,=A(BUFLEN)      SET BUFFER SIZE                           
*                                                                               
         MVI   NEWSW,C'Y'          INITIALIZE TO NEW MODE                       
*                                  CLEAR SCREEN                                 
         LA    R3,ICSLIN1H                                                      
         LA    R4,L'ICSLIN1+8                                                   
         LA    R5,ICSLAST-1                                                     
CLR2     OC    8(L'ICSLIN1,R3),8(R3)                                            
         BZ    CLR4                ALREADY CLEAR                                
         XC    8(L'ICSLIN1,R3),8(R3)                                            
         FOUT  (R3)                                                             
CLR4     BXLE  R3,R4,CLR2                                                       
         EJECT                                                                  
*                   VALIDATE MEDIA                                              
         SPACE 3                                                                
DB1      LA    R2,ICSMEDH                                                       
         TM    4(R2),X'20'                                                      
         BO    DB3                                                              
         XC    HKEY,HKEY                                                        
         CLI   5(R2),0                                                          
         BNE   DB1AA                                                            
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                             
*                                                                               
DB1AA    LA    R3,MEDERR                                                        
         GOTO1 =V(MEDGET),DMCB,(ICSMED,AGYALPHA),VDATAMGR,WORK,        X        
               RR=RELO00                                                        
*                                                                               
         CLI   DMCB+8,X'FF'                                                     
         BE    DB1B                ERROR                                        
         MVC   ICSMEDN,WORK+1                                                   
         MVC   HKEY+1(1),WORK      AGYMED BYTE                                  
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGYALPHA                                                
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         MVC   COUNTRY,AGYPROF+7-AGYHDR+IOAREA                                  
         MVI   SPADSW,C'N'                                                      
         CLI   AGYFLAG1-AGYHDR+IOAREA,X'80'  ADDS INTEFACE                      
         BZ    *+8                                                              
         MVI   SPADSW,C'Y'                                                      
*                                                                               
         MVI   CENTS,C'Y'          CENTS (ALWAYS Y)                             
         B     DB2                                                              
*                                                                               
DB1B     DS    0H                                                               
         XC    ICSMEDN,ICSMEDN                                                  
         FOUT  ICSMEDNH                                                         
         B     MYERR                                                            
DB2      DS    0H                                                               
         FOUT  ICSMEDNH                 SEND MED NAME                           
         NI    ICSSTAH+4,X'DF'          RESET VAL. BITS                         
         NI    ICSHPRDH+4,X'DF'                                                 
         NI    ICSCLTH+4,X'DF'                                                  
         NI    ICSOPTH+4,X'DF'                                                  
         OI    4(R2),X'20'                                                      
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
         OI    CHSW,X'80'                                                       
         EJECT                                                                  
*                   VALIDATE STATION                                            
         SPACE 3                                                                
DB3      LA    R2,ICSSTAH                                                       
         TM    ICSCLTH+4,X'20'     ON CHANGE OF CLIENT                          
         BNZ   *+12                                                             
         NI    ICSSTAH+4,X'DF'     FORCE STATION VALIDATION                     
         NI    ICSHPRDH+4,X'DF'    FORCE PRD/MODE VALIDATION                    
         TM    4(R2),X'20'                                                      
         BO    DB8                                                              
         CLI   5(R2),0                                                          
         BNE   DB3A                                                             
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPSTA                            
*                                                                               
DB3A     DS    0H                                                               
         CLI   ICSCLTH+5,0         AND GET CLIENT                               
         BNE   DB3B                                                             
         GOTO1 VGLOBBER,DMCB,=C'GETF',ICSCLTH,,GLVSPCLT                         
*                                                                               
DB3B     DS    0H                                                               
         XC    STEQSTA,STEQSTA                                                  
         XC    ICSMKTN,ICSMKTN                                                  
         FOUT  ICSMKTNH                                                         
         LA    R3,STAERR                                                        
*                                                                               
         XC    DMCB(8),DMCB        GET A(STAVAL)                                
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSTAVAL                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
*                                                                               
         LA    R3,STAERR                                                        
         LA    R4,WORK                                                          
         USING STABLKD,R4                                                       
         XC    WORK,WORK                                                        
         ST    R2,STBADDR                                                       
         MVC   STBMED,ICSMED                                                    
         MVC   STBCTRY,COUNTRY                                                  
         GOTO1 (RF),DMCB,STABLKD                                                
         CLI   STBERR,0                                                         
         BNE   MYERR                                                            
*                             BUILD STATION KEY                                 
         MVC   STA,STBSTA     SAVE STATION ALPHA (AND NET)                      
         CLI   STBSTA+4,C'/'       BLANK AND /                                  
         BH    DB3D                                                             
         MVI   STA+4,C'T'          BECOME T ON FILE                             
         MVC   ICSSTA(8),STBSTA                                                 
         MVI   ICSSTA+4,C'/'       NEED / ON SCREEN                             
         FOUT  ICSSTAH                                                          
*                                                                               
DB3D     DS    0H                                                               
         MVI   SKEY,C'0'                                                        
         MVC   SKEY+1(16),SKEY                                                  
         MVI   SKEY,C'S'                                                        
         MVC   SKEY+1(1),ICSMED                                                 
         MVC   SKEY+2(5),STA                                                    
         MVC   SKEY+7(2),AGYALPHA                                               
         MVC   SKEY+9(3),ICSCLT                                                 
         OI    SKEY+11,X'40'                                                    
         BAS   RE,READSTA                                                       
         CLC   SKEY(15),IOAREA     TEST FOUND STATION                           
         BNE   MYERR                                                            
         USING STAREC,R9                                                        
         SPACE 1                                                                
         XC    SVOLDMK1(4),SVOLDMK1                                             
         CLC   STAKLEN,=H'256'     TEST OLD MKT NUM IN RECORD                   
         BL    *+16                                                             
         MVC   SVOLDMK1,STOLDMK1   SAVE OLD MARKET NUMBERS FOR PW               
         MVC   SVOLDMK2,STOLDMK2                                                
*                                  PACK MARKET AND STATION                      
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPMED,ICSMED                                                   
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPQMKT,SMKT                                                    
         MVC   STAPQSTA(8),STA                                                  
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BNE   MYERR                                                            
         MVC   BMS,STAPMKST                                                     
         MVC   HOMEMKT,BMS         SAVE HOME MARKET                             
*                                                                               
         NI    CBLHOPT,255-CBLHHAQ   SET OF CBH REQ AND ALL NETS                
         MVI   BCBLNET,0           CABLE NETWORK                                
         CLI   ICSSTA+4,C'/'                                                    
         BNE   DB3F                                                             
*                                                                               
         OI    CBLHOPT,CBLHHDQ     SET IS CBH REQ                               
         MVC   BCBLNET,BMS+4                                                    
         NI    BCBLNET,X'7F'       ONLY 7 BITS                                  
         BNZ   *+8                                                              
         OI    CBLHOPT,CBLHALQ     ALL NETS                                     
                                                                                
         DROP  R1                                                               
*                                                                               
DB3F     DS    0H                                                               
         MVI   SKEY,C'0'                                                        
         MVC   SKEY+1(16),SKEY                                                  
         MVI   SKEY,C'M'                                                        
         MVC   SKEY+1(1),ICSMED                                                 
         MVC   SKEY+2(4),SMKT                                                   
         MVC   SKEY+6(2),AGYALPHA                                               
         BAS   RE,READSTA                                                       
*                                                                               
         L     RF,VTWA                                                          
         LA    RE,6(RF)                                                         
         CLI   0(RE),C'+'          TEST MKT LIMIT ACCESS                        
         BE    DB7A                                                             
         LA    RE,8(RF)            TEST ALTERNATE LOCATION                      
         CLI   0(RE),C'+'                                                       
         BNE   DB7C                                                             
         USING MKTREC,R9                                                        
*                                                                               
DB7A     LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
DB7B     DS    0H                                                               
         CLC   1(1,RE),0(R1)                                                    
         BE    DB7C                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,DB7B                                                          
*                                                                               
         LA    R3,ACCERR                                                        
         B     MYERR                                                            
DB7C     DS    0H                                                               
         MVC   ICSMKTN,MKTNAME                                                  
         MVC   HOMEMKTN,MKTNAME                                                 
         FOUT  ICSMKTNH                                                         
*                                                                               
         CLI   ICSMED,C'N'         IF NETWORK                                   
         BNE   DB7D                                                             
         CLI   COUNTRY,C'C'        AND CANADA                                   
         BNE   DB7D                                                             
         XC    BMS(2),BMS          BUYS ARE IN MARKET 0000                      
         XC    HOMEMKT,HOMEMKT                                                  
*                                                                               
DB7D     DS    0H                                                               
         OI    4(R2),X'20'         SET VALIDATION BIT                           
         NI    ICSOPTH+4,X'DF'     FORCE OPTION REVAL                           
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPSTA                            
         OI    CHSW,X'40'                                                       
         EJECT                                                                  
*                   VALIDATE CLIENT                                             
         SPACE 3                                                                
DB8      LA    R2,ICSCLTH                                                       
         OC    ICSCLT,SPACES                                                    
         TM    4(R2),X'20'                                                      
         BO    DB8A                                                             
*                                                                               
         CLI   5(R2),0                                                          
         BNE   DB8AA                                                            
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPCLT                            
         OC    ICSCLT,SPACES                                                    
*                                                                               
DB8AA    XC    STEQCLT,STEQCLT                                                  
         LA    R3,CLTERR                                                        
         XC    ICSCLTN,ICSCLTN                                                  
         FOUT  ICSCLTNH                                                         
         GOTO1 VCLPACK,DMCB,ICSCLT,BCLT                                         
         CLI   DMCB,0                                                           
         BNE   MYERR                                                            
*                                                                               
         XC    HKEY+2(11),HKEY+2                                                
         MVC   HKEY+2(2),BCLT                                                   
         MVC   KEY(13),HKEY                                                     
         BAS   RE,HIGH                                                          
         LA    R3,CLTERR2                                                       
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MYERR               CLIENT NOT FOUND                             
         MVC   CLTDSK,KEY+14       SAVE CLTHDR DISK ADDR                        
         B     DB8B                                                             
DB8A     MVC   KEY+14(4),CLTDSK                                                 
*                                  ALWAYS READ CLTHDR FOR PRODUCT LIST          
DB8B     BAS   RE,GETREC                                                        
         USING CLTHDR,R9                                                        
         MVC   SVCLPROF,CPROF                                                   
         MVC   SVTRACLT,CMCLTCOD  TRAFFIC OVERRIDE CLIENT                       
         MVI   SVPWCLT,C'N'                                                     
         OC    CPWPCT,CPWPCT                                                    
         BZ    *+8                                                              
         MVI   SVPWCLT,C'Y'                                                     
*                                                                               
         MVI   SVIDRQD,C'N'        ID REQUIRED SWITCH                           
         CLI   CEXTRA+2,C'A'       ON IF ID LETTER IS A-F                       
         BL    DB8B2                                                            
         CLI   CEXTRA+2,C'F'                                                    
         BH    DB8B2                                                            
         MVI   SVIDRQD,C'Y'                                                     
*                                                                               
DB8B2    DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    DB8B4                                                            
*                                  GET PROFILE                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0I2'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),ICSMED                                                 
         MVC   WORK+7(3),ICSCLT                                                 
         CLI   COFFICE,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         CLI   DMCB,0                                                           
         BNE   MYERR                                                            
*                                  GET I2X PROF (SUPPLEMENTAL)                  
         XC    I2XPROF,I2XPROF                                                  
         MVC   WORK(4),=C'SI2X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR EXT PROF             
         GOTO1 (RF),(R1),,I2XPROF                                               
         CLI   DMCB,0                                                           
         BNE   MYERR                                                            
*                                  GET I2Y PROF (SUPPLEMENTAL)                  
         XC    I2YPROF,I2YPROF                                                  
         MVC   WORK(4),=C'SI2Y'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR EXT PROF             
         GOTO1 (RF),(R1),,I2YPROF                                               
         CLI   DMCB,0                                                           
         BNE   MYERR                                                            
*                                                                               
         MVC   DEFDCHG,I2YPROF+9   DEFERRED DATE CHANGE OPT                     
*                                                                               
         NI    CBLHOPT,255-CBLHNMQ   SET NEW CBLHEAD MODE SWITCH                
         OI    CBLHOPT,CBLHNMQ                                                  
*                                  GET I2S PROF (SEPARATION)                    
         XC    I2SPROF,I2SPROF                                                  
         MVC   WORK(4),=C'SI2S'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR EXT PROF             
         GOTO1 (RF),(R1),,I2SPROF                                               
         CLI   DMCB,0                                                           
         BNE   MYERR                                                            
*                                                                               
         CLI   INTCOPT,0           ENSURE INTERVAL OPTION OK                    
         BNE   *+8                                                              
         MVI   INTCOPT,C'N'                                                     
*                                  GET TRAFFIC PROF                             
         MVC   WORK(4),=C'S0TI'                                                 
         GOTO1 (RF),(R1),,TRAFPROF                                              
         CLI   DMCB,0                                                           
         BNE   MYERR                                                            
*                                                                               
         XC    REQPROF,REQPROF                                                  
         MVC   WORK(4),=C'SI2R'                                                 
         NI    WORK,X'BF'     MAKE SYSTEM LOWER CASE FOR SUPPL PROF             
         GOTO1 (RF),(R1),,REQPROF                                               
*                                                                               
         XC    ISYSPROF,ISYSPROF   READ I0 PROFILE                              
         MVC   WORK(4),=C'S0I0'                                                 
         GOTO1 (RF),(R1),,ISYSPROF                                              
         MVC   AUTOPAY,ISYSPROF    SET AUTOPAY OPTION                           
         MVC   BUYIDLN,ISYSPROF+2  LEN FOR BUY REC ID COMP                      
*                                                                               
DB8B4    DS    0H                                                               
         OC    PROGPROF,PROGPROF                                                
         BNZ   DB8C                                                             
         MVC   PROGPROF,DFLTPROF                                                
*                                                                               
*                                                                               
*                                  GET VALUES FROM CPROF IF                     
*                                  NO PROG PROFILE                              
         LA    R5,TIMLST                                                        
         LA    R6,TIMLSTN                                                       
*                                                                               
DB8B5    DS    0H                                                               
         CLC   CPROF+12(1),0(R5)                                                
         BE    DB8B6                                                            
         LA    R5,5(R5)                                                         
         BCT   R6,DB8B5                                                         
*                                                                               
DB8B6    DS    0H                                                               
         MVC   PROGPROF(1),2(R5)                                                
*                                                                               
         MVI   PROGPROF+1,C'N'                                                  
         CLI   CPROF+11,C'0'                                                    
         BE    *+8                                                              
         MVI   PROGPROF+1,C'Y'                                                  
*                                                                               
DB8C     DS    0H                                                               
         XC    TIMOPT1(4),TIMOPT1                                               
         MVC   TIMOPT1+1(1),PROGPROF   BEFORE AND AFTER                         
         MVC   TIMOPT2+1(1),PROGPROF   AFTER ONLY                               
         CLI   I2XPROF+13,0        TEST ANY SPECIAL FOR AFTER PROG              
         BE    DB8C1               NO                                           
         MVC   TIMOPT2+1(1),I2XPROF+13                                          
         CLI   I2XPROF+13,255      255=0                                        
         BNE   *+8                                                              
         MVI   TIMOPT2+1,0                                                      
*                                                                               
DB8C1    DS    0H                                                               
         MVC   PSTOPT,PROGPROF+1                                                
         CLI   ISYSPROF+1,C'Y'     UNLESS POSTING OK FOR RADIO                  
         BE    DB8C2                                                            
         CLI   ICSMED,C'R'                                                      
         BNE   DB8C2                                                            
         MVI   PSTOPT,C'N'         NO POSTING FOR RADIO                         
*                                                                               
DB8C2    DS    0H                                                               
         MVI   TAXOPT,C'Y'                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    DB9                                                              
*                                                                               
*                                                                               
*                                                                               
         L     RF,VTWA                                                          
         OC    6(2,RF),6(RF)       TEST ANY ACCESS CODE                         
         BZ    DB8D                NO                                           
         CLI   6(RF),C'+'          BYPASS MKT LIMIT ACCESS                      
         BE    DB8D                                                             
         CLC   6(2,RF),CKEY+2                                                   
         BE    DB8D                                                             
         CLI   6(RF),C'*'          OFFICE                                       
         BNE   DB8C4                                                            
*                                                                               
         LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,COFFICE                                                       
         LA    R0,1                                                             
*                                                                               
DB8C3    CLC   7(1,RF),0(R1)        TEST RIGHT OFFICE                           
         BE    DB8D                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,DB8C3                                                         
         B     DB8C5                                                            
*                                                                               
DB8C4    DS    0H                                                               
         CLI   6(RF),C'$'          OFFICE LIST                                  
         BNE   DB8C5                                                            
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T215FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
T21500   CSECT                                                                  
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,VCOMFACS                                           
         CLI   0(R1),0                                                          
         BE    DB8D                OK                                           
*                                                                               
DB8C5    DS    0H                                                               
         LA    R3,ACCERR           NO ACCESS                                    
         B     MYERR                                                            
*                                                                               
DB8D     DS    0H                                                               
         MVC   ICSCLTN,CNAME                                                    
         FOUT  ICSCLTNH                                                         
         XC    QPRD(6),QPRD                                                     
         NI    ICSHPRDH+4,X'DF'                                                 
         NI    ICSOPTH+4,X'DF'                                                  
         OI    4(R2),X'20'                                                      
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPCLT                            
         OI    CHSW,X'20'                                                       
         DROP  R9                                                               
         EJECT                                                                  
*                   VALIDATE PRODUCT/MODE                                       
         SPACE 3                                                                
*                                                                               
DB9      LA    R2,ICSHPRDH                                                      
         TM    4(R2),X'20'                                                      
         BO    DB21                                                             
         CLI   5(R2),0                                                          
         BNE   DB9X                                                             
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPPRD                            
* PW CLIENTS NEED ESTIMATE                                                      
         CLI   SVPWCLT,C'Y'                                                     
         BNE   DB9X                                                             
         CLI   5(R2),3             UNLESS IT IS THERE ALREADY                   
         BH    DB9X                                                             
         GOTO1 VGLOBBER,DMCB,=C'GETD',DUB,8,GLVSPEST                            
* APPEND TO END OF PRODUCT                                                      
         LA    RE,10(R2)           POINT TO LAST CHAR OF PRODUCT                
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(3,RE),DUB         MOVE ESTIMATE                                
         SR    RE,RE                                                            
         IC    RE,5(R2)            ADJUST FIELD LENGTH                          
         LA    RE,4(RE)                                                         
         STC   RE,5(R2)                                                         
*                                                                               
DB9X     LA    R3,MISSERR                                                       
         CLI   5(R2),0                                                          
         MVI   BEST,0                                                           
         BE    PERR                                                             
         LA    R3,PRDERRA                                                       
*                                  MOVE DATA TO WORK AS XXX/XXX/XXX             
         BAS   RE,MOVE                                                          
         CLI   WORK+2,C'-'                                                      
         BE    DB10                                                             
         CLI   WORK+2,C'/'                                                      
         BNE   DB11                                                             
DB10     MVC   WORK+30(10),WORK+2                                               
         MVC   WORK+3(10),WORK+30                                               
         MVI   WORK+2,X'40'                                                     
DB11     CLI   WORK+6,C'-'                                                      
         BE    DB12                                                             
         CLI   WORK+6,C'/'                                                      
         BNE   DB13                                                             
DB12     MVC   WORK+30(10),WORK+6                                               
         MVC   WORK+7(10),WORK+30                                               
         MVI   WORK+6,X'40'                                                     
DB13     DS    0H                                                               
         LA    R4,WORK                                                          
         LA    R5,BPRD                                                          
         BAS   RE,GETPCOD1                                                      
         CLI   BPRD,0                                                           
         BE    PERR                                                             
         CLC   WORK(3),=C'POL'                                                  
         BE    DB17                                                             
*                                  NON-POOL                                     
         MVC   QPRD,WORK                                                        
         MVI   BPRD2,0             CLEAR 2ND PROD                               
         XC    QPRD2,QPRD2                                                      
*                                                                               
DB13A    CLI   WORK+3,C'-'                                                      
         BE    DB14                                                             
         CLI   WORK+3,C'/'                                                      
         BE    DB14                                                             
         CLC   SPACES,WORK+3                                                    
         BNE   PERR                                                             
         B     DB16                                                             
*                                  GET 2ND PRODUCT                              
DB14     DS    0H                                                               
         MVI   BPRD2,X'FF'                                                      
         CLC   WORK+4(3),=C'ALL'   PIGGY BACK OPTIONS                           
         BE    DB14F               (AFFECT OFF-LINE REQ ONLY)                   
         CLC   WORK+4(3),=C'YES'                                                
         BE    DB14F                                                            
         MVI   BPRD2,0             NO PIGGIES                                   
         CLC   WORK+4(3),=C'NO '                                                
         BE    DB14F                                                            
*                                                                               
         CLI   WORK+4,C'0'         NUMERIC MEANS ITS AN ESTIMATE                
         BNL   DB15A                                                            
*                                                                               
         LA    R4,WORK+4           MUST BE 2ND PRD                              
         LA    R5,BPRD2                                                         
         BAS   RE,GETPCOD                                                       
         LA    R3,PRDERRB                                                       
         CLI   BPRD2,0                                                          
         BE    PERR                                                             
*                                                                               
DB14F    DS    0H                                                               
         LA    RF,QPRD2            SET MOD/PRD IN QPRD2                         
         CLC   WORK(3),=C'POL'     BUT FOR POL                                  
         BNE   *+8                                                              
         LA    RF,QPRD             REQ ROUTINE SETS FROM QPRD                   
         MVC   0(3,RF),WORK+4                                                   
         B     DB16                                                             
*                                                                               
DB15A    DS    0H                  LOOK FOR ESTIMATE                            
         LA    R3,ESTERR                                                        
         SR    RF,RF                                                            
         CLI   WORK+5,C'0'                                                      
         BL    DB15B                                                            
         LA    RF,1(RF)                                                         
         CLI   WORK+6,C'0'                                                      
         BL    DB15B                                                            
         LA    RF,1(RF)                                                         
DB15B    DS    0H                                                               
         OC    WORK+4(3),=3C'0'                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+4(0)                                                    
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    PERR                                                             
         CH    R0,=H'255'                                                       
         BH    PERR                                                             
*                                                                               
         STC   R0,BEST                                                          
*                                                                               
         MVC   KEY(13),HKEY                                                     
         MVC   KEY+4(3),WORK                                                    
         STC   R0,KEY+7            EST                                          
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PERR                                                             
         B     DB20B                                                            
*                                                                               
DB16     CLC   SPACES,WORK+7                                                    
         BNE   PERR                                                             
         B     DB20                                                             
*                                  POOL                                         
DB17     LA    R3,PRDERRA          PW CLIENT CANNOT DO POL                      
         CLI   SVPWCLT,C'Y'                                                     
         BE    PERR                                                             
         XC    QPRD(6),QPRD                                                     
         XC    BPRD(2),BPRD                                                     
         XC    ICSHPRN,ICSHPRN                                                  
         MVC   ICSHPRN(7),=C'VARIOUS'                                           
         CLI   WORK+3,C'/'                                                      
         BE    DB18                                                             
         CLI   WORK+3,C'-'                                                      
         BE    DB18                                                             
         CLC   SPACES,WORK+3                                                    
         BNE   PERR                                                             
         B     DB20B                                                            
*                                  POL/PRD                                      
DB18     DS    0H                                                               
         B     DB14                CHECK FOR PRD,MODE,EST                       
*                                  POL/PRD                                      
DB20     CLC   HKEY+4(3),QPRD      GET PNAME IF QPRD CHANGED                    
         BE    DB20C                                                            
         MVC   HKEY+4(3),QPRD                                                   
         MVC   KEY(13),HKEY                                                     
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         USING PRDHDR,R9                                                        
         MVC   ICSHPRN,PNAME                                                    
*                                                                               
DB20B    FOUT  ICSHPRNH                                                         
*                                                                               
DB20C    OI    4(R2),X'20'                                                      
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPPRD                            
         OI    CHSW,X'10'                                                       
         B     DB21                                                             
PERR     XC    ICSHPRN,ICSHPRN                                                  
         FOUT  ICSHPRNH                                                         
*                                                                               
MYERR    CLI   SVPWCLT,C'Y'        TEST PW CLIENT                               
         BNE   ERROR                                                            
         NI    ICSSTAH+4,X'DF'     FORCE STATION VALIDATION                     
         NI    ICSHPRDH+4,X'DF'    FORCE PRODUCT VALIDATION                     
         B     ERROR                                                            
         DROP  R9                                                               
         EJECT                                                                  
DB21     CLI   SVPWCLT,C'Y'        TEST PW CLIENT                               
         BNE   DB21NPW             MUST SPECIFY ESTIMATE                        
         LA    R3,ESTERR                                                        
         CLI   BEST,0              TEST ESTIMATE ENTERED                        
         BE    PERR                                                             
*                                                                               
         GOTO1 =A(CHKPWMKT),RR=RELO00  GO DETERMINE CORRECT MKT                 
*                                                                               
*                   VALIDATE PERIOD                                             
         SPACE 3                                                                
DB21NPW  LA    R2,ICSPERH                                                       
         TM    4(R2),X'20'                                                      
         BO    DB30                                                             
         CLI   5(R2),0                                                          
         BNE   DB21AA                                                           
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPPER                            
*                                                                               
DB21AA   XC    ICSPERN,ICSPERN               CLEAR DISPLAYED DATES              
         XC    MONLST,MONLST                                                    
         FOUT  ICSPERNH                                                         
         LA    R3,MISSERR                                                       
         CLI   5(R2),0                                                          
         BE    MYERR                                                            
         LA    R3,PERERR                                                        
         GOTO1 VDATVAL,DMCB,8(R2),WORK  VALIDATE FOR MM/DD/YY                   
         CLI   DMCB+3,0            IF MDY NOT VALID                             
         BE    DB22                TRY MY                                       
         MVI   WEEKSW,C'Y'                                                      
         GOTO1 VGETDAY,(R1),WORK,WORK+6                                         
         CLI   DMCB,1                                                           
         BE    DB21A                                                            
         LA    R3,MONERR                DATE NOT A MONDAY                       
         B     MYERR                                                            
DB21A    GOTO1 VADDAY,(R1),,,6          GET SUNDAY                              
         MVI   DMCB,0              CLEAR HOB                                    
         GOTO1 VDATCON,(R1),,(2,BSTART)                                         
         GOTO1 (RF),(R1),WORK+6,(2,BEND)                                        
         MVC   MONLST(2),BSTART                                                 
*                                  GET START OF BROADCAST MONTH                 
         MVC   BRDMON,BSTART                                                    
         CLC   WORK+2(2),WORK+8                                                 
         BNE   DB21E               ALREADY HAVE                                 
         CLC   WORK+4(2),=C'01'                                                 
         BE    DB21E               ALREADY HAVE                                 
         MVC   WORK+12(6),WORK                                                  
         LA    R4,WORK+12          SET PARS                                     
         LA    R5,WORK+12                                                       
         LA    R6,7                                                             
         LCR   R6,R6                                                            
         STM   R4,R6,DMCB                                                       
         L     RF,VADDAY                                                        
DB21B    GOTO1 (RF),(R1)                                                        
         SPACE 1                                                                
         CLC   WORK+2(2),WORK+14                                                
         BNE   DB21C                                                            
         CLC   WORK+16(2),=C'01'                                                
         BE    DB21C                                                            
         B     DB21B                                                            
DB21C    GOTO1 VDATCON,(R1),,(2,BRDMON)      HAVE START                         
*                                                                               
DB21E    DS    0H                                                               
         MVC   PREVWKP,=X'FFFF'                                                 
         CLC   BSTART,BRDMON       IF FIRST WEEK OF MONTH                       
         BNE   DB28                                                             
         LA    R0,7                GET START OF LAST WEEK OF                    
         LNR   R0,R0               PREV MONTH FOR OUT OF WEEKS                  
         GOTO1 VADDAY,DMCB,WORK,DUB,(R0)                                        
         GOTO1 VDATCON,(R1),DUB,(2,PREVWKP)                                     
         B     DB28                                                             
         SPACE 2                                                                
*                                  MONTH ENTERED                                
DB22     CLI   5(R2),6                                                          
         BH    MYERR              PROTECT AGAINST MMMDD/YR                      
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK   VALIDATE FOR MMM/YY                
         CLI   DMCB+3,0                                                         
         BE    MYERR                                                            
         MVI   WEEKSW,C'N'                                                      
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VGETDAY,(R1),WORK,FULL        GET DAY OF WEEK                    
*                                                                               
         CLI   DMCB,0                                                           
         BE    MYERR                                                            
         L     RF,VADDAY                                                        
         SR    R0,R0                                                            
         IC    R0,DMCB                                                          
         BCT   R0,*+8                                                           
         B     DB23                          ALREADY HAVE MONDAY                
         LCR   R0,R0                                                            
         GOTO1 (RF),(R1),WORK,WORK,(R0)                                         
DB23     MVC   WORK+12(6),WORK                                                  
         LA    R4,WORK+12                                                       
         LA    R5,WORK+18                                                       
         MVC   DMCB+8(4),=F'7'                                                  
         LA    R6,5                                                             
*                                            GET 5 MORE MONDAYS                 
DB24     GOTO1 (RF),(R1),(R4),(R5)                                              
         LA    R4,6(R4)                                                         
         LA    R5,6(R5)                                                         
         BCT   R6,DB24                                                          
         LA    R4,WORK+42                                                       
         LA    R5,WORK+6                                                        
         ST    R5,DMCB+4                                                        
         LA    R0,6                                                             
         ST    R0,DMCB+8                                                        
*                                                                               
DB25     DS    0H                                                               
         MVC   WORK+6(6),0(R4)     USE MONDAY                                   
         CLI   PROGPROF+9,C'C'     IF CALENDAR MONTHS                           
         BE    DB25B                                                            
         GOTO1 (RF),(R1),(R4)      ELSE GET SUNDAY                              
DB25B    DS    0H                                                               
         CLC   WORK+8(2),WORK+20        CHECK SAME MONTH                        
         BE    DB26                                                             
         MVC   0(6,R4),=6C'0'                                                   
         SH    R4,=H'6'            BACK UP WEEK                                 
         B     DB25                                                             
*                                       CONVERT TO 2 BYTE DATES                 
DB26     LA    R4,WORK                                                          
         LA    R5,BSTART                                                        
         L     RF,VDATCON                                                       
         LA    R6,8                                                             
DB27     GOTO1 (RF),(R1),(R4),(2,(R5))                                          
         LA    R4,6(R4)                                                         
         LA    R5,2(R5)                                                         
         BCT   R6,DB27                                                          
         MVC   BRDMON,BSTART                                                    
*                                                                               
         LA    R0,7                GET START OF LAST WEEK OF                    
         LNR   R0,R0               PREV MONTH FOR OUT OF WEEKS                  
         GOTO1 VADDAY,DMCB,WORK,DUB,(R0)                                        
         GOTO1 VDATCON,(R1),DUB,(2,PREVWKP)                                     
*                                                                               
         CLI   PROGPROF+9,C'C'                                                  
         BNE   DB28                                                             
*                                  IF CALENDAR MONTHS RESET                     
*                                  BSTART AND BEND                              
         MVC   WORK(6),WORK+6                                                   
         MVC   WORK+4(2),=C'01'                                                 
         PACK  DUB,WORK+2(2)                                                    
         CVB   RF,DUB                                                           
         SLL   RF,1                X 2                                          
         LA    RF,MTHEND-2(RF)     LAST DAY OF MONTH                            
         MVI   MTHEND+3,C'8'                                                    
         PACK  DUB,WORK(2)                                                      
         CVB   R0,DUB                                                           
         STC   R0,DUB                                                           
         TM    DUB,X'03'        TEST LEAP YEAR                                  
         BNZ   *+8                                                              
         MVI   MTHEND+3,C'9'                                                    
         MVC   WORK+10(2),0(RF)                                                 
         GOTO1 VDATCON,DMCB,WORK,(2,BSTART)                                     
         GOTO1 (RF),(R1),WORK+6,(2,BEND)                                        
         B     DB28                                                             
*                                                                               
MTHEND   DC    C'312831303130313130313031'                                      
*                                                                               
*                                       CONVERT DATES FOR DISPLAY               
DB28     EQU   *                                                                
         LA    R4,WORK                                                          
         LA    R5,ICSPERN                                                       
         LA    R6,2                                                             
DB28A    GOTO1 (RF),(R1),(R4),(11,(R5))                                         
         SPACE 1                                                                
         LA    R4,6(R4)                                                         
         LA    R5,9(R5)                                                         
         BCT   R6,DB28A                                                         
         MVI   ICSPERN+8,C'-'                                                   
*                                                                               
         GOTO1 (RF),(R1),WORK+6,(1,DUB)   GET BINARY MOS (END DATE              
         MVC   BMOS,DUB                   WILL ALWAYS BE IN MOS)                
*                                                                               
         OI    4(R2),X'20'              VALIDATION BIT                          
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPPER                            
         OI    CHSW,X'08'                                                       
         EJECT                                                                  
DB30     EQU   *                                                                
         SPACE 2                                                                
*                   PAGE EDIT                                                   
         SPACE 2                                                                
DB35     EQU   *                                                                
         TM    CHSW,X'F8'          TEST ANY HEADLINE CHANGE                     
         BNZ   DB35D               YES- SKIP ANY AUTOPAY                        
         CLC   ICSPAG,=C'PAY'      TEST AUTOPAY SWAP                            
         BNE   DB35D                                                            
         XC    ICSSRV,ICSSRV                                                    
         MVC   ICSSRV(7),=C'+PAY,SV'                                            
         XC    ICSPAG,ICSPAG                                                    
         MVI   ICSPAG,C'1'                                                      
         OI    ICSPAGH+6,X'01'     FORCE INPUT NEXT TIME                        
         FOUT  ICSPAGH                                                          
         LA    R2,ICSMEDH          CURSOR TO MEDIA                              
         B     EXIT                                                             
*                                                                               
DB35D    DS    0H                                                               
         CLC   ICSPAG,=C'REQ'                                                   
         BNE   *+20                                                             
         ZAP   THISPAG,=P'0'                                                    
         ZAP   PAGIN,=P'1'                                                      
         B     DB40                                                             
         TM    CHSW,X'F8'                                                       
         BZ    *+10                                                             
         ZAP   THISPAG,=P'0'                                                    
         LA    R2,ICSPAGH                                                       
         CLI   5(R2),0             NO INPUT                                     
         BE    DB38                                                             
         MVC   PAGIN,=C'TO'                                                     
         CLI   ICSPAG,C'T'                                                      
         BE    DB40                                                             
         MVC   PAGIN,=C'UN'                                                     
         CLI   ICSPAG,C'U'                                                      
         BE    DB40                                                             
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         LA    R4,ICSPAG                                                        
         LR    R1,R4                                                            
DB36     OI    0(R4),X'F0'                                                      
         CLI   0(R4),C'0'                                                       
         BL    DB39                NOT NUMERIC                                  
         CLI   0(R4),C'9'                                                       
         BH    DB39                NOT NUMERIC                                  
         LA    R4,1(R4)                                                         
         BCT   R5,DB36                                                          
*                                                                               
         SR    R4,R1                                                            
         BNP   DB38                                                             
         BCTR  R4,R0                                                            
         EX    R4,PAGPAK                                                        
         CP    DUB,=P'0'                                                        
         BNH   DB38                                                             
         ZAP   PAGIN,DUB                                                        
         CP    PAGIN,THISPAG                                                    
         BNE   *+10                                                             
         AP    PAGIN,=P'1'         BUMP PAGE                                    
         B     DB40                                                             
PAGPAK   PACK  DUB,0(0,R1)                                                      
DB38     ZAP   PAGIN,=P'1'                                                      
DB39     EQU   DB38                                                             
         EJECT                                                                  
*                                  REQ EDIT                                     
         SPACE 2                                                                
DB40     EQU   *                                                                
         LA    R2,ICSREQH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   DB40Z                                                            
         XC    RQOPTS,RQOPTS                                                    
         CLI   ICSPAG,C'R'                                                      
         BNE   *+8                                                              
         MVI   RQNAME,C' '         NON-ZERO WILL CAUSE REQ                      
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    DB40Z               NO                                           
         L     RF,ABUFFER                                                       
         XC    0(256,RF),0(RF)                                                  
         ST    RF,DMCB+4                                                        
         GOTO1 VSCANNER,DMCB,(12,ICSREQH)                                       
*                                                                               
         ZIC   R0,DMCB+4                                                        
         L     R5,ABUFFER                                                       
         LA    R3,REQERR                                                        
DB40C    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DB40X                                                            
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'NAME'                                                
         BNE   DB40D                                                            
         MVC   RQNAME,22(R5)                                                    
         B     DB40W                                                            
DB40D    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'BOOK'                                                
         BNE   DB40J                                                            
         CLC   =C'NO',22(R5)       TEST FOR BOOK = NO                           
         BNE   DB40E                                                            
         CLI   24(R5),C' '                                                      
         BH    DB40E                                                            
         MVI   RQBOOK,X'FF'                                                     
         B     DB40W                                                            
*                                                                               
DB40E    DS    0H                                                               
         CLC   =C'ACT',22(R5)      TEST FOR BOOK = ACT                          
         BNE   DB40F                                                            
         MVC   RQBOOK,SPACES                                                    
         MVC   RQBOOK(3),=C'ACT'                                                
         B     DB40W                                                            
*                                                                               
DB40F    DS    0H                                                               
         CLC   =C'LAT',22(R5)      TEST FOR BOOK = LATEST                       
         BNE   DB40G                                                            
         MVC   RQBOOK,SPACES                                                    
         MVC   RQBOOK(3),=C'LAT'                                                
         B     DB40W                                                            
*                                                                               
DB40G    DS    0H                                                               
         LA    R3,BKERR                                                         
         GOTO1 VDATVAL,DMCB,(2,22(R5)),RQBOOK                                   
         OC    DMCB,DMCB                                                        
         BZ    DB40V                                                            
         MVC   RQBOOK+4(2),=2C' '                                               
         CLC   DMCB+3(1),1(R5)          TEST ONLY DATE GIVEN                    
         BE    DB40H                                                            
*                                                                               
         LA    RF,22(R5)                LOOK FOR HUT                            
         A     RF,DMCB                                                          
         MVC   RQBOOK+4(2),1(RF)                                                
         CLC   RQBOOK+4(2),=C'NO'                                               
         BE    DB40H                                                            
         OC    RQBOOK+4(2),=2C'0'                                               
         CLC   RQBOOK+4(2),=2C'0'                                               
         BNH   DB40V                                                            
         CLC   RQBOOK+4(2),=C'12'                                               
         BH    DB40V                                                            
*                                                                               
DB40H    DS    0H                                                               
         MVC   WORK(4),RQBOOK                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,WORK,(3,WORK)                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),ICSMED                                                  
         MVI   KEY+2,C'A'          ALWAYS ARB FOR RADIO ????                    
         CLI   ICSMED,C'R'                                                      
         BE    DB40H2                                                           
         MVI   KEY+2,C'N'          NIELSON                                      
         CLI   SVCLPROF+3,C'0'                                                  
         BE    DB40H2                                                           
         CLC   WORK(2),=X'5E01'    FOR JAN94+ ALWAYS NIELSEN                    
         BNL   DB40H2                                                           
         MVI   KEY+2,C'A'          ELSE, ARB                                    
DB40H2   DS    0H                                                               
         MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         LA    R3,BKERR2                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA,DMWORK            
         TM    DMCB+8,X'FF'                                                     
         BNZ   DB40V                                                            
         CLC   KEY(5),IOAREA                                                    
         BNE   DB40V                                                            
*                                                                               
         B     DB40W                                                            
*                                                                               
DB40J    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'DEST'                                                
         BNE   DB40P                                                            
*                                                                               
         LA    R3,REQERR                                                        
         XC    RQDEST,RQDEST                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,22(R5)                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,IOAREA,DMWORK            
         CLC   IOAREA(25),KEYSAVE                                               
         BNE   DB40V                                                            
*                                                                               
DB40J4   DS    0H                                                               
         LA    R4,IOAREA+CTIDATA-CTIREC    FIRST ELEM                           
*                                                                               
DB40J6   DS    0H                                                               
         CLI   0(R4),0             EOR                                          
         BE    DB40V               ERROR                                        
*                                                                               
         CLI   0(R4),X'02'         ID NUM ELEMENT                               
         BNE   DB40J7                                                           
         MVC   RQDEST,2(R4)                                                     
         B     DB40J9                                                           
*                                                                               
DB40J7   DS    0H                                                               
         CLI   0(R4),X'06'         AGY ELEM                                     
         BNE   DB40J9                                                           
*                                                                               
         CLC   AGYALPHA,2(R4)                                                   
         BNE   DB40V               ERROR- WRONG AGENCY ALPH                     
         B     DB40W                                                            
*                                                                               
DB40J9   DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DB40J6                                                           
*                                                                               
DB40P    DS    0H                                                               
*                                                                               
         LA    R3,REQERR                                                        
DB40V    DS    0H                                                               
         NI    ICSPERH+4,X'DF'     ON REQ ERROR FORCE RE-VAL OF DATE            
*                                  ENSURE DATA READ                             
         B     MYERR                                                            
*                                                                               
DB40W    DS    0H                                                               
         LA    R5,34(R5)           NEXT FIELD                                   
         B     DB40C                                                            
*                                                                               
DB40X    DS    0H                                                               
         OI    CHSW,X'02'                                                       
         OI    4(R2),X'20'         SET VAL                                      
*                                                                               
         OC    RQOPTS,RQOPTS                                                    
         BZ    DB40Z                                                            
*                                                                               
         CLC   RQNAME,=CL10' '                                                  
         BH    DB40X1                                                           
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',RQNAME,10,GLVSPREQ                        
         B     DB40X2                                                           
*                                                                               
DB40X1   GOTO1 VGLOBBER,DMCB,=C'PUTD',RQNAME,10,GLVSPREQ                        
*                                                                               
DB40X2   L     RF,ABUFFER                                                       
         XC    0(256,RF),0(RF)                                                  
*                                                                               
DB40Z    DS    0H                                                               
         EJECT                                                                  
*        EDIT OPTIONS LINE                                                      
         SPACE 2                                                                
DB42     DS    0H                                                               
         LA    R2,ICSOPTH                                                       
         CLI   5(R2),0                                                          
         BNE   DB42A                                                            
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPOPT                            
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
DB42A    DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BNZ   DB42Z                                                            
         MVI   EASISW,C'B'         CLEAR EASI SWITCH - SET TO 'BOTH'            
         MVI   QBYID,0                                                          
         MVC   SVOTOOPT,OTOOPT     SAVE OTO OPT                                 
         MVI   OTOOPT,C'N'         AND CLEAR IT                                 
         MVC   SVPSEUDO,PSEUDOPT   SAVE PSEUDO SWITCH                           
         MVI   PSEUDOPT,C'N'       AND CLEAR IT                                 
         L     RF,ABUFFER                                                       
         XC    0(256,RF),0(RF)                                                  
         ST    RF,DMCB+4                                                        
         OI    DMCB+4,X'80'        SET TO RETURN DISPLACEMENTS                  
         GOTO1 VSCANNER,DMCB,(12,ICSOPTH)                                       
*                                                                               
         ZIC   R0,DMCB+4                                                        
         L     R5,ABUFFER                                                       
         LA    R3,OPTNERR                                                       
*                                                                               
DB42C    DS    0H                                                               
         CLI   0(R5),0             END                                          
         BE    DB42X                                                            
*                                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'FILM'   FILM REPORT OPTION                           
         BNE   DB42L                                                            
         MVC   FILMRSW,22(R5)                                                   
         CLI   FILMRSW,C'N'        IF NOT N                                     
         BE    *+8                                                              
         MVI   FILMRSW,C'Y'        SET TO Y                                     
         CLI   TRAFSW,C'Y'         MUST BE OK FOR TRAFFIC                       
         BNE   DB42P                                                            
         B     DB42W                                                            
*                                                                               
DB42L    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'POST'                                                
         BNE   DB42N                                                            
*                                                                               
         MVC   PSTOPT,22(R5)                                                    
         CLI   PSTOPT,C'N'                                                      
         BE    *+8                                                              
         MVI   PSTOPT,C'Y'                                                      
         CLI   ICSMED,C'R'         NO POSTING FOR RADIO                         
         BNE   *+12                                                             
         CLI   PSTOPT,C'N'                                                      
         BNE   DB42P                                                            
         B     DB42W                                                            
*                                                                               
DB42N    DS    0H                  ID REQ                                       
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'ID'                                                  
         BNE   DB42P                                                            
         CLI   1(R5),0             MUST HAVE 2ND FIELD                          
         BE    DB42V                                                            
*                                                                               
         CLC   BCLT,STEQCLT        TEST SAME CLIENT                             
         BNE   DB42N1                                                           
         CLC   STA,STEQSTA         AND STATION                                  
         BNE   DB42N1                                                           
         CLC   BUYID,22(R5)                                                     
         BE    DB42W                                                            
*                                                                               
DB42N1   DS    0H                                                               
         OI    CHSW,X'04'          SET CHANGE TO FORCE REDO                     
         MVC   BUYID,22(R5)        SET ACTUAL ID CODE                           
*                                                                               
         CLC   =C'NO',BUYID       NO= DO NOT DO BY ID                           
         BNE   DB42N2                                                           
         CLI   BUYID+2,C' '                                                     
         BH    DB42N2                                                           
         MVC   BUYID(3),SPACES                                                  
         MVI   QBYID,0                                                          
         B     DB42N9                                                           
*                                                                               
DB42N2   DS    0H                                                               
         OI    QBYID,QBYIDYQ       SET DOING BY ID                              
*                                                                               
         CLC   =C'NONE ',BUYID                                                  
         BNE   *+12                                                             
         OI    QBYID,QBYIDNQ                                                    
         B     DB42N9                                                           
*                                                                               
         CLC   =C'HOME ',BUYID                                                  
         BNE   *+12                                                             
         OI    QBYID,QBYIDHQ                                                    
         B     DB42N9                                                           
*                                                                               
         OI    QBYID,QBYIDSQ       BY ID, STANDARD                              
*                                                                               
         CLC   BCLT,STEQCLT        TEST SAME CLIENT                             
         BNE   *+14                                                             
         CLC   STA,STEQSTA         AND STATION                                  
         BE    DB42N6                                                           
         MVC   STEQCLT,BCLT                                                     
         MVC   STEQSTA,STA                                                      
*                                                                               
         XC    STEQDSK,STEQDSK                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STEKEY,R4                                                        
         MVC   STEKTYP,=X'0D44'                                                 
         MVC   STEKAGMD,HKEY+1     AGYMED                                       
         MVC   STEKCLT,BCLT                                                     
         MVC   STEKSTA,STA                                                      
         CLI   STEKSTA,C'0'        IF CABLE                                     
         BL    *+8                                                              
         MVI   STEKSTA+4,C' '      GET RID OF T                                 
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DB42N8D                                                          
         MVC   STEQDSK,KEY+14                                                   
*                                                                               
DB42N6   DS    0H                                                               
         OC    STEQDSK,STEQDSK     RECORD EXISTS                                
         BZ    DB42N8D             NO- DONE                                     
*                                                                               
         MVC   KEY+14(4),STEQDSK   DA OF STEQ REC                               
         BAS   RE,GETREC                                                        
*                                                                               
         PACK  WORK(3),BUYID+1(5)  PACK MGR NUMBER                              
         LA    R2,24(R9)           START OF FIRST ELEM                          
*                                                                               
DB42N7   DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BE    DB42N8D                                                          
         CLI   0(R2),X'03'         MGR ELEM                                     
         BNE   DB42N8                                                           
         CLC   WORK(2),3(R2)       TEST RIGHT MGR NUMBER                        
         BNE   DB42N8                                                           
         CLC   BUYID(1),2(R2)      TEST MGR SCHEME                              
         BNE   DB42N8                                                           
*                                                                               
         MVC   BMS(2),5(R2)         SET NEW MARKET NUMBER                       
         SR    R0,R0                                                            
         ICM   R0,3,BMS                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB         MKT EBCDIC                                   
*                                                                               
         MVI   SKEY,C'0'                                                        
         MVC   SKEY+1(16),SKEY                                                  
         MVI   SKEY,C'M'                                                        
         MVC   SKEY+1(1),ICSMED                                                 
         MVC   SKEY+2(4),WORK                                                   
         MVC   SKEY+6(2),AGYALPHA                                               
         LA    R2,ICSOPTH                                                       
         BAS   RE,READSTA                                                       
*                                                                               
         MVC   ICSMKTN(4),WORK                                                  
         MVI   ICSMKTN+4,C' '                                                   
         USING MKTREC,R9                                                        
         MVC   ICSMKTN+5(15),MKTNAME                                            
         FOUT  ICSMKTNH                                                         
*                                                                               
         B     DB42W                                                            
*                                                                               
DB42N8   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     DB42N7                                                           
*                                                                               
DB42N8D  DS    0H                  IF NO STAEQ FOUND                            
         MVC   BMS(2),HOMEMKT      USE HOME                                     
*                                                                               
DB42N9   DS    0H                                                               
         B     DB42W                                                            
*                                                                               
DB42P    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'AUTOPAY'                                             
         BNE   DB42Q                                                            
         MVC   AUTOPAY,22(R5)                                                   
         CLI   AUTOPAY,C'N'        IF NOT AN N                                  
         BE    DB42W                                                            
         CLI   AUTOPAY,C'S'        OR S                                         
         BE    DB42W                                                            
         MVI   AUTOPAY,C'Y'        SET TO Y                                     
         B     DB42W                                                            
*                                                                               
DB42Q    DS    0H                                                               
         CLC   =C'NEW',12(R5)                                                   
         BNE   DB42R                                                            
         MVC   NEWSW,22(R5)                                                     
         CLI   NEWSW,C'N'                                                       
         BE    *+8                                                              
         MVI   NEWSW,C'Y'                                                       
         MVI   NEWIDSW,C'N'                                                     
         CLI   12(R5),C'I'         I = NEW AND NEW IDS                          
         BNE   *+8                                                              
         MVI   NEWIDSW,C'Y'                                                     
         B     DB42W                                                            
*                                                                               
DB42R    DS    0H                                                               
         CLC   =C'EASI',12(R5)                                                  
         BNE   *+16                                                             
         MVI   EASISW,C'Y'                                                      
         OI    CHSW,X'04'                                                       
         B     DB42W                                                            
         CLC   =C'NOEASI',12(R5)                                                
         BNE   *+16                                                             
         MVI   EASISW,C'N'                                                      
         OI    CHSW,X'04'                                                       
         B     DB42W                                                            
*                                                                               
DB42R2   DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'RESPONSE'                                            
         BNE   DB42S                                                            
         MVC   PSEUDOPT,22(R5)                                                  
         CLI   PSEUDOPT,C'N'                                                    
         BE    *+8                                                              
         MVI   PSEUDOPT,C'R'                                                    
         B     DB42W                                                            
*                                                                               
DB42S    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'MCT'                                                 
         BNE   DB42T                                                            
         MVC   PSEUDOPT,22(R5)                                                  
         CLI   PSEUDOPT,C'N'                                                    
         BE    *+8                                                              
         MVI   PSEUDOPT,C'M'                                                    
         B     DB42W                                                            
*                                                                               
DB42T    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'OTO'    OTO OPTION                                   
         BNE   DB42U                                                            
         CLI   1(R5),0             MUST HAVE 2ND FIELD                          
         BE    DB42V                                                            
         CLI   22(R5),C'N'         N MEANS NO                                   
         BE    DB42TD                                                           
         CLI   22(R5),C'*'         SO DOES *                                    
         BE    DB42TD                                                           
         CLI   22(R5),C'Y'                                                      
         BNE   DB42V                                                            
*                                                                               
         CLI   I2YPROF+14,C'Y'     MUST BE AUTHORIZED BY PROFILE                
         BNE   DB42V                                                            
         MVI   OTOOPT,C'Y'         SET TO DO OTO'S                              
         ZIC   RF,8(R5)            DISPLACEMENT OF 2ND FIELD                    
         LA    RF,ICSOPT(RF)                                                    
         MVI   0(RF),C'*'          CHANGE Y TO * (FOR SAFETY ?)                 
         FOUT  ICSOPTH                                                          
*                                                                               
DB42TD   DS    0H                                                               
         B     DB42W                                                            
*                                                                               
DB42U    DS    0H                                                               
         LA    R3,OPTNERR                                                       
*                                                                               
DB42V    DS    0H                                                               
         B     MYERR                                                            
*                                                                               
DB42W    DS    0H                                                               
         LA    R5,34(R5)           NEXT FIELD                                   
         B     DB42C                                                            
*                                                                               
DB42X    DS    0H                                                               
         CLI   PSTOPT,C'Y'         IF NOT POSTING                               
         BE    DB42X2                                                           
         CLI   OTOOPT,C'N'         MUST NOT BE DOING OTO'S                      
         BE    DB42X2                                                           
         B     DB42V                                                            
*                                                                               
DB42X2   DS    0H                                                               
         L     RF,ABUFFER                                                       
         XC    0(256,RF),0(RF)                                                  
         CLC   OTOOPT,SVOTOOPT     DID OTO SWITCH CHANGE?                       
         BE    *+12                                                             
         OI    CHSW,X'04'          YES, FORCE REDO                              
         B     DB42Z                                                            
         CLC   PSEUDOPT,SVPSEUDO   DID PSEUDO SWITCH CHANGE?                    
         BE    *+8                                                              
         OI    CHSW,X'04'          YES, FORCE REDO                              
*                                                                               
         OI    4(R2),X'20'         SET HAVE VALIDATED                           
*                                                                               
DB42Z    DS    0H                                                               
*                                  SET MARKET NAME IN HEADLINES                 
         TM    QBYID,QBYIDSQ       IF 'STANDARD' ID MODE                        
         BNZ   DB50                RIGHT MARKET SHOULD BE SET                   
*                                                                               
         MVC   BMS(2),HOMEMKT      ELSE USE HOME MARKET                         
         MVC   X(20),HOMEMKTN                                                   
         CLI   SVIDRQD,C'Y'        IF ID'S NOT REQUIRED DON'T                   
         BNE   DB42Z6              SHOW MKT # (NEVER HAVE)                      
*                                                                               
         EDIT  (B2,HOMEMKT),(4,X),FILL=0   ELSE SHOW # AND NAME                 
         MVI   X+4,C' '                                                         
         MVC   X+5(15),HOMEMKTN                                                 
*                                                                               
DB42Z6   DS    0H                                                               
         CLC   ICSMKTN,X                                                        
         BE    DB50                                                             
         MVC   ICSMKTN,X                                                        
         FOUT  ICSMKTNH                                                         
*                                                                               
DB50     DS    0H                                                               
         EJECT                                                                  
*                   GET OVERLAYS                                                
         SPACE 3                                                                
DB90     MVI   OVLYSW3,1           ALWAYS NEED OVLY3                            
         TM    CHSW,X'FC'                                                       
         BZ    DB90A                                                            
         MVI   OVLYSW1,1           IF CHANGE NEED OVLY1                         
         MVI   OVLYSW2,1           IF CHANGE NEED OVLY2                         
         B     DB90B                                                            
*                                  'RELOCATE' BUFFER ADDRS FROM TWA             
*                                  IF NO 'CHANGE'                               
DB90A    L     R0,AFBY                                                          
         AR    R0,RC                                                            
         ST    R0,AFBY                                                          
         L     R0,ALBY                                                          
         AR    R0,RC                                                            
         ST    R0,ALBY                                                          
         L     R0,AFINV                                                         
         AR    R0,RC                                                            
         ST    R0,AFINV                                                         
         L     R0,ALINV                                                         
         AR    R0,RC                                                            
         ST    R0,ALINV                                                         
*                                                                               
*                                                                               
DB90B    LA    R4,1                                                             
         SR    R5,R5                                                            
         LA    R6,3                                                             
         LA    R1,DMCB                                                          
         LA    R2,ICSMEDH                                                       
DB91     LA    RF,OVLYSWS-1(R4)                                                 
         CLI   0(RF),0                                                          
         BE    DB92                                                             
         GOTO1 VCALLOV,(R1),((R4),(R5)),(RA)                                    
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC)                                                   
         CLI   BUFFSW,X'FF'                                                     
         BE    DB93                TEMSTR CLOBBERED BY BROADCAST                
         CLI   ERRAREA,0                                                        
         BE    DB92                                                             
         NI    ICSPERH+4,X'DF'     IF ERROR UNVALIDATE SOMETHING                
         B      EXIT                                                            
DB92     LA    R4,1(R4)                                                         
         BCT   R6,DB91                                                          
*                                  MAKE BUFFER ADDRS (STORED IN TWA)            
*                                  RELATIVE BEFORE EXITING                      
         L     R0,AFBY                                                          
         SR    R0,RC                                                            
         ST    R0,AFBY                                                          
         L     R0,ALBY                                                          
         SR    R0,RC                                                            
         ST    R0,ALBY                                                          
         L     R0,AFINV                                                         
         SR    R0,RC                                                            
         ST    R0,AFINV                                                         
         L     R0,ALINV                                                         
         SR    R0,RC                                                            
         ST    R0,ALINV                                                         
*                                                                               
         LA    R2,ICSMEDH          CURSOR TO MEDIA FIELD                        
*                                                                               
         CLI   ORBSW,C'Y'                                                       
         BNE   DB92B                                                            
         XC    ICSMSG,ICSMSG                                                    
         MVC   ICSMSG(45),=C'**UNSUCCESSFUL - ORBITS PRESENT, NO POSTINX        
               G**'                                                             
         FOUT ICSMSGH                                                           
*                                                                               
DB92B    DS    0H                                                               
         CLI   ICSPAG,C'R'                                                      
         BE    REQ                                                              
         OC    RQOPTS,RQOPTS       TEST NEED REQ                                
         BNZ   DB29D                                                            
         CLI   REQPROF+1,C'Y'     TEST AUTO TA                                  
         BNE   EXIT                                                             
*                                                                               
DB29D    DS    0H                                                               
         CLI   CHSW,0              TEST HAVE DONE                               
         BE    EXIT                                                             
         B     REQ                                                              
*                                                                               
DB93     LA    R4,DSKPOINT         CLEAR WORK FROM DSKPOINT TO END              
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         BAS   RE,CLEARWRK                                                      
         MVI   CHSW,X'80'              FORCE RESTART FROM SCRATCH               
         MVI   BUFFSW,0                                                         
         MVI   PSTOPT,0            DO NOT RE-POST AFFS                          
         B     DB90                                                             
*                                                                               
SPACES   DC    CL10' '                                                          
*                                                                               
*                                                                               
DFLTPROF DS    0C                                                               
         DC    AL1(2)                                                           
         DC    C'Y'                                                             
         DC    C'Y'                                                             
         DC    C'1'                                                             
         DC    AL1(0)                                                           
         DC    11X'00'                                                          
*                                                                               
         EJECT                                                                  
*                   GET PRODUCT CODE                                            
         SPACE 3                                                                
GETPCOD  EQU   *                                                                
         USING CLTHDR,R9                                                        
         MVI   0(R5),0                                                          
         CLC   0(3,R4),=C'POL'                                                  
         BCR   8,RE                                                             
GETPCOD1 DS    0H                                                               
         MVI   0(R5),0                                                          
         LA    R6,CLIST                                                         
GPC2     CLC   0(3,R6),0(R4)                                                    
         BE    GPC3                                                             
         CLI   0(R6),0                                                          
         BCR   8,RE                                                             
         LA    R6,4(R6)                                                         
         B     GPC2                                                             
GPC3     MVC   0(1,R5),3(R6)                                                    
GPCEXT   BR    RE                                                               
         DROP  R9                                                               
         EJECT                                                                  
*                                                                               
*                                  TIME CODE LIST                               
         SPACE 2                                                                
         CNOP  2,4                                                              
TIMLST   DS    0D                                                               
         DC    C'0',HL2'0000',HL2'0000'                                         
         DC    C'1',HL2'0001',HL2'0001'                                         
         DC    C'2',HL2'0002',HL2'0002'                                         
         DC    C'3',HL2'0005',HL2'0005'                                         
         DC    C'4',HL2'0010',HL2'0010'                                         
         DC    C'5',HL2'0015',HL2'0015'                                         
         DC    C'6',HL2'0030',HL2'0030'                                         
         DC    C'7',HL2'0060',HL2'0060'                                         
         DC    C'8',HL2'0120',HL2'0120'                                         
         DC    C'9',HL2'9999',HL2'9999'                                         
TIMLSTN  EQU   (*-TIMLST)/5                                                     
         EJECT                                                                  
*                                  GENERATE REQUEST                             
         SPACE 2                                                                
REQ      EQU   *                                                                
         LA    R9,IOAREA                                                        
         USING QRECD,R9                                                         
         XC    QCTL,QCTL                                                        
         MVC   QCTL+11(2),RQDEST   DESTINATION OVERRIDE                         
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QCODE,=C'U2'                                                     
         MVC   QAGY,AGYALPHA                                                    
         MVC   QMED,ICSMED                                                      
         MVC   QCLT,ICSCLT                                                      
         CLI   BPRD,0                                                           
         BNE   REQ2                                                             
*                                  POL PRDS                                     
         MVC   QPROD,=C'POL'                                                    
         CLI   REQPROF+2,C'A'      TEST 'ALL' INSTEAD OF POL                    
         BNE   *+10                                                             
         MVC   QPROD,=C'ALL'                                                    
         MVC   QPROD2(6),QPRD                                                   
         OC    QPROD2(6),SPACES                                                 
         B     REQ4                                                             
*                                  NON-POL                                      
REQ2     EQU   *                                                                
         MVC   QPROD,QPRD                                                       
         CLI   QPRD2,C' '                                                       
         BNH   REQ4                                         *****               
         MVC   QPROD2,QPRD2                                                     
REQ4     EQU   *                                                                
         CLI   BEST,0              TEST EST REQ                                 
         BE    REQ5                                                             
         ZIC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
REQ5     DS    0H                                                               
         MVC   HALF,BMS                                                         
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   QSTA,ICSSTA                                                      
         CLI   QSTA+3,C'-'                                                      
         BNE   *+12                                                             
         MVI   QSTA+3,C' '                                                      
         B     REQ6                                                             
         CLI   QSTA+4,C'/'                                                      
         BE    REQ6                                                             
         CLI   QSTA+4,C'A'         KEEP X OF WABCX                              
         BNL   REQ6                                                             
         MVC   QSTA+4(1),ICSSTA+5  WABC-X                                       
REQ6     EQU   *                                                                
         OC    QSTA,SPACES                                                      
*                                  IF SECOND MONDAY 0, THEN PERIOD IS           
*                                  WEEK, OTHERWISE MONTH                        
*                                  IF WEEK USE 1ST MONDAY                       
*                                  IF MONTH USE 2ND MONDAY                      
*                                  (WHICH WILL BE WITHIN CAL. MON)              
         LA    R2,MONLST+2                                                      
         CLI   MONLST+2,0                                                       
         BNE   *+8                                                              
         LA    R2,MONLST                                                        
         GOTO1 VDATCON,DMCB,(2,(R2)),QSTART                                     
*                                                                               
         CLI   MONLST+2,0                                                       
         BE    *+14                                                             
         MVC   QSTART+4(2),SPACES                                               
         B     REQ8                                                             
         GOTO1 VADDAY,DMCB,QSTART,QSTART+6,6     SET END DAY OF WEEK            
*                                                                               
REQ8     DS    0H                                                               
         CLI   RQBOOK,C'A'         TEST BOOK SPECIFIED                          
         BL    REQ8B                                                            
         CLI   RQBOOK,X'FF'        TEST BOOK=NO                                 
         BE    REQ8M                                                            
         MVC   QBOOK(6),RQBOOK                                                  
         B     REQ8M                                                            
*                                                                               
REQ8B    DS    0H                  NO BOOK GIVEN                                
         CLI   REQPROF+3,C'N'      TEST HUT OPTION                              
         BNE   *+10                                                             
         MVC   QBOOK+4(2),=C'NO'                                                
         MVC   WORK(6),QSTART      USE MOS                                      
         CLI   WORK+4,C' '         IF FULL DATE GIVEN                           
         BNH   REQ8D                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK,6    ADD 6 DAYS TO ENSURE                  
*                                         RIGHT CALENDAR MONTH                  
REQ8D    DS    0H                                                               
         PACK  DUB,WORK+2(2)       LOOK UP BOOK BASED ON MOS MONTH              
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         IC    R0,REQPROF+3(R1)    BOOKS IN REQPROF+4 THRU +15                  
         LTR   R0,R0               TEST VALUE PRESENT                           
         BZ    REQ8M               NO - NO BOOK                                 
         CH    R0,=H'13'           MONTH 13 = ACT BOOK                          
         BNE   REQ8F                                                            
         MVC   QBOOK(4),=C'ACT '                                                
         B     REQ8M                                                            
*                                                                               
REQ8F    DS    0H                                                               
         MVI   BYTE,0                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QBOOK+2(2),DUB      SET MONTH                                    
         MVC   QBOOK(2),WORK       SET YEAR = MOS YEAR                          
         CR    R0,R1               UNLESS BOOK GT MOS                           
         BNH   REQ8J                                                            
*                                                                               
REQ8H    DS    0H                  SET YEAR -1                                  
         MVC   WORK(4),QBOOK                                                    
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK,F'-1'                               
         MVC   QBOOK(2),WORK                                                    
*                                  SEE IF BOOK EXISTS                           
REQ8J    DS    0H                                                               
         MVC   WORK(4),QBOOK                                                    
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,WORK,(3,WORK)                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),ICSMED                                                  
         MVI   KEY+2,C'A'          ALWAYS ARB FOR RADIO ????                    
         CLI   ICSMED,C'R'                                                      
         BE    REQ8L                                                            
         MVI   KEY+2,C'N'          NIELSON                                      
         CLI   SVCLPROF+3,C'0'                                                  
         BE    REQ8L                                                            
         CLC   WORK(2),=X'5E01'    FOR JAN94+ ALWAYS NIELSEN                    
         BNL   REQ8L                                                            
         MVI   KEY+2,C'A'          ELSE ARB                                     
REQ8L    DS    0H                                                               
         MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         LA    R3,BKERR2                                                        
         LA    R2,ICSREQH                                                       
*                                  NOTE- IOAREA+200 USED BECAUSE                
*                                        REQUEST IS BUILD AT IOAREA             
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA+200,     X        
               DMWORK                                                           
         TM    DMCB+8,X'FF'                                                     
         BNZ   MYERR                                                            
         CLC   KEY(5),IOAREA+200                                                
         BE    REQ8M               BOOK OK                                      
*                                                                               
         CLI   BYTE,0              NOT ON FILE, TRY TO BACK UP 1 YEAR           
         BNE   MYERR               ALREADY HAVE DONE                            
         MVI   BYTE,1                                                           
         B     REQ8H                                                            
*                                                                               
REQ8M    DS    0H                                                               
         MVC   QUESTOR(10),RQNAME                                               
         OC    QUESTOR(10),SPACES                                               
*                                                                               
         CLI   PSEUDOPT,C'R'       RESPONSE REQ?                                
         BNE   *+8                                                              
         MVI   QAREA+32,C'R'       SET OPT IN REQ                               
         CLI   PSEUDOPT,C'M'       MCT REQ?                                     
         BNE   *+8                                                              
         MVI   QAREA+32,C'M'       SET OPT IN REQ                               
*                                                                               
         PACK  DUB,QCODE                                                        
         CVB   R0,DUB                                                           
         STC   R0,QCTL+10                                                       
         MVI   QCTL+14,106                                                      
         IC    R0,TERMNAL                                                       
*                                                                               
         CLI   QSTA+4,C'/'         FOR LOCAL CABLE                              
         BNE   REQ9H                                                            
         MVI   QREC2,C' '          NEED 2ND CARD FOR NETWORK                    
         MVC   QREC2+1(L'QREC2-1),QREC2                                         
         MVC   QCBLNET,ICSSTA+5                                                 
         OC    QCBLNET,=3C' '                                                   
         CLI   QCBLNET,C' '                                                     
         BNH   REQ9H                                                            
         MVI   QCONT,C'*'          CONTINUED INDICATOR                          
         OI    QCTL+15,X'10'       SET HAVE 2ND REQ                             
         MVI   QCTL+14,186                                                      
*                                                                               
REQ9H    DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',QCTL,QCTL,((R0),0)           
         TM    8(R1),X'FF'                                                      
         BZ    REQ10                                                            
         SR    R3,R3                                                            
         B     MYERR                                                            
*                                                                               
REQ10    DS    0H                                                               
         MVC   WORK(L'ICSMSG),ICSMSG                                            
         LA    R4,WORK+L'ICSMSG-9                                               
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   2(5,R4),=C'(REQ)'                                                
         MVC   ICSMSG,WORK                                                      
         LA    R2,ICSPAGH                                                       
*                                                                               
         CLC   AGYALPHA,=C'GB'     CATCH GRIFFEN/BACALL U2 BUG                  
         BNE   EXIT                                                             
         CLI   QUESTOR,C' '                                                     
         BH    EXIT                                                             
*                                                                               
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
         EJECT                                                                  
*                   COMMUNICATION WITH DATA MANAGER (STATION FILE)              
         SPACE 3                                                                
READSTA  MVC   COMMAND,=C'DMREAD'                                               
         B     STAFILE                                                          
         SPACE 2                                                                
SEQSTA   MVC   COMMAND,=C'DMRSEQ'                                               
         B     STAFILE                                                          
         SPACE 2                                                                
HIGHSTA  MVC   COMMAND,=C'DMRDHI'                                               
         B     STAFILE                                                          
         SPACE 2                                                                
ADDSTA   MVC   COMMAND,=C'DMADD '                                               
         B     STAFILE                                                          
         SPACE 2                                                                
WRITESTA MVC   COMMAND,=C'DMWRT '                                               
         B     STAFILE                                                          
         SPACE 2                                                                
STAFILE  NTR                                                                    
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'STATION',               X        
               SKEY,IOAREA,((R3),0)                                             
         B     DMCHECK                                                          
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
         EJECT                                                                  
*============================================================*                  
* FOR PW CLIENT, FIND THE MARKET WITH BUYS IN CASE OF MKTFIX *                  
*============================================================*                  
         SPACE 1                                                                
CHKPWMKT NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(3),HKEY+1       A-M/CLT                                      
         MVC   KEY+3(1),BPRD       PRD                                          
         MVC   KEY+4(5),BMS        MKT/STA                                      
         MVC   KEY+9(1),BEST       ESTIMATE                                     
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     IF FOUND, USE THIS MKT                       
         BE    CHKPWX                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE                                      
         OC    SVOLDMK1,SVOLDMK1                                                
         BZ    CHKPWX                                                           
         MVC   KEY+4(2),SVOLDMK1   TRY FIRST OLD MKT                            
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    CHKPW2                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE                                      
         OC    SVOLDMK2,SVOLDMK2                                                
         BZ    CHKPWX                                                           
         MVC   KEY+4(2),SVOLDMK2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   CHKPWX              IF NOT FOUND USE CURRENT MARKET              
*                                                                               
CHKPW2   MVC   BMS(5),KEY+4        SAVE THIS MKT/STA                            
         MVC   HOMEMKT,KEY+4       INSURANCE                                    
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,KEY+4          SET NEW MARKET VALUES                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
*                                                                               
         MVI   SKEY,C'0'                                                        
         MVC   SKEY+1(16),SKEY                                                  
         MVI   SKEY,C'M'                                                        
         MVC   SKEY+1(1),ICSMED                                                 
         MVC   SKEY+2(4),WORK                                                   
         MVC   SKEY+6(2),AGYALPHA                                               
         LA    R2,ICSOPTH                                                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'STATION',SKEY,IOAREA                 
*                                                                               
         MVC   ICSMKTN(4),WORK                                                  
         MVI   ICSMKTN+4,C' '                                                   
         LA    R9,IOAREA                                                        
         USING MKTREC,R9                                                        
         MVC   ICSMKTN+5(15),MKTNAME                                            
         MVC   HOMEMKTN,MKTNAME                                                 
         FOUT  ICSMKTNH                                                         
         DROP  R9                                                               
CHKPWX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR REQUEST RECORD                                         
*                                                                               
QRECD    DSECT                                                                  
QCTL     DS    CL26                                                             
QAREA    DS    0CL80                                                            
QCODE    DS    CL2                 1                                            
QAGY     DS    CL2                 3                                            
QMED     DS    CL1                 5                                            
QCLT     DS    CL3                 6                                            
         DS    CL3                                                              
QPROD    DS    CL3                 12                                           
QMKT     DS    CL4                                                              
QSTA     DS    CL5                 19                                           
QEST     DS    CL3                 24                                           
         DS    CL4                                                              
QCONT    DS    CL1                 31                                           
         DS    CL3                                                              
QPROD2   DS    CL3                 35                                           
QSTART   DS    CL6                 38                                           
         DS    CL6                                                              
QBOOK    DS    CL6                                                              
         DS    CL13                                                             
QUESTOR  DS    CL12                69                                           
*                                                                               
QREC2    DS    0CL80               2ND REQUEST CARD                             
         DS    CL12                                                             
QCBLNET  DS    CL3                                                              
         DS    CL65                                                             
*                                                                               
         DSECT                                                                  
       ++INCLUDE SPSTABLK                                                       
*                                                                               
*INCLUDE DDCOMFACS/FATIOB/DDGLOBEQUS/DDCOREQUS                                  
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPMATWKN                                                       
