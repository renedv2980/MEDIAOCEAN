*          DATA SET SPINV00    AT LEVEL 114 AS OF 07/12/02                      
*********************************************************************           
****                                                                            
****                                                                            
****   THIS PROGRAM IS DEAD - RIP                                               
****   DO NOT BOTHER UPDATING ANYTHING HERE                                     
****   NEW VERSION IN NINV - SPSNV                                              
****                                                                            
****                                                                            
*********************************************************************           
*PHASE T20600B,+0                                                               
*INCLUDE TIMUNPK                                                                
*INCLUDE TIMPK                                                                  
*INCLUDE MEDGET                                                                 
         TITLE 'T20600 - I.C.S. DATA ENTRY BASE MODULE'                         
         SPACE 3                                                                
T20600   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 INVWKL,T20600                                                    
         SPACE 2                                                                
         LA    R8,COMSUBS          SET ADDRESSABILITY FOR                       
         USING COMSUBS,R8          COMMON SUBROUTINES                           
         USING GENOLD,RC                                                        
         BAS   RE,INITL                                                         
         ST    R1,SYSPARMS         SAVE A(PARMS)                                
         USING T206FFD,RA                                                       
         L     RF,16(R1)           COMFACS                                      
         ST    RF,VCOMFACS                                                      
         MVC   VDATCON,CDATCON-COMFACSD(RF)                                     
         MVC   VSCANNER,CSCANNER-COMFACSD(RF)                                   
         MVC   VGETPROF,CGETPROF-COMFACSD(RF)                                   
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
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
         MVI   WASPAY,C'N'                                                      
         XC    WORK,WORK                                                        
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,3,GLVPGM                             
         CLC   WORK(3),=C'PAY'                                                  
         BNE   *+8                                                              
         MVI   WASPAY,C'Y'                                                      
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'INV',3,GLVPGM                          
*                                                                               
         LH    R1,=Y(BUFFX-BUFFER)                                              
         STH   R1,BUFFL            BUFFER LENGTH                                
         SR    R0,R0                                                            
*                                                                               
         RELOC RELO00                                                           
*                                                                               
         MVI   NEWSW,C'Y'          INTIALIZE TO NEW MODE                        
*                                                                               
MP4      DS    0H                                                               
         MVI   ICSRCOD,C'0'        CLEAR SCREEN ERROR RETURN CODE               
         FOUT  ICSRCODH                                                         
         LA    RF,PRDLST                                                        
         ST    RF,APRDLST                                                       
         XC    RECWRK,RECWRK                                                    
         LA    R9,IOAREA                                                        
         L     RF,=V(TIMUNPK)                                                   
         A     RF,RELO00                                                        
         ST    RF,VTIMUNPK                                                      
         L     RF,=V(TIMPK)                                                     
         A     RF,RELO00                                                        
         ST    RF,VTIMPK                                                        
         L     RF,=A(EDIT)                                                      
         A     RF,RELO00                                                        
         ST    RF,VEDIT                                                         
         LA    RF,GETALL                                                        
         ST    RF,VGETALL                                                       
         ST    RB,PASSRB                                                        
         ST    R8,PASSR8                                                        
         EJECT                                                                  
*                   VALIDATE MEDIA                                              
         SPACE 3                                                                
         LA    R2,ICSMEDH                                                       
         TM    4(R2),X'20'                                                      
         BO    DB3                                                              
         CLI   5(R2),0                                                          
         BNE   DB0                                                              
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                             
*                                                                               
DB0      LA    R3,MEDERR                                                        
         GOTO1 =V(MEDGET),DMCB,(ICSMED,AGYALPHA),VDATAMGR,WORK,        X        
               RR=RELO00                                                        
*                                                                               
         CLI   DMCB+8,X'FF'                                                     
         BE    DB1B                                                             
         MVC   ICSMEDN,WORK+1                                                   
         MVC   BMED,WORK           AGYMED BYTE                                  
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGYALPHA                                                
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         MVC   BILLROPT,AGYPROF+14-AGYHDR+IOAREA BILLER NAME OPTION             
         MVC   COUNTRY,AGYPROF+7-AGYHDR+IOAREA                                  
         MVI   SPADSW,C'N'         ADD INTERFACE SWITCH                         
         TM    AGYFLAG1-AGYHDR+IOAREA,X'80'                                     
         BZ    *+8                                                              
         MVI   SPADSW,C'Y'                                                      
*                                  GET SPOTPROF                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),AGYALPHA  A/M ONLY - NO CLIENT                         
         MVC   WORK+6(1),ICSMED                                                 
         GOTO1 VGETPROF,DMCB,WORK,SPOTPROF,VDATAMGR                             
*                                                                               
         MVI   NETPSW,C'N'         SET NETPAK SWITCH                            
         CLI   ICSMED,C'N'                                                      
         BNE   DB0L                                                             
         OC    SPOTPROF+9(2),SPOTPROF+9                                         
         BZ    DB0L                                                             
         MVI   NETPSW,C'Y'         IS NETPAK                                    
*                                                                               
DB0L     DS    0H                                                               
         MVI   CENTS,C'Y'          CENTS                                        
         CLI   NETPSW,C'Y'         IF NETPAK                                    
         BE    DB2                                                              
         CLI   ICSMED,C'N'         OR IF NOT NETWORK                            
         BNE   DB2                                                              
         CLI   COUNTRY,C'C'        AND NOT CANADA                               
         BE    DB2                                                              
DB1A     DS    0H                                                               
         MVI   CENTS,C'N'                                                       
         B     DB2                                                              
*                                                                               
DB1B     DS    0H                                                               
         XC    ICSMEDN,ICSMEDN                                                  
         FOUT  ICSMEDNH                                                         
         B     ERRXT                                                            
DB2      DS    0H                                                               
         CLI   BILLROPT,C'Y'       ARE WE DOING BILLER NAME                     
         BE    DB2H                                                             
         TM    ICSBLRH+1,X'20'     NO, IF FIELD ALREADY PROTECTED               
         BNZ   DB2P                OK                                           
         OI    ICSBLRH+1,X'20'     ELSE PROTECT IT                              
         XC    ICSBLRW,ICSBLRW                                                  
         FOUT  ICSBLRWH                                                         
         XC    ICSBLR,ICSBLR                                                    
         FOUT  ICSBLRH                                                          
         B     DB2P                                                             
*                                                                               
DB2H     DS    0H                  ARE DOING BILLER NAME                        
         TM    ICSBLRH+1,X'20'     IS FIELD ALREADY PROTECTED                   
         BZ    DB2J                NO, OK                                       
         NI    ICSBLRH+1,X'DF'     ELSE UNPROTECT IT                            
         MVC   ICSBLRW(6),=C'BILLER'                                            
         FOUT  ICSBLRWH                                                         
         FOUT  ICSBLRH                                                          
*                                                                               
DB2J     DS    0H                                                               
DB2P     DS    0H                                                               
         MVI   INVKEY,X'0B'                                                     
         MVC   INVKAM,BMED                                                      
         FOUT  ICSMEDNH                 SEND MED NAME                           
         NI    ICSSTAH+4,X'DF'          RESET VAL. BITS                         
         NI    ICSCLTH+4,X'DF'                                                  
         MVC   TRFILM,SPACES                                                    
         XC    ELLIST,ELLIST                                                    
         MVI   SVINVID,0                                                        
         XC    LISTKEY,LISTKEY                                                  
         OI    4(R2),X'20'                                                      
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
         MVI   HLCHG,C'Y'                                                       
         EJECT                                                                  
*                   VALIDATE STATION                                            
         SPACE 3                                                                
DB3      DS    0H                                                               
         LA    R2,ICSSTAH                                                       
         TM    ICSCLTH+4,X'20'     ON CHG OF CLT UNVAL STAION                   
         BNZ   *+8                                                              
         NI    4(R2),X'DF'                                                      
         TM    4(R2),X'20'                                                      
         BO    DB8                                                              
         CLI   5(R2),0                                                          
         BNE   DB3A                                                             
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPSTA                            
*                                                                               
DB3A     XC    ICSMKTN,ICSMKTN                                                  
         FOUT  ICSMKTNH                                                         
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
         BNE   ERRXT                                                            
*                             BUILD STATION KEY                                 
         MVI   SKEY,C'0'                                                        
         MVC   SKEY+1(16),SKEY                                                  
         MVI   SKEY,C'S'                                                        
         MVC   SKEY+1(1),ICSMED                                                 
         MVC   SKEY+2(5),STBSTA                                                 
         MVC   STA,STBSTA                                                       
         CLI   STBSTA+4,C'/'       IF MEDIA BLANK OR /                          
         BH    DB3D                                                             
         MVI   SKEY+6,C'T'         SET TO T FOR FILE READS                      
         MVI   STA+4,C'T'                                                       
         MVC   ICSSTA(8),STBSTA                                                 
         MVI   ICSSTA+4,C'/'       NEED / ON SCREEN                             
         FOUT  ICSSTAH                                                          
*                                                                               
DB3D     DS    0H                                                               
         MVC   SKEY+7(2),AGYALPHA                                               
         MVC   SKEY+9(3),ICSCLT                                                 
         OI    SKEY+11,X'40'                                                    
         BAS   RE,READSTA                                                       
         CLC   SKEY(15),IOAREA      TEST FOUND STATION                          
         BNE   ERRXT                                                            
         USING STAREC,R9                                                        
*                                                                               
         XC    DMCB(8),DMCB        GET A(STAPACK)                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSTAPACK                                                  
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAPACK,0(R1)                                                   
*                                  PACK MARKET AND STATION                      
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPMED,ICSMED                                                   
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPQMKT,SMKT                                                    
         MVC   STAPQSTA(8),STA                                                  
         OC    STAPQSTA(8),SPACES                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   INVKSTA,STAPMKST+2                                               
         MVC   BMS,STAPMKST                                                     
         DROP  R1                                                               
*                                                                               
         MVI   SKEY,C'0'                                                        
         MVC   SKEY+1(16),SKEY                                                  
         MVI   SKEY,C'M'                                                        
         MVC   SKEY+1(1),ICSMED                                                 
         MVC   SKEY+2(4),SMKT                                                   
         MVC   SKEY+6(2),AGYALPHA                                               
         BAS   RE,HIGHSTA                                                       
         CLC   SKEY(15),IOAREA     TEST FOUND MARKET                            
         BNE   ERRXT                                                            
*                                                                               
         L     RF,VTWA                                                          
         CLI   6(RF),C'+'          TEST MKT LIMIT ACCESS                        
         BNE   DB7B                                                             
         USING MKTREC,R9                                                        
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
DB7A     DS    0H                                                               
         CLC   7(1,RF),0(R1)                                                    
         BE    DB7B                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,DB7A                                                          
*                                                                               
         LA    R3,ACCERR                                                        
         B     ERRXT                                                            
DB7B     DS    0H                                                               
         USING MKTREC,R9                                                        
         MVC   ICSMKTN(24),MKTNAME                                              
         FOUT  ICSMKTNH                                                         
*                                                                               
         MVC   TRFILM,SPACES       CLEAR 'BASE' FILM                            
         XC    ELLIST,ELLIST                                                    
         MVI   SVINVID,0                                                        
         XC    LISTKEY,LISTKEY                                                  
         OI    4(R2),X'20'         SET VALIDATION BIT                           
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPSTA                            
         MVI   HLCHG,C'Y'                                                       
         EJECT                                                                  
*                   VALIDATE CLIENT                                             
         SPACE 3                                                                
DB8      DS    0H                                                               
         LA    R2,ICSCLTH                                                       
         TM    4(R2),X'20'         TEST ALREADY VALIDATED                       
         BO    DB12                                                             
         CLI   5(R2),0                                                          
         BNE   DB8A                                                             
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPCLT                            
*                                                                               
DB8A     LA    R3,CLTERR                                                        
         XC    ICSCLTN,ICSCLTN                                                  
         FOUT  ICSCLTNH                                                         
         OC    ICSCLT,=3C' '                                                    
         GOTO1 VCLPACK,DMCB,ICSCLT,BCLT                                         
         CLI   DMCB,0                                                           
         BNE   ERRXT                                                            
*                                                                               
         MVI   BLRPRD,0            CLEAR BILLER PRD/EST                         
         MVI   BLREST,0                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BMED                                                    
         MVC   KEY+2(2),BCLT                                                    
         BAS   RE,HIGH                                                          
         LA    R3,CLTERR2                                                       
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRXT               CLIENT NOT FOUND                             
         BAS   RE,GETREC                                                        
         USING CLTHDR,R9                                                        
         L     RF,APRDLST                                                       
         LA    R1,880              L'CLIST                                      
         LA    RE,CLIST                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC   SVCLPROF,CPROF                                                   
         MVC   SVTRACLT,CMCLTCOD   SAVE TRAFFIC CLIENT                          
         MVC   SVTRAPRD,CMCLTPRD   AND PRODUCT                                  
*                                                                               
         L     RF,VTWA                                                          
         OC    6(2,RF),6(RF)       TEST ANY ACCESS CODE                         
         BZ    DB8D                NO                                           
         CLI   6(RF),C'+'          BYPASS MKT LIMIT ACCESS                      
         BE    DB8D                                                             
         CLC   6(2,RF),CKEY+2                                                   
         BE    DB8D                                                             
         CLI   6(RF),C'*'          OFFICE                                       
         BNE   DB8B                                                             
         CLC   7(1,RF),COFFICE                                                  
         BE    DB8D                                                             
         B     DB8C4                                                            
*                                                                               
DB8B     DS    0H                                                               
         CLI   6(RF),C'$'          OFFICE LIST                                  
         BNE   DB8C4                                                            
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
         MVC   OFCAUTH,T206FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
T20600   CSECT                                                                  
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,VCOMFACS                                           
         CLI   0(R1),0                                                          
         BE    DB8D                OK                                           
*                                                                               
DB8C4    DS    0H                                                               
         LA    R3,ACCERR           NO ACCESS                                    
         B     ERRXT                                                            
*                                                                               
DB8D     DS    0H                                                               
         MVC   ICSCLTN,CNAME                                                    
         FOUT  ICSCLTNH                                                         
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
*                                  GET TRAFFIC PROFILE                          
         MVC   BYTE,TRAFSW         SAVE MEDIA TRAFSW                            
         MVC   WORK(4),=C'S0TI'                                                 
         GOTO1 (RF),(R1),,TRAFPROF                                              
         MVC   TRAFSW(1),BYTE         RESTORE MEDIA TRAFSW                      
*                                  GET AUTO REQ PROFILE                         
         MVC   WORK(4),=C'SI2R'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE R               
         GOTO1 (RF),(R1),,REQPROF                                               
*                                  GET I2X PROFILE                              
         MVC   WORK(4),=C'SI2X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE R               
         GOTO1 (RF),(R1),,I2XPROF                                               
*                                  GET I2Y PROFILE                              
         MVC   WORK(4),=C'SI2Y'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE R               
         GOTO1 (RF),(R1),,I2YPROF                                               
*                                                                               
DB9      DS    0H                  GET TRAFFIC PROFILE                          
         MVC   WORK(4),=C'S0TI'                                                 
         GOTO1 (RF),(R1),,TRAFPROF                                              
*                                                                               
         CLI   TRAFSW,C'Y'         ARE WE DOING FILMS?                          
         BE    DB9F                YES                                          
*                                  NO, PROTECT FILM CODE FIELDS                 
         CLI   ICSFCHD+1,C' '      ARE THEY ALREADY SET                         
         BNH   DB10                                                             
*                                                                               
         MVC   ICSLAST(3),=X'000101'  FORCE RE-TRANSMIT                         
         LA    R4,ICSFILMH                                                      
         LA    R5,ICSDMYH          FOR END CHECK                                
*                                                                               
DB9D     DS    0H                                                               
         OC    8(L'ICSFILM,R4),8(R4)    TEST ANY INPUT                          
         BZ    DB9D4                                                            
         XC    8(L'ICSFILM,R4),8(R4)                                            
         FOUT  (R4)                                                             
*                                                                               
DB9D4    DS    0H                                                               
         OI    1(R4),X'20'         SET PROTECTED                                
         LA    R4,LINLGTH(R4)                                                   
         CR    R4,R5                                                            
         BL    DB9D                                                             
*                                                                               
         XC    ICSFCHD,ICSFCHD     CLEAR FILM CODE HEADING                      
         FOUT  ICSFCHDH                                                         
         B     DB10                                                             
*                                  ARE DOING FILMS                              
DB9F     DS    0H                  UNPROTECT FILM CODE FIELDS                   
         CLI   ICSFCHD+1,C' '      ARE THEY ALREADY SET                         
         BH    DB10                                                             
*                                                                               
         MVC   ICSLAST(3),=X'000101'  FORCE RE-TRANSMIT                         
         LA    R4,ICSFILMH                                                      
         LA    R5,ICSDMYH          FOR END CHECK                                
*                                                                               
DB9H     DS    0H                                                               
         NI    1(R4),X'DF'         SET UNPROTECTED                              
         LA    R4,LINLGTH(R4)                                                   
         CR    R4,R5                                                            
         BL    DB9H                                                             
*                                                                               
         MVC   ICSFCHD,=C'FILM CODE '    FILM CODE HEADING                      
         FOUT  ICSFCHDH                                                         
*                                                                               
DB10     DS    0H                                                               
         MVC   SVOFFC,COFFICE                                                   
         MVC   INVKCLT,BCLT                                                     
         MVC   TRFILM,SPACES       CLEAR 'BASE' FILM                            
         XC    ELLIST,ELLIST                                                    
         MVI   SVINVID,0                                                        
         XC    LISTKEY,LISTKEY                                                  
         OI    4(R2),X'20'                                                      
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPCLT                            
         MVI   HLCHG,C'Y'                                                       
         SPACE 3                                                                
*        VALIDATE INVOICE NUMBER                                                
*                                                                               
DB12     DS    0H                                                               
         LA    R2,ICSINVH                                                       
         CLI   ICSACT,C'A'         IF ADD                                       
         BNE   DB12D                                                            
         TM    1(R2),X'20'         AND FIELD IS PROTECTED                       
         BZ    DB12D                                                            
         XC    8(L'ICSINV,R2),8(R2) CLEAR OUT INVOICE NUMBER                    
         MVI   5(R2),0              SO WONT BE ACCIDENTLLY REUSED               
         NI    4(R2),X'DF'         UNVALIDATE                                   
         FOUT  (R2)                                                             
*                                                                               
DB12D    DS    0H                                                               
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    DB14                                                             
         XC    INVNUM,INVNUM                                                    
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    DB12M                                                            
         CLI   5(R2),L'IHDINV      TEST TOO LONG                                
         BNH   *+12                                                             
         LA    R3,INVLNERR                                                      
         B     ERRXT                                                            
*                                                                               
         MVC   INVNUM,ICSINV                                                    
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'INVNUM-1),WORK                                          
         OC    INVNUM,WORK         OR WITH SPACES                               
*                                                                               
DB12M    DS    0H                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
         CLI   ICSACT,C'C'         FOR CHANGE DONT RESET BECAUSE                
         BE    DB14                JUST CHANGING INVOICE NUMBER                 
         XC    ELLIST,ELLIST                                                    
         MVI   SVINVID,0                                                        
*                                  NOTE- DO NOT SET HLCHG- IT ONLY              
*                                        CONTROLS REQUESTS                      
*                                                                               
DB14     DS    0H                                                               
         EJECT                                                                  
*                   VALIDATE ACTION                                             
         SPACE 3                                                                
DB18     LA    R2,ICSACTH                                                       
         TM    4(R2),X'20'                                                      
         LA    R3,ACTERR                                                        
         CLI   ICSACT,C'T'         TOTAL                                        
         BE    DB19                                                             
         CLI   ICSACT,C'D'         DISP                                         
         BE    DB19                                                             
         CLI   ICSACT,C'A'         ADD                                          
         BE    DB19                                                             
         CLI   ICSACT,C'C'         CHANGE                                       
         BE    DB20                NO RECORD SET NO. ON CHANGE                  
         CLI   ICSACT,C'R'                                                      
         BE    DB20                OR REQUEST                                   
         CLI   ICSACT,C'L'                                                      
         BE    DB20                OR LIST                                      
         XC    ICSACT,ICSACT                                                    
         OI    6(R2),X'80'                                                      
         CLC   AGYALPHA,=C'OM'     IF AGENCY IS OGILVY& MATHER                  
         BNE   DB19                                                             
         CLI   WASPAY,C'Y'                                                      
         BNE   *+14                                                             
         MVC   ICSACT(3),=C'ADD'   DEFAULT TO ADD IF CAME FROM $PAY             
         B     *+10                                                             
         MVC   ICSACT(4),=C'DISP'  DEFAULT TO DISP OTHERWISE                    
*                                                                               
DB19     DS    0H                                                               
         ZIC   R5,5(R2)            LENGTH OF INPUT                              
         LA    R5,7(R2,R5)         POINT TO LAST CHAR                           
         MVI   RSET,C'1'           RECORD SET NO.                               
         CLI   0(R5),C'1'          IF LAST CHAR NUMERIC                         
         BNH   *+10                                                             
         MVC   RSET,0(R5)                                                       
         NI    RSET,X'0F'                                                       
DB20     DS    0H                                                               
         EJECT                                                                  
*              VALIDATE REQUEST OPTIONS                                         
DB20B    DS    0H                                                               
         LA    R2,ICSREQH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   DB20Z                                                            
         MVI   SEQSW,C'N'                                                       
         XC    RQOPTS,RQOPTS                                                    
         XC    BUFFER(256),BUFFER                                               
         GOTO1 VSCANNER,DMCB,(0,ICSREQH),BUFFER                                 
*                                                                               
         ZIC   R0,DMCB+4                                                        
         LA    R5,BUFFER                                                        
         LA    R3,REQERR                                                        
DB20C    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DB20X                                                            
         CLC   =C'I4',12(R5)       REPORT I4                                    
         BNE   DB20C2                                                           
         MVC   RQREP,12(R5)                                                     
         MVI   RQSW,C'Y'                                                        
         B     DB20W                                                            
*                                                                               
DB20C2   DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'NAME'                                                
         BNE   DB20D                                                            
         MVI   RQSW,C'Y'                                                        
         MVC   RQNAME,22(R5)                                                    
         B     DB20W                                                            
DB20D    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'BOOK'                                                
         BNE   DB20J                                                            
         MVI   RQSW,C'Y'                                                        
         CLC   22(2,R5),=C'NO'     TEST BOOK=NO                                 
         BNE   DB20E                                                            
         CLI   24(R5),C' '                                                      
         BH    DB20E                                                            
         MVI   RQBOOK,X'FF'                                                     
         B     DB20W                                                            
*                                                                               
DB20E    DS    0H                                                               
         CLC   =C'ACT',22(R5)                                                   
         BNE   DB20F                                                            
         MVC   RQBOOK,=C'ACT   '                                                
         B     DB20W                                                            
*                                                                               
DB20F    DS    0H                                                               
         LA    R3,BKERR                                                         
         GOTO1 VDATVAL,DMCB,(2,22(R5)),RQBOOK                                   
         OC    DMCB,DMCB                                                        
         BZ    ERRXT                                                            
         MVC   RQBOOK+4(2),=2C' '                                               
         CLC   DMCB+3(1),1(R5)          TEST ONLY DATE GIVEN                    
         BE    DB20H                    YES                                     
*                                                                               
         LA    RF,22(R5)                LOOK FOR HUT                            
         A     RF,DMCB                                                          
         MVC   RQBOOK+4(2),1(RF)                                                
         CLC   RQBOOK+4(2),=C'NO'                                               
         BE    DB20H                                                            
         OC    RQBOOK+4(2),=2C'0'                                               
         CLC   RQBOOK+4(2),=2C'0'                                               
         BNH   ERRXT                                                            
         CLC   RQBOOK+4(2),=C'12'                                               
         BH    ERRXT                                                            
*                                                                               
DB20H    DS    0H                                                               
         MVC   WORK(4),RQBOOK                                                   
         MVC   WORK+4(2),=C'01'                                                 
**NOP    GOTO1 VDTCNV,DMCB,WORK,(1,WORK)                                        
         GOTO1 VDATCON,DMCB,WORK,(3,WORK)                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),ICSMED                                                  
         MVI   KEY+2,C'N'          NIELSEN                                      
         CLI   SVCLPROF+3,C'0'                                                  
         BE    DB20H2                                                           
         CLC   WORK(2),=X'5E01'    JAN94+ ALWAYS NEILSEN                        
         BNL   DB20H2                                                           
         MVI   KEY+2,C'A'          ELSE ARB                                     
DB20H2   DS    0H                                                               
         MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         LA    R3,BKERR2                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA,DMWORK            
         TM    DMCB+8,X'FF'                                                     
         BNZ   ERRXT                                                            
         CLC   KEY(5),IOAREA                                                    
         BNE   ERRXT                                                            
*                                                                               
         B     DB20W                                                            
*                                                                               
DB20J    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'PRD'                                                 
         BNE   DB20P                                                            
         MVI   RQSW,C'Y'                                                        
         LA    R3,PRDERR                                                        
         CLI   1(R5),2                                                          
         BL    ERRXT                                                            
         MVI   WORK,C' '                                                        
         MVC   WORK+1(9),WORK                                                   
         OC    22(7,R5),WORK                                                    
         MVC   WORK(7),22(R5)                                                   
         CLI   WORK+2,C'-'                                                      
         BNE   DB20L                                                            
         MVI   WORK+2,C' '                                                      
         MVI   WORK+3,C'-'                                                      
         MVC   WORK+4(3),22+3(R5)                                               
DB20L    DS    0H                                                               
         XC    SAVPRD(8),SAVPRD                                                 
         GOTO1 VEDIT,DMCB,GENOLD                                                
         XC    SAVPRD(8),SAVPRD                                                 
         CLI   ERRAREA,0                                                        
         BE    DB20N                                                            
         SR    R3,R3                                                            
         CLI   ERRAREA,X'FF'                                                    
         BE    *+8                                                              
         IC    R3,ERRAREA                                                       
         B     ERRXT                                                            
*                                                                               
DB20N    DS    0H                                                               
         MVC   RQPRD(3),WORK                                                    
         MVC   RQPRD+3(3),WORK+4                                                
         B     DB20W                                                            
*                                                                               
DB20P    DS    0H                                                               
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'BUY'                                                 
         BNE   DB20P6                                                           
         MVI   RQSW,C'Y'                                                        
         MVC   RQBUYOPT,22(R5)                                                  
         B     DB20W                                                            
*                                                                               
DB20P6   DS    0H                                                               
*                                                                               
DB20Q    DS    0H                                                               
         MVI   BYTE,C'M'           SET HAVE 'MOS' ENTERED                       
         LA    R4,22(R5)                                                        
         ZIC   RF,0(R5)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'MOS'                                                 
         BE    DB20Q2                                                           
*                                CHECK FOR DATE ENTERED BY ITSELF               
         MVI   BYTE,0              NO 'MOS' ENTERED                             
         CLI   0(R5),8             8 MAX LENGTH                                 
         BH    DB20S                                                            
         LA    R4,12(R5)                                                        
DB20Q2   DS    0H                  FIRST TRY FULL DATE                          
         GOTO1 VDATVAL,DMCB,0(R4),DUB                                           
         CLI   DMCB+3,0                                                         
         BE    DB20Q3                                                           
         GOTO1 VGETDAY,DMCB,DUB,FULL                                            
         CLI   DMCB,1              MUST BE MONDAY                               
         BE    DB20Q4                                                           
         B     DB20Q3D                                                          
*                                                                               
DB20Q3   DS    0H                                                               
         CLI   BYTE,C'M'          IF 'MOS' ENTERED, SKIP LENGTH TESTS           
         BE    DB20Q3B                                                          
         CLI   0(R5),6             IF YM ONLY, 6 MAX LENGTH                     
         BH    DB20S                                                            
         CLI   0(R5),5             5 MINIMUM                                    
         BL    DB20S                                                            
DB20Q3B  DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(2,0(R4)),DUB    YM ONLY                            
         MVC   DUB+4(2),=C'  '                                                  
         CLC   DUB(2),=C'75'       MUST BE 1975 OR HIGHER                       
         BL    DB20Q3D                                                          
         CLI   DMCB+3,0                                                         
         BNE   DB20Q4                                                           
DB20Q3D  DS    0H                                                               
         LA    R3,DATERR                                                        
         CLI   BYTE,C'M'           ERROR IF 'MOS' ENTERED                       
         BNE   DB20S                                                            
         B     ERRXT                                                            
*                                                                               
DB20Q4   DS    0H                                                               
         MVC   RQMOS,DUB                                                        
         MVI   RQSW,C'Y'                                                        
         B     DB20W                                                            
DB20S    DS    0H                                                               
         CLC   =C'SEQ',12(R5)      OPTION TO SEQUENCE ELEMS                     
         BNE   DB20V                                                            
         L     RF,VTWA                                                          
         CLI   1(RF),C'*'          DDS ONLY                                     
         BNE   DB20V                                                            
         MVI   SEQSW,C'Y'                                                       
         B     DB20W                                                            
DB20V    DS    0H                                                               
         LA    R3,REQERR                                                        
         B     ERRXT                                                            
*                                                                               
DB20W    DS    0H                                                               
         LA    R5,32(R5)                                                        
         B     DB20C                                                            
DB20X    DS    0H                                                               
         XC    BUFFER(256),BUFFER                                               
         OI    4(R2),X'20'         SET VALIDATED                                
         MVI   HLCHG,C'Y'                                                       
*                                                                               
DB20Z    DS    0H                                                               
         EJECT                                                                  
*                                  VALIDATE OPTIONS FIELD                       
         SPACE 2                                                                
DB21     DS    0H                                                               
         MVC   SVPSEUDO,PSEUDOPT   SAVE PSEUDOPT                                
         MVI   PSEUDOPT,C'N'                                                    
         LA    R2,ICSOPTNH                                                      
*                                                                               
         XC    BUFFER(256),BUFFER                                               
         XC    OPTFLDS,OPTFLDS                                                  
         GOTO1 VSCANNER,DMCB,(12,ICSOPTNH),BUFFER                               
*                                                                               
         ZIC   R0,DMCB+4                                                        
         LA    R5,BUFFER                                                        
         LA    R3,OPTERR                                                        
DB21C    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DB21X                                                            
         CLC   =C'NET',12(R5)                                                   
         BNE   DB21E                                                            
         CLI   ICSACT,C'A'         ONLY SET NETSW FOR ADD                       
         BE    DB21C2                                                           
         CLI   ICSACT,C'C'         OR CHANGE                                    
         BNE   DB21W                                                            
DB21C2   DS    0H                                                               
         MVI   NETSW,C'Y'                                                       
         B     DB21W                                                            
*                                                                               
DB21E    DS    0H                                                               
         CLC   =C'ID',12(R5)                                                    
         BNE   DB21F                                                            
         MVC   IDNAME,22(R5)                                                    
         B     DB21W                                                            
*                                                                               
DB21F    DS    0H                                                               
         CLC   =C'NEW',12(R5)      SET NEW KEY SWITCH **TEMP**                  
         BNE   DB21G                                                            
         MVC   NEWSW,22(R5)                                                     
         CLI   NEWSW,C'Y'                                                       
         BNE   DB21W               **SKIP INVOICE FIELD ACTIVATE**              
*                                                                               
         TM    ICSINVH+1,X'20'     TEST INV FIELD PROTECTED                     
         BZ    DB21W                                                            
         NI    ICSINVH+1,X'DF'     UN-PROTECT                                   
         MVC   ICSINVW(7),=C'INVOICE'                                           
         FOUT  ICSINVWH                                                         
         FOUT  ICSINVH                                                          
         B     DB21W                                                            
*                                                                               
DB21G    DS    0H                                                               
         CLC   =C'CL',12(R5)      SET CLIENT FOR CHANGE                         
         BNE   DB21J                                                            
         MVC   NEWCLT,22(R5)                                                    
         OC    NEWCLT,=3C' '                                                    
         GOTO1 VCLPACK,DMCB,NEWCLT,DUB                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BMED                                                    
         MVC   KEY+2(2),DUB                                                     
         BAS   RE,HIGH                                                          
         LA    R3,CLTERR2                                                       
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRXT               CLIENT NOT FOUND                             
         B     DB21W                                                            
*                                                                               
DB21J    DS    0H                                                               
         CLC   =C'RES',12(R5)      RESPONSE SWITCH                              
         BNE   DB21K                                                            
         CLI   ICSACT,C'A'         ONLY SET RESPONSE FOR ADD                    
         BE    DB21J2                                                           
         CLI   ICSACT,C'C'         OR CHANGE                                    
         BNE   DB21W                                                            
DB21J2   DS    0H                                                               
         MVI   PSEUDOPT,C'R'                                                    
         CLI   22(R5),C'Y'                                                      
         BE    DB21W                                                            
         CLI   22(R5),C' '                                                      
         BE    DB21W                                                            
         MVI   PSEUDOPT,C'N'                                                    
         B     DB21W                                                            
*                                                                               
DB21K    DS    0H                                                               
         CLC   =C'MCT',12(R5)      MCT SWITCH                                   
         BNE   DB21L                                                            
         CLI   ICSACT,C'A'         ONLY SET FOR ADD                             
         BE    DB21K2                                                           
         CLI   ICSACT,C'C'         OR CHANGE                                    
         BNE   DB21W                                                            
DB21K2   DS    0H                                                               
         MVI   PSEUDOPT,C'M'                                                    
         CLI   22(R5),C'Y'                                                      
         BE    DB21W                                                            
         CLI   22(R5),C' '                                                      
         BE    DB21W                                                            
         MVI   PSEUDOPT,C'N'                                                    
         B     DB21W                                                            
*                                                                               
DB21L    DS    0H                                                               
         B     ERRXT                                                            
*                                                                               
DB21W    DS    0H                                                               
         LA    R5,34(R5)                                                        
         B     DB21C                                                            
*                                                                               
DB21X    DS    0H                                                               
         XC    BUFFER(256),BUFFER                                               
         CLC   SVPSEUDO,PSEUDOPT   DID PSEUDOPT CHANGE?                         
         BE    *+8                 NO, OK                                       
         MVI   ICSHD1,C' '         FORCE RESET OF COLUMN HEADS                  
*                                                                               
         SPACE 3                                                                
*        COMSUBS- COMMON SUBROUTINES AND DATA AREAS - R8 IS BASE                
*              ** TAG MOVED HERE BECAUSE RB SPACE TOO LARGE                     
*                                                                               
COMSUBS  DS    0H                                                               
*                                                                               
*                                  GET OVERLAY                                  
*                                  -----------                                  
         LA    R3,1                                                             
         SR    R4,R4                                                            
         GOTO1 VCALLOV,DMCB,((R3),(R4)),(RA)                                    
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC)                                                   
         SPACE 2                                                                
*                              *** GET BILLER NAME ***                          
BLRNAM   DS    0H                                                               
         CLI   BILLROPT,C'Y'       ARE WE DOING BILLER NAME                     
         BNE   BLRNAM4                                                          
         LA    RE,ICSBLRH                                                       
         CLI   BLRPRD,0            IF PRD MISSING                               
         BE    BLRNAM3             SKIP BILLER NAME                             
         CLI   BLREST,0            OR ESTIMATE                                  
         BNE   BLRNAM3D                                                         
*                                                                               
BLRNAM3  DS    0H                                                               
         OC    8(L'ICSBLR,RE),8(RE)                                             
         BZ    BLRNAM4                                                          
         XC    8(L'ICSBLR,RE),8(RE)                                             
         FOUT  ICSBLRH                                                          
         B     BLRNAM4                                                          
*                                                                               
BLRNAM3D DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING GETBUBLD,R4                                                      
*                                                                               
         MVC   GBCOMFAC,VCOMFACS                                                
         LA    RF,IOAREA                                                        
         ST    RF,GBIOA                                                         
         ST    RE,GBNAMFLD                                                      
         MVC   GBAGY,AGYALPHA                                                   
         MVC   GBMEDEBC,ICSMED                                                  
         MVC   GBCLTEBC,ICSCLT                                                  
         MVC   GBOFFICE,SVOFFC                                                  
         MVC   GBAGYMD,INVKAM                                                   
         MVC   GBCLT,INVKCLT                                                    
         MVC   GBPRD,BLRPRD                                                     
         MVC   GBEST,BLREST                                                     
         MVC   GBMKT(5),BMS                                                     
         MVI   GBTYPE,C'I'         SET FOR BILLER NAME                          
*                                  GET A(GETBUBL)                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A77'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         CLI   GBERR,0                                                          
         BE    BLRNAM4                                                          
         CLI   GBERR,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                DIE FOR NOW ON DMGR ERROR                    
         LA    R3,MISSERR                                                       
         LA    R2,ICSBLRH                                                       
         BE    ERRXT                                                            
*                                                                               
BLRNAM4  DS    0H                                                               
         DROP  R4                                                               
         SPACE 2                                                                
*                                  RE-CREATE OPTIONS LINE                       
DB22     DS    0H                                                               
         LA    R2,WORK+1                                                        
         XC    WORK,WORK                                                        
         CLI   NETSW,C'Y'                                                       
         BNE   DB22B                                                            
         MVC   0(4,R2),=C'NET,'                                                 
         LA    R2,4(R2)                                                         
*                                                                               
DB22B    DS    0H                                                               
         CLI   IDNUM,0                                                          
         BE    DB22C                                                            
         MVC   0(3,R2),=C'ID='                                                  
         MVC   3(12,R2),IDNAME                                                  
         LA    R2,15(R2)                                                        
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
*                                                                               
DB22C    DS    0H                                                               
         CLI   PSEUDOPT,C'R'                                                    
         BNE   DB22C2                                                           
         MVC   0(9,R2),=C'RESPONSE,'                                            
         LA    R2,9(R2)                                                         
         CLI   ICSACT,C'L'         DONT ADJUST HEADER IF LIST                   
         BE    DB22D                                                            
         CLC   =C'RESP',ICSHD2                                                  
         BE    DB22D                                                            
         MVC   ICSHD2,=C'RESPONSE'                                              
         FOUT  ICSHD2H                                                          
         B     DB22D                                                            
*                                                                               
DB22C2   DS    0H                                                               
         CLI   PSEUDOPT,C'M'                                                    
         BNE   *+14                                                             
         MVC   0(4,R2),=C'MCT,'                                                 
         LA    R2,4(R2)                                                         
*                                                                               
         CLI   ICSACT,C'L'         DONT ADJUST HEADER IF LIST                   
         BE    DB22D                                                            
         CLC   =C'COST',ICSHD2                                                  
         BE    DB22D                                                            
         MVC   ICSHD2,=C'COST    '                                              
         FOUT  ICSHD2H                                                          
*                                                                               
DB22D    DS    0H                                                               
         BCTR  R2,R0                                                            
         MVI   0(R2),C' '                                                       
         CLC   ICSOPTN,WORK+1        XMIT IF NECESSARY                          
         BE    DB22G                                                            
         MVC   ICSOPTN,WORK+1                                                   
         FOUT  ICSOPTNH                                                         
*                                                                               
DB22G    DS    0H                                                               
         LA    R2,ICSMEDH                                                       
         CLI   ERRAREA,0                                                        
         BNE   DB22Z                                                            
*                                                                               
         CLI   SPADSW,C'Y'         TEST DOING ADDS INTERFACE                    
         BNE   DB22H                                                            
         CLI   ICSACT,C'A'         ONLY FOR ACTION ADD                          
         BE    *+12                                                             
         CLI   ICSACT,C'C'         OR CHANGE                                    
         BNE   DB22H                                                            
         L     RF,VCOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         MVC   DMCB+4(4),=X'D9000A5E'                                           
         GOTO1 (RF),DMCB,0                                                      
         MVC   FULL,0(R1)          A(SPADINT)                                   
*                                                                               
         LA    R4,IOAREA           USING IOAREA FOR SPADINTD                    
         USING SPADINTD,R4                                                      
         LR    RE,R4                                                            
         LA    RF,SPADINTL                                                      
         XCEFL                                                                  
         MVI   ADACTN,ADAUNMAT     UNMATCH                                      
         MVC   ADACOMFC,VCOMFACS                                                
         MVC   ADQAGYMD,BMED       AGYALPHA/MEDIA                               
         MVC   ADQAGY,AGYALPHA                                                  
         MVC   ADQCLT,BCLT                                                      
         MVI   ADQPRD,X'FF'                                                     
         MVI   ADQEST,0                                                         
         MVC   ADQMKT,BMS                                                       
         MVC   ADQSTA,BMS+2                                                     
         MVC   ADQYM,BMOS                                                       
*                                                                               
         GOTO1 FULL,DMCB,SPADINTD                                               
         CLI   ADERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
DB22H    DS    0H                                                               
         MVC   LASTACT,ICSACT      SAVE LAST SUCCESSFUL ACTION                  
         L     RF,SYSPARMS         IF PFKEY NUMBER 3 PRESSED                    
         L     RF,0(RF)                                                         
         USING TIOBD,RF                                                         
         CLI   TIOBAID,3                                                        
         BE    DB22I                                                            
         CLI   TIOBAID,3+12                                                     
         BNE   EXIT                                                             
*                                                                               
DB22I    XC    ICSSRV,ICSSRV       THEN AUTOSWAP TO $MATCH                      
         MVC   ICSSRV(4),=C'+MAT'                                               
         B     EXIT                                                             
*                                                                               
DB22Z    MVI   ICSRCOD,C'1'        SET ERROR RETURN                             
         B     EXXMOD              IF THERE WAS AN ERROR                        
*                                  BYPASS SETTING OF CURSOR                     
         SPACE 3                                                                
ERRXT    DS    0H                                                               
         MVI   ICSRCOD,C'1'        SET ERROR RETURN CODE                        
         B     ERRORX                                                           
*                                                                               
ERRORX   DS    0H                  INSTEAD OF GENEROLS ERROR                    
         L     R4,ERRAREA          TO SET SYSTEM CODE                           
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVI   DMCB+20,2           SPOT SYSTEM CODE                             
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(X'FF',DMCB)                        
         B     EXIT                                                             
*                                                                               
       ++INCLUDE GENEROL                                                        
*                                                                               
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
         SPACE 3                                                                
*                                                                               
HIGHTRF  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     TRFDIR                                                           
         SPACE 2                                                                
TRFDIR   NTR                                                                    
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'TRFDIR',KEY,KEY,((R3),0)         
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETTRF   MVC   COMMAND,=C'GETREC'                                               
         B     TRFFILE                                                          
         SPACE 2                                                                
TRFFILE  NTR                                                                    
         LA    R2,KEY+14                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R3,TERMNAL                                                       
         IC    R4,DMINBTS                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'TRFFILE',               X        
               (R2),IOAREA,((R3),DMWORK)                                        
         B     DMCHECK                                                          
         EJECT                                                                  
*                   GET PRODUCT CODE                                            
         SPACE 1                                                                
GETPCOD  EQU   *                                                                
         MVI   0(R5),0                                                          
         CLC   0(3,R4),=C'POL'                                                  
         BCR   8,RE                                                             
GETPCOD1 DS    0H                                                               
         MVI   0(R5),0                                                          
         L     R6,APRDLST                                                       
GPC2     CLC   0(3,R6),0(R4)                                                    
         BE    GPC3                                                             
         CLI   0(R6),0                                                          
         BCR   8,RE                                                             
         LA    R6,4(R6)                                                         
         B     GPC2                                                             
GPC3     MVC   0(1,R5),3(R6)                                                    
GPCEXT   BR    RE                                                               
         SPACE 3                                                                
*                   CLEAR IOAREA                                                
         SPACE 1                                                                
CLRIO    LA    R4,IOAREA                                                        
         LA    R5,8                                                             
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         BCT   R5,*-10                                                          
         BR    RE                                                               
         SPACE 3                                                                
         EJECT                                                                  
*                                  READ ALL INVOICE RECORDS                     
*                                  AND BUILD BUFFER                             
         SPACE 2                                                                
GETALL   NTR1                                                                   
         SPACE 1                                                                
         L     RB,PASSRB                                                        
         L     R8,PASSR8                                                        
         MVI   ERRAREA,0                                                        
         XC    SVACTEL,SVACTEL                                                  
         XC    FLMTAB,FLMTAB                                                    
         MVI   SVINVHI,0                                                        
         MVI   HAVINV,0                                                         
         CLI   ICSACT,C'A'         FOR ADDS                                     
         BNE   *+8                                                              
         MVI   SVINVID,0           CLEAR SAVED ID                               
*                                                                               
         MVC   BUFFER(2),=H'2'     INITIAL LNGTH                                
         MVI   DMOUTBTS,0                                                       
         MVC   KEY(13),INVKEY                                                   
         ZIC   R4,RSET                                                          
         BCTR  R4,R0                                                            
         MH    R4,=H'10'            10 RECS PER SET                             
*                                                                               
         CLI   NEWSW,C'Y'                                                       
         BNE   GA1D                                                             
         LA    RF,KEY+INVKSQ-INVKEY    **NEW SEQ POS**                          
         MVC   KEY+INVKINV-INVKEY(1),SVINVID   INTERNAL INV ID                  
         B     GA1F                                                             
GA1D     DS    0H                                                               
         LA    RF,KEY+INVKSEQ-INVKEY   **OR OLD**                               
GA1F     DS    0H                                                               
         STC   R4,0(RF)                                                         
*                                                                               
         LA    R3,10(R4)           HIGH LIMIT FOR SET                           
         LR    R6,R4               SAVE START                                   
*                                                                               
GA1H     DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     *+8                                                              
GA2      BAS   RE,SEQ                                                           
         BAS   RE,GADMCHK                                                       
         CLC   KEY(09),KEYSAVE                                                  
         BNE   GA4                                                              
*                                                                               
         MVC   SVINVHI,KEY+INVKINV-INVKEY    SAVE HIGHEST USED ID               
         SR    R2,R2                                                            
         IC    R2,KEY+INVKSEQ-INVKEY   **OLD SEQ POS**                          
         CLI   NEWSW,C'Y'                                                       
         BNE   GA2M                                                             
         IC    R2,KEY+INVKSQ-INVKEY    **NEW SEQ POS**                          
*                                                                               
         CLI   RSET,1                 IF D2, D3, ETC                            
         BNH   GA2D                                                             
         CLI   KEY+INVKINV-INVKEY,0   AND HAVE FOUND INV WITH                   
         BNE   GA4                    INVNO, THEN SKIP IT AND QUIT              
*                                                                               
GA2D     DS    0H                                                               
         OC    INVNUM,INVNUM     TEST LOOKING FOR SPECIFIC INVOICE              
         BNZ   GA2F                YES                                          
         CLI   ICSACT,C'A'         NO- IF ADD                                   
         BE    GA2H                DO ONLY INVOICE 0                            
         CLI   HAVINV,0            ELSE IF HAVE FOUND SOME INVOICE              
         BNE   GA2H                SKIP ANY OTHERS                              
         B     GA2M                                                             
*                                                                               
GA2F     DS    0H                                                               
         CLI   KEY+INVKINV-INVKEY,0    SKIP 0 INVOICE                           
         BE    GA3H                                                             
         CLI   SVINVID,0           AND IF HAVE FOUND INVOICE                    
         BE    GA2M                                                             
*                                                                               
GA2H     DS    0H                                                               
         CLC   SVINVID,KEY+INVKINV-INVKEY   SKIP ANY OTHERS                     
         BNE   GA4                                                              
*                                                                               
GA2M     DS    0H                                                               
         CR    R2,R3                                                            
         BNL   GA4                 BYPASS HIGHER 'SET'                          
         CR    R4,R2               CHECK SEQ NO                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,1(R4)                                                         
         SR    R2,R6               GET REC NO WITHIN SET                        
         SLL   R2,2                X 4                                          
         LA    R2,RECWRK(R2)                                                    
         MVC   0(4,R2),KEY+14                                                   
         BAS   RE,GETREC                                                        
         BAS   RE,GADMCHK                                                       
*                                                                               
         CLI   IOAREA+10,0         FOR 1ST REC (SEQ=ZERO)                       
         BNE   *+10                                                             
         XC    SVIHDEL,SVIHDEL     CLEAR SAVED INV HDR ELEM                     
*                                                                               
         LA    R9,IOAREA+24        LOOP THRU ELEMS BEFORE BUFFIN                
*                                                                               
GA3      DS    0H                                                               
         CLI   0(R9),0             EOR                                          
         BE    GA3T                                                             
         CLI   0(R9),X'B0'         STATUS ELEM                                  
         BE    GA3D                                                             
         CLI   0(R9),X'05'         INVOICE HEADER ELEM                          
         BE    GA3F                                                             
*                                                                               
GA3C     DS    0H                                                               
         ZIC   R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     GA3                                                              
*                                                                               
GA3D     DS    0H                  STATUS ELEM                                  
         USING ICTLELEM,R9                                                      
         CLI   ICSACT,C'C'         SKIP FOR CHANGE                              
         BE    GA3C                                                             
         MVI   NETSW,C'N'                                                       
         TM    ICTLCTL,ICTLNET     TEST NET INV                                 
         BZ    *+8                                                              
         MVI   NETSW,C'Y'                                                       
*                                                                               
         MVI   PSEUDOPT,C'R'                                                    
         TM    ICTLCTL,ICTLRES     TEST RESPONSE                                
         BO    GA3C                                                             
         MVI   PSEUDOPT,C'M'                                                    
         TM    ICTLCTL,ICTLMCT     TEST MCT                                     
         BO    GA3C                                                             
         MVI   PSEUDOPT,C'N'                                                    
         B     GA3C                                                             
         DROP  R9                                                               
*                                                                               
GA3F     DS    0H                                                               
         CLI   0(R9),X'05'         INVOICE HEADER ELEM                          
         BNE   GA3C                                                             
         CLI   NEWSW,C'Y'                                                       
         BNE   GA3C                                                             
         USING IHDELEM,R9                                                       
         OC    INVNUM,INVNUM       IF NO INVOICE NUMBER GIVEN                   
         BNZ   GA3F6               TAKE FIRST ONE WE FIND                       
         CLI   ICSACT,C'C'         BUT IF CHANGE                                
         BNE   GA3F4                                                            
         MVI   ERRAREA,INVNERR     CANNOT REMOVE INVOICE NUMBER                 
         LA    R2,ICSINVH                                                       
         B     GAERR                                                            
*                                                                               
GA3F4    DS    0H                                                               
         XC    ICSINV,ICSINV                                                    
         MVC   ICSINV(L'IHDINV),IHDINV       SET ON SCREEN                      
         FOUT  ICSINVH                                                          
         OI    ICSINVH+4,X'20'     SET VALIDATED                                
         MVC   INVNUM,IHDINV                                                    
         B     GA3F8                                                            
*                                                                               
GA3F6    DS    0H                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'IHDINV-1),WORK                                          
         OC    IHDINV,WORK         OR WITH SPACES                               
         MVC   X(L'INVNUM),INVNUM                                               
         OC    X(L'INVNUM),WORK                                                 
         CLI   ICSACT,C'C'         IF CHANGE                                    
         BNE   *+14                                                             
         MVC   IHDINV,X            SET NEW INVOICE NUMBER                       
         B     GA3F8                                                            
         CLC   IHDINV,X            ELSE- TEST RIGHT INVOICE                     
         BNE   GA3G                NO - SKIP                                    
*                                                                               
GA3F8    DS    0H                                                               
         MVI   EASISW,C'N'         SET NOT AN EASI INVOICE                      
         CLI   IHDRDT,0            UNLESS HAS RECEIVED DATE                     
         BE    GA3F10                                                           
         MVI   EASISW,C'Y'                                                      
*                                                                               
         CLI   ICSACT,C'A'         CANNOT ADD TO AN EASI INV                    
         BNE   GA3F10                                                           
         LA    R2,ICSACTH                                                       
         MVI   ERRAREA,EASIERR                                                  
         B     GAEXT                                                            
*                                                                               
GA3F10   DS    0H                                                               
         MVC   SVIHDEL,0(R9)                                                    
         MVC   SVINVID,KEY+INVKINV-INVKEY   SAVE INVOICE KEY ID                 
         B     GA3C                                                             
*                                  NOT RIGHT INVOICE - SKIP                     
GA3G     DS    0H                  NOTE- ONLY FOR NEW TYPE INVOICES             
         MVC   KEY(9),INVKEY                                                    
GA3H     DS    0H                                                               
         LA    R2,KEY+INVKINV-INVKEY                                            
         CLI   0(R2),X'FF'         BUMP TO NEXT INVOICE                         
         BE    GA4                 UNLESS AT HIGHEST                            
         ZIC   RF,0(R2)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R2)                                                         
         MVI   1(R2),0                                                          
*                                                                               
         ZIC   R4,RSET                                                          
         BCTR  R4,R0                                                            
         MH    R4,=H'10'            10 RECS PER SET                             
         B     GA1H                                                             
*                                                                               
GA3T     DS    0H                                                               
         OC    INVNUM,INVNUM       IF HAVE INVOICE FILTER                       
         BZ    GA3V                                                             
         CLI   SVIHDEL,0           AND HAD NO HEADER ELEM                       
         BE    GA3G                SKIP INVOICE                                 
*                                                                               
GA3V     DS    0H                                                               
         MVI   HAVINV,C'Y'         SET HAVE SOME INVOICE                        
         LA    RF,BUFFIN           USE NORMAL INPUT                             
         CLI   SEQSW,C'Y'                                                       
         BNE   *+8                                                              
         LA    RF,SEQIN                                                         
         BASR  RE,RF                                                            
         CLI   ERRAREA,0                                                        
         BE    GA2                                                              
         LA    R2,ICSACTH                                                       
         B     GAEXT                                                            
*                                                                               
GA4      DS    0H                  DONE WITH READS                              
         CLI   ICSACT,C'A'         IF ADDING                                    
         BNE   GA5                                                              
         CLI   HAVINV,0            AND NOT ALREADY THERE                        
         BNE   GA5                                                              
*                                                                               
         L     R1,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,X'03'       CONNECTED TO THE NET SYSTEM?                 
         BE    GA5                 YES, ALLOW ADD FOR NOW                       
         DROP  R1                                                               
*                                                                               
         LA    R2,ICSACTH          'TRUE ADDS' NO LONGER ALLOWED                
         MVI   ERRAREA,ACTERR                                                   
         B     GAERR                                                            
*                                                                               
         OC    INVNUM,INVNUM       IF HAVE INVOICE NUMBER                       
         BZ    GA5                                                              
*                                  ADD INV HEADER ELEM                          
         XC    RECWRK,RECWRK                                                    
         XC    WORK,WORK                                                        
         LA    R9,WORK                                                          
         USING IHDELEM,R9                                                       
         MVI   IHDELEM,X'05'                                                    
         MVI   IHDELEM+1,40                                                     
         CLI   SVINVID,X'FF'       TEST HAVE RUN OUT OF IDS                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,SVINVHI          BUMP INTERNAL ID NUM                         
         LA    RF,1(RF)                                                         
         STC   RF,SVINVID                                                       
         MVC   IHDID,SVINVID                                                    
         MVC   IHDINV,INVNUM                                                    
         GOTO1 VRECUP,DMCB,(X'FF',BUFFER),IHDELEM,BUFFER+2                      
         DROP  R9                                                               
*                                                                               
GA5      DS    0H                  TOTAL SPOTS AND DOLLARS                      
         LA    R9,BUFFER+2                                                      
         USING INVELEM,R9                                                       
         XC    TSPOTS(8),TSPOTS                                                 
         MVI   BYTE,0                                                           
GA6      DS    0H                                                               
         CLI   0(R9),0             END                                          
         BE    GA22                                                             
         CLI   0(R9),X'B1'         INV ELEM                                     
         BE    GA8                                                              
         CLI   0(R9),X'02'         ID ELEM                                      
         BNE   GA7                                                              
         CLI   IDNAME,C' '                                                      
         BNH   GA20                                                             
         CLC   BYTE,14(R9)         KEEP HIGHEST USED ID NUMBER                  
         BNL   *+10                                                             
         MVC   BYTE,14(R9)                                                      
         CLC   2(12,R9),IDNAME                                                  
         BNE   GA20                                                             
         MVC   IDNUM,14(R9)        SET IDNUM                                    
         MVC   SAVPRD2,IDNUM       FOR ID FILTER                                
         B     GA20                                                             
*                                                                               
GA7      DS    0H                                                               
         CLI   0(R9),X'04'         FILM TRANS ELEM                              
         BNE   GA10                                                             
         CLC   SAVFC1,3(R9)        TRANSLATE FILTERS                            
         BNE   *+10                                                             
         MVC   SAVFLM1,2(R9)                                                    
         CLC   SAVFC2,3(R9)                                                     
         BNE   *+10                                                             
         MVC   SAVFLM2,2(R9)                                                    
         B     GA20                                                             
*                                                                               
GA8      DS    0H                  INVOICE ITEM ELEM                            
*                                                                               
         CLI   SAVLEN,X'FF'                                                     
         BE    *+14                                                             
         CLC   INVLEN,SAVLEN                                                    
         BNE   GA20                                                             
         CLI   SAVPRD,X'FF'                                                     
         BE    *+14                                                             
         CLC   INVPRD,SAVPRD                                                    
         BNE   GA20                                                             
         CLC   =C'NONE',IDNAME                                                  
         BNE   *+12                                                             
         TM    INVSTAT,X'08'                                                    
         BNZ   GA20                                                             
         CLI   SAVPRD2,X'FF'                                                    
         BE    *+14                                                             
         CLC   INVPRD2,SAVPRD2                                                  
         BNE   GA20                                                             
         CLC   SAVCOST,=4X'FF'     COST FILTER                                  
         BE    GA8C                                                             
         MVC   FULL(1),INVCOSTX    COST EXTENSION (1ST BYTE)                    
         MVC   FULL+1(3),INVCOST                                                
         CLC   FULL,SAVCOST                                                     
         BE    GA8C                                                             
         CLC   SAVCOST,ZVAL4       TEST SPECIAL CODE FOR ZERO                   
         BNE   GA20                                                             
         OC    FULL,FULL                                                        
         BNZ   GA20                                                             
*                                                                               
GA8C     DS    0H                                                               
         CLC   SAVFLM1(2),=3X'FF'  FILMS                                        
         BE    GA9                                                              
         XC    WORK(2),WORK                                                     
         CLI   INVELEM+1,13                                                     
         BE    GA8D                NO FILMS                                     
         MVC   WORK(2),INVFILM                                                  
*                                                                               
GA8D     DS    0H                                                               
         CLC   WORK(2),SAVFLM1                                                  
         BNE   GA20                                                             
*                                                                               
GA9      DS    0H                                                               
         MVC   FULL(1),INVCOSTX                                                 
         MVC   FULL+1(3),INVCOST                                                
         L     R4,FULL                                                          
         TM    INVSTAT,X'01'       TEST NEG                                     
         BZ    *+6                                                              
         LCR   R4,R4                                                            
         A     R4,TDOLLS                                                        
         ST    R4,TDOLLS                                                        
         L     R4,TSPOTS                                                        
         LA    R4,1(R4)                                                         
         ST    R4,TSPOTS                                                        
*                                                                               
GA10     DS    0H                                                               
         CLI   0(R9),X'F1'         ACTIVITY ELEM                                
         BNE   GA12                                                             
         MVC   SVACTEL,0(R9)       SAVE IT                                      
         B     GA20                                                             
*                                                                               
GA12     DS    0H                                                               
GA20     DS    0H                                                               
         ZIC   R0,1(R9)            NEXT ELEM                                    
         AR    R9,R0                                                            
         B     GA6                                                              
GA22     DS    0H                                                               
         CLI   IDNUM,0             TEST ID ELEM FOUND                           
         BNE   GA25                YES                                          
         CLI   IDNAME,0            TEST NEED ONE                                
         BE    GA25                NO                                           
         CLI   IDNAME,C'?'         QUESTION MARK                                
         BE    GA25                                                             
         CLC   =C'NONE',IDNAME                                                  
         BE    GA25                                                             
*        ADD ID ELEM TO BUFFER                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(2),=X'020F'                                                 
         MVC   WORK+2(12),IDNAME                                                
         ZIC   R3,BYTE             LAST ID NUMBER                               
         LA    R3,1(R3)                                                         
         STC   R3,IDNUM            RELATIVE ID NUMBER                           
         STC   R3,WORK+14                                                       
         GOTO1 VRECUP,DMCB,(X'FF',BUFFER),WORK,BUFFER+2                         
*                                                                               
GA25     DS    0H                                                               
GAEXT    DS    0H                                                               
         XIT1                                                                   
GADMCHK  DS    0H                                                               
         TM    DMCB+8,X'FF'                                                     
         BZR   RE                                                               
         MVI   ERRAREA,X'FF'                                                    
GAERR    DS    0H                                                               
         ST    R2,FULL                                                          
         B     GAEXT                                                            
         EJECT                                                                  
*                                  ADD TO BUFFER                                
         SPACE 2                                                                
BUFFIN   NTR1                                                                   
         SPACE 1                                                                
         LA    R3,IOAREA+27        FIRST ELEM AFTER B0                          
         LR    R0,R3                                                            
*                                                                               
BUFFIN1  DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    BUFFIN1D                                                         
         CLI   1(R3),0                                                          
         BE    BUFFIN1D                                                         
*                                                                               
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     BUFFIN1                                                          
*                                                                               
BUFFIN1D DS    0H                                                               
         SR    R3,R0               R3 HAS LENGTH TO MOVE                        
*                                                                               
         LH    R2,BUFFER                                                        
         LA    R4,BUFFER(R2)                                                    
         AR    R2,R3                                                            
         STH   R2,BUFFER                                                        
         CH    R2,BUFFL                                                         
         BNH   *+12                                                             
         MVI   ERRAREA,OVFERR                                                   
         B     BUFFINX                                                          
         LA    R6,0(R4,R3)                                                      
         MVI   0(R6),0                                                          
         LA    R5,IOAREA+27        BYPASS B0 ELEM WHICH WILL BE FIRST           
BUFFIN2  EQU   *                                                                
         CH    R3,=H'256'                                                       
         BL    BUFFIN4                                                          
         MVC   0(256,R4),0(R5)                                                  
         LA    R4,256(R4)                                                       
         LA    R5,256(R5)                                                       
         SH    R3,=H'256'                                                       
         B     BUFFIN2                                                          
BUFFIN4  EQU   *                                                                
         LTR   R3,R3                                                            
         BZ    BUFFIN6                                                          
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R5)                                                    
BUFFIN6  EQU   *                                                                
BUFFINX  EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
SEQIN    NTR1                                                                   
         LA    R6,IOAREA+27                                                     
*                                                                               
SEQIN4   DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    SEQINX                                                           
         CLI   0(R6),04            SKIP FILM ELEMS                              
         BE    SEQIN6                                                           
         CLI   0(R6),X'F0'         AND SPECIALS                                 
         BNL   SEQIN6                                                           
         MVC   INVELEM,0(R6)                                                    
         CLI   0(R6),X'B1'         FOR INV ELEMS                                
         BNE   *+10                                                             
         XC    INVELEM+13(2),INVELEM+13                                         
         BAS   RE,ADDEL                                                         
*                                                                               
SEQIN6   DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SEQIN4                                                           
*                                                                               
SEQINX   DS    0H                                                               
         XIT1                                                                   
*                   ELEMENT INSERTION                                           
ADDEL    NTR                                                                    
         SPACE 2                                                                
         SR    R0,R0                                                            
         LA    R4,BUFFER                                                        
         LA    R5,BUFFER+2                                                      
AE2      CLI   0(R5),0                                                          
         BE    AE4                                                              
         CLI   0(R5),X'F0'         SKIP SPECIAL ELEMS                           
         BNL   AE3                                                              
         CLC   INVELEM(1),0(R5)                                                 
         BL    AE4                                                              
         BH    AE3                                                              
         CLC   INVELEM+2(11),2(R5)                                              
         BNH   AE4                                                              
AE3      IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     AE2                                                              
AE4      EQU   *                                                                
         GOTO1 VRECUP,DMCB,(X'FF',(R4)),(R9),(R5)                               
         SPACE 1                                                                
         CLC   BUFFER(2),BUFFL                                                  
         BH    AEOVF                                                            
AEEXT    XIT                                                                    
         SPACE 2                                                                
AEOVF    MVI   ERRAREA,OVFERR                                                   
         B     AEEXT                                                            
         SPACE 3                                                                
ZVAL4    DC    X'80000000'         4 BYTE SPECIAL CODE FOR ZERO COST            
SPACES   DC    CL20' '                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                   EDIT DETAIL LINES                                           
         SPACE 3                                                                
         DS    0H                                                               
EDIT     NMOD1 0,EDIT                                                           
         SPACE 1                                                                
         L     RC,0(R1)            RESTORE WORK AREA REG                        
         L     R8,PASSR8           RESTORE COMSUBS ADDRESS                      
         LA    R9,ELWORK                                                        
         USING INVELEM,R9                                                       
         XC    INVELEM,INVELEM                                                  
         LA    R0,ICSREQH          TEST EDITING REQUEST FLD                     
         CR    R2,R0                                                            
         BE    ED42                YES - EDIT PRODUCT                           
*                                                                               
         LA    R2,ICSDATH(R7)                                                   
         CLI   5(R2),0                                                          
         BNE   ED0                                                              
         CLI   ICSACT,C'L'                                                      
         BE    ED0                                                              
         CLI   ICSACT,C'A'                                                      
         BE    ED0                                                              
         LTR   R7,R7                                                            
         BNE   ED0                                                              
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPPER                            
*                                  TEST IF ANY DATA ON THIS LINE                
         USING ICSDATH,R2                                                       
ED0      CLI   ICSDATH+5,0                                                      
         BNE   ED1                                                              
         CLI   ICSTIMH+5,0                                                      
         BNE   ED1                                                              
         CLI   ICSLENH+5,0                                                      
         BNE   ED1                                                              
         CLI   ICSPRDH+5,0                                                      
         BNE   ED1                                                              
         CLI   ICSCOSTH+5,0                                                     
         BNE   ED1                                                              
         CLI   ICSFILMH+5,0                                                     
         BE    EDEXT                                                            
         DROP  R2                                                               
*                                  DATE  ROUTINE                                
ED1      CLI   5(R2),0             TEST ANY DATE PRESENT                        
         BNE   ED1A                                                             
         CLI   ICSACT,C'L'         NO- OK IF LIST                               
         BE    ED20                                                             
         MVI   ERRAREA,MISSERR                                                  
         OC    INVDAT,SAVDAT                                                    
         BZ    EDERR                                                            
         B     ED20                                                             
ED1A     DS    0H                                                               
         MVI   ERRAREA,DATERR                                                   
         CLI   5(R2),2                                                          
         BH    ED2                                                              
         BAS   RE,PACK             ASSUME INPUT IS DAY ONLY                     
         UNPK  SAVDA,DUB                                                        
         B     ED6                                                              
ED2      CLC   8(3,R2),=C'DEL'     DELETE                                       
         BNE   ED3                                                              
         MVI   BYTE2,1             SET DATA ENTERED                             
         CLI   ICSACT,C'C'         OK ONLY IF CHANGE                            
         BE    ED71                                                             
         B     EDERR                                                            
ED3      L     RF,VDATVAL                                                       
         LA    R1,DMCB                                                          
         LA    R4,8(R2)                                                         
         LA    R5,DUB                                                           
         STM   R4,R5,DMCB                                                       
         CLI   ICSACT,C'T'         FOR TOTAL                                    
         BE    ED3A                                                             
         CLI   ICSACT,C'L'         AND LIST, MMMYY OK                           
         BE    ED3A                                                             
         GOTO1 (RF)                DATVAL - M/D/Y                               
         CLI   DMCB+3,0                                                         
         BNE   ED4                                                              
         ST    R4,DMCB                                                          
         MVI   DMCB,1                                                           
         GOTO1 (RF)                DATVAL- M/D                                  
         CLI   DMCB+3,0                                                         
         BNE   ED5                                                              
         CLI   ICSACT,C'D'                                                      
         BE    *+12                                                             
         CLI   ICSACT,C'R'                                                      
         BNE    EDERR                                                           
         ST    R4,DMCB                                                          
*                                  MMM/YY OK FOR TOTAL + DISPLAY                
*                                  + REQ + LIST                                 
ED3A     MVI   DMCB,2                                                           
         GOTO1 (RF)                DATVAL - MMM/YY                              
         CLI   DMCB+3,0                                                         
         BE    EDERR                                                            
         MVC   DUB+4(2),=C'01'                                                  
         MVI   BYTE3,1             SET MMM/YY FLAG                              
ED4      MVC   SAVYR,DUB                                                        
ED5      MVC   SAVMO(4),DUB+2                                                   
ED6      GOTO1 VGETDAY,DMCB,SAVDATE,FULL     GET DAY OF WEEK                    
         SR    R5,R5                                                            
         IC    R5,DMCB                                                          
         LTR   R5,R5                                                            
         BZ    EDERR                         INVALID DATE                       
         CLI   DMCB,1              TEST MONDAY                                  
         BE    ED6A                                                             
         CLI   BYTE3,1             TEST MMM/YY                                  
         BE    ED6A                                                             
         CLI   ICSACT,C'R'                                                      
         BE    EDERR               REQ DATE MUST BE MONDAY                      
ED6A     EQU   *                                                                
         SPACE 2                                                                
**NOP    GOTO1 VDTCNV,DMCB,SAVDATE,(2,SAVDAT)     GET 2 BYTE DATE               
         GOTO1 VDATCON,DMCB,SAVDATE,(2,SAVDAT)    GET 2 BYTE DATE               
         MVC   INVDAT,SAVDAT                                                    
         CLI   ICSACT,C'R'         REQ                                          
         BE    ED20                                                             
         CLI   ICSACT,C'C'                                                      
         BE    ED11                                                             
         CLI   BYTE2,0             TEST FIRST TIME                              
         BNE   ED11                                                             
*                             FIRST LINE DATE FOR ADD AND DISP                  
*                                             AND TOTAL                         
ED7      MVC   WORK(6),SAVDATE                                                  
         CLI   PROGPROF+9,C'C'     IF CALENDAR MONTH USE 15TH TO                
*                                  GET MONTH START/END                          
         BNE   ED700                                                            
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 VGETDAY,DMCB,WORK,FULL                                           
         ZIC   R5,DMCB                                                          
ED700    DS    0H                                                               
         L     RF,VADDAY                                                        
         BCT   R5,*+8                                                           
         B     ED7A                                                             
         LCR   R5,R5                                                            
         GOTO1 (RF),(R1),WORK,WORK,(R5)   GET MONDAY                            
         SPACE 1                                                                
ED7A     LA    R5,6                                                             
         GOTO1 (RF),(R1),WORK,WORK+6,(R5)    GET SUNDAY                         
         MVC   WORK+12(6),WORK+6                                                
         LA    R3,WORK+12                                                       
         LA    R4,WORK+12                                                       
         LA    R5,7                                                             
         STM   R3,R5,DMCB                                                       
ED7B     MVC   SUNDATE,WORK+12     END OF BRD MON                               
         GOTO1 (RF),(R1)                                                        
         SPACE 1                                                                
         CLC   WORK+8(2),WORK+14                                                
         BE    ED7B                                                             
         CLC   WORK+2(2),SUNDATE+2                                              
         BNE   ED7D                                                             
         CLC   WORK+4(2),=C'01'                                                 
         BE    ED7D                                                             
         MVC   WORK+6(6),WORK                                                   
         LA    R3,WORK                                                          
         LA    R4,WORK                                                          
         LA    R5,7                                                             
         LCR   R5,R5                                                            
         STM   R3,R5,DMCB                                                       
ED7C     GOTO1 (RF),(R1)                                                        
         SPACE 1                                                                
         CLC   WORK+2(2),WORK+8                                                 
         BNE   ED7D                                                             
         CLC   WORK+4(2),=C'01'                                                 
         BE    ED7D                                                             
         B     ED7C                                                             
ED7D     MVC   MONDATE,WORK        HAVE START OF BRD MON                        
**NOP    GOTO1 VDTCNV,(R1),MONDATE,(2,INVKDAT)    GET 2 BYTE DATE               
         GOTO1 VDATCON,(R1),MONDATE,(2,INVKDAT)   GET 2 BYTE DATE               
         CLI   PROGPROF+9,C'C'                                                  
         BNE   ED7E                                                             
*                                  FOR CALENDAR MONTHS RESET                    
*                                  'MONDATE' AND 'SUNDATE' TO                   
*                                  START/END                                    
*                                  NOTE- INV REC KEY DATE STILL                 
*                                  1ST MONDAY OF BRD MON                        
         MVC   MONDATE,SUNDATE                                                  
         MVC   MONDATE+4(2),=C'01'                                              
         PACK  DUB,MONDATE(2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,DUB                                                           
         MVI   MTHEND+3,C'8'                                                    
         TM    DUB,X'03'     TEST LEAPP YEAR                                    
         BNZ   *+8                                                              
         MVI   MTHEND+3,C'9'                                                    
         PACK  DUB,MONDATE+2(2)                                                 
         CVB   RF,DUB                                                           
         SLL   RF,1                X 2                                          
         LA    RF,MTHEND-2(RF)                                                  
         MVC   SUNDATE+4(2),0(RF)                                               
         B     ED7E                                                             
*                                                                               
MTHEND   DC    C'312831303130313130313031'                                      
*                                                                               
ED7E     DS    0H                                                               
         C     R7,=F'0'                                                         
         BNE   ED7F                                                             
         GOTO1 VDATCON,DMCB,(0,SUNDATE),(6,WORK)                                
         GOTO1 (RF),(R1),,(3,DUB)                                               
         MVC   BMOS,DUB            MONTH OF SERVICE (BINARY)                    
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,6,GLVSPPER                           
ED7F     MVI   FILMOPT,C'N'        NOTE- FILMOPT NO-OPED                        
         SPACE 2                                                                
ED7G     DS    0H                                                               
         CLI   ICSACT,C'A'                                                      
         BNE   ED10                                                             
         BAS   RE,GETALL                                                        
         CLI   ERRAREA,0                                                        
         BE    ED20                                                             
         L     R2,FULL             A(SCREEN FIELD IN ERROR)                     
         BNE   EDERR                                                            
*                                                                               
ED10     CLI   BYTE3,1             TEST MMM/YY ENTERED                          
         BNE   ED20                NO                                           
         MVC   SAVDAT,INVKDAT      YES - USE BRD MTH START                      
         B     ED20                                                             
*                                  TEST WITHIN MONTH                            
ED11     MVI   ERRAREA,DATERR2                                                  
         CLC   SAVDATE,MONDATE                                                  
         BL    EDERR                                                            
         CLC   SAVDATE,SUNDATE                                                  
         BH    EDERR                                                            
*                                                                               
ED20     DS    0H                                                               
         CLI   ICSACT,C'C'       CANNOT CHANGE DATE OF EASI INVOICE             
         BNE   ED20B                                                            
         CLI   EASISW,C'Y'                                                      
         B     ED20B               ***NO-OP***                                  
         MVI   ERRAREA,EASIERR                                                  
         OC    ELSAVE,ELSAVE       MUST HAVE SAVED ELEM                         
         BZ    EDERR                                                            
         L     RF,ELSAVE           ADDR OF SAVED ELEM                           
         CLC   INVDAT,INVDAT-INVELEM(RF)                                        
         BNE   EDERR                                                            
*                                  TIME ROUTINE                                 
ED20B    DS    0H                                                               
         CLI   ICSACT,C'L'         SKIP FOR LISTS                               
         BE    ED71                                                             
         CLC   =C'DEL',ICSACT       AND DELETES                                 
         BE    ED71                                                             
         MVI   DMOUTBTS,X'FD'      RESTORE DMOUTBTS                             
         CLI   ICSACT,C'T'                                                      
         BE    ED30                                                             
         CLI   ICSACT,C'R'         REQ                                          
         BE    ED40                                                             
         LA    R2,ICSTIMH(R7)                                                   
         CLI   5(R2),0                                                          
         BNE   ED21                                                             
         MVI   ERRAREA,MISSERR                                                  
         OC    INVTIM,SAVTIM                                                    
         BZ    EDERR                                                            
         B     ED30                                                             
ED21     MVI   ERRAREA,TIMERR                                                   
         LA    R4,8(R2)                                                         
         CLI   0(R4),C'N'          TEST ACCEPT FOR INTERVAL CHECK               
         BNE   ED21D                                                            
         LA    R4,1(R4)                                                         
         OI    INVSTAT,X'20'       SET STATUS BIT                               
         OI    SAVSTAT,X'20'                                                    
         B     ED21F                                                            
*                                                                               
ED21D    DS    0H                                                               
         CLI   0(R4),C'T'          TEST IGNORE TIME FOR MATCHING                
         BNE   ED21F                                                            
         LA    R4,1(R4)                                                         
         OI    INVSTAT,X'02'       SET STATUS BIT                               
         OI    SAVSTAT,X'02'                                                    
*                                                                               
ED21F    DS    0H                                                               
         CLI   0(R4),X'40'                                                      
         BNE   *+8                                                              
         MVI   0(R4),X'F0'                                                      
         GOTO1 VTIMPK,DMCB,0(R4),HALF                                           
         CLI   DMCB,0                                                           
         BE    EDERR                                                            
         LH    R0,HALF                                                          
         CH    R0,=H'600'                                                       
         BNL   *+12                                                             
         AH    R0,=H'2400'                                                      
         STH   R0,HALF                                                          
         MVC   INVTIM,HALF                                                      
         MVC   SAVTIM,HALF                                                      
*                                                                               
ED30     DS    0H                                                               
         CLI   ICSACT,C'C'       CANNOT CHANGE TIME OF EASI INVOICE             
         BNE   ED30B                                                            
         CLI   EASISW,C'Y'                                                      
         B     ED30B               ***NO-OP***                                  
         MVI   ERRAREA,EASIERR                                                  
         OC    ELSAVE,ELSAVE       MUST HAVE SAVED ELEM                         
         BZ    EDERR                                                            
         L     RF,ELSAVE           ADDR OF SAVED ELEM                           
         CLC   INVTIM,INVTIM-INVELEM(RF)                                        
         BNE   EDERR                                                            
*                                  LENGTH ROUTINE                               
ED30B    DS    0H                                                               
         LA    R2,ICSLENH(R7)                                                   
         CLI   5(R2),0                                                          
         BNE   ED31                                                             
         MVI   ERRAREA,MISSERR                                                  
         OC    INVLEN,SAVLEN                                                    
         BZ    EDERR                                                            
         B     ED40                                                             
ED31     MVI   ERRAREA,LENERR                                                   
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    EDERR                                                            
         CH    R0,=H'255'                                                       
         BH    EDERR                                                            
         STC   R0,INVLEN                                                        
         STC   R0,SAVLEN                                                        
*                                                                               
ED40     DS    0H                                                               
         CLI   ICSACT,C'C'       CANNOT CHANGE LEN OF EASI INVOICE              
         BNE   ED40B                                                            
         CLI   EASISW,C'Y'                                                      
         B     ED40B               ***NO-OP***                                  
         MVI   ERRAREA,EASIERR                                                  
         OC    ELSAVE,ELSAVE       MUST HAVE SAVED ELEM                         
         BZ    EDERR                                                            
         L     RF,ELSAVE           ADDR OF SAVED ELEM                           
         CLC   INVLEN,INVLEN-INVELEM(RF)                                        
         BNE   EDERR                                                            
*                                  PRODUCT ROUTINE                              
ED40B    DS    0H                                                               
         LA    R2,ICSPRDH(R7)                                                   
         CLI   5(R2),0             ANY INPUT                                    
         BNE   ED41                YES                                          
         LTR   R7,R7               IS IT THE FIRST LINE                         
         BNZ   *+12                                                             
         MVI   BLRPRD,0            YES, CLEAR BILLER PRD AND EST                
         MVI   BLREST,0                                                         
*                                                                               
         MVI   ERRAREA,MISSERR                                                  
         OC    INVPRD(2),SAVPRD                                                 
         BZ    EDERR                                                            
         TM    SAVSTAT,X'40'                                                    
         BZ    *+8                                                              
         OI    INVSTAT,X'40'                                                    
         TM    SAVSTAT,X'08'                                                    
         BZ    *+8                                                              
         OI    INVSTAT,X'08'                                                    
         B     ED50                                                             
ED41     MVI   ERRAREA,PRDERR                                                   
         BAS   RE,MOVE                                                          
         LA    R4,WORK                                                          
         LA    R5,7                                                             
ED41A    CLI   0(R4),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         BCT   R5,ED41A                                                         
         CLI   WORK+2,C'-'                                                      
         BNE   ED42                                                             
         MVC   DUB(5),WORK+2                                                    
         MVC   WORK+3(5),DUB                                                    
         MVI   WORK+2,X'40'                                                     
ED42     MVI   ERRAREA,PRDERR                                                   
         CLC   WORK(3),SAVPRDN                                                  
         BE    ED43                                                             
         LA    R4,WORK                                                          
         LA    R5,SAVPRD                                                        
         BAS   RE,GETPCOD1                                                      
         CLI   SAVPRD,0                                                         
         BE    EDERR                                                            
ED42C    MVC   SAVPRDN,WORK                                                     
ED43     MVC   INVPRD,SAVPRD                                                    
         MVI   ERRAREA,PRDERR2                                                  
*                                                                               
         CLI   WORK+3,X'40'                                                     
         BH    ED44                                                             
         CLC   WORK+4(4),WORK+3                                                 
         BNE   EDERR                                                            
         CLI   ICSACT,C'D'                                                      
         BE    ED43B                                                            
         CLI   ICSACT,C'T'                                                      
         BE    ED43B                                                            
         MVI   SAVPRD2,0                                                        
ED43B    DS    0H                                                               
         XC    SAVPRD2N,SAVPRD2N                                                
         NI    SAVSTAT,X'BF'       SET OFF EST IND BIT                          
         B     ED45                                                             
ED44     CLI   WORK+3,C'-'                                                      
         BNE   EDERR                                                            
         CLI   WORK+7,X'40'                                                     
         BNE   EDERR                                                            
         CLC   WORK+4(3),SAVPRD2N                                               
         BE    ED45                                                             
         CLI   ICSACT,C'R'         REQ                                          
         BNE   ED44B                                                            
*                                  2ND PRD PIG OPTIONS                          
         CLC   WORK+4(3),=C'YES'                                                
         BE    ED44C                                                            
         CLC   WORK+4(3),=C'ALL'                                                
         BE    ED44C                                                            
         CLC   WORK+4(3),=C'NO '                                                
         BE    ED44C                                                            
ED44B    DS    0H                                                               
         CLI   WORK+4,C'0'         IF PRD2 NUMERIC TREAT AS EST NO              
         BL    ED44B8                                                           
         SR    RF,RF                                                            
         CLI   WORK+5,C'0'                                                      
         BL    ED44B2                                                           
         LA    RF,1(RF)                                                         
         CLI   WORK+6,C'0'                                                      
         BL    *+8                                                              
         LA    RF,1(RF)                                                         
ED44B2   DS    0H                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    WORK+4(0),=3C'0'                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+4(0)                                                    
         UNPK  WORK+4(3),DUB                                                    
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ED44B6                                                           
         STC   R0,SAVPRD2                                                       
         OI    SAVSTAT,X'40'                                                    
         OI    INVSTAT,X'40'                                                    
         CLC   WORK(3),SAVPRDN                                                  
         BNE   ED44B4                                                           
         CLC   WORK+4(3),SAVPRD2N                                               
         BE    ED44C                                                            
ED44B4   DS    0H                  READ FOR ESTIMATE                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BMED                                                    
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),SAVPRDN                                                 
         MVC   KEY+7(1),SAVPRD2                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    ED44C                                                            
ED44B6   DS    0H                                                               
         MVI   ERRAREA,ESTERR                                                   
         B     EDERR                                                            
ED44B8   DS    0H                                                               
         NI    SAVSTAT,X'BF'       SET OF EST IND BIT                           
         CLI   FILMOPT,C'Y'        IF FILM OPTION                               
         BE    EDERR               THEN NO 2ND PRD                              
         LA    R4,WORK+4                                                        
         LA    R5,SAVPRD2                                                       
         BAS   RE,GETPCOD                                                       
         CLI   SAVPRD2,0                                                        
         BE    EDERR                                                            
ED44C    MVC   SAVPRD2N,WORK+4                                                  
         B     ED45                                                             
*                                  FILM TYPE CHECK - NO-OPED                    
ED45     MVC   INVPRD2,SAVPRD2                                                  
         TM    SAVSTAT,X'40'                                                    
         BZ    *+8                                                              
         OI    INVSTAT,X'40'                                                    
*                                                                               
         NI    SAVSTAT,X'F7'       SET OFF ID IND                               
         CLI   IDNUM,0             IF HAVE ID NUMBER                            
         BE    ED49D                                                            
         CLI   INVPRD2,0           THEN CANNOT BE PIGGY OR BY EST               
         BE    *+12                                                             
         MVI   ERRAREA,IDERR2                                                   
         B     EDERR                                                            
*                                                                               
ED49     OI    INVSTAT,X'08'                                                    
         OI    SAVSTAT,X'08'                                                    
         MVC   INVPRD2,IDNUM                                                    
         MVC   SAVPRD2,IDNUM                                                    
*                                                                               
ED49D    DS    0H                                                               
         CLI   I2XPROF+14,C'Y'    TEST ESTIMATE FORCED                          
         BNE   ED49F                                                            
         CLI   SAVPRD2,0           FOR NOW ACCEPT EST, PIG, OR ID               
         BNE   ED49F                                                            
         MVI   ERRAREA,ESTERR                                                   
         B     EDERR                                                            
*                                                                               
ED49F    DS    0H                                                               
         LTR   R7,R7               TEST DOING FIRST LINE                        
         BNZ   ED50                                                             
*                                                                               
         MVC   BLRPRD,SAVPRD       YES, SET BILLER PRD/EST                      
         MVI   BLREST,0                                                         
         TM    SAVSTAT,X'40'                                                    
         BZ    *+10                                                             
         MVC   BLREST,SAVPRD2                                                   
*                                                                               
         LA    R0,3                                                             
         CLI   SAVPRDN+2,X'40'                                                  
         BH    *+6                                                              
         BCTR  R0,0                                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',SAVPRDN,(R0),GLVSPPRD                     
*                                                                               
*                             COST ROUTINE                                      
ED50     DS    0H                                                               
         CLI   ICSACT,C'L'         FOR LIST                                     
         BE    ED71                SKIP COST AND FILM                           
         LA    R0,ICSREQH          DONE IF EDITING PRD FROM                     
         CR    R2,R0               REQUEST FIELD                                
         BE    ED71                                                             
*                                                                               
         LA    R2,ICSCOSTH(R7)                                                  
         MVI   ERRAREA,MISSERR                                                  
         CLI   5(R2),0                                                          
         BNE   ED51                                                             
         OC    SAVCOST,SAVCOST     ANY COST TO CARRY DOWN?                      
         BZ    EDERR                                                            
         MVC   FULL,SAVCOST                                                     
         CLC   SAVCOST,ZVAL4       BUT IF SPECIAL ZERO CODE                     
         BNE   *+10                                                             
         XC    FULL,FULL           SET TO ZERO                                  
         MVC   INVCOSTX,FULL       SET COST EXTENSION (1ST BYTE)                
         MVC   INVCOST,FULL+1      SET SAVED COST                               
*                                                                               
         TM    SAVSTAT,X'80'                                                    
         BZ    *+8                                                              
         OI    INVSTAT,X'80'                                                    
         TM    SAVSTAT,X'01'                                                    
         BZ    *+8                                                              
         OI    INVSTAT,X'01'       NEG IND                                      
         B     ED60                                                             
ED51     MVI   ERRAREA,COSTERR                                                  
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         LA    R3,8(R2)                                                         
         CLI   8(R2),C'N'                                                       
         BNE   ED51B                                                            
         LA    R3,1(R3)                                                         
         OI    INVSTAT,X'80'       SET TO BYPASS MATCH ON COST                  
         OI    SAVSTAT,X'80'                                                    
         BCTR  R0,R0                                                            
ED51B    DS    0H                                                               
         CLI   8(R2),C'M'                                                       
         BNE   ED51C                                                            
         OI    INVSTAT,X'04'       MG IND                                       
         OI    SAVSTAT,X'04'                                                    
         LA    R3,1(R3)                                                         
         BCTR  R0,R0                                                            
ED51C    DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+1,0(R3)                                                     
         LA    RF,WORK                                                          
         AR    RF,R0               POINT TO LAST CHAR                           
         CLI   0(RF),C'='          = MEANS CARRY DOWN % EVEN IF NO IPT          
         BNE   *+12                  IN COST FIELDS BELOW                       
         MVI   0(RF),C'%'                                                       
         MVI   SAVPCTC,C'Y'                                                     
         CLI   0(RF),C'%'          DO WE HAVE ANY PCT ADJ                       
         BNE   ED51H               NO                                           
         LR    RE,RF                                                            
*                                                                               
ED51D    DS    0H                                                               
         CLI   0(RE),C' '          NO PCT IF NO - OR +                          
         BNH   ED51H               NOTE-  WORK STARTS WITH NULL                 
         CLI   0(RE),C'-'          EXPRESSION STARTS WITH - OR +                
         BE    ED51F                                                            
         CLI   0(RE),C'+'                                                       
         BE    ED51F                                                            
         BCT   RE,ED51D                                                         
*                                                                               
ED51F    DS    0H                                                               
         MVC   SAVPCT,0(RE)        SAVE PCT ADJ ESPRESSION                      
         LA    R1,1(RF)                                                         
         SR    R1,RE                                                            
         STC   R1,SAVPCTL          LENGHT OF PCT ADJ EXT                        
         B     ED51J                                                            
*                                                                               
ED51H    DS    0H                                                               
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BZ    ED51I               NO, CARRY DOWN %                             
         CLI   SAVPCTC,C'Y'        YES, WERE WE ASKED TO CARRY ANYWAY?          
         BNE   ED51J               NO, SKIP PCT ADD ON                          
*                                                                               
ED51I    DS    0H                                                               
         MVC   1(L'SAVPCT,RF),SAVPCT  ELSE, ADD ON ANY PREV PCT ADJ             
         ZIC   R1,SAVPCTL          ADD LENGTH                                   
         AR    R0,R1                                                            
ED51J    DS    0H                                                               
         GOTO1 VCASHVAL,DMCB,WORK+1,(R0)                                        
         CLI   DMCB,0                                                           
         BNE   EDERR                                                            
         L     R1,DMCB+4                                                        
         CLI   PSEUDOPT,C'R'       FOR RESPONSES, NO PENNIES                    
         BE    ED51L                                                            
         CLI   CENTS,C'Y'                                                       
         BE    ED52                                                             
ED51L    DS    0H                                                               
         LR    R0,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'100'          NO PENNIES                                   
ED52     DS    0H                                                               
         CLI   PSEUDOPT,C'R'       IF RESPONSES                                 
         BNE   ED52D                                                            
         C     R1,=F'32767'        LIMIT TO TWO BYTES                           
         BH    ED52B                                                            
         LTR   R1,R1                                                            
         BNM   ED53                                                             
ED52B    DS    0H                                                               
         MVI   ERRAREA,INVERR                                                   
         B     EDERR                                                            
ED52D    DS    0H                                                               
         LTR   R1,R1                                                            
         BNM   ED53                                                             
         OI    INVSTAT,X'01'       SET NEG IND                                  
         OI    SAVSTAT,X'01'                                                    
         LPR   R1,R1                                                            
ED53     DS    0H                                                               
         ST    R1,DMCB+4                                                        
         MVC   SAVCOST,DMCB+4                                                   
         MVC   INVCOSTX,DMCB+4     INVCOST EXTENSION (1ST BYTE)                 
         MVC   INVCOST(3),DMCB+5                                                
         OC    SAVCOST,SAVCOST     IF COST IS ZERO                              
         BNZ   *+10                                                             
         MVC   SAVCOST,ZVAL4       SAVE AS SPECIAL CODE                         
*                                                                               
ED60     DS    0H                                                               
         CLI   ICSACT,C'C'       CANNOT CHANGE COST OF EASI INVOICE             
         BNE   ED60A                                                            
         CLI   EASISW,C'Y'                                                      
         B     ED60A               ***NO-OP***                                  
         MVI   ERRAREA,EASIERR                                                  
         OC    ELSAVE,ELSAVE       MUST HAVE SAVED ELEM                         
         BZ    EDERR                                                            
         L     RF,ELSAVE           ADDR OF SAVED ELEM                           
         CLC   INVCOST,INVCOST-INVELEM(RF)   TEST COST                          
         BNE   EDERR                                                            
         CLC   INVCOSTX,INVCOSTX-INVELEM(RF)  AND EXTENSION                     
         BNE   EDERR                                                            
*                                                                               
ED60A    DS    0H                  FILM CODE EDIT                               
         LA    R2,ICSFILMH(R7)                                                  
         TM    1(R2),X'20'         IF FILM CODE FIELD PROTECTED                 
         BNZ   ED71                SKIP EDIT (FEATURE NOT USED)                 
*                                  FIRST SET 2ND PRD SWITCH                     
         MVI   P2SW,C'N'                                                        
         CLI   SAVPRD2,0                                                        
         BE    ED60A4                                                           
         CLI   SAVPRD2,X'FF'                                                    
         BE    ED60A4                                                           
         TM    SAVSTAT,X'48'       EST OR ID INV                                
         BNZ   ED60A4                                                           
         MVI   P2SW,C'Y'           HAVE 2ND PROD                                
*                                                                               
ED60A4   DS    0H                                                               
         MVI   FCPYSW,0               SET OFF FILM COPIED DOWN SWITCH           
         NI    INVSTAT,255-INVSBLBQ   SET OFF BILLBOARD IND                     
         NI    SAVSTAT,255-INVSBLBQ   SET OFF BILLBOARD IND                     
         LA    R2,ICSFILMH(R7)                                                  
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    *+12                                                             
         ST    R2,SVFLMHDR         YES, SAVE A(LAST FILM HDR WITH DATA)         
         B     ED60D                                                            
*                                                                               
         CLI   ICSACT,C'D'         FOR DISPLAY FILTERS                          
         BE    ED70                DONT ADJUST IF NO INPUT                      
         CLI   ICSACT,C'T'         AND FOR TOTAL ACTION                         
         BE    ED70                                                             
         CLI   P2SW,C'N'           IF THIS LINE NOT PIGGY                       
         BNE   *+8                                                              
*                                                                               
ED60B    DS    0H                                                               
         MVI   SAVFLM2,0           CLEAR SAVED 2ND FILM                         
*                                                                               
         OC    INVFILM(2),SAVFLM1                                               
         BZ    ED70                                                             
         LTR   R7,R7               UNLESS FIRST LINE                            
         BZ    ED70                MUST REVALIDATE FILM                         
         L     RF,SVFLMHDR         (PRD/EST MAY HAVE CHANGED)                   
         MVC   8(L'ICSFILM,R2),8(RF)  COPY PREVIOUS FILM                        
         MVC   5(1,R2),5(RF)          SET INPUT LENGTH                          
         MVI   FCPYSW,1               SET HAVE COPIED FILM DOWN                 
*                                                                               
ED60D    DS    0H                                                               
         CLC   =C'N-',8(R2)        N IN FRONT MEANS IGNORE                      
         BNE   ED60F               FILM ERRORS                                  
         OI    INVSTAT2,X'80'                                                   
         MVC   8(L'ICSFILM-2,R2),8+2(R2)                                        
         MVC   8+L'ICSFILM-2(2,R2),=C'  '                                       
         ZIC   RF,5(R2)            DECREMENT LENGTH                             
         SH    RF,=H'2'                                                         
         STC   RF,5(R2)                                                         
*                                                                               
ED60F    DS    0H                                                               
         CLC   =C'NONE',8(R2)                                                   
         BNE   ED61                                                             
         XC    SAVFLM1(18),SAVFLM1   CLEAR FILM ID'S AND CODES                  
         CLI   8+4(R2),C' '        TEST 'NONE' BY ITSELF                        
         BNH   ED70                                                             
         CLC   8+4(2,R2),=C'-B'    BILLBOARD                                    
         BNE   ED61                                                             
         OI    INVSTAT,INVSBLBQ     SET BILLBOARD STATUS                        
         OI    SAVSTAT,INVSBLBQ                                                 
         B     ED70                                                             
*                                                                               
ED61     DS    0H                                                               
         MVC   8+L'ICSFILM-2(2,R2),=C'  '  CLEAR ERROR INDICATORS               
         LA    R4,8+L'ICSFILM-2(R2)         ADJUST INPUT LENGTH                 
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
*                                                                               
         LA    RF,7(R2)                                                         
         SR    R4,RF                                                            
*                                                                               
         STC   R4,5(R2)            SET NEW INPUT LENGTH                         
         XC    X,X                                                              
         MVC   DMCB+8(4),=C',=-='   USE - NOT ,                                 
         CLI   TRAFSEP,0           DIFFERENT SEPARATOR                          
         BE    ED61B                                                            
         CLI   TRAFSEP,C'N'                                                     
         BE    ED61B                                                            
         MVC   DMCB+8+2(1),TRAFSEP                                              
*                                                                               
ED61B    DS    0H                                                               
         GOTO1 VSCANNER,DMCB,(R2),(3,X)                                         
*                                                                               
         LA    R4,X                FIRST FILM                                   
         CLC   12(2,R4),=C'B '     B MEANS BILLBOARD                            
         BNE   ED61D                                                            
         OI    INVSTAT,INVSBLBQ     SET BILLBOARD STATUS                        
         OI    SAVSTAT,INVSBLBQ                                                 
         CLI   32(R4),0           SHOULD BE LAST FIELD                          
         BNE   ED62D                                                            
         B     ED63                                                             
ED61D    DS    0H                                                               
         MVC   WPRD,SAVPRD                                                      
         GOTO1 =A(CKFILM),DMCB,(RC),RR=RELO00                                   
         CLI   ERRAREA,0                                                        
         BNE   EDERR                                                            
         MVC   INVFILM,TRFLMID                                                  
         MVC   SAVFC1,TRFILM       SAVE WHOLE CODE                              
*                                                                               
         LA    R4,X+32             TRY FOR 2ND FILM                             
         CLI   0(R4),0             NONE                                         
         BE    ED63                                                             
*                                                                               
         CLC   12(2,R4),=C'B '     -B MEANS BILLBOARD                           
         BNE   ED62                                                             
         OI    INVSTAT,INVSBLBQ     SET BILLBOARD STATUS                        
         OI    SAVSTAT,INVSBLBQ                                                 
         CLI   32(R4),0           SHOULD BE LAST FIELD                          
         BNE   ED62D                                                            
         B     ED63                                                             
ED62     DS    0H                                                               
         CLC   32+12(2,R4),=C'B '   BILLBOARD MAY BE IN THIRD POS               
         BNE   ED62C                                                            
         OI    INVSTAT,INVSBLBQ     SET BILLBOARD STATUS                        
         OI    SAVSTAT,INVSBLBQ                                                 
*                                                                               
ED62C    DS    0H                  2ND FILM                                     
         CLI   ICSACT,C'D'         OK IF DISPLAY                                
         BE    ED64                                                             
         CLI   ICSACT,C'T'         OR TOTAL                                     
         BE    ED64                                                             
         CLI   P2SW,C'N'           MUST HAVE 2ND PRD                            
         BE    ED62D                                                            
         B     ED64                                                             
*                                                                               
ED62D    DS    0H                                                               
         MVI   ERRAREA,FLMCERR2                                                 
         B     EDERR                                                            
*                                                                               
ED63     DS    0H                  ONLY ONE FILM                                
         CLI   P2SW,C'Y'           IF HAVE 2ND PRODUCT                          
         BNE   ED65                                                             
         MVC   WPRD,SAVPRD2        MUST CHECK IT ALSO                           
         LA    R4,X                RE-POINT TO FIRST FILM                       
         GOTO1 =A(CKFILM),DMCB,(RC),RR=RELO00                                   
         CLI   ERRAREA,0                                                        
         BNE   EDERR                                                            
         B     ED65                                                             
*                                                                               
ED64     DS    0H                  TWO FILMS AND TWO PRODUCTS                   
         MVC   WPRD,SAVPRD2                                                     
         GOTO1 =A(CKFILM),DMCB,(RC),RR=RELO00                                   
         CLI   ERRAREA,0                                                        
         BNE   EDERR                                                            
         MVC   INVFILM2,TRFLMID                                                 
         MVC   SAVFC2,TRFILM       SAVE 2ND CODE                                
*                                                                               
ED65     DS    0H                                                               
*                                                                               
         CLI   ICSACT,C'D'         DONT SHOW ERROR FOR DISPLAY FILTER           
         BE    ED66                                                             
         CLI   ICSACT,C'T'         OR TOTAL FILTER                              
         BE    ED66                                                             
         CLI   TRAFOK,C'Y'         OR IF REJECTING BAD FILMS                    
         BNE   ED66                                                             
         CLI   TRFERR,0            ANY FILM ERROR                               
         BE    ED66                NO                                           
         LA    R4,8+L'ICSFILM-2(R2)   PUT ERROR IN LAST 2 BYTES                 
         MVI   0(R4),C'*'                                                       
         MVI   1(R4),C'F'           FILM NOT ON FILE                            
         TM    TRFERR,X'80'                                                     
         BNZ   ED65D                                                            
         MVI   1(R4),C'L'          SECONDS LENGTH                               
         TM    TRFERR,X'40'                                                     
         BNZ   ED65D                                                            
         MVI   1(R4),C'P'          PRODUCT                                      
         TM    TRFERR,X'20'                                                     
         BNZ   ED65D                                                            
         MVI   1(R4),C'?'                                                       
*                                                                               
ED65D    DS    0H                                                               
         FOUT  (R2)                                                             
*                                                                               
ED66     DS    0H                                                               
         MVC   SAVFLM1(2),INVFILM                                               
*                                                                               
ED70     DS    0H                                                               
         CLI   FCPYSW,0               IF HAVE COPIED FILM DOWN                  
         BE    *+14                                                             
         XC    8(L'ICSFILM,R2),8(R2)  CLEAR IT NOW                              
         MVI   5(R2),0                                                          
ED71     DS    0H                                                               
         MVI   ERRAREA,0                                                        
         CLI   INVDAT,0                                                         
         BE    *+8                                                              
         MVI   BYTE2,1             SET DATA ENTERED                             
EDEXT    LA    R2,ICSMEDH                                                       
EDERR    ST    R2,FULL             SAVE R2                                      
EDEXTX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        EDIT FILM INPUT                                                        
         SPACE 2                                                                
         DS    0H                                                               
CKFILM   NMOD1 0,CKFILM                                                         
         L     RC,0(R1)            RESTORE WORK AREA REG                        
*                                                                               
         MVI   ERRAREA,0                                                        
         MVI   TRFLMID,0                                                        
         XC    TRFLMSQ,TRFLMSQ                                                  
         MVI   TRFERR,0                                                         
*                                  FIRST COMPLETE FILM IN TRFILM                
         SR    R6,R6                                                            
         ICM   R6,1,0(R4)            LENGTH                                     
         BZ    CKFERR                                                           
         CLI   0(R4),8                                                          
         BH    CKFERR                                                           
*                                                                               
         CLI   TRAFSEP,0           TRAFSEP 0 OR N MEANS NO LENGTH               
         BE    CKF3                AND NO USE OF PREVIOUS ENTRY                 
         CLI   TRAFSEP,C'N'                                                     
         BE    CKF3                                                             
         LA    RF,TRFILM                                                        
         MVC   TRFILM,SPACES                                                    
         B     CKF3B                                                            
*                                                                               
CKF3     DS    0H                                                               
         LA    RF,TRFILM+8                                                      
         SR    RF,R6                                                            
*                                                                               
CKF3B    DS    0H                                                               
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),12(R4)                                                   
*                                                                               
         CLI   TRFILM,C' '         FILM CODE NOT COMPLETE                       
         BNH   CKFERR                                                           
*                                  SEARCH BUFFER FOR FILM                       
         LA    R5,BUFFER+2                                                      
CKF4     DS    0H                                                               
         CLI   0(R5),0             EOL                                          
         BE    CKF6                                                             
         CLI   0(R5),X'F0'         SKIP SPECIALS                                
         BNL   CKF5                                                             
         CLI   0(R5),4             ELEM CODE                                    
         BH    CKF6                                                             
         BL    CKF5                                                             
*                                                                               
         USING INVFLMEL,R5                                                      
         CLC   INVFCD,TRFILM                                                    
         BNE   CKF5                                                             
         MVC   TRFLMID,INVFID      SET ID NO                                    
         MVC   TRFLMSQ,INVFSQ      SET SEQ ID NO.                               
         B     CKF6                                                             
*                                                                               
CKF5     DS    0H                                                               
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     CKF4                                                             
         DROP  R5                                                               
*                                  NOTE- FORMAT OF FLMTAB IS                    
*                                        BYTE 0   - MY FILM ID                  
*                                        BYTE 1   - TOTAL ENTRY LENGTH          
*                                        BYTE 2   - SECONDS LENGTH              
*                                        BYTE 3+  - PRD LIST                    
*                                                                               
*                                                                               
CKF6     DS    0H                  CHECK FILM IN CURRENT TABLE                  
         LA    R5,FLMTAB           FOR SLN AND PRD VALIDATION                   
*                                                                               
CKF6B    DS    0H                                                               
         ST    R5,AFTBENT          SAVE ENTRY ADDRESS                           
         CLI   0(R5),0             END OF TABLE                                 
         BE    CKF8                                                             
         CLC   0(1,R5),TRFLMID                                                  
         BE    CKF7                                                             
         SR    R0,R0                                                            
         ICM   R0,1,1(R5)          ENTRY LENGTH                                 
         BZ    CKF8                                                             
         AR    R5,R0                                                            
         B     CKF6B                                                            
*                                                                               
CKF7     DS    0H                  TEST SLN AND PRD                             
         L     R5,AFTBENT          ADDR OF TABLE ENTRY                          
         CLI   1(R5),2             IF ENTRY FOR NON-EXISTENT                    
         BE    CKFX                FILM THEN SKIP TESTS                         
*                                  TEST INV SLN VS FILM SLN                     
         CLC   WPRD,SAVPRD         IF DOING 2ND PRD TEST SUM                    
         BNE   CKF7F                                                            
         MVC   HFLEN,2(R5)         SAVE FIRST SLN OF PIGGY                      
         CLI   P2SW,C'Y'           IF HAVE 2ND PRD, DONE FOR NOW                
         BE    CKF7M                                                            
         CLC   HFLEN,INVLEN        TEST ONE PRD SLN                             
         BE    CKF7M                                                            
*                                                                               
CKF7D    DS    0H                                                               
         OI    TRFERR,X'40'        WRONG SECONDS LENGTH                         
         CLI   TRAFOK,C'Y'         TEST OK TO ACCEPT BAD FILMS                  
         BE    CKF7M                                                            
         MVI   ERRAREA,FSLNERR                                                  
         B     CKFX                                                             
*                                                                               
CKF7F    DS    0H                  DOING 2ND PRD                                
         ZIC   RF,HFLEN            SUM 2 SLN'S                                  
         CLI   X+32,0              TEST HAVE 2ND FILM                           
         BE    CKF7H               NO, DON'T DOUBLE LENGTH                      
         ZIC   RE,2(R5)                                                         
         AR    RF,RE                                                            
CKF7H    DS    0H                                                               
         ZIC   RE,INVLEN                                                        
         CR    RF,RE               TEST SUM VS INVLEN                           
         BNE   CKF7D                                                            
*                                  TEST PRODUCTS                                
CKF7M    DS    0H                                                               
         ZIC   R6,1(R5)                                                         
         SH    R6,=H'3'            R6 = NO. OF PRODS                            
         LA    RF,3(R5)                                                         
*                                                                               
CKF7P    DS    0H                                                               
         CLI   0(RF),X'FF'         PRD=ALL                                      
         BE    CKF7R                                                            
*                                                                               
         LA    RE,WPRD                                                          
         CLI   SVTRAPRD,0          USE TRAFFIC PRODUCT OVERRIDE                 
         BE    *+8                 IF PRESENT                                   
         LA    RE,SVTRAPRD                                                      
         CLC   0(1,RE),0(RF)                                                    
         BE    CKF7R                                                            
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R6,CKF7P                                                         
*                                                                               
         OI    TRFERR,X'20'        PRD INVALID                                  
         CLI   TRAFOK,C'Y'         OK TO ACCEPT BAD FILMS                       
         BE    CKF7R                                                            
*                                                                               
         MVI   ERRAREA,FPRDERR                                                  
         B     CKFX                                                             
*                                                                               
CKF7R    DS    0H                                                               
         B     CKFX                                                             
*                                                                               
CKF8     DS    0H                  FILM NOT IN FLMTAB                           
         XC    KEY,KEY             READ FOR FILM                                
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(1),BMED                                                    
         OC    KEY+3(2),SVTRACLT   USE TRAFFIC CLIENT                           
         BNZ   *+10                IF ANY                                       
         MVC   KEY+3(2),BCLT       ELSE USE NORMAL CLIENT                       
         MVC   KEY+5(8),TRFILM                                                  
***      BAS   RE,HIGH                                                          
         BAS   RE,HIGHTRF                                                       
         CLC   KEY(13),KEYSAVE                                                  
         BE    CKF9                                                             
         OI    TRFERR,X'80'        SET FILM NOT FOUND                           
         CLI   TRAFOK,C'Y'         OK TO ACCEPT BAD FILMS                       
         BE    CKF15                                                            
         B     CKFERR                                                           
*                                                                               
CKF9     DS    0H                                                               
***      BAS   RE,GETREC                                                        
         BAS   RE,GETTRF                                                        
         LA    R6,IOAREA+24                                                     
*                                                                               
CKF10    DS    0H                                                               
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                BAD FILM REC                                 
         CLI   0(R6),X'10'         CMML DATA ELEM                               
         BE    CKF11                                                            
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKF10                                                            
*                                                                               
CKF11    DS    0H                                                               
         USING CMLDTAEL,R6                                                      
         MVC   TRFLMSQ,CMLSEQ+1    SET CMML SEQ NO.                             
         MVC   TRFLEN,CMLSLN       SET LENGTH                                   
*                                  SET PRODUCTS                                 
         LA    R6,IOAREA+24                                                     
*                                                                               
CKF12    DS    0H                                                               
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                BAD FILM REC                                 
         CLI   0(R6),X'20'         PROD ELEM                                    
         BE    CKF13                                                            
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKF12                                                            
*                                                                               
CKF13    DS    0H                                                               
         ZIC   RF,1(R6)                                                         
         LA    RE,1(RF)            TEST ENTRY WILL FIT IN FLMTAB                
         A     RE,AFTBENT          ENTRY ADDRESS                                
         LA    R0,FLMTABX                                                       
         CR    RE,R0                                                            
         BL    *+6                                                              
         DC    H'0'                FLMTAB FULL - SHOULD NOT HAPPEN              
*                                                                               
         SH    RF,=H'3'            RF = NO. OF PRODS -1                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R5),2(R6)                                                    
         MVC   0(1,R5),TRFLMID     SET ID (IF HAVE)                             
         MVC   2(1,R5),TRFLEN      LENGTH                                       
         LA    RF,4(RF)            TOTAL TABLE ENTRY LENGTH                     
         STC   RF,1(R5)                                                         
*                                                                               
*                                                                               
CKF15    DS    0H                  ADD FILM ELEM TO BUFFER                      
         LA    R5,WORK                                                          
         USING INVFLMEL,R5                                                      
         XC    WORK,WORK                                                        
         MVI   INVFLMEL,4          ELEM CODE                                    
         MVI   INVFLMEL+1,13           LEN                                      
         MVC   INVFCD,TRFILM                                                    
         MVC   INVFSQ,TRFLMSQ                                                   
         DROP  R5                                                               
*                                                                               
         LA    R5,BUFFER+2                                                      
         SR    R1,R1                                                            
*                                                                               
CKF21    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    CKF23                                                            
         CLI   0(R5),X'F0'         SKIP SPECIALS                                
         BNL   CKF22                                                            
         CLI   0(R5),4                                                          
         BL    CKF22                                                            
         BH    CKF23                                                            
*                                                                               
         CLC   TRFLMID,2(R5)       TEST ALREADY THERE                           
         BNE   CKF21D                                                           
         MVC   11(2,R5),TRFLMSQ    SET SEQ NO.                                  
         B     CKF7                NOW TEST LENGTH AND PRDS                     
*                                                                               
CKF21D   DS    0H                                                               
         ZIC   R1,2(R5)            SAVE CURRENT SEQ NO.                         
*                                                                               
CKF22    DS    0H                                                               
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     CKF21                                                            
*                                                                               
CKF23    DS    0H                                                               
         LA    R1,1(R1)                                                         
         CH    R1,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STC   R1,WORK+2           SET NEW ID NO.                               
         STC   R1,TRFLMID                                                       
         L     RF,AFTBENT          FLMTAB ENTRY                                 
         STC   R1,0(RF)            SET NEW ID                                   
         CLI   1(RF),0             TEST FLMTAB ENTRY FOR                        
         BNE   *+8                 NON-EXISTENT FILM                            
         MVI   1(RF),2             YES- MUST SET ENTRY LENGTH                   
*                                  AS THERE IS NO REAL ENTRY                    
         GOTO1 VRECUP,DMCB,(X'FF',BUFFER),WORK,(R5)                             
         B     CKF7                TEST LEN AND PRDS                            
*                                                                               
CKFERR   DS    0H                                                               
         MVI   ERRAREA,FLMCERR     1ST FILM ERR                                 
         CLC   WPRD,SAVPRD                                                      
         BE    *+8                                                              
         MVI   ERRAREA,FLMCERR2    OR SECOND                                    
         B     CKFX                                                             
*                                                                               
CKFX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         SPACE 1                                                                
* DDCOREQUS                                                                     
* SPSTAPACKD                                                                    
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
*  SPADINTD                                                                     
         DSECT                                                                  
       ++INCLUDE SPADINTD                                                       
         SPACE 2                                                                
*  SPSTABLK                                                                     
         DSECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         SPACE 2                                                                
*  SPGETBUBLD                                                                   
         DSECT                                                                  
       ++INCLUDE SPGETBUBLD                                                     
         SPACE 2                                                                
*  SPINVWK                                                                      
       ++INCLUDE SPINVWK                                                        
