*          DATA SET SRCON00    AT LEVEL 056 AS OF 08/14/20                      
*PHASE T11000A                                                                  
*INCLUDE FAVERCHK                                                               
*INCLUDE FAXPTAB                                                                
*INCLUDE PWDVAL                                                                 
         TITLE '$CT - CONNECT TO AN APPLICATION PROGRAM'                        
CONNECT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$CT**,RA,R2,RR=RE                                  
*                                                                               
         USING WORKD,RC            RC=A(W/S)                                    
         XC    WORKD(256),WORKD    Clear 2K of WORKD                            
         XC    WORKD+256(256),WORKD+256                                         
         XC    WORKD+512(256),WORKD+512                                         
         XC    WORKD+768(256),WORKD+768                                         
         XC    WORKD+1024(256),WORKD+1024                                       
         XC    WORKD+1280(256),WORKD+1280                                       
         XC    WORKD+1536(256),WORKD+1536                                       
         XC    WORKD+1792(256),WORKD+1792                                       
*                                                                               
         ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
         LR    RE,R1                                                            
         ST    R1,ASRPARM                                                       
         ST    RD,SAVERD                                                        
         USING SRPARMD,RE          RE=A(S/R PARM LIST)                          
         L     R1,SRPARM2                                                       
         ST    R1,ATIA             SAVE A(TIA)                                  
         L     R9,SRPARM1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,SRPARM6                                                       
         USING SRCONFFD,R8         R8=A(TWA)                                    
         L     R7,SRPARM3                                                       
         USING UTLD,R7             R7=A(UTL ENTRY)                              
         MVC   ATIOB,SRPARM8       SAVE A(TIOB)                                 
         LA    R1,CNTSREQH                                                      
         ST    R1,FADR                                                          
         DROP  RE                                                               
*                                                                               
CON1     BRAS  RE,INIT             INITIALIZE WORKING STORAGE                   
*&&US                                                                           
         CLC   TLUID(2),=C'TP'     TALENT LU'S                                  
         BNE   CON3                NOT TALENT SO CONTINUE                       
         CLI   TLUID+7,C'S'        IS IT A SHUTTLE?                             
         BE    CON3                YES, THEN LEAVE ALONE FOR NOW                
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         CLI   SSBSYSN1,C'S'       CSC                                          
         BE    CON3X               YES SO BLOCK ACCESS IN CSC                   
         CLI   SSBSYSN1,C'U'       MEL                                          
         BE    CON3X               YES SO BLOCK ACCESS                          
         NI    TSTAT,255-TSTATDDS  NO DDS TERMINALS FOR TP LU'S                 
         DROP  RE                                                               
*&&                                                                             
CON3     TM    TSTAT5,TST5TBF      TEST TERM NOT AUTH'D FOR THIS FACPAK         
         BZ    CON4                                                             
CON3X    LA    RE,112                                                           
         B     ERROR                                                            
*                                                                               
CON4     L     RE,VSSB             TEST USER INPUT INHIBITED                    
         TM    SSBSTAT1-SSBD(RE),SSBUII                                         
         BZ    CON5                                                             
         TM    TSTAT,TSTATDDS      BUT ALLOW DDS TERMINALS ACCESS               
         BNZ   CON5                                                             
         LA    RE,113                                                           
         B     ERROR                                                            
*                                                                               
CON5     MVI   TSVCREQ+1,0         SET FOR ERROR EXIT                           
*&&US                                                                           
CONBIAS  TM    TTYPE2,TTYPE2BI     TEST BIAS TERMINAL                           
         BZ    CONBIASX                                                         
         LA    RE,CNTSREQH         POINT TO SRVREQ FIELD                        
         LLC   R0,0(RE)                                                         
         AR    RE,R0               POINT TO 'PROGRAM' FIELD                     
         IC    R0,0(RE)                                                         
         AR    RE,R0               POINT TO PROGRAM INPUT FIELD                 
         CLI   5(RE),0             TEST ANY INPUT                               
         BE    EXIT                                                             
         BRAS  RE,BIAS             GO LOAD AND SET FF CONNECT SCREEN            
CONBIASX EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* CLEAR ALL GLOBAL VARIABLES IF $CT INPUT BY ITSELF                   *         
***********************************************************************         
         SR    R0,R0               SET CLEAR IF $CT INPUT                       
         BRAS  RE,GLOCLR                                                        
*                                                                               
SETSOX   BRAS  RE,SSOX             SET SOX CONTROL FLAGS                        
*                                                                               
SETLOGO  BRAS  RE,SLOGO            SET LOGO ON CONNECT SCREEN                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE SERVICE REQUEST FIELD                                      *         
***********************************************************************         
VALSRV   MVC   SAVESREQ,CNTSREQ    SAVE INPUT VALUE IN W/S                      
         MVI   DATASW,C'N'                                                      
         MVI   CTSW,C'N'                                                        
         MVI   DONESW,C'N'                                                      
         MVI   REUSFLAG,C'N'       SET REUSE USERID/PASSWORD FLAG OFF           
         MVI   REUSPFLG,C'N'       SET REUSE SYSTEM/PROGRAM FLAG OFF            
         MVI   AUTOCON,C'N'        SET AUTOMATIC CONNECT FLAG  OFF              
         MVI   PIDENTER,C'Y'       SET PERSONALID ENTERED FLAG ON               
         XC    IDOPTS,IDOPTS       CLEAR ID OPTIONS                             
         MVI   UIDLLO,0            CLEAR ID LAST LOGON RELATIVE MONTH           
         XC    UIDLLON,UIDLLON                                                  
*                                                                               
VALSRV1  MVC   BYTE,CNTSREQH+5     SAVE LENGTH OF S/R FIELD                     
         CLI   BYTE,17                                                          
         BNE   VALSRV1C                                                         
         CLI   CNTSREQ+14,C'*'     LOOK FOR STEREO FLAGS LURKING AT END         
         BNE   VALSRV1C                                                         
         CLC   CNTSREQ+15(2),=C'00'                                             
         BE    *+14                                                             
         CLC   CNTSREQ+15(2),=C'10'                                             
         BNE   VALSRV1C                                                         
         LA    RE,CNTSREQ                                                       
         LA    RF,CNTSREQ+13                                                    
VALSRV1A CLI   0(RF),C' '          SCAN TO FIND END OF S/R DATA                 
         BH    VALSRV1B                                                         
         BCTR  RF,0                                                             
         CR    RF,RE                                                            
         BH    VALSRV1A                                                         
         B     VALSRV1C                                                         
*                                                                               
VALSRV1B LA    RF,1(RF)                                                         
         SR    RF,RE                                                            
         STC   RF,CNTSREQH+5       SET SIGNIFICANT LENGTH OF DATA               
VALSRV1C GOTO1 FVAL,CNTSREQH                                                    
         MVC   CNTSREQH+5(1),BYTE                                               
         BZ    VALUSR              NO INPUT - MUST BE FROM $CT SCREEN           
         MVI   REUSFLAG,C'Y'       SET REUSE USERID/PASSWORD FLAG ON            
         XC    CNTSREQ,CNTSREQ                                                  
*                                                                               
VALSRV2  CLC   FLD+1(2),=C'CT'     CHECK FOR $CT/=CT/+CT INPUT                  
         BNE   VALSRV8                                                          
         CLI   FLDH+5,3            IF ONLY $CT                                  
         BE    VALSRV2B                                                         
         CLI   FLD+3,C','          CHECK FOR OTHER THAN CONNECT                 
         BE    VALSRV2B                                                         
         CLI   FLD+3,C'/'                                                       
         BNE   VALSRV8                                                          
VALSRV2B MVI   CTSW,C'Y'                                                        
         CLI   FLDH+5,3                                                         
         BNE   VALSRV3                                                          
         MVI   REUSFLAG,C'N'       SET USERID/REUSE PASSWORD FLAG TO NO         
*                                                                               
VALSRV2C CLI   CNTIDH+5,0          CHECK USER-ID INPUT                          
         BNE   VALUSR                                                           
         CLI   CTSW,C'Y'                                                        
         BE    DISCON                                                           
         MVI   DONESW,C'Y'                                                      
         B     DISCON                                                           
*                                                                               
VALSRV3  CLI   FLDH+5,8            CHECK FOR $CT,X.. SPECIAL VALUES             
         BNE   VALSRV3A                                                         
         CLC   FLD+3(2),=C',*'     $CT,*FS*- FACID/SESSION                      
         BNE   VALSRV3A                                                         
         CLC   FLD+5(1),SYSIDCHR                                                
         BNE   VALSRV3A                                                         
         CLI   FLD+7,C'*'                                                       
         BE    VALSRV3C                                                         
VALSRV3A CLI   FLDH+5,7                                                         
         BH    VALSRV6                                                          
         CLC   FLD+3(3),=C',EQ'    $CT,EQ - EQUATED TO CLEAR KEY                
         BE    VALSRV3C                                                         
         CLC   FLD+3(2),=C'/A'     $CT/A  - TESTING A VERSION                   
         BE    VALSRV3C                                                         
         CLC   FLD+3(3),=C',SV'    $CT,SV - CONNECT AND SAVE SESSION            
         BE    VALSRV3B                                                         
         CLI   FLDH+5,6                                                         
         BH    VALSRV6             $CT,UID                                      
         BL    EIIF                                                             
         CLC   FLD+3(3),=C',AC'    $CT,AC - AUTOMATIC CONNECT                   
         BE    VALSRV3D                                                         
         B     VALSRV6             $CT,UID                                      
*                                                                               
VALSRV3B MVI   FERN,X'FF'                                                       
         LA    R6,SCANBLK          FAKE OUT SCAN BLOCK                          
         MVC   12(3,R6),FLD+4                                                   
         BRAS  RE,VALSAVE          GO TO VALIDATION ROUTINE                     
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
VALSRV3C CLI   CNTIDH+5,0          CHECK USER-ID INPUT                          
         BNE   VALUSR                                                           
         B     DISCON                                                           
VALSRV3D MVI   AUTOCON,C'Y'        FLAG AUTOMATIC CONNECT REQUESTED             
         B     VALSRV3C                                                         
*                                                                               
VALSRV6  CLI   FLD+3,C','          CHECK FOR $CT,USER-ID,SYS,PRG,PWD            
         BE    VALSRV6A                                                         
         CLI   FLD+5,C','          AFTER A '/A'?                                
         BNE   EIIF                                                             
VALSRV6A BRAS  RE,ANYDATA                                                       
         GOTO1 VSCUNKEY,DMCB,(C',',FLDH),(5,CNTSREQH)                           
         XC    CNTSREQ,CNTSREQ                                                  
         MVI   REUSPFLG,C'Y'       SET REUSE SYSTEM/PROGRAM FLAG ON             
         CLI   CNTIDH+5,4                                                       
         BNE   VALUSR                                                           
         CLC   CNTID(3),=C'XCTL'   TEST FOR A VERY SPECIAL ID                   
         BNE   VALUSR                                                           
         MVC   XCTLTYP,CNTID+3     SAVE TRANSFER CONTROL TYPE (L OR N)          
         MVC   XCTLSYS,CNTSYS      SAVE SYSTEM                                  
*                                                                               
         LA    R0,L'CNTPGM                                                      
         LA    RF,CNTPGM                                                        
         CLI   0(RF),C' '          STRIP FUNNIES FOR OM (2 CHARS)               
         BH    *+8                                                              
         MVI   0(RF),C' '                                                       
         AHI   RF,1                                                             
         BCT   R0,*-16                                                          
         MVC   XCTLPGM,CNTPGM      SAVE PROGRAM                                 
*                                                                               
         CLI   XCTLTYP,C'L'        TEST IF NEW SESSION REQUESTED                
         BE    VALSRV12                                                         
         MVI   FERN,X'FF'          YES-NEED TO CALL SESSION GRABBER             
         LA    R6,SCANBLK          FAKE OUT SCAN BLOCK                          
         MVC   12(3,R6),=C'SVU'                                                 
         CLI   XCTLTYP,C'N'        ANY NEW SESSION                              
         BE    *+10                                                             
         MVC   14(1,R6),XCTLTYP    NO-SET NOMINATED NEW SESSION                 
         BRAS  RE,VALSAVE          GO TO VALIDATION ROUTINE                     
         B     VALSRV12                                                         
*                                  CHECK FOR $PGM(ETC.) OR +PGM(ETC.)           
VALSRV8  CLI   TSYS,0              MUST ALREADY BE CONNECTED                    
         BNE   VALSRV10                                                         
         CLC   FLD+1(2),=C'RE'     IF HERE FROM =RE GIVE CONNECT SCREEN         
         BE    DISCON                                                           
         MVI   DUB,X'FD'                                                        
         BAS   RE,SCROVER          IF NOT EXIT WITH SERVICE ERROR SCRN          
         B     EXIT                                                             
VALSRV10 CLI   FLDH+5,1            INPUT MUST BE AT LEAST 2 BYTES LONG          
         BNH   EIIF                                                             
         CLI   FLD,C'$'            AND START WITH A CURRENCY SYMBOL             
         BE    VALSRV11                                                         
         CLI   FLD,C'='            OR AN EQUALS SIGN                            
         BE    VALSRV11                                                         
         CLI   FLD,C'+'            OR A PLUS SIGN                               
         BNE   EIIF                                                             
         MVI   DATASW,C'Y'                                                      
*                                                                               
VALSRV11 BRAS  RE,ANYDATA          CHECK FOR ANY CONNECT DATA IN TWA            
         LLC   R1,FLDH+5                                                        
         AHI   R1,-2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CNTPGM(0),FLD+1     MOVE PROGRAM NAME TO PROGRAM FIELD           
         LA    R1,1(R1)                                                         
         STC   R1,CNTPGMH+5                                                     
         OI    SOXFL1,SOXPGMSR     SET PROGRAM NAME IN S/R FIELD                
*                                                                               
VALSRV12 BRAS  RE,REBUILD          BUILD ORIGINAL CONNECT SCREEN                
         BE    VALSAC              PROCEED TO STANDARD VALIDATION               
         EJECT                                                                  
***********************************************************************         
* DISCONNECT TERMINAL FROM SYSTEM                                     *         
***********************************************************************         
DISCON   TM    TSTAT1,TSTATDDS     RESTORE OFFICE CODE                          
         BZ    *+8                                                              
         MVI   TOFFCODE,C'*'                                                    
         XC    TCTDATA,TCTDATA     CLEAR CONNECT TERMINAL DATA                  
         XC    TAPRG,TAPRG                                                      
         XC    TPERSON,TPERSON                                                  
         NI    TSTAT7,255-(TST7PS0+TST7PS1+TST7PS2+TST7PSWN)                    
         MVI   TPASSEXP,0                                                       
         MVI   TSTATB,0                                                         
         XC    TAGYPER,TAGYPER                                                  
         XC    TAGYSEC,TAGYSEC                                                  
         XC    TUPDFAC,TUPDFAC                                                  
*                                                                               
         L     R1,TUTLXADR                                                      
         USING XAUTLD,R1                                                        
         SAM31                                                                  
         XC    TTICKETN,TTICKETN                                                
         SAM24                                                                  
         DROP  R1                                                               
*                                                                               
         XC    TXPINFO,TXPINFO                                                  
         XC    TOSIN,TOSIN                                                      
         NI    TSTATC,255-TSTCDDLP                                              
         NI    TSTATD,255-TSTATCBY                                              
         XC    TRUNTAB,TRUNTAB                                                  
         MVI   TSVCREQ,1                                                        
         LLC   R1,TSESSION                                                      
         IC    R1,SSNABITS(R1)                                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         NI    TSSBITS,0           TURN OFF SESSION ACTIVE BIT                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         NI    TSSXBITS,0          TURN OFF CANT USE SESSION BIT                
         L     R1,ASRPARM                                                       
         MVI   0(R1),X'FF'                                                      
         OI    CNTIDH+6,X'40'                                                   
*                                                                               
         L     RF,VSSB             SET R3 TO A(TCB)                             
         L     R3,SSBTKADR-SSBD(RF)                                             
         BRAS  RE,CLRTEMP          UNALLOCATE TEMPEST                           
         OI    TCBINDS2-TCBD(R3),TCBFCTD SET DISCONNECT TRANSACTION             
*                                                                               
         OC    BILLREF,BILLREF     END BILLING IF STILL ACTIVE                  
         BZ    DISC010                                                          
         L     RF,VSSB             MOVE BILLREF TO TASK ENTRY                   
         L     RF,SSBTKADR-SSBD(RF)                                             
         MVC   TCBBILL-TCBD(L'TCBBILL,RF),BILLREF                               
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,C'E'                                                        
         GOTO1 VBILLIT,DMCB                                                     
*                                                                               
DISC010  EQU   *                                                                
         TM    TSTAT6,TST6STSS     TEST STEREO SPECIAL MODE                     
         BNZ   *+8                 IF SO PRESERVE STEREO FLAGS                  
         NI    TSTAT6,255-TST6STRO-TST6STFU-TST6STSS TURN OFF STEREO            
         NI    TSTAT1,255-TSTATBIL TURN OFF BILLING                             
         NI    TSTAT2,255-TSTATBCP TURN OFF BRDCST PENDING                      
         NI    TSTAT2,255-TSTATTCT TURN OFF JUST CONNECTED FLAG                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE USER-ID FIELD                                              *         
***********************************************************************         
VALUSR   GOTO1 FVAL,CNTIDH                                                      
         BNZ   VALUSR1             IF USERID NOT ENTERED TRY TO                 
         BAS   RE,LOADUID            LOAD FROM VALUE IN UTL                     
         BNE   EMIF                                                             
*                                                                               
VALUSR1  BRAS  RE,SCRIPUID         SCAN USER ID FIELD FOR SCRIPT UID#           
         BNE   EIIF                EXIT IF INVALID                              
         NI    CNTIDH+6,X'FF'-X'40'                                             
         BAS   RE,SCANUID          SCAN USER ID FIELD AND EXTRACT               
         BE    VALUSR1A            USERID AND OPTIONAL PERSONAL ID              
         CLI   FNDX,2                                                           
         BE    EIPI                INVALID PERSONAL ID FIELD                    
         BL    EIID                INVALID USERID FIELD                         
         B     EIIF                INVALID DDS AGY OPTION FIELD                 
VALUSR1A EQU   *                                                                
*NOP*    BRAS  RE,CHKIT            THIS NOW CALLED AT VALPRG14                  
*NOP*    BNE   EIPI                                                             
         XC    IDOPTS,IDOPTS       CLEAR ID OPTIONS FIELD                       
         MVI   UIDLLO,0            CLEAR ID LAST LOGON RELATIVE MONTH           
         XC    UIDLLON,UIDLLON     New YM binary LOGON                          
         L     R4,AUIDREC          READ USER ID RECORD                          
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD ID(ALPHA) KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,USERID                                                    
         BAS   RE,CTREAD                                                        
         BNE   EIID                                                             
         MVC   UIDLLO,CTISTAT      STATUS BYTE HAS LAST LOGON MONTH NUM         
         NI    UIDLLO,X'7F'                                                     
         LA    R4,CTIDATA                                                       
         SR    R1,R1                                                            
*                                                                               
VALUSR2  CLI   0(R4),0             FIND ID(NUMBER) & AGENCY ELEMENTS            
         BE    VALUSR2X                                                         
         CLI   0(R4),CTACTELQ      X'01' activity element                       
         BNE   VALUSR2A                                                         
         USING CTACTD,R4                                                        
         CLI   CTACTLEN,CTACTLNQ   What lenght is it?                           
         BNH   VALUSR2A                                                         
         MVC   UIDLLON,CTACTLLO    Save off date                                
         MVC   CTACTLLO,TODAY3     Last log in year/month binary                
         B     VALUSR2N                                                         
*                                                                               
VALUSR2A CLI   0(R4),X'02'         ID(NUMBER) ELEMENT                           
         BNE   VALUSR2B                                                         
         MVC   USERNO,2(R4)                                                     
         B     VALUSR2N                                                         
*                                                                               
VALUSR2B CLI   0(R4),CTAGYELQ      AGENCY ELEMENT                               
         BNE   VALUSR2E                                                         
         USING CTAGYD,R4                                                        
         MVC   USERALPH,CTAGYID                                                 
         MVC   AGYSEC,CTAGYID                                                   
         MVC   CTLANG,CTAGYLNG                                                  
         MVC   IDTYPE,CTAGYIDT                                                  
         B     VALUSR2N                                                         
*                                                                               
VALUSR2E CLI   0(R4),X'07'         ID OPTIONS ELEMENT                           
         BNE   VALUSR2F                                                         
         CLI   1(R4),3                                                          
         BL    VALUSR2N                                                         
         IC    R1,1(R4)                                                         
         CLI   1(R4),L'IDOPTS+2                                                 
         BNH   *+8                                                              
         LA    R1,L'IDOPTS+2                                                    
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     VALUSR2N                                                         
         MVC   IDOPTS(0),2(R4)     SAVE ID OPTIONS IN W/S                       
         DROP  R4                                                               
*                                                                               
VALUSR2F CLI   0(R4),CTTOUELQ      TIME OUT ELEMENT                             
         BNE   VALUSR2N                                                         
         USING CTTOUD,R4                                                        
         MVC   TOUTUID,CTTOUADV    SAVE USERID LEVEL ADV TIME OUT               
         DROP  R4                                                               
         B     VALUSR2N                                                         
*                                                                               
VALUSR2N IC    R1,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R1                                                            
         B     VALUSR2                                                          
*                                                                               
VALUSR2X TM    SOXFL,SOXDDS        TEST DDS TERMINAL                            
         BO    VALUSR4                                                          
*&&US*&& CLC   USERNO,=H'17'       NO-CANT LOG ON WITH SJR                      
*&&US*&& BE    EIID                                                             
*                                                                               
VALUSR4  L     R4,AACCREC          READ ACCESS RECORD & SET CTRY/CURR           
         USING CT5REC,R4           BUILD KEY OF ACCESS RECORD                   
         MVI   DDSACC,0            CLEAR DDS AGENCY ACCESS LEVEL FLAGS          
         MVI   AGYOPTS,0           CLEAR AGENCY OPTIONS FLAG                    
         XC    MEDAGY,MEDAGY       CLEAR UK MEDIA AGENCY ALPHA ID               
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         OC    CT5KALPH,USERALPH                                                
         BZ    EIID                                                             
         BAS   RE,CTREAD           READ ACCESS RECORD                           
         BNE   EIID                                                             
         LA    R4,CT5DATA                                                       
         SR    R1,R1                                                            
VALUSR4A CLI   0(R4),0                                                          
         BE    VALUSRX                                                          
         CLI   0(R4),CTAGDELQ      AGENCY GROUP DETAILS ELEMENT                 
         BE    VALUSR4C                                                         
         CLI   0(R4),CTSEAELQ                                                   
         BE    VALUSR4D                                                         
         CLI   0(R4),CTTOUELQ                                                   
         BE    VALUSR4E                                                         
*&&DO*&& CLI   0(R4),CTMAGELQ                                                   
*&&DO*&& BE    VALUSR4F                                                         
         CLI   0(R4),CTAADELQ                                                   
         BE    VALUSR4G                                                         
         CLI   0(R4),CTACCELQ      X'1C' AGENCY ACCESS DATES                    
         BE    VALUSR4H                                                         
VALUSR4B IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALUSR4A                                                         
*                                                                               
         USING CTAGDD,R4                                                        
VALUSR4C MVC   AGYOPTS,CTAGOPTS    SAVE AGENCY OPTIONS FLAG                     
         TM    CTAGDDA,CTAGDDAN    EXIT IF DDS TERMINAL WITH NO ACCESS          
         BZ    *+12                                                             
         TM    SOXFL,SOXDDS                                                     
         BO    EDNC                                                             
         MVC   DDSACC,CTAGDDA      SAVE DDS AGENCY ACCESS LEVEL FLAGS           
         CLI   CTAGDLEN,CTAGDL2Q   TEST EXTENDED LENGTH                         
         BL    VALUSR4B                                                         
         MVC   AGYCTRY,CTAGDCTY    SET AGENCY COUNTRY & CURRENCY                
         MVC   AGYCURR,CTAGDCUR                                                 
         B     VALUSR4B                                                         
*                                                                               
         USING CTSEAD,R4                                                        
VALUSR4D MVC   AGYSEC,CTSEAAID     SAVE SECURITY AGENCY ID                      
         B     VALUSR4B                                                         
*                                                                               
         USING CTTOUD,R4                                                        
VALUSR4E MVC   TOUTAGY,CTTOUADV    SAVE AGENCY LEVEL ADV TIME OUT               
         B     VALUSR4B                                                         
*&&DO                                                                           
         USING CTMAGD,R4                                                        
VALUSR4F MVC   MEDAGY,CTMAGAID     SAVE UK MEDIA AGENCY ID                      
         B     VALUSR4B                                                         
*&&                                                                             
         USING CTAADD,R4                                                        
VALUSR4G MVC   SERVTEAM,CTAACSTN   CLIENT SERVICE TEAM / INACTIVE FLAG          
         TM    CTAADFLG,CTAADDAB   TEST DATA BUILD MODE                         
         BZ    VALUSR4B                                                         
         TM    SYSIDTY,FACITST     TEST IF THIS A TEST FACPAK                   
         BZ    VALUSR4B                                                         
         TM    SYSIDTY,FACIFQA     TEST IF THIS IS FQA                          
         BO    VALUSR4B                                                         
         OI    DATABLD,X'01'       SET DATA BUILD MODE                          
         B     VALUSR4B                                                         
*                                                                               
         USING CTACCD,R4                                                        
VALUSR4H ST    R4,ACTACC           SAVE A(ACCESS DATE ELEMENT)                  
         B     VALUSR4B                                                         
*                                                                               
VALUSRX  B     VALSAC              VALIDATE AGENCY SECURITY RECORD              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY SECURITY RECORD AND READ PERSON ID RECORD IF INPUT  *         
***********************************************************************         
VALSAC   BRAS  RE,CHKSAC           READ AGY SEC REC AND PERSON REC              
         BE    VALSYS                                                           
         LLC   RE,ERRNUM           GET ERROR NUMBER                             
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,VALSACX(RE)                                                   
         BR    RE                                                               
VALSACX  B     EIID                                                             
         B     EMPI                                                             
         B     EIPI                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SYSTEM FIELD                                               *         
***********************************************************************         
VALSYS   GOTO1 FVAL,CNTSYSH                                                     
         BNZ   VALSYS1                                                          
         BAS   RE,LOADSYS          LOAD FROM VALUE IN UTL                       
         BNE   EMIF                                                             
VALSYS1  MVI   FERN,X'FF'          VALIDATE SYSTEM FIELD OPTIONS                
         BRAS  RE,VALSOP           GO TO VALIDATION ROUTINE                     
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         TM    XPVERFLG,X'20'      TEST INVALID PCP VERSION                     
         BO    EVNV                                                             
         L     RE,ASYSLST                                                       
         USING SYSLSTD,RE          RE=A(SYSTEM TABLE)                           
         LA    RE,SYSLLEN(RE)      BUMP PAST SERVICE ENTRY                      
         LLC   R1,SYSNLEN                                                       
         BCTR  R1,0                                                             
VALSYS2  CLI   SYSLNUM,0                                                        
         BE    EISE                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),SYSNAME                                              
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)                                                   
         B     VALSYS2                                                          
         MVC   OVSYSN,SYSLSHRT     SET OVERLAY SYSTEM NAME & NUMBER             
         MVC   OVSYS,SYSLNUM                                                    
         DROP  RE                                                               
*                                                                               
         L     R4,AUIDREC          GET SYSTEM SE NUM FROM ID RECORD             
         LA    R1,AUIDSYS          SAVE VALID ID SYSTEM ELEMENT                 
         BAS   RE,GETSYS                                                        
         BZ    EISE                                                             
         USING CTSYSD,RF                                                        
         MVC   SESYSN,CTSYSSE                                                   
*                                                                               
         TM    CTSYSIND,CTSYSIYP   SYSTEM REQUIRES A PASSWORD                   
         BNZ   VALSYS4                                                          
         TM    CTSYSIND,CTSYSINP   SYSTEM DOES NOT REQUIRE PASSWORD             
         BNZ   VALSYS6                                                          
         TM    IDOFL1,X'80'        USER ID REQUIRES PASSWORD                    
         BZ    VALSYS6                                                          
VALSYS4  OI    PWFLAG,PWREQD       SET P/W REQUIRED                             
         DROP  RF                                                               
*                                                                               
VALSYS6  L     R3,VSELIST          FIND SELIST ENTRY FOR SESYS                  
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
         CLC   SESYS,SESYSN                                                     
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                                                             
         ST    R3,ASENTRY          SAVE A(SELIST ENTRY)                         
         MVC   APGMLIST,SEPGMS     SAVE A(PROGRAM LIST)                         
*                                                                               
         TM    TSTAT8,TST8ASWP     AUTO SWAP NO RESTRICTIONS                    
         BNZ   VALSYS7                                                          
         TM    SEIND,SEINOP        CHECK SYSTEM IS OPERATIONAL                  
         BNZ   ESNO                                                             
         TM    SEIND,SEISTRT                                                    
         BZ    ESNO                                                             
         B     VALSYS7A                                                         
*                                                                               
VALSYS7  TM    SEIND,SEINOP        CHECK SYSTEM IS OPERATIONAL                  
         BNZ   *+12                                                             
         TM    SEIND,SEISTRT                                                    
         BNZ   VALSYS7A                                                         
         OI    ERRFLAG,X'80'       FLAG SYS NOT OP                              
VALSYS7A TM    SEIND,SEIRONLY      CHECK SYSTEM IS READ-ONLY                    
         BZ    VALSYS7B                                                         
         OI    SOXFL2,SOXROSYS                                                  
VALSYS7B TM    SEIND,SEISETRO      TEST SET TO READ ONLY                        
         BZ    VALSYS8                                                          
         OI    SOXFL2,SOXROSYS                                                  
         B     VALSYS8             *NOP* BROADCAST                              
         MVI   TBRSYS,1            NOTIFY READ ONLY                             
         OI    TSTAT2,TSTATBCP                                                  
*                                                                               
VALSYS8  B     VALPRG                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGRAM FIELD (PROGRAM(=MAPID,KEYWORD=VALUE ETC.))         *         
***********************************************************************         
VALPRG   GOTO1 FVAL,CNTPGMH                                                     
         BNZ   VALPRG1                                                          
         BAS   RE,LOADPRG          LOAD FROM VALUE IN UTL                       
         BNE   EMIF                                                             
*                                                                               
VALPRG1  CLI   FLD,C'?'            HELP INVOLKED?                               
         BE    SRPROG              OK HELP THEM OUT                             
*                                                                               
         BRAS  RE,NEWTKT           VALIDATE NEW TICKET NUMBER                   
*                                                                               
         XC    OPTIONS,OPTIONS     CLEAR SAVE AREAS                             
         XC    PROGINFO,PROGINFO                                                
         XC    AUTHVAL,AUTHVAL                                                  
         XC    AUTHFNDX,AUTHFNDX                                                
         MVI   CTTEST,0                                                         
         GOTO1 VSCANNER,DMCB,FLDH,(10,SCANBLK),C',=,='                          
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R6,SCANBLK          R6=A(SCAN BLOCK ENTRY)                       
*                                                                               
VALPRG2  CLC   FNDX,NLINES                                                      
         BH    VALPRG18                                                         
         CLI   0(R6),0             L'FIRST HALF                                 
         BE    EIIF                                                             
*                                                                               
         L     RE,=A(OPTTAB)       FIND KEYWORD IN TABLE                        
         USING OPTD,RE                                                          
         A     RE,RELO                                                          
         CLI   FNDX,1                                                           
         BE    VALPRG10            FIRST ENTRY MUST BE PROGRAM                  
         LA    RE,OPTLNQ(RE)                                                    
         LLC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
*                                                                               
VALPRG4  CLI   0(RE),0             END OF TABLE                                 
         BE    EIIF                                                             
         CLC   0(1,R6),OPTMINKL    CHECK MIN KEYWORD LENGTH                     
         BL    VALPRG6                                                          
         CLC   0(1,R6),OPTMAXKL    CHECK MAX KEYWORD LENGTH                     
         BH    VALPRG6                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OPTKYW(0),12(R6)                                                 
         BE    VALPRG8             FOUND IT                                     
*                                                                               
VALPRG6  LA    RE,OPTLNQ(RE)       TRY NEXT                                     
         B     VALPRG4                                                          
*                                                                               
VALPRG8  CLC   1(1,R6),OPTMINDL    CHECK MIN DATA LENGTH                        
         BL    EIIF                                                             
         CLC   1(1,R6),OPTMAXDL    CHECK MAX DATA LENGTH                        
         BH    EIIF                                                             
*                                                                               
VALPRG10 TM    TSTAT,TSTATDDS      CHECK FOR DDS ONLY KEYWORD                   
         BNZ   VALPRG14                                                         
         TM    OPTIND1,OPTIDDS     DDS ONLY OPTION?                             
         BZ    VALPRG14                                                         
*                                                                               
         TM    SYSIDTY,FACITST+FACIFQA TEST IF THIS IS A TST/FQA                
         BZ    EIIF                NO-NOT ALLOWED                               
         TM    TSTAT1,TSTATWEB                                                  
         BZ    EIIF                ALLOW WEB TERMINALS ON TST AS DDS            
*                                                                               
VALPRG14 MVC   DUB(4),OPTNUM       DUB=OPTION NUMBER/A(ROUTINE)                 
         NI    DUB,X'7F'                                                        
         LLC   R1,DUB                                                           
         LA    R1,OPTIONS-1(R1)                                                 
         CLI   0(R1),0             CHECK NOT PREVIOUSLY INPUT                   
         BNE   EDIF                                                             
         DROP  RE                                                               
*                                                                               
         OI    0(R1),X'80'                                                      
         L     RF,DUB                                                           
         LA    RF,0(RF)                                                         
         A     RF,RELO             RELOCATE ADCON                               
         MVI   FERN,X'FF'                                                       
         BASR  RE,RF               GO TO VALIDATION ROUTINE                     
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
*                                                                               
         LA    R6,32(R6)           BUMP TO NEXT ENTRY                           
         LLC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     VALPRG2                                                          
*                                                                               
VALPRG18 MVI   FNDX,0                                                           
         TM    XPVERFLG,X'20'      TEST INVALID PCP VERSION                     
         BO    EVNV                                                             
         BAS   RE,GETPSEC          GET ACCESS RECORD SECURITY FLAG              
*                                                                               
VALPRG20 BRAS  RE,CHKIT            CHECK USERID/PID/SYS/PRG COMPATIBLE          
         BNE   EIPI                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PASSWORD FIELD                                             *         
***********************************************************************         
VALPWD   XC    PASSINPT,PASSINPT   CLEAR PASSWORD INPUT DATA                    
         MVC   PASSLEN,CNTPWDH+5   SET ACTUAL INPUT LENGTH                      
         TM    PWFLAG,PWPVAL       TEST PASSWORD HAS BEEN PRE-VALIDATED         
         BZ    VALPWD0                                                          
         GOTO1 FVAL,CNTPWDH        YES-POINT TO PASSWORD FIELD AND SKIP         
         B     VALPWDX                                                          
*                                                                               
VALPWD0  TM    TSTAT7,TST7PS1+TST7PS2 TEST IF CHANGING PASSWORD                 
         BZ    VALPWD0B                                                         
         CLC   PIDSEC,TIDBITS      MUST BE SAME SECURITY AGENCY                 
         BNE   VALPWD0A                                                         
         OC    PIDNUM,PIDNUM       GET PERSON IF NOT SET                        
         BNZ   *+8                                                              
         BRAS  RE,READPID                                                       
         CLC   PIDNUM,TIDBITS+2    MUST BE SAME PERSON                          
         BE    VALPWD0B                                                         
VALPWD0A NI    TSTAT7,X'FF'-TST7PS1-TST7PS2 TURN OFF PASSWORD CHG STATE         
*                                                                               
VALPWD0B TM    SOXFL,SOXDDS        TEST THIS IS A DDS TERMINAL                  
         BZ    VALPWD1                                                          
         TM    DDSACC,CTAGDDAY     TEST DDS SECURITY OVERRIDE                   
         BZ    VALPWD1                                                          
         CLI   PSECFLAG,C'Y'       TEST NEW SECURITY PROGRAM                    
         BNE   VALTRM                                                           
         TM    PWFLAG,PWREQD       TEST IF PASSWORD REQUIRED                    
         BZ    EIIF                                                             
         MVC   FLD,=CL10'DDS'      FORCE SPECIAL DDS PASSWORD                   
         MVI   PASSLEN,3                                                        
         B     VALPWD4                                                          
*                                                                               
VALPWD1  GOTO1 FVAL,CNTPWDH                                                     
         BRAS  RE,SCRIPPWD                                                      
         BNE   EIIF                                                             
         CLI   FLDH+5,MPWDL        TEST MAXIMUM PASSWORD LENGTH                 
         BNH   *+12                                                             
         CLI   PIDREQD,C'Y'        CHECK THIS LATER FOR PPS AGENCY              
         BNE   VPWDER1                                                          
         CLI   FLDH+5,0                                                         
         BNE   VALPWD2                                                          
         BAS   RE,LOADPWD          TRY TO LOAD PASSWORD FROM UTL VALUE          
         BE    VALPWD1A                                                         
*&&UK                                                                           
         L     RF,VSSB             CHECK FOR LP2 SYSTEM                         
         TM    SSBSTAT5-SSBD(RF),SSB5LP2                                        
         BZ    VALPWD10                                                         
         CLI   PERSONID,0          WAS PID INPUT IN USERID FIELD                
         BE    VALPWD10                                                         
         BAS   RE,GETPID           YES-GET IT                                   
         MVC   CNTPWD(MPWDL),PIDPWD                                             
         MVI   CNTPWDH+5,MPWDL     SET PASSWORD                                 
         B     VALPWD1             AND TRY AGAIN                                
*&&                                                                             
VALPWD10 CLI   PSECFLAG,C'Y'       NEW SECURITY REQUIRES PASSWORD               
         BE    EMIFPWD                                                          
*&&UK*&& TM    SYSIDTY,FACITST     BYPASS OLD SECURITY ON TEST SYSTEMS          
*&&UK*&& BO    VALTRM                                                           
         TM    PWFLAG,PWREQD       TEST PASSWORD REQUIRED FLAG SET              
         BZ    VALPWDX                                                          
         B     EMIFPWD                                                          
*                                                                               
VALPWD1A CLI   PSECFLAG,C'Y'       NEW SECURITY REQUIRES PASSWORD               
         BE    VALPWD2                                                          
*&&UK*&& TM    SYSIDTY,FACITST     BYPASS OLD SECURITY ON TEST SYSTEMS          
*&&UK*&& BO    VALTRM                                                           
         TM    PWFLAG,PWREQD       TEST PASSWORD REQUIRED FLAG SET              
         BZ    VALPWDX                                                          
*                                                                               
VALPWD2  L     RF,=V(PWDVAL)       PROCESS/VALIDATE INPUT PASSWORD              
         A     RF,RELO                                                          
         XC    DMCB(20),DMCB                                                    
         LA    RE,FLD                                                           
         ST    RE,DMCB                                                          
         MVC   DMCB+0(1),FLDH+5                                                 
         LA    RE,WORK             RETURN TEXT AREA                             
         ST    RE,DMCB+4                                                        
         XC    0(MPWDL,RE),0(RE)                                                
         MVC   DMCB+4(1),PRULENUM                                               
         MVC   DMCB+8(4),VSSB                                                   
         MVI   DMCB+8,2            PROCESS INPUT PASSWORD                       
         LA    R0,9                DEBUG                                        
*                                                                               
         TM    TSTAT7,TST7PS1+TST7PS2                                           
         BZ    VALPWD2A                                                         
         TM    TSTAT7,TST7PS2      DONT VALIDATE IF 2ND PASS                    
         BO    VALPWD2A                                                         
         MVI   DMCB+8,3            VALIDATE INPUT PASSWORD ON 1ST PASS          
         LA    R0,10               DEBUG                                        
*                                                                               
VALPWD2A MVC   BYTE,DMCB+8         RF=V(PWDVAL)                                 
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   BYTE,2              TEST PROCESS CALL                            
         BE    VALPWD3             YES                                          
*                                                                               
VALPWD2B SR    RE,RE               VALIDATE CALL RETURNS E1-EF=225-239          
         ICM   RE,1,8(R1)                                                       
         BZ    VALPWD3             NO ERRORS                                    
*                                                                               
VALPWD2C CHI   RE,231              ERROR RETURN E2-E7=226-231                   
         BH    *+12                                                             
         AHI   RE,116              ERROR MESSAGES SRV=342-347                   
         B     VALPWD2E                                                         
*                                                                               
VALPWD2D ICM   RE,1,PRULENUM       PASSWORD RULE  NUM 001-012                   
         BZ    *+12                                                             
         AHI   RE,360              RULE MESSAGES  SRV=361-372                   
         B     VALPWD2E                                                         
         ICM   RE,1,8(R1)          IF NO RULE USE SRV=348-355                   
         AHI   RE,116                                                           
*                                                                               
VALPWD2E STH   RE,ACTERR#          SET ACTUAL ERROR NUMBER                      
         B     EINPPV                                                           
*                                                                               
VALPWD3  TM    PWFLAG,PWREQD       TEST REQUIRES PASSWORD                       
         BO    VALPWD4                                                          
         MVC   PASSWD,FLD          SAVE PASSWORD IN W/S                         
         MVC   PASSWDU,WORK                                                     
         B     VALPWDX                                                          
VALPWD4  MVC   SECCODE,FLD         SAVE SECRET CODE IN W/S                      
         MVC   SECCODU,WORK                                                     
         TM    SOXFL,SOXDDS        ALLOW SHORTY FOR DDS TERMINALS               
         BZ    VALPWD4A                                                         
         CLC   SECCODE(8),SHORTY                                                
         BNE   VALPWD4A                                                         
         MVC   SECCODE,SHORTPWD                                                 
         MVC   SECCODU,SECCODE                                                  
VALPWD4A BAS   RE,DDSPWD           TEST DDS PASSWORD INPUT                      
         BZ    VALPWDX                                                          
         TM    SOXFL,SOXSCRP       OK FOR SCRIPTS                               
         BO    VALPWDX                                                          
         TM    SOXFL1,SOXDDPSC+SOXDDPST+SOXDDPTS+SOXDDPCM                       
         BNZ   VALPWDX                                                          
*&&UK                                                                           
         L     RF,VSSB             CHECK FOR LP2 SYSTEM                         
         TM    SSBSTAT5-SSBD(RF),SSB5LP2                                        
         BO    VALPWDX             YES-DDS PASSWORD IS OK                       
*&&                                                                             
         TM    SOXFL,SOXDDS        TEST THIS IS A DDS TERMINAL                  
         LA    R0,11               DEBUG                                        
         BZ    EISC                NO-ERROR                                     
         TM    DDSACC,CTAGDDAP     TEST DDS AGENCY PASSWORD ACCESS OK           
         BO    EDPN                NO-ERROR                                     
*                                                                               
VALPWD5  TM    SOXFL,SOXTST        DDS PASSWORD ON A TEST SYSTEM                
         BZ    VALPWD6                                                          
         B     VALPWDX                                                          
*                                                                               
VALPWD6  EQU   *                   DDS PASSWORD ON A PROD SYSTEM                
         LA    R0,12               DEBUG                                        
*&&US*&& B     EISC                                                             
*&&UK*&& B     EISC                WAS B VALPWDX                                
*                                                                               
VALPWDX  B     VALPAS              VALIDATE PASSWORD AUTH RECORDS               
*                                                                               
VPWDER1  EQU   *                                                                
         XC    CNTPWD,CNTPWD       CLEAR PASSWORD FIELD                         
         MVI   CNTPWDH+5,0                                                      
         OI    CNTPWDH+6,X'80'                                                  
         B     EFTL                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PASSWORD AUTHORISATION RECORDS                             *         
***********************************************************************         
VALPAS   TM    PWFLAG,PWPVAL       TEST P/W PRE-VALIDATED                       
         BO    VALPAS0             YES                                          
         OC    SECCODE,SECCODE     NO-TEST P/W INPUT                            
         BZ    VALPASX                                                          
         CLI   PIDREQD,C'N'        IS A PID REQUIRED                            
         BE    VALPAS1                                                          
         CLI   PERSONID,0          TEST IF PID INPUT                            
         BNZ   VALPAS0                                                          
         CLI   AUTOCON,C'N'                                                     
         BE    VPASER1                                                          
         MVC   PERSONID,SECCODE                                                 
         BAS   RE,READPID                                                       
         BNE   VPASER2                                                          
*                                                                               
VALPAS0  MVC   SECNO,PIDNUM                                                     
*                                                                               
VALPAS1  L     R4,ASECREC                                                       
         USING CT0REC,R4                                                        
         XC    CT0KEY,CT0KEY       BUILD AUTH(ALPHA) KEY                        
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,PIDSEC                                                   
         TM    PWFLAG,PWPVAL       USE PIDNUM IF P/W PRE-VALIDATED              
         BO    VALPAS1A                                                         
         CLI   PIDREQD,C'N'                                                     
         BNE   *+14                                                             
         MVC   CT0KCODE,SECCODE                                                 
         B     *+10                                                             
VALPAS1A MVC   CT0KNUM,SECNO                                                    
         BAS   RE,CTREAD                                                        
         LA    R0,13               DEBUG                                        
         BNE   EISC                                                             
         BRAS  RE,VALID            ENSURE ID IS COMPATIBLE WITH AUTH            
         BE    VALPAS2                                                          
         MVI   FNDX,0                                                           
         LA    R1,CNTIDH                                                        
         ST    R1,FADR                                                          
         B     EINP                ERROR EXIT ID/PASSWORD INCOMPATIBLE          
*                                                                               
VALPAS2  CLI   PIDREQD,C'N'        GOT VALPER IF PERSONID/PASSWORD              
         BNE   VALPER                                                           
         TM    PWFLAG,PWPVAL       TEST IF P/W PRE-VALIDATED                    
         BO    VALPER                                                           
         LA    R4,CT0DATA-CT0REC(R4)                                            
         SR    R1,R1                                                            
VALPAS4  CLI   0(R4),0             FIND AUTH(NUMERIC) AND PID ELEMENTS          
         JE    *+2                                                              
         CLC   0(2,R4),=X'0304'                                                 
         BE    VALPAS6                                                          
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALPAS4                                                          
VALPAS6  MVC   SECNO,2(R4)         SAVE AUTH NUMBER IN W/S                      
         BAS   RE,VALPEF           VALIDATE PASSWD REC EFFECTIVE DATE           
         BNE   EPED                                                             
         TM    PASSDDS,PASSDPID+PASSDPWD    CHECK DDS PASSWORD OVERIDE          
         BNZ   VALPASX                                                          
         CLI   PONCE,C'Y'                                                       
         BNE   VALPASX                                                          
         BRAS  RE,VALPONCE         VALIDATE CONNECT ONCE WITH SAME PWD          
         BNE   ECON                                                             
VALPASX  B     VALPER              VALIDATE PERSON RECORD                       
         DROP  R4                                                               
VPASER1  LA    R1,CNTIDH                                                        
         ST    R1,FADR                                                          
         MVI   FNDX,2                                                           
         B     EMPI                                                             
VPASER2  LA    R1,CNTIDH                                                        
         ST    R1,FADR                                                          
         MVI   FNDX,2                                                           
         B     EIPI                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SECURITY ACCESS PERSONAL ID RECORD                         *         
***********************************************************************         
VALPER   TM    PASSDDS,PASSDPWD    CHECK DDS PASSWORD OVERIDE                   
         BNZ   VALPERX                                                          
         TM    PWFLAG,PWPVAL       TEST PASSWORD PRE-VALIDATED                  
         BZ    *+12                                                             
         CLI   PERSONID,0          YES-VALIDATE PERSON IF INPUT                 
         BNE   VALPER4                                                          
         OC    SECNO,SECNO         TEST IF PASSWORD NUMBER SET                  
         BZ    VALPEREX                                                         
         XC    AGROUPNO,AGROUPNO                                                
         XC    LGROUPNO,LGROUPNO                                                
         CLI   PERSONID,0          WAS PID INPUT IN USERID FIELD                
         BNE   VALPER4                                                          
         BAS   RE,GETPID           NO-GET ID FROM PASSWORD REC                  
         BNE   VALPEREX                                                         
         BAS   RE,READPID          READ PERSONAL ID RECORD INTO BUFFER          
         LA    R0,1                DEBUG                                        
         BNE   EIPP                                                             
*                                                                               
VALPER4  L     R4,APERREC                                                       
         USING SAPEREC,R4                                                       
         LA    R3,SAPEDATA                                                      
         SR    R1,R1                                                            
VALPER6  CLI   0(R3),0                                                          
         BE    VALPER50                                                         
         CLI   0(R3),SALLOELQ                                                   
         BE    VALPER9                                                          
         CLI   0(R3),SAAGCELQ                                                   
         BE    VALPER10                                                         
         CLI   0(R3),SALACELQ                                                   
         BE    VALPER20                                                         
         CLI   0(R3),SAPERELQ                                                   
         BE    VALPER30                                                         
         CLI   0(R3),SAPWDELQ                                                   
         BE    VALPER40                                                         
VALPER8  IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VALPER6                                                          
*                                                                               
         USING SALLOD,R3                                                        
VALPER9  LR    RF,R3                                                            
         S     RF,APERREC                                                       
         STH   RF,PIDLLOEL         SAVE DISPLACEMENT TO SALLOD ELEMENT          
         MVC   PIDLLO,SALLODT      SAVE LAST LOGON DATE                         
         CLI   SALLOLEN,SALLOLNQ                                                
         BL    VALPER8                                                          
         MVC   PIDLLOT,SALLOTME    SAVE TIME OF LAST PASSWORD ERROR             
         MVC   PIDLLOE,SALLOCNT    SAVE PASSWORD ERROR COUNT                    
         MVC   PIDLLFL,SALLOFLG    SAVE PASSWORD STATUS FLAG                    
         B     VALPER8                                                          
*                                                                               
         USING SAAGCD,R3                                                        
VALPER10 EQU   *                                                                
         CLI   PSECFLAG,C'Y'       IF PROGRAM CONVERTED TO NEW SECURITY         
         BNE   VALPER8                                                          
         MVC   AGROUPNO,SAAGCNUM   SAVE ACCESS GROUP NUMBER                     
         B     VALPER8                                                          
*                                                                               
         USING SALACD,R3                                                        
VALPER20 MVC   LGROUPNO,SALACNUM   SAVE DATA ACCESS GROUP CODE                  
         B     VALPER8                                                          
*                                                                               
         USING SAPERD,R3                                                        
VALPER30 MVC   PWDCNTL,SAPERPCN    SAVE PASSWORD CONTROL VALUE                  
         OC    SAPERDTE,SAPERDTE   CHECK LEAVING/TERMINATION DATE               
         BZ    VALPER32                                                         
         CLC   SAPERDTE,TODAYB     ERROR IF < TODAY                             
         LA    R0,2                DEBUG                                        
         BNH   EIPP                                                             
VALPER32 OC    SAPERDHI,SAPERDHI   CHECK HIRE DATE                              
         BZ    VALPER8                                                          
         CLC   SAPERDHI,TODAYB                                                  
         LA    R0,3                DEBUG                                        
         BH    EIPP                ERROR IF > TODAY                             
         B     VALPER8                                                          
*                                                                               
         USING SAPWDD,R3                                                        
VALPER40 EQU   *                                                                
         CLI   PSECFLAG,C'Y'       IF PROGRAM CONVERTED TO NEW SECURITY         
         BNE   VALPER8                                                          
         TM    PWFLAG,PWPVAL       TEST PASSWORD HAS BEEN PRE-VALIDATED         
         BO    VALPER42                                                         
         CLC   SECNO,SAPWDNUM      COMPARE PASSWORD POINTER ELEMENT             
         LA    R0,4                DEBUG                                        
         BNE   EIPP                MUST MATCH PASSWORD NUMBER                   
VALPER42 OC    PIDNUM,PIDNUM                                                    
         BNZ   VALPER8             GET PERSON NUMBER FROM ELEMENT               
         TM    SOXFL,SOXDDS        UNLESS DDS TERMINAL                          
         BZ    *+14                                                             
         MVC   PIDNUM,TPERSON                                                   
         B     VALPER8                                                          
         MVC   PIDNUM,SAPWDNUM                                                  
         MVC   PIDPWD,SAPWDCOD     PID PASSWORD                                 
         MVC   PIDPWDU,PIDPWD      PID PASSWORD COPY                            
         LA    RE,PIDPWDU+MPWDL-1                                               
         LA    RF,MPWDL                                                         
         CLI   0(RE),C' '          FIND LENGTH OF PID PASSWORD COPY             
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         STC   RF,PIDPWDL                                                       
         B     VALPER8                                                          
*                                                                               
VALPER50 CLI   PONCE,C'Y'          TEST IF PERSON LOGON ONLY ONCE               
         BNE   VALPER60                                                         
         CLI   PIDENTER,C'Y'       TEST IF PID ENTERED THIS TIME                
         BNE   VALPER60                                                         
         BRAS  RE,VALPONCE         VALIDATE CONNECT ONCE WITH SAME PID          
         BE    VALPER60                                                         
         MVI   FNDX,2                                                           
         B     ESPI                PERSON ID ALREADY LOGGED ON                  
*                                                                               
VALPER60 CLI   PSECFLAG,C'Y'       IF PROGRAM CONVERTED TO NEW SECURITY         
         BNE   VALPER70                                                         
         OC    AGROUPNO,AGROUPNO   CHECK NON ZERO ACCESS GROUP NUMBER           
         LA    R0,5                DEBUG                                        
*&&US*&& BZ    EIPP                ERROR IF NULL                                
*&&UK                                                                           
         BNZ   VALPER63                                                         
         CLC   PERSONID,=C'BRANDSEC'                                            
         BNE   EIPP                                                             
         CLI   OVSYS,X'06'         BRANDSEC PID VALID FOR ACC/BRAND             
         BNE   VALPER62                                                         
         CLI   PROGNUM,X'24'                                                    
         BNE   EIPP                                                             
         B     VALPER70                                                         
VALPER62 CLI   OVSYS,X'04'         BRANDSEC PID VALID FOR MED/LNK               
         BNE   EIPP                                                             
         CLI   PROGNUM,X'1B'                                                    
         BNE   EIPP                                                             
         B     VALPER70                                                         
VALPER63 CLC   PERSONID,=C'BRANDSEC' IF NOT NULL ENSURE NOT BRANDSEC            
         BE    EIPP                  WHICH MUST NOT HAVE A GROUP                
*&&                                                                             
VALPER70 OC    LGROUPNO,LGROUPNO   CHECK DATA ACCESS GROUP NUMBER               
         BZ    VALPER80                                                         
         L     R4,ALAGREC          GET DATA ACCESS GROUP RECORD DATA            
         USING SALAREC,R4                                                       
         XC    SALAKEY,SALAKEY     BUILD DATA ACCESS GROUP KEY                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,PIDSEC                                                   
         MVC   SALAAGN,LGROUPNO                                                 
         BAS   RE,CTREAD                                                        
         LA    R0,6                DEBUG                                        
         BNE   EIPP                                                             
         BAS   RE,GETLAS           GET LIMIT ACCESS FOR SYSTEM/PROGRAM          
*                                                                               
VALPER80 CLI   PIDPWDL,0           CONVERT PID PASSWORD COPY TO UPPER           
         BE    VALPERX                                                          
         XC    DMCB(20),DMCB                                                    
         LA    RE,PIDPWDU                                                       
         ST    RE,DMCB                                                          
         MVC   DMCB+0(1),PIDPWDL                                                
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
         XC    0(MPWDL,RE),0(RE)                                                
         MVI   DMCB+4,0            RULE#0 TO CONVERT TO UPPER                   
         MVC   DMCB+8(4),VSSB                                                   
         MVI   DMCB+8,2            PROCESS ACTION                               
         GOTO1 =V(PWDVAL),DMCB,RR=RELO                                          
         CLC   PIDPWD,PIDPWDU                                                   
         BNE   *+8                                                              
         OI    PIDPWDF,X'02'       SET PIDPWD IS UPPER CASE                     
         B     VALPERX                                                          
*                                                                               
VALPEREX CLI   PSECFLAG,C'Y'       IF PROGRAM CONVERTED TO NEW SECURITY         
         LA    R0,7                DEBUG                                        
         BE    EIPP                THEN EXIT WITH ERROR MESSAGE                 
*                                                                               
VALPERX  EQU   *                                                                
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TERMINAL RECORD AND REMAINING VALIDATION CROSS CHECKING    *         
***********************************************************************         
VALTRM   L     R4,ATRMREC          CLEAR TERMINAL RECORD                        
         USING CTTREC,R4                                                        
         XC    0(30,R4),0(R4)                                                   
         TM    TVIFLAG,TVIVIRT     VIRTUAL TERMINALS HAVE NO RECORDS            
         BO    VALTRM14                                                         
         TM    TTYPE,TTYPEWAP      WEB APPL USES IP SECURITY                    
         BO    VALTRM8                                                          
*                                                                               
         MVI   CTTKTYP,C'T'        BUILD KEY OF TERMINAL RECORD                 
         MVC   CTTKTID,TLUID                                                    
         OC    SECNO,SECNO         TEST IF PERSONAL PASSWORD SECURITY           
         BNZ   VALTRM1                                                          
*                                                                               
         TM    SOXFL,SOXDDS        TEST DDS TERMINAL                            
         BZ    VALTRM1                                                          
         TM    DDSACC,CTAGDDAP     CHECK IF DDS ACCESS RETRICTED                
         BZ    VALTRM1             TO SPECIAL TERMINAL ID 'ID00000T'            
         MVC   CTTKLINE(2),USERALPH                                             
         MVC   CTTKLINE+2(6),=CL6'00000T'                                       
         OC    PASSWD,PASSWD                                                    
         BZ    EPRQ                                                             
*                                                                               
VALTRM1  MVC   CTTKPASS,PASSWD     READ FOR SPECIFIC TERMINAL                   
         BAS   RE,CTREAD                                                        
         BE    VALTRM2             GOT SPECIFIC                                 
*                                                                               
         OC    SECNO,SECNO         NOW TEST IF WE ALLOW A GENERIC               
         BNZ   VALTRM1A                                                         
         TM    SOXFL,SOXDDS                                                     
         BZ    VALTRM1A                                                         
         TM    DDSACC,CTAGDDAP                                                  
         BZ    VALTRM1A                                                         
*                                                                               
         OC    PASSWD,PASSWD                                                    
         BZ    ETND                                                             
         B     EPND                                                             
*                                                                               
VALTRM1A XC    CTTKEY,CTTKEY       BUILD AND TEST GENERIC KEY                   
         MVI   CTTKTYP,C'T'                                                     
         MVC   CTTKTID,TLUID                                                    
         MVC   CTTKTID+4(4),=C'%%%T'                                            
         MVC   CTTKPASS,PASSWD                                                  
         BAS   RE,CTREAD           READ FOR GENERIC TERMINAL                    
         BE    VALTRM2                                                          
*                                                                               
VALTRM1X OC    PASSWD,PASSWD                                                    
         BZ    ETND                                                             
         B     EPND                                                             
*                                                                               
VALTRM2  L     R4,ATRMREC                                                       
         MVC   CTTKTID,TLUID       RESTORE IN CASE GENERIC                      
*                                                                               
         BRAS  RE,VALID            ENSURE ID IS COMPATIBLE WITH TRM             
         BE    VALTRM4                                                          
         MVI   FNDX,0              ERROR IF NOT                                 
         LA    R1,CNTIDH           RESET FIELD ADDRESS TO USER-ID               
         ST    R1,FADR                                                          
         B     EINT                ERROR EXIT ID/TERMINAL INCOMPATIBLE          
VALTRM4  OC    PASSWD,PASSWD                                                    
         BZ    VALTRM8                                                          
         OC    PASSNO,PASSNO                                                    
         BNZ   VALTRM8                                                          
         L     R4,ATRMREC                                                       
         LA    R4,CTTDATA-CTTREC(R4)                                            
         SR    R1,R1                                                            
VALTRM6  CLI   0(R4),0             FIND TERMINAL(NUMERIC) ELEMENT               
         JE    *+2                 IF A PASSWORD WAS INPUT                      
         CLI   0(R4),X'03'                                                      
         BE    *+14                                                             
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALTRM6                                                          
         MVC   PASSNO,2(R4)        SAVE PASSWORD NUMBER IN W/S                  
         DROP  R4                                                               
*                                                                               
VALTRM8  TM    PASSDDS,PASSDPWD    CHECK DDS PASSWORD OVERIDE                   
         BNZ   VALTRM9                                                          
         CLI   PIDREQD,C'N'        TEST PID REQUIRED                            
         BE    VALTRM9                                                          
         CLI   PIDREQD,C'Y'        CHECK PPS AGENCY                             
         BE    VALTRM8A                                                         
         TM    SOXFL2,SOXDDSOV     CHECK DDS PID OVERRIDE                       
         BZ    VALTRM9                                                          
*                                                                               
VALTRM8A CLI   PIDENTER,C'N'       TEST PERSONALID ENTERED FLAG ON              
         BNE   *+14                                                             
         MVC   PEXPDAYS,TPASSEXP   NO-NEED TO REVALIDATE JUST SAVE              
         B     VALTRM9                                                          
         TM    PWFLAG,PWPVAL       TEST PASSWORD HAS BEEN PRE-VALIDATED         
         BZ    VALTRM8D                                                         
         TM    PWFLAG,PWCTSW       YES-TEST SWAP TYPE CONNECT                   
         BZ    VALTRM8C                                                         
         TM    PIDLLFL,SALLOSWP    YES-ONLY VALID IF PERSON REC FLAGGED         
         BO    VALTRM8B                                                         
         TM    SYSIDTY,FACITST     OK FOR TEST SYSTEMS                          
         BO    VALTRM8C                                                         
         LA    R0,8                DEBUG                                        
         B     EIPP                                                             
VALTRM8B OI    PWDSTAT,PSTRESET    RESET PASSWORD STATUS FLAG                   
VALTRM8C L     RE,ASECREC          SET HAVE NOT READ PASSWORD REC               
         XC    0(L'SA0KEY,RE),0(RE)                                             
         B     VALTRM9                                                          
VALTRM8D GOTO1 =A(VALUSPW),RR=RELO CHECK IF PPS PASSWORD HAS EXPIRED            
         LA    R0,8                DEBUG                                        
         BNE   EIPP                                                             
*                                  VALIDATE SYSTEM ELEMENTS IN RECORDS          
VALTRM9  TM    TTYPE,TTYPEWAP      WEB APPL USES IP SECURITY                    
         BO    VALTRM10                                                         
         LA    R1,CNTSYSH                                                       
         ST    R1,FADR                                                          
         CLI   ALLVALID,C'Y'       TEST ALL ID'S VALID                          
         BE    VALTRM10                                                         
         L     R4,ATRMREC          TERMINAL RECORD                              
         LA    R1,ATRMSYS                                                       
         BAS   RE,GETSYS                                                        
         BNZ   VALTRM10                                                         
         OC    SECNO,SECNO         OR IF SECRET CODE NOT REQUIRED               
         BZ    EISE                                                             
*                                                                               
VALTRM10 L     R4,ASECREC          PERSONAL AUTHORIZATION RECORD                
         LA    R1,ASECSYS                                                       
         OC    SECNO,SECNO         TEST SECRET CODE REQUIRED                    
         BZ    *+12                                                             
         BAS   RE,GETSYS                                                        
         BZ    EISE                MUST BE PRESENT                              
         L     R4,ATRMREC          EXTRACT ANY TIME OUT ELEMENT DATA            
         LA    R4,CTTDATA-CTTREC(R4)                                            
         SR    R1,R1                                                            
VALTRM12 CLI   0(R4),0                                                          
         BE    VALTRM14                                                         
         CLI   0(R4),CTTOUELQ                                                   
         BE    *+14                                                             
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALTRM12                                                         
         USING CTTOUD,R4                                                        
         MVC   TOUTTRM,CTTOUADV    SAVE TERMINAL LEVEL ADV TIME OUT             
         DROP  R4                                                               
*                                                                               
VALTRM14 EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* EXTRACT SYSTEM/PROGRAM/FACKPAKID TO CHECK VERSION TABLE OVERRIDES   *         
* IF SSBTBLET IS NONZERO VERSION TABLE IS IN TABSDSP DSPACE           *         
***********************************************************************         
         STAR  CLEAR=YES,ARS=OFF                                                
         TM    CTTEST,TTESTLVL     TTESTLVL EXPLICIT VERSION                    
         BNZ   VALVRSNX                                                         
         MVC   DUB(1),OVSYS                                                     
         MVC   DUB+1(1),PROGNUM                                                 
         MVC   DUB+2(1),SYSIDNO                                                 
*&&US                                                                           
         CLC   DUB(2),=X'0D16'                                                  
         BNE   *+8                                                              
         MVI   DUB,X'02'                                                        
*&&                                                                             
         USING DMSPACED,R1                                                      
VALVRSN1 L     RF,VSSB                                                          
         LAM   AR1,AR1,SSBTBLET-SSBD(RF)                                        
         ICM   R1,15,VVRSNTAB      GET VERSION TABLE ADDRESS                    
         BZ    VALVRSNX                                                         
         SAC   512                                                              
         XR    RE,RE                                                            
         ICM   RE,3,DSPTWIDE                                                    
         ICM   RF,15,DSPTEND                                                    
         XR    R0,R0                                                            
         ICM   R0,7,DSPTFRST+1                                                  
         LR    R1,R0                                                            
         USING VRSNTABD,R1                                                      
*                                                                               
VALVRSN2 OC    VRSNPGM(VRSNLEN),VRSNPGM FIRST EMPTY = EOT                       
         BZ    VALVRSNX                                                         
         CLI   VRSNABC,0           IGNORE ENTRIES WITHOUT A TEST LEVEL          
         BE    VALVRSN6                                                         
         CLC   VRSNPGM(2),DUB      COMPARE OVSYS/PRG                            
         BNE   VALVRSN6                                                         
         CLC   VRSNADV,DUB+2       RIGHT FACPAK                                 
         BE    VALVRSN4                                                         
         CLC   VRSNSEN,SESYSN      RIGHT APPL SYSTEM                            
         BE    VALVRSN4                                                         
         CLC   VRSNAGY,USERALPH    RIGHT AGENCY                                 
         BE    VALVRSN4                                                         
         B     VALVRSN6                                                         
*                                                                               
VALVRSN4 NI    CTTEST,ALLBITS-TTESTLVL                                          
         OC    CTTEST,VRSNABC                                                   
*                                                                               
VALVRSN6 BXLE  R1,RE,VALVRSN2                                                   
                                                                                
***********************************************************************         
* CHECK FOR PRESET TEST LEVEL ON ID AND TERMINAL RECORDS              *         
***********************************************************************         
VALVRSNX REAR  ARS=OFF                                                          
         TM    CTTEST,TTESTLVL+TTESTCIL                                         
         BNZ   VALTRMX                                                          
         GOTO1 VHEXOUT,DMCB,DUB,DUB+2,2,=C'TOG'                                 
         MVI   DUB+2,C'T'                                                       
         L     R4,AUIDREC                                                       
         BAS   RE,SETTEST                                                       
         L     R4,ATRMREC                                                       
         BAS   RE,SETTEST                                                       
VALTRMX  EQU   *                                                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,B'1100',DUB      SYSTEM/PROGRAM                               
         ICM   R0,B'0010',SESYSN   SENUM                                        
         GOTO1 =V(FAVERCHK),DMCB,(R9),(R0),RR=RELO                              
         CLI   DMCB,X'FF'                                                       
         BE    EPNO                PROGRAM IS UNAVAILABLE (VIA =VER)            
*                                                                               
         B     CHKAUT                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK SYSTEM/PROGRAM AUTHORISATION                                  *         
***********************************************************************         
CHKAUT   MVI   FNDX,0                                                           
         LA    R1,CNTPGMH                                                       
         ST    R1,FADR             RESET CURSOR FOR ERROR                       
         MVC   PROGAUTH,=X'FFFF'   SET MAX AUTHORIZATION (FOR DDS)              
         TM    PROGIND,PGMIAOR                                                  
         BZ    CHKAUT2                                                          
         MVI   PROGAUTH,0                                                       
         TM    PROGIND,PGMIAHW                                                  
         BZ    *+8                                                              
         MVI   PROGAUTH+1,0                                                     
*                                                                               
CHKAUT2  TM    PASSDDS,PASSSPID+PASSWSPD  TEST SPECIAL PID FOR $MAD             
         BNZ   CHKAUT2A                   OR PRISMA WEB SPECAIL                 
         TM    PASSDDS,PASSDPWD+PASSDPID  TEST DDS PASSWORD/PID                 
         BNZ   CHKAUT14                                                         
*                                                                               
CHKAUT2A TM    PROGIND4,PGMINAUT   TEST NO PGM AUTHORISATION FLAG               
         BNZ   CHKAUT14                                                         
         TM    PWFLAG,PWREQD       SET AUTH IF PASSWORD INPUT                   
         BO    *+12                                                             
         CLI   ALLVALID,C'Y'                                                    
         BE    CHKAUT14                                                         
         TM    PROGOPT,X'40'       IF P=XX INPUT ASSUME MAX AUTH                
         BO    CHKAUT14                                                         
         LLC   R1,PROGANUM                                                      
         SLL   R1,1                                                             
         CLI   PROGANUM,64         USE ALL VALUE IF GR MAX PROG NUM             
         BNH   *+6                                                              
         SR    R1,R1                                                            
         L     R0,AUIDSYS                                                       
         BAS   RE,SETEXPEL         GET EXTENDED ELEMENT                         
         LA    RE,EXPIDEL          NEW ELEMENT                                  
         USING CTSYSD,RE                                                        
         MVC   PROGAUTH,CTSYSALL   SET PROGRAM AUTH FROM ID REC                 
         CLI   CTSYSLEN,16                                                      
         BNH   *+14                                                             
         LA    RE,CTSYSPGM-2(R1)                                                
         MVC   PROGAUTH,0(RE)                                                   
*                                                                               
*&&UK*&& OC    PROGAUTH,PROGAUTH   PROG=N ON USER MEANS NO ACCESS               
*&&UK*&& BZ    EPNA                                                             
*                                                                               
         OC    SECNO,SECNO         TEST IF PERSONAL PASSWORD                    
         BZ    CHKAUT3A            IF NOT THEN GET TERMINAL AUTH                
         B     CHKAUT3B            IGNORE TERMINAL AUTH                         
*                                                                               
CHKAUT3A ICM   R0,15,ATRMSYS       SET PROGRAM AUTH FROM TERM REC               
         BNZ   *+14                                                             
CHKAUT3B MVC   DUB(2),PROGAUTH     USE ID AUTH IF NO TERM AUTH                  
         B     CHKAUT4                                                          
         BAS   RE,SETEXPEL                                                      
         LA    RE,EXPIDEL                                                       
         MVC   DUB(2),CTSYSALL                                                  
         CLI   CTSYSLEN,16                                                      
         BNH   *+14                                                             
         LA    RE,CTSYSPGM-2(R1)                                                
         MVC   DUB(2),0(RE)                                                     
CHKAUT4  MVC   DUB+2(2),DUB        SET SECRET AUTH EQ TERMINAL AUTH             
         LA    RF,2                                                             
         ICM   R0,15,ASECSYS                                                    
         BZ    CHKAUT6                                                          
         BAS   RE,SETEXPEL                                                      
         LA    RE,EXPIDEL                                                       
         MVC   DUB+2(2),CTSYSALL                                                
         CLI   CTSYSLEN,16                                                      
         BNH   *+14                                                             
         LA    RE,CTSYSPGM-2(R1)                                                
         MVC   DUB+2(2),0(RE)      SET SECRET AUTH FROM AUTH RECORD             
*&&UK                                                                           
         CLI   OVSYS,X'04'         TEST IF UK MEDIA SYSTEM                      
         BE    CHKAUT5A                                                         
         CLI   OVSYS,X'05'         TEST IF UK MPL SYSTEM                        
         BE    CHKAUT5A                                                         
         CLI   OVSYS,X'07'         TEST IF UK FEE SYSTEM                        
         BE    CHKAUT5A                                                         
         B     CHKAUT5B                                                         
CHKAUT5A CLI   PSECFLAG,C'Y'       IGNORE AUTH CODE IF NEW SECURITY             
         BE    CHKAUT14                                                         
CHKAUT5B OC    DUB+2(2),DUB+2      ELSE PROG=N ON AUTH MEANS NO ACCESS          
         BZ    EPNA                                                             
*&&                                                                             
*&&US                                                                           
CHKAUT5C CLI   OVSYS,X'03'         US NETWORK SYSTEM                            
         BNE   CHKAUT5D                                                         
         OC    PROGAUTH,PROGAUTH   PROG=N ON USERID NO ACCESS                   
         BZ    EPNA                                                             
         OC    DUB+2(2),DUB+2      PROG=N ON AUTH NO ACCESS                     
         BZ    EPNA                                                             
         B     CHKAUT6                                                          
CHKAUT5D CLI   OVSYS,X'04'         US PRINT SYSTEM                              
         BNE   CHKAUT5X                                                         
         CLI   PROGNUM,X'18'       MBC PROGRAM                                  
         BNE   CHKAUT5X                                                         
         OC    DUB+2(2),DUB+2      PROG=N ON AUTH NO ACCESS                     
         BZ    EPNA                                                             
         B     CHKAUT6                                                          
CHKAUT5X EQU   *                                                                
*&&                                                                             
CHKAUT6  TM    PROGIND,PGMIAHW     TEST 16 BIT AUTHORIZATIONS                   
         BZ    CHKAUT10                                                         
CHKAUT8  TM    PROGIND,PGMIAOR     TEST IF AUTH IS OR'ED NOT AND'ED             
         BZ    *+14                                                             
         OC    PROGAUTH(2),DUB                                                  
         B     *+10                                                             
         NC    PROGAUTH(2),DUB                                                  
         MVC   DUB(2),DUB+2                                                     
         BCT   RF,CHKAUT8                                                       
         B     CHKAUT12                                                         
CHKAUT10 TM    PROGIND,PGMIAOR     TEST IF AUTH IS OR'ED NOT AND'ED             
         BZ    *+14                                                             
         OC    PROGAUTH(1),DUB                                                  
         B     *+10                                                             
         NC    PROGAUTH(1),DUB     WORK OUT COMPOSITE AUTH                      
         CLC   PROGAUTH+1(1),DUB+1                                              
         BNH   *+10                                                             
         MVC   PROGAUTH+1(1),DUB+1                                              
         MVC   DUB(2),DUB+2        MOVE SECRET AUTH FOR SECOND PASS             
         BCT   RF,CHKAUT10                                                      
CHKAUT12 TM    PROGIND,PGMIAOR     TEST OR'ED NOT AND'ED AUTH                   
         BZ    *+16                                                             
         CLI   PROGAUTH+1,0                                                     
         BE    EPNA                                                             
         B     CHKAUT14                                                         
         OC    PROGAUTH,PROGAUTH   TEST IF PROGRAM AUTHORIZED                   
         BZ    EPNA                                                             
*                                                                               
CHKAUT14 OC    AUTHVAL,AUTHVAL     CHECK FOR OVERIDE IN PROGRAM FIELD           
         BZ    CHKAUTX                                                          
         MVC   FNDX,AUTHFNDX       RESTORE SUB FIELD INDEX                      
         TM    PASSDDS,PASSDPWD+PASSDPID                                        
         BNZ   *+12                                                             
         TM    PWFLAG,PWREQD                                                    
         BO    EIIF                                                             
         MVC   PROGAUTH,AUTHVAL                                                 
*                                                                               
CHKAUTX  EQU   *                                                                
*                                                                               
         BRAS  RE,CHKADV           CHECK CORRECT ADV SYSTEM                     
         BNE   EIIF                                                             
         TM    ERRFLAG,X'80'                                                    
         BO    ESNO                                                             
*&&US                                                                           
CONCK10  BRAS  RE,CESTLAVI         BYE TO SMG. C'EST LA VIE                     
*        BE    CONCKOK                                                          
         BE    CONCK20                                                          
         BL    EPRA                PROGRAM ACCESS NOW RESTRICTED                
         CLI   CONERR,1                                                         
         BE    EPNA                NOT AUTORIZED FOR PROGRAM                    
         B     EIID                ID'S NO LONGER VALID (INVALID)               
*&&                                                                             
CONCK20  NI    TDDSBITS,255-TDDSNOUP                                            
*                                                                               
*&&UK*&& CLC   PIDSEC,=C'#E'       IS THIS DDSEUR                               
*&&US*&& CLC   PIDSEC,=C'#N'       OR EQUIVALENT                                
         JNE   CONCKOK             NO SO OK TO UPDATE                           
*                                                                               
         L     RF,VSSB                                                          
         TM    SSBSYSFL-SSBD(RF),SSBSYTST+SSBSYCSC+SSBSYFQA                     
         JNZ   CONCKOK             TEST SYSTEM ARE OK TO UPDATE                 
         TM    AGYOPTS,TAGCTST     AND MAYBE +TAGCUAT+TAGCTNG                   
         JNZ   CONCKOK             OK TO UPDATE                                 
*                                                                               
         TM    PASSFLAG,PFSCRIPT   SCRIPT PID/PW SET?                           
         BO    CONCKOK             ALLOW SCRIPTS TO CONNECT W/O TICKET          
         TM    TSTAT6,TST6SCRP     TEST SCRIPT BIT SET IN UTL                   
         BO    CONCKOK             ALLOW SCRIPTS TO CONNECT W/O TICKET          
*                                                                               
         CLI   CTICKET,X'FF'       IF TICKET USED                               
         JNE   CONCKOK             OK TO UPDATE                                 
         TM    SOXFL1,SOXPGMSR     IF =PROG CONNECT                             
         JO    CONCK31                                                          
*                                                                               
         CLI   XCTLTYP,0           OR IF XCTL CONNECT                           
         JE    CONCK32                                                          
*                                                                               
* CODE BELOW IS FOR CATCHING SITUATIONS, WHEN XCTLTYP IS NONZERO                
* CODE SKIPPED BY DEFAULT, THE JUMP INSTRUCTION BELOW                           
* NEEDS TO BE PATCHED TO ACTIVATE                                               
         J     CONCK31                                                          
*                                                                               
         CLI   SYSIDNO,X'02'       ADV1                                         
         JNE   CONCK31                                                          
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         LA    RE,WORK+2                                                        
         MVC   0(20,RE),=CL20'SRCON00: XCTLTYP<>0 '                             
         LA    RE,20(RE)                                                        
         LLC   RF,SYSNLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAME                                                  
         LA    RE,2(RE,RF)                                                      
         MVC   0(L'PROGNAME,RE),PROGNAME                                        
         LA    RE,L'PROGNAME+1(RE)                                              
         L     RF,TSIN                                                          
         EDIT  (RF),(6,0(RE)),WRK=WORK+100,FILL=0,ALIGN=LEFT                    
         LA    RE,6(RE)                                                         
         LA    RF,WORK                                                          
         SR    RE,RF                                                            
         STCM  RE,3,WORK                                                        
         WTO   TEXT=WORK                                                        
*                                                                               
CONCK31  L     R1,TUTLXADR                                                      
         USING XAUTLD,R1                                                        
         SAM31                                                                  
         OC    TTICKETN,TTICKETN   TEST FOR CURRENT TICKET                      
         SAM24                                                                  
         JNZ   CONCKOK                                                          
         DROP  R1                                                               
*                                                                               
CONCK32  TM    CTTEST,TTESTNOU     ALREADY UPDATE=NO                            
         JO    CONCKOK                                                          
         TM    READONLY,X'01'      OR ALREADY HAS READONLY BIT ON               
         JO    CONCKOK                                                          
         OI    READONLY,X'01'      SET CALLER INTO READ-ONLY MODE               
         OI    TDDSBITS,TDDSNOUP   SET SO WHOAMI CAN RE-ENABLE UPDATES          
*                                                                               
CONCKOK  BRAS  RE,CONOK            CONNECT VALIDATE OK                          
*&&US                                                                           
         BRAS  RE,SETVSM           SET VSAM OVERRIDES                           
*                                                                               
         L     RF,VSSB                                                          
         CLC   SSBSYSN4-SSBD(4,RF),=C'REPB'                                     
         BE    *+14                                                             
         CLC   SSBSYSN4-SSBD(4,RF),=C'REPC'                                     
         BNE   TSTBRDX                                                          
         CLI   TOVSYS,8            TEST REP SYSTEM                              
         BNE   TSTBRDX                                                          
         CLI   TPRG,X'1B'          TEST DEM PROGRAM                             
         BE    *+12                                                             
         CLI   TPRG,2              TEST CONTRACT PROGRAM                        
         BNE   TSTBRDX                                                          
         OI    TFLAG,TFLAGIRB      INHIBIT RECEIVING OF BROADCASTS              
TSTBRDX  EQU   *                                                                
*&&                                                                             
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
EMIF     LA    RE,SREMIF           MISSING INPUT FIELD                          
         B     ERROR                                                            
EIIF     LA    RE,SREIIF           INVALID INPUT FIELD                          
         B     ERROR                                                            
EFTL     LA    RE,SREFLH           FIELD TOO LONG                               
         LA    RE,303              PASSWORD TOO LONG (SAME AS =PASS)            
         B     ESECPWD                                                          
EINP     LA    RE,310              ID INCOMPATIBLE WITH PASSWORD                
         CLI   GETOVER,0                                                        
         BE    ERROR                                                            
         LA    RE,311              AS ABOVE + GETIDS OVERFLOW                   
         B     ERROR                                                            
EINT     LA    RE,312              ID INCOMPATIBLE WITH TERMINAL                
         BE    ERROR                                                            
         CLI   GETOVER,0                                                        
         BE    ERROR                                                            
         LA    RE,313              AS ABOVE + GETIDS OVERFLOW                   
         B     ERROR                                                            
EIID     LA    RE,SREUID           INVALID USER-ID                              
         B     ESEC                                                             
ETND     LA    RE,100              TERMINAL NOT DEFINED                         
         B     ERROR                                                            
EPND     LA    RE,101              PASSWORD NOT DEFINED FOR TRM                 
         B     ESEC                                                             
EISE     LA    RE,SRESYS           INVALID SYSTEM                               
         B     ERROR                                                            
ESNO     LA    RE,102              SYSTEM NOT OPERATIONAL                       
         B     ERROR                                                            
EIPN     LA    RE,SREPRG           INVALID PROGRAM                              
         B     ERROR                                                            
EPNO     LA    RE,103              PROGRAM NOT OPERATIONAL                      
         B     ERROR                                                            
EPRA     LA    RE,104              PROGRAM ACCESS IS RESTRICTED                 
         B     ERROR                                                            
EPNA     LA    RE,105              NOT AUTHORISED FOR PROGRAM                   
         B     ERROR                                                            
ERNC     LA    RE,SREMBC           NOT CONNECTED                                
         B     ERROR                                                            
EIAM     LA    RE,106              TSTTAB ENTRY NOT FOUND                       
         B     ERROR                                                            
EIMI     LA    RE,107              INVALID MAP ID                               
         B     ERROR                                                            
ETSE     LA    RE,108              TWA SAVE ERROR - CONTACT DDS                 
         B     ERROR                                                            
EDIF     LA    RE,SREDK            DUPLICATE KEYWORD                            
         B     ERROR                                                            
EISC     LA    RE,109              INVALID SECRET CODE WORD                     
         MVI   FNDX,1                                                           
         STC   R0,FSUB                                                          
         B     ESECPWD                                                          
ECSV     LA    RE,111              CAN'T SVS IN IAM MODE                        
         B     ERROR                                                            
ENORA    LA    RE,129              PROGRAM NOT AVAIL IN THIS ADV APPLIC         
         B     ERROR                                                            
EPED     LA    RE,130              PASSWORD EFFECTIVE DATE OUT OF RANGE         
         B     ESECPWD                                                          
EIPP     LA    RE,SE$INVPP         INVALID PERSONAL PASSWORD                    
         MVI   FNDX,1                                                           
         STC   R0,FSUB                                                          
         B     ESECPWD                                                          
EINPPV   OI    PWDSTAT,PSTBUMP     BUMP PASSWORD ERROR COUNT                    
         B     *+8                 NUMBER ALREADY SET IN RE                     
EINPP    LA    RE,SE$INVPP         INVALID NEW PERSONAL PASSWORD                
         MVI   FNDX,1                                                           
         STC   R0,FSUB                                                          
         B     ESECPWD                                                          
EASCIU   LA    RE,133              ALL SESSIONS CURRENTLY IN USE                
         B     ERROR                                                            
EDNC     LA    RE,277              DDS TERMINAL ACCESS NOT ALLOWED              
         B     ESEC                                                             
EDPN     LA    RE,278              DDS PASSWORD NOT VALID                       
         B     ESECPWD                                                          
EPRQ     LA    RE,279              PASSWORD REQUIRED FOR THIS AGENCY            
         B     ESECPWD                                                          
ECON     LA    RE,314              PASSWORD CONNECT RESTRICTION                 
         B     ESECPWD                                                          
EMPI     LA    RE,315              MISSING PERSONAL ID                          
         B     EPID                                                             
EIPI     LA    RE,316              INVALID PERSONAL ID                          
         B     EPID                                                             
ESPI     LA    RE,322              PERSONAL ID ALREADY LOGGED ON                
         B     EPID                                                             
EVNV     LA    RE,XPVERERR         INVALID PCP VERSION                          
         B     ERROR                                                            
***********************************************************************         
* HANDLE ERROR ON PERSONAL ID FIELD (SUB FIELD 2 WITHIN USERID FIELD) *         
***********************************************************************         
EPID     L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR WITHIN USERID FIELD               
         LA    R1,CNTIDH                                                        
         SR    R1,R8                                                            
         STCM  R1,3,TIOBCURD                                                    
         MVC   TIOBCURI,PIDOFF                                                  
         XC    TIOBCURS,TIOBCURS                                                
         DROP  RF                                                               
         B     ESEC                                                             
         EJECT                                                                  
***********************************************************************         
* HANDLE SECURITY VIOLATIONS                                          *         
***********************************************************************         
ESECPWD  OI    PWDERROR,X'01'      SET ERROR IN PASSWORD VALIDATION             
         TM    PWDSTAT,PSTNOP                                                   
         BZ    *+12                                                             
         MVI   SECWHY,TSSVCPIX     PERSON RECORD MAX ERROR COUNT NO-OP          
         B     ESEC1                                                            
         TM    PWDSTAT,PSTUPDT+PSTBUMP                                          
         BO    ESEC5               PERSON RECORD ERROR COUNT UPDATED            
*                                                                               
ESEC     CLI   PIDREQD,C'Y'        TEST PPS PASSWORD                            
         BE    ESEC5                                                            
         TM    TSTAT1,TSTATWEB     WEB TERMINALS ARE NOT VIOLATABLE             
         BO    ESEC4                                                            
         LLC   R1,TSTATSVC         BUMP VIOLATION COUNT                         
         LA    R1,1(R1)                                                         
         STC   R1,TSTATSVC                                                      
         CHI   R1,TSTATCSM         TEST MAXIMUM CONNECT VIOLATION COUNT         
         BL    ESEC4                                                            
         MVI   SECWHY,TSSVCGTM     MAX 6 ATTEMPTS FOR NON PERSON                
*                                                                               
ESEC1    XC    TSTATSVC,TSTATSVC   RESET VIOLATION COUNT                        
         BRAS  RE,LOGSEC           LOG SECURITY EVENT                           
         OI    TSTATU,TSTATDNE     SET CLSDEST REQUIRED                         
         MVC   TSVCREQ,=X'01EE'    SET TO =BYE TO CANCEL CONNECT                
         MVI   PWDERROR,0                                                       
*                                                                               
ESEC2    LA    RE,110              SUSPECTED SECURITY VIOLATION                 
         B     ERROR                                                            
*                                                                               
ESEC4    XC    CNTPWD,CNTPWD       CLEAR & TRANSMIT PASSWORD                    
         OI    CNTPWDH+6,X'80'                                                  
         B     ERROR                                                            
*                                                                               
ESEC5    TM    PWDSTAT,PSTMAX      TEST IF MAX ATTEMPTS EXCEEDED                
         BO    ESEC2                                                            
         CLI   PWDTRYS,1           TEST IF JUST USED LAST TRY                   
         BNE   ESEC4                                                            
         MVI   SECWHY,TSSVCPID     MORE THAN MAX ATTEMPTS FOR PERSON            
         BRAS  RE,LOGSEC           LOG SECURITY EVENT                           
         B     ESEC2                                                            
         EJECT                                                                  
***********************************************************************         
* SWITCH TO =PROG IF USER REQUESTS HELP ON PGM                        *         
***********************************************************************         
SRPROG   L     R1,TBUFF            POINT TO TBUFF                               
         MVI   0(R1),X'08'                                                      
         MVC   1(2,R1),CNTSREQH+2  FAKE SCREEN ADDR                             
         MVC   3(5,R1),=C'=PROG'                                                
         TM    SOXFL,SOXDDS        ALL ONLY VALID FOR DDS TERMINALS             
         BZ    *+18                                                             
         MVI   0(R1),X'0A'                                                      
         MVC   8(2,R1),=C',C'                                                   
         LA    R1,2(R1)                                                         
         MVI   8(R1),6                                                          
         MVC   9(2,R1),CNTIDH+2                                                 
         MVC   11(3,R1),OVSYSN                                                  
         MVI   14(R1),13                                                        
         MVC   15(2,R1),CNTSYSH+2                                               
         MVC   17(10,R1),CNTID                                                  
         MVI   27(R1),13                                                        
         MVC   28(2,R1),CNTPWDH+2                                               
         MVC   30(10,R1),CNTPWD                                                 
         MVI   40(R1),0                                                         
         MVC   CNTSREQ(8),=C'=GOBACK'                                           
         SR    R1,R1                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT A MISSING PASSWORD ERROR MESSAGE AND EXPIRY WARNING          *         
***********************************************************************         
EMIFPWD  CLI   PIDREQD,C'N'        TEST IF PERSONAL ID REQUIRED                 
         BE    EMIF                                                             
         CLI   PEXPIRY,0           TEST EXPIRY AND/OR WARNING REQUIRED          
         BE    EMIF                                                             
         CLI   PEXPWARN,0                                                       
         BE    EMIF                                                             
         OC    PERSONID,PERSONID                                                
         BZ    EMIF                                                             
*                                                                               
         GOTO1 =A(PWDWARN),RR=RELO TEST IF WARNING PERIOD                       
         BNE   EMIF                                                             
         LA    RE,SREMIF                                                        
         MVI   FERN,0                                                           
         LR    R0,R1                                                            
         LA    R4,GTB              POINT TO GETTXT BLOCK                        
         USING GETTXTD,R4                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FNDX         SET FIELD NUMBER                             
         STH   RE,GTMSGNO          SET ERROR NUMBER                             
*                                                                               
         SR    RF,RF               GET CURRENT LANGUAGE CODE                    
         CLI   TLANG,7                                                          
         BH    *+8                                                              
         IC    RF,TLANG                                                         
         MHI   RF,37                                                            
         L     R3,=A(PWDWARNT)     R3=A(PASSWORD EXPIRY WARNING TEXT)           
         A     R3,RELO                                                          
         AR    R3,RF                                                            
         MVC   SCANBLK(36),1(R3)                                                
*                                                                               
         LLC   RF,PEXPDAYS         GET DAYS TO EXPIRY (ZERO=EXPIRED)            
         LLC   RE,0(R3)            GET LOCATION OF DAYS FIELD                   
         LA    R3,SCANBLK(RE)                                                   
         EDIT  (RF),(3,(R3)),ZERO=NOBLANK,WRK=SAELEM                            
         OC    0(3,R3),=C'000'                                                  
*                                                                               
         LA    RE,SCANBLK                                                       
         STCM  RE,7,GTATXT                                                      
         MVI   GTLTXT,36                                                        
         B     ERROR1                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT AN ERROR MESSAGE & EXIT. RE=ERROR MESSAGE NUMBER.            *         
***********************************************************************         
ERROR    MVI   FERN,0                                                           
         STH   RE,HALF                                                          
         OC    ACTERR#,ACTERR#     TEST AND SET ACTUAL ERROR NUMBER             
         BNZ   *+8                                                              
         STH   RE,ACTERR#                                                       
         IC    R0,FSUB                                                          
         SLL   R0,4                                                             
         STC   R0,BYTE                                                          
         OC    ACTERR#(1),BYTE     SET REASON CODE IN FIRST 4 BITS              
         LR    R0,R1                                                            
*                                                                               
         TM    TSTAT1,TSTATWEB     WEB TERMINALS ARE EXEMPT                     
         BO    ERROR0                                                           
         TM    PWDERROR,X'01'      PASSWORD ERRORS ARE EXEMPT                   
         BO    ERROR0                                                           
         LLC   R1,TSTATNUL         BUMP FAILED CONNECT COUNT                    
         LA    R1,1(R1)                                                         
         STC   R1,TSTATNUL                                                      
         CHI   R1,TSTATNUM         TEST MAXIMUM FAILED COUNT                    
         BL    ERROR0                                                           
         XC    TSTATNUL,TSTATNUL   CLEAR COUNT                                  
         MVI   SECWHY,TSSVCMAX     MAX 50 ATTEMPTS                              
         BRAS  RE,LOGSEC           LOG SECURITY EVENT                           
         OI    TSTATU,TSTATDNE     SET CLSDEST REQUIRED                         
         MVC   TSVCREQ,=X'01EE'    SET TO =BYE TO CANCEL CONNECT                
*                                                                               
ERROR0   LA    R4,GTB              POINT TO GETTXT BLOCK                        
         USING GETTXTD,R4                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FNDX         SET FIELD NUMBER                             
         MVC   GTSUBX,FSUB         SET FIELD REASON CODE                        
         STH   RE,GTMSGNO          SET ERROR NUMBER                             
*                                                                               
ERROR0A  CHI   RE,102              TEST SYSTEM NOT OPERATIONL                   
         BNE   ERROR0B                                                          
         MVC   WORK(10),=CL10'SE#(00) - '  APPEND SE# TO MESSAGE                
         L     R3,ASENTRY          RESTORE A(SELIST ENTRY)                      
         USING SELISTD,R3                                                       
         GOTO1 VHEXOUT,DMCB,SESYS,WORK+4,1,=C'TOG'                              
         MVC   WORK+10(L'SENAME),SENAME                                         
         LA    RE,WORK                                                          
         STCM  RE,7,GTATXT                                                      
         MVI   GTLTXT,17                                                        
         B     ERROR1                                                           
         DROP  R3                                                               
*                                                                               
ERROR0B  CHI   RE,XPVERERR         TEST INVALID PCP VERSION                     
         BNE   ERROR1                                                           
         LA    RE,SREPRG           USE ERR NUM 33 - INVALID PROGRAM             
         STH   RE,GTMSGNO                                                       
         MVC   WORK(21),=CL21'- Use Spectra DS app '                            
         LA    RE,WORK                                                          
         STCM  RE,7,GTATXT                                                      
         MVI   GTLTXT,21                                                        
         CLI   XPVERNEW,C' '       TEST IF NEW APPICATION NAME SET              
         BNH   ERROR1                                                           
         MVC   WORK+21(10),XPVERNEW                                             
         MVI   GTLTXT,31                                                        
         B     ERROR1              USE OLD 033 - INVALID PROGRAM                
*                                                                               
         LA    RE,XPVERERR         USE NEW 373 - NO LONGER SUPPORTED            
         STH   RE,GTMSGNO                                                       
         CLI   XPVERNEW,C' '       TEST IF NEW APPICATION NAME SET              
         BNH   ERROR1                                                           
         LA    RE,XPVERNEW                                                      
         STCM  RE,7,GTATXT                                                      
         MVI   GTLTXT,10                                                        
         B     ERROR1                                                           
*                                                                               
ERROR1   MVI   GTMAXL,60           SET L'MESSAGE AREA                           
         LA    RF,FLD                                                           
         STCM  RF,7,GTAOUT         SET A(MESSAGE AREA)                          
         MVI   GTMTYP,GTMERR       SET ERROR MESSAGE                            
         OI    GT1INDS,GT1OWRK     SET T'MESSAGE AREA TO WORK                   
         MVI   GTMSYS,1            SET SERVICE SYSTEM MESSAGES                  
         L     RF,VGETTXT                                                       
         LR    R1,R4                                                            
         BASR  RE,RF                                                            
         LR    R1,R0                                                            
         DROP  R4                                                               
*                                                                               
ERROR2   CLI   TSYS,1              ARE WE CONNECTED TO APPLICATION SYS          
         BNH   ERROR4                                                           
         MVI   DUB,X'FE'           YES-LOAD IN FE SCREEN                        
         BAS   RE,SCROVER                                                       
         LA    R1,CNTSREQH         POINT TO S/R FIELD                           
         ST    R1,FADR                                                          
         CLI   CTSW,C'Y'                                                        
         BE    ERROR4                                                           
         MVC   CNTSREQ,SAVESREQ    RESTORE INPUT VALUE                          
*                                                                               
ERROR4   MVC   CNTMSG,FLD          MOVE ERROR MESSAGE TO FIRST FIELD            
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD IN ERROR                 
*                                                                               
ERROR5   TM    PWDERROR,X'01'      TEST IF PASSWORD ERROR                       
         BZ    ERROR9                                                           
         TM    PWDSTAT,PSTMAXW     TEST IF ABOUT TO EXCEED MAXIMUM              
         BZ    ERROR8                                                           
*                                                                               
ERROR6   SR    RF,RF               GET CURRENT LANGUAGE CODE                    
         CLI   TLANG,7                                                          
         BH    *+8                                                              
         IC    RF,TLANG                                                         
         MHI   RF,L'PWDMAXWT                                                    
         L     RE,=A(PWDMAXWT)     RE=A(PASSWORD LAST TRY TABLE)                
         A     RE,RELO                                                          
         AR    RE,RF               RE=A(LAST TRY ENTRY FOR LANGUAGE)            
*                                                                               
ERROR7   LA    RF,FLD+59           FIND END OF ERROR MESSAGE                    
         LHI   R0,59                                                            
ERROR7A  CLI   0(RF),C' '                                                       
         BH    ERROR7B                                                          
         BCTR  RF,0                                                             
         BCT   R0,ERROR7A                                                       
ERROR7B  LA    RF,4(RF)            RF=A(FREE SPACE AT END)                      
         LA    R1,FLD+60                                                        
         SR    R1,RF               R1=L'FREE SPACE AT END                       
         CLM   R1,1,0(RE)                                                       
         BL    *+8                 TRUNCATE WARNING TEXT                        
         IC    R1,0(RE)                                                         
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RE)       MOVE WARNING MESSAGE                         
         SHI   RF,3                                                             
         MVC   0(3,RF),=C' - '                                                  
         MVC   CNTMSG,FLD          MOVE ERROR MESSAGE TO FIRST FIELD            
*                                                                               
ERROR8   TM    TSTAT7,TST7PS1+TST7PS2                                           
         BZ    ERROR9                                                           
         TM    TSTAT7,TST7PS2      IF ERROR ON 1ST PASS ALLOW RETRY             
         BO    ERROR9                                                           
         NI    TSTAT7,255-(TST7PS0+TST7PSWN)                                    
         B     ERRORX                                                           
*                                                                               
ERROR9   NI    TSTAT7,255-(TST7PS0+TST7PS1+TST7PS2+TST7PSWN)                    
*                                                                               
ERRORX   MVC   TSYS,SVTSYS         RESTORE TSYS                                 
         L     RF,VSSB             RETURN ERRNUM AND REASON CODE IN TCB         
         L     RF,SSBTKADR-SSBD(RF)                                             
         MVC   TCBUDATA-TCBD(2,RF),ACTERR#                                      
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* SET PROGRAM TEST LEVEL FROM AN ID OR TERMINAL RECORD                *         
***********************************************************************         
SETTEST  LA    R4,CTIDATA-CTIREC(R4)                                            
         SR    R1,R1                                                            
         USING CTPRGD,R4           R4=A(TEST ELEMENT)                           
SETTEST2 CLI   CTPRGEL,0                                                        
         BER   RE                                                               
         CLI   CTPRGEL,X'23'                                                    
         JNE   SETTEST4                                                         
         CLC   CTPRGRAM,DUB+2                                                   
         JNE   SETTEST4                                                         
         NI    CTTEST,ALLBITS-TTESTLVL                                          
         MVC   DUB(1),CTPRGTST                                                  
         NI    DUB,TTESTLVL                                                     
         OC    CTTEST,DUB                                                       
         BR    RE                                                               
*                                                                               
SETTEST4 IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         J     SETTEST2                                                         
         DROP  R4                                                               
                                                                                
***********************************************************************         
* GETSYS IS NOW RELATIVE. NO BASE REGISTERS USED.                     *         
* GET A(SYSTEM ELEMENT) FOR AN ID/TERMINAL/AUTHORIZATION RECORD       *         
* ON ENTRY R4=A(RECORD), R1=A(ELEMENT ADDRESS), OVSYS=SYSTEM NUMBER   *         
* ON EXIT RF=A(FOUND ELEMENT)                                         *         
***********************************************************************         
GETSYS   LA    RF,CTIDATA-CTIREC(R4)                                            
         SR    R0,R0                                                            
GETSYS2  CLI   0(RF),0             TEST E-O-R                                   
         JNE   GETSYS3                                                          
         SR    RF,RF                                                            
         J     GETSYSX                                                          
*                                                                               
GETSYS3  CLI   0(RF),X'21'         TEST FOR CORRECT SYSTEM ELEMENT              
         JNE   *+14                                                             
         CLC   OVSYS,CTSYSNUM-CTSYSD(RF)                                        
         JE    GETSYSX                                                          
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         J     GETSYS2                                                          
*                                                                               
GETSYSX  LTR   RF,RF               EXIT IF SYSTEM ELEMENT FOUND                 
         JNZ   GETSYSX1                                                         
         CLI   OVSYS,X'0A'         ELSE IF CONTROL SYSTEM POINT                 
         JNE   GETSYSX1            TO DUMMY SYSTEM LEMENT WITH                  
         BRAS  RF,GETSYSX1         RF=A(ELEMENT BELOW)                          
*                                                                               
         DC    X'21130A0A0080000000000000000000000CFFFF'                        
*                                                                               
GETSYSX1 ST    RF,0(R1)            RETURN A(ELEMENT) OR ZERO                    
         LTR   RF,RF               SET CC=EQ IF NO ELEMENT FOUND                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGRAM (P=XX OR PROGNAME(=MAPID))                         *         
***********************************************************************         
VALPROG  NTR1                                                                   
         GOTO1 =A(VALPGM),RR=RELO  ROUTINE TO VALIDATE PROGRAM                  
         BE    EXIT                                                             
         L     RE,RSLT             RSLT=ERROR NUMBER                            
         B     ERROR                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TEST MODE (TEST=A/B/C/NO)                                  *         
***********************************************************************         
VALTEST  NTR1                                                                   
         LLC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,NOOPT            CHECK FOR 'NO'                               
         BNE   *+12                                                             
         NI    CTTEST,ALLBITS-TTESTLVL-TTESTCIL                                 
         B     VALTESTX                                                         
         CLI   1(R6),1             CHECK FOR A,B OR C                           
         BNE   EIIF                                                             
         CLI   22(R6),C'A'                                                      
         BL    EIIF                                                             
         CLI   22(R6),C'C'                                                      
         BH    EIIF                                                             
         MVC   DUB(1),22(R6)                                                    
         NI    DUB,TTESTLVL                                                     
         NI    CTTEST,ALLBITS-TTESTLVL                                          
         OC    CTTEST,DUB                                                       
VALTESTX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILE UPDATE MODE (UPDATE=YES/NO)                           *         
***********************************************************************         
VALUPDT  NTR1                                                                   
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,YESOPT                                                        
         BNE   *+12                                                             
         NI    CTTEST,ALLBITS-TTESTNOU-TTESTUEN-TTESTUEV                        
         B     VALUPDTX                                                         
         EX    R1,NOOPT                                                         
         BNE   *+12                                                             
         OI    CTTEST,TTESTNOU+TTESTUEN                                         
         B     VALUPDTX                                                         
         EX    R1,VEROPT                                                        
         BNE   EIIF                                                             
         OI    CTTEST,TTESTNOU+TTESTUEN+TTESTUEV                                
         B     VALUPDTX                                                         
VALUPDTX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE TEST ID (IAM=TESTID)                                       *         
***********************************************************************         
VALIAM   NTR1                                                                   
         L     R3,VTSTTAB          SET-UP FOR TEST TABLE BXLE                   
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING TSTTABD,R3                                                       
         CLC   TSTACCS,22(R6)      CHECK TSTTAB ENTRY EXISTS                    
         BE    VALIAMA                                                          
         BXLE  R3,RE,*-10                                                       
         TM    TSTAT6,TST6SCRP     TEST SCRIPT BIT SET IN UTL                   
         BZ    EIAM                ELSE EXIT WITH ERROR                         
         B     VALIAMX             IGNORE FAILED IAM= FOR SCRIPT                
VALIAMA  ST    R3,ATSTNTRY         SAVE A(TEST TAB ENTRY)                       
         OI    CTTEST,TTESTTAC                                                  
VALIAMX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CIL MODE (CIL=YES/NO)                                      *         
***********************************************************************         
VALCIL   NTR1                                                                   
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,YESOPT                                                        
         BNE   *+12                                                             
         OI    CTTEST,TTESTCIL                                                  
         B     VALCILX                                                          
         EX    R1,NOOPT                                                         
         BNE   EIIF                                                             
         NI    CTTEST,ALLBITS-TTESTCIL                                          
VALCILX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE BROADCAST RECEIVE MODE (BC=NO/YES)                         *         
***********************************************************************         
VALBC    NTR1                                                                   
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,YESOPT                                                        
         BNE   *+12                                                             
         NI    TFLAG,255-TFLAGIRB                                               
         B     VALBCX                                                           
         EX    R1,NOOPT                                                         
         BNE   EIIF                                                             
         OI    TFLAG,TFLAGIRB                                                   
VALBCX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE 32 BIT PC MODE (32=NO/YES)                                 *         
***********************************************************************         
VAL32    NTR1                                                                   
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,YESOPT                                                        
         BNE   *+12                                                             
         OI    TSTAT8,TST8PC32                                                  
         B     VAL32X                                                           
         EX    R1,NOOPT                                                         
         BNE   EIIF                                                             
         NI    TSTAT8,255-TST8PC32                                              
VAL32X   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE X=SOX STRING - DO NOTHING HERE - PRE-ALIDATED BY SSOX      *         
***********************************************************************         
VALXXX   NTR1                                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET TICKET NUMBER FOR CONNECT TICKET=XXXXXXXX                       *         
***********************************************************************         
VALTKT   NTR1                                                                   
         MVC   CTICKET,22(R6)                                                   
VALTKTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXTRACT TICKET=FIELD FROM FLD AND MOVE TO CTICKET                   *         
***********************************************************************         
NEWTKT   NTR1                                                                   
         LLC   R1,FLDH+5           R1=INPUT LEN                                 
         LA    RF,FLD              RF=FIELD                                     
NEWTKT1  LA    RE,4(RF)                                                         
         CLC   0(4,RF),=C'TKT='                                                 
         JE    NEWTKT2                                                          
         LA    RE,7(RF)                                                         
         CLC   0(4,RF),=C'TICKET='                                              
         JE    NEWTKT2                                                          
         LA    RF,1(RF)            SCAN FOR KEYWORDS                            
         JCT   R1,NEWTKT1                                                       
         J     NEWTKTX             NOT FOUND                                    
*                                                                               
NEWTKT2  DS    0H                                                               
         MVC   CTICKET,SPACES                                                   
         LA    R6,CTICKET          COPY INTO CTICKET                            
         LA    R0,16+1                                                          
NEWTKT3  MVC   0(1,R6),0(RE)       BYTE BY BYTE                                 
         AHI   R0,-1                                                            
         LA    RE,1(RE)                                                         
         LA    R6,1(R6)                                                         
         CLI   0(RE),C' '          UNTIL SPACE                                  
         JE    NEWTKT4                                                          
         CLI   0(RE),C','          OR ,                                         
         JE    NEWTKT4                                                          
         CLI   0(RE),0             OR ZERO                                      
         JNE   NEWTKT3                                                          
*                                                                               
NEWTKT4  LTR   R0,R0               CHECK LEN DID NOT GO > 16                    
         JNP   EIIF                                                             
         LA    R1,FLD+80                                                        
         SR    R1,RE               R1=REMAINING LEN                             
         AHI   RF,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)       COPY OUT TICKET FIELD                        
         SR    RE,RF                                                            
         LLC   RF,FLDH+5                                                        
         SR    RF,RE               REMOVE LEN OF TICKET=FIELD                   
         STC   RF,FLDH+5                                                        
*                                                                               
NEWTKTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATA BUILD MODE OVERRIDE (BUILD=YES)                       *         
***********************************************************************         
VALBLD   NTR1                                                                   
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,YESOPT                                                        
         BNE   VALBLD1                                                          
         TM    DATABLD,X'01'       BUILD=Y ONLY VALID IF IN BUILD MODE          
         BZ    EIIF                                                             
         OI    DATABLD,X'02'       SET BUILD=Y WAS INPUT                        
         B     VALBLDX                                                          
VALBLD1  EX    R1,NOOPT                                                         
         BNE   EIIF                                                             
VALBLDX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE OSIN VALUE OSIN=XXXXXXXX HEX CHRS                          *         
***********************************************************************         
VALOSI   NTR1                                                                   
         TM    3(R6),X'20'         TEST HEX                                     
         BZ    EIIF                                                             
         CLI   1(R6),8             TEST LENGTH                                  
         BNE   EIIF                                                             
         LLC   R0,1(R6)                                                         
         GOTO1 VHEXIN,DMCB,22(R6),COSIN,(R0)                                    
VALOSIX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE DDS TERM IN USER MODE (DDS=YES/NO/OFFC) TO SET OFFICE CODE *         
***********************************************************************         
VALUSER  NTR1                                                                   
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,YESOPT                                                        
         BE    VALUSER2                                                         
         EX    R1,NOOPT                                                         
         BE    VALUSER4                                                         
         MVC   OFFICE,22(R6)                                                    
         B     VALUSERX                                                         
VALUSER2 TM    SOXFL,SOXDDS        IF DDS TERMINAL IGNORE DDS=YES               
         BO    VALUSERX                                                         
         MVI   OFFICE,C'*'                                                      
         B     VALUSERX                                                         
VALUSER4 MVI   OFFICE,C' '                                                      
VALUSERX B     EXIT                                                             
*                                                                               
* VALIDATE PROGRAM AUTHORIZATION OVERRIDE (AUTH=XXXX)                           
*                                                                               
VALAUTH  NTR1                                                                   
         MVC   AUTHFNDX,FNDX                                                    
         TM    3(R6),X'20'                                                      
         BZ    EIIF                                                             
         GOTO1 VHEXIN,DMCB,22(R6),AUTHVAL,4                                     
         OC    AUTHVAL,AUTHVAL                                                  
         BZ    EIIF                                                             
VALAUTHX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE MAP ID                                                     *         
***********************************************************************         
VALMAP   NTR1                                                                   
VALMAP2  L     R4,AMAPREC                                                       
         USING CTMREC,R4                                                        
         XC    CTMKEY,CTMKEY       BUILD KEY OF MAP RECORD                      
         MVI   CTMKTYP,C'M'                                                     
         MVC   CTMKSYS,OVSYS                                                    
         MVC   CTMKPROG,PROGNUM                                                 
         MVI   CTMKSCRN,X'FF'                                                   
         MVC   CTMKUSER,USERALPH                                                
         MVC   CTMKOTHR,22(R6)                                                  
         BAS   RE,CTREAD           READ MAP RECORD                              
         BNE   EIMI                                                             
VALMAPX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE CURRENT SCREEN BEFORE CONNECT TO NEW PROGRAM                   *         
***********************************************************************         
VALSAVE  NTR1                                                                   
         GOTO1 =A(VALSAV),RR=RELO                                               
         BE    EXIT                                                             
         L     RE,RSLT             RSLT=ERROR NUMBER                            
         B     ERROR                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIMIT ACCESS VALUE                                         *         
***********************************************************************         
VALLIMA  NTR1                                                                   
         TM    3(R6),X'20'         TEST HEX                                     
         BZ    VALLIMA2                                                         
         TM    1(R6),1             TEST EVEN NUMBER OF BYTES INPUT              
         BNZ   VALLIMA2                                                         
         LLC   R0,1(R6)                                                         
         GOTO1 VHEXIN,DMCB,22(R6),LIMACCS,(R0)                                  
         B     VALLIMAX                                                         
VALLIMA2 CLI   1(R6),4                                                          
         BH    EIIF                                                             
         MVC   LIMACCS,22(R6)                                                   
VALLIMAX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE LANGUAGE CODE FROM SYSTEM LANGUAGE TABLE                   *         
***********************************************************************         
VALLANG  NTR1                                                                   
         L     RE,VSSB                                                          
         L     RE,SSBALANG-SSBD(RE)                                             
         LA    RE,6(RE)                                                         
         USING LANGTABD,RE                                                      
         LLC   R1,1(R6)            GET LENGTH OF LANGUAGE NAME                  
         CHI   R1,3                                                             
         BNH   *+8                                                              
         LHI   R1,3                                                             
         BCTR  R1,0                                                             
VALLANG1 CLI   LANGCODE,X'FF'      TEST END OF LANGUAGE TABLE                   
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),LANGSHR    COMPARE ENGLISH SHORT NAME                   
         BE    VALLANG2                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),LANGSHRN   COMPARE NATIVE SHORT NAME                    
         BE    VALLANG2                                                         
         LA    RE,LANGTABL(RE)                                                  
         B     VALLANG1                                                         
VALLANG2 MVC   CTLANG,LANGCODE     SAVE LANGUAGE CODE                           
VALLANGX B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE COUNTRY CODE FROM SYSTEM COUNTRY TABLE                     *         
***********************************************************************         
VALCTRY  NTR1                                                                   
         L     RE,VSSB                                                          
         L     RE,SSBACTRY-SSBD(RE)                                             
         LA    RE,6(RE)                                                         
         USING CTRYTABD,RE                                                      
VALCTRY1 CLI   CTRYCODE,X'FF'      TEST END OF COUNTRY TABLE                    
         BE    EIIF                                                             
         CLC   22(2,R6),CTRYSHR    COMPARE ENGLISH SHORT NAME                   
         BE    VALCTRY2                                                         
         CLC   22(2,R6),CTRYSHRN   COMPARE NATIVE SHORT NAME                    
         BE    VALCTRY2                                                         
         LA    RE,CTRYTABL(RE)                                                  
         B     VALCTRY1                                                         
VALCTRY2 MVC   CTCTRY,CTRYCODE     SAVE COUNTRY CODE                            
         CLI   AGYCTRY,X'FF'       TEST AGENCY COUNTRY SET                      
         BNE   *+10                                                             
         MVC   AGYCTRY,CTCTRY      NO-USE INPUT VALUE                           
VALCTRYX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE STEREO FLAG STEREO=ABC  A=Y/N TOGGLES TST6STRO             *         
*                                  B=Y/N TOGGLES TST6STFU             *         
*                                  C=Y/N TOGGLES TST6STSS             *         
***********************************************************************         
VALSTRO  NTR1                                                                   
         MVC   CTSTIN1,22(R6)      SAVE FIRST INPUT CHR                         
         CLI   22(R6),C'Y'                                                      
         BNE   *+12                                                             
         OI    CTSTEREO,TST6STRO                                                
         B     VALSTRO1                                                         
         CLI   22(R6),C'N'                                                      
         BNE   *+12                                                             
         NI    CTSTEREO,255-TST6STRO                                            
         B     VALSTRO1                                                         
         CLI   22(R6),C'*'                                                      
         BNE   EIIF                                                             
VALSTRO1 CLI   1(R6),2             TEST IF SECOND CHR INPUT                     
         BNL   *+16                                                             
         MVI   CTSTIN2,C'*'                                                     
         MVI   CTSTIN3,C'*'                                                     
         B     VALSTROX                                                         
         MVC   CTSTIN2,23(R6)                                                   
         CLI   23(R6),C'Y'                                                      
         BNE   *+12                                                             
         OI    CTSTEREO,TST6STFU                                                
         B     VALSTRO2                                                         
         CLI   23(R6),C'N'                                                      
         BNE   *+12                                                             
         NI    CTSTEREO,255-TST6STFU                                            
         B     VALSTRO2                                                         
         CLI   23(R6),C'*'                                                      
         BNE   EIIF                                                             
VALSTRO2 CLI   1(R6),3             TEST IF THIRD CHR INPUT                      
         BNL   *+12                                                             
         MVI   CTSTIN3,C'*'                                                     
         B     VALSTROX                                                         
         MVC   CTSTIN3,24(R6)                                                   
         CLI   24(R6),C'Y'                                                      
         BNE   *+20                                                             
         OI    CTSTEREO,TST6STSS                                                
         OI    CTSTERE8,TST8STSS                                                
         NI    CTSTERE8,X'FF'-TST8BINT                                          
         B     VALSTROX                                                         
         CLI   24(R6),C'N'                                                      
         BNE   *+16                                                             
         NI    CTSTEREO,255-TST6STSS                                            
         NI    CTSTERE8,255-TST8STSS-TST8BINT                                   
         B     VALSTROX                                                         
*                                                                               
         CLI   24(R6),C'B'                                                      
         BNE   VALSTRO3                                                         
         OI    CTSTEREO,TST6STSS                                                
         OI    CTSTERE8,TST8STSS                                                
         TM    TSTAT9,TSTNVRSN                                                  
         BZ    *+8                                                              
         OI    CTSTERE8,TST8BINT                                                
         B     VALSTROX                                                         
*                                                                               
VALSTRO3 CLI   24(R6),C'*'                                                      
         BNE   EIIF                                                             
*                                                                               
VALSTROX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE STORAGE PROTECTION MODE OVERRIDE, PRO(TECT)=Y/N            *         
* Y=SETS TST6STFU,N=SETS TST6STSS                                     *         
***********************************************************************         
VALPROT  NTR1                                                                   
         CLI   1(R6),1             TEST IF >ONE CHR INPUT                       
         BNE   EIIF                                                             
         MVC   CTPROT,22(R6)       SAVE 1ST INPUT CHR                           
         CLI   22(R6),C'Y'                                                      
         BE    VALPROTX                                                         
         CLI   22(R6),C'N'                                                      
         BE    VALPROTX                                                         
         B     EIIF                                                             
VALPROTX B     EXIT                                                             
*                                                                               
YESOPT   CLC   22(0,R6),=C'YES '                                                
NOOPT    CLC   22(0,R6),=C'NO  '                                                
VEROPT   CLC   22(0,R6),=C'VERIFY'                                              
         EJECT                                                                  
***********************************************************************         
* TEST AND SET DDS PASSWORD INPUT WHICH IS SAVED IN SECCODE/SECCODU   *         
***********************************************************************         
DDSPWD   NI    PASSDDS,255-PASSDPWD TURN OFF SPECIAL DDS PWD INPUT              
         CLC   SECCODU,=CL10'DDS'                                               
         JE    DDSPWDY                                                          
*&&US*&& CLC   SECCODU,=CL10'DRAUG'                                             
*&&US*&& JE    DDSPWDY                                                          
DDSPWDN  J     *+8                                                              
DDSPWDY  OI    PASSDDS,PASSDPWD                                                 
         TM    PASSDDS,PASSDPWD    EXIT WITH CC=EQ IF DDS PWD INPUT             
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE AN INPUT FIELD ADDRESSED BY R1.                        *         
* RETURN WITH CC=EQ IF FIELD NOT INPUT.                               *         
***********************************************************************         
FVAL     ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVC   FLDH,0(R1)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD                                               
         LLC   RF,FLDH+5                                                        
         AHI   RF,-1                                                            
         JM    FVALX                                                            
         EXRL  RF,FVAL1                                                         
         J     FVALX                                                            
FVAL1    MVC   FLD(0),8(R1)                                                     
FVALX    CLI   FLDH+5,0                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* OVERLAY AN ERROR SCREEN INTO TWA                                    *         
***********************************************************************         
SCROVER  NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D90110'                                             
         MVC   DMCB+7(1),DUB                                                    
         GOTO1 VCALLOV,DMCB,CNTMSGH                                             
         CLI   4(R1),X'FF'                                                      
         BNE   EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* READ A RECORD FROM THE CONTROL FILE                                 *         
* EXIT WITH CC=NEQ ON ERROR                                           *         
***********************************************************************         
CTREAD   NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',(R4),(R4)              
         CLI   8(R1),0                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONVERT SYSTEM AUTH ELEMENT TO NEW STYLE                            *         
***********************************************************************         
SETEXPEL NTR1                                                                   
         LR    R1,R0               R0=A(SYSTEM AUTHORIZATION ELEMENT)           
         MVC   EXPIDEL(80),0(R1)   COPY TO MAX OF ELEMENT                       
         USING CTSYSD,R1                                                        
         TM    5(R1),X'80'         NEW FORMAT?                                  
         BZ    SETEXPLX            NO-EXIT                                      
         MVC   EXPIDEL+16(2),CTSYSALL                                           
         MVC   EXPIDEL+18(126),EXPIDEL+16 MAKE ALL CTSYSALL                     
         SR    RE,RE                                                            
         LLC   RF,CTSYSLEN                                                      
         AHI   RF,-16                                                           
         BNP   SETEXPLM            EXIT IF NO PROGRAM                           
         D     RE,=F'3'            GET NUM OF PROGRAMS                          
         LTR   RE,RE                                                            
         JNZ   *+2                                                              
         LA    R3,CTSYSPGM         POINT TO FIRST ENTRY                         
         LA    R4,EXPIDEL+16                                                    
SETEXPLP LLC   RE,0(R3)            GET THE PROGRAM NUM                          
         BCTR  RE,0                -1 FOR OFFSET                                
         SLL   RE,1                                                             
         AR    RE,R4               ADDRESS INTO EXPIDEL                         
         MVC   0(2,RE),1(R3)       COPY AUTHORIZATION CODE FOR PROG             
         LA    R3,3(R3)            NEXT ENTRY                                   
         BCT   RF,SETEXPLP         LOOP UNTIL ALL ARE COPIED                    
SETEXPLM CLI   CTSYSNUM,X'0A'      TEST FOR CONTROL SYSTEM ELEMENT              
         BNE   SETEXPLX                                                         
         CLI   PROGNUM,X'0C'       TEST PROG=MAD                                
         BNE   SETEXPLX                                                         
*                                                                               
         MVC   EXPIDEL+CTSYSALL-CTSYSD(2),=XL2'FFFF'                            
         LA    R4,EXPIDEL+16                                                    
         LA    RE,X'0C'                                                         
         BCTR  RE,0                                                             
         SLL   RE,1                                                             
         AR    RE,R4                                                            
         MVC   0(2,RE),=XL2'FFFF'  SO SET MAD=MAX AUTH OVERIDE                  
SETEXPLX B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SCAN USER ID FIELD AT FLDH                                          *         
* RETURN USERID AND OPTIONALLY PERSONID  ('USERID,PERSONID')          *         
* ALLOW USERID,PERSONID,DDS TO SPECIFY USE DDS AGENCY SEC ON DDS TERM *         
***********************************************************************         
SCANUID  NTR1                                                                   
         XC    USERID,USERID                                                    
         XC    PERSONID,PERSONID                                                
         GOTO1 VSCANNER,DMCB,FLDH,(3,SCANBLK),C',=,='                           
         CLI   4(R1),0                                                          
         BE    SUIDNO                                                           
         CLI   4(R1),3                                                          
         BH    SUIDNO                                                           
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R6,SCANBLK          R6=A(SCAN BLOCK ENTRY)                       
*                                                                               
SUID010  CLC   FNDX,NLINES                                                      
         BH    SUID110             EXIT OK                                      
         CLI   0(R6),0             L'FIRST HALF                                 
         BE    SUIDNO                                                           
         CLI   FNDX,1              FIRST ENTRY IS USERID                        
         BE    SUID020                                                          
         CLI   FNDX,3              THIRD ENTRY IS OPTIONAL USE DDS SEC          
         BE    SUID030                                                          
*                                                                               
         CLI   1(R6),0             L'SECOND HALF = L'PERSONID                   
         BNE   SUIDNO                                                           
         LLC   RF,PIDOFF           UPDATE PIDOFF FOR COMMA                      
         LA    RF,1(RF)                                                         
         STC   RF,PIDOFF                                                        
         CLI   0(R6),L'PERSONID                                                 
         BH    SUIDNO                                                           
         MVC   PERSONID,12(R6)                                                  
         TM    SOXFL,SOXDDS        ALLOW SHORTY FOR DDS TERMINALS               
         BZ    SUID100                                                          
         CLC   12(8,R6),SHORTY                                                  
         BNE   SUID100                                                          
         MVC   PERSONID(8),SHORTPID                                             
         B     SUID100                                                          
*                                                                               
SUID020  CLI   1(R6),0             L'SECOND HALF = 0                            
         BNE   SUIDNO                                                           
         CLI   0(R6),L'USERID                                                   
         BH    SUIDNO                                                           
         MVC   USERID,12(R6)                                                    
         MVC   PIDOFF,0(R6)        SAVE OFFSET TO START OF PERSONAL ID          
         B     SUID100                                                          
*                                                                               
SUID030  CLI   1(R6),0             THIRD OPTION FIELD                           
         BNE   SUIDNO                                                           
         TM    SOXFL,SOXDDS        ONLY FOR DDS TERMINALS                       
         BZ    SUIDNO                                                           
         LLC   R1,0(R6)            GET FIELD LENGTH                             
         CHI   R1,3                                                             
         BH    SUIDNO                                                           
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=CL10'DDS' USE DDS SECURITY AGY FOR PERSON              
         BNE   SUIDNO                                                           
         OI    SOXFL1,SOXUSEDD     SET TO USE AGY SEC FOR DDS TERMINAL          
*                                                                               
SUID100  LA    R6,32(R6)           BUMP TO NEXT ENTRY                           
         LLC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     SUID010                                                          
*                                                                               
SUID110  MVI   FNDX,0                                                           
         B     SUIDOK                                                           
*                                                                               
SUIDOK   SR    RC,RC               EXIT CC .EQ.                                 
SUIDNO   LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PASSWORD AUTHORISATION RECORD EFFECTIVE DATE ELEMENT       *         
* IF ELEMENT NOT FOUND OR DATES IN RANGE RETURN CC .EQ.               *         
* RETURN CC .NE. IF DATES OUT OF RANGE                                *         
***********************************************************************         
VALPEF   NTR1                                                                   
         L     R4,ASECREC                                                       
         USING SA0REC,R4                                                        
         LA    R3,SA0DATA          FIND EFFECTIVE DATES ELEMENT                 
         SR    R1,R1                                                            
VALPEF2  CLI   0(R3),0                                                          
         BE    VALPEFOK                                                         
         USING SAPEFEL,R3                                                       
         CLI   SAPEFEL,SAPEFELQ                                                 
         BE    VALPEF4                                                          
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VALPEF2                                                          
VALPEF4  CLC   SAPEFSTA,SAPEFEND   CHECK START<=END DATE                        
         BH    VALPEFNO                                                         
         CLC   SAPEFSTA,TODAYB     CHECK START<=TODAY                           
         BH    VALPEFNO                                                         
         CLC   SAPEFEND,TODAYB     CHECK END>=TODAY                             
         BL    VALPEFNO                                                         
VALPEFOK SR    RC,RC               EXIT CC .EQ.                                 
VALPEFNO LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* READ PERSONAL ID RECORD INTO BUFFER                                 *         
***********************************************************************         
READPID  NTR1                                                                   
         MVI   PIDPWDL,0           SET NO PASSWORD FOUND                        
         L     R4,APERREC                                                       
         USING SAPEREC,R4                                                       
         XC    SAPEKEY,SAPEKEY     BUILD PERSON KEY                             
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,PIDSEC                                                   
         MVC   SAPEPID,PERSONID                                                 
         MVC   SAPEDEF,=X'FFFF'                                                 
         XC    SAPEDEF,TODAYB                                                   
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI '),=C'CTFILE ',(R4),(R4)              
         CLI   8(R1),0                                                          
         BNE   RPIDNO                                                           
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BNE   RPIDNO                                                           
*                                  GET PASSWORD ELEMENT DATA                    
         LA    R3,SAPEDATA                                                      
         SR    R1,R1                                                            
RPID100  CLI   0(R3),0                                                          
         BE    RPIDOK                                                           
         CLI   0(R3),SAPWDELQ                                                   
         BE    RPID120                                                          
RPID110  IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     RPID100                                                          
*                                                                               
         USING SAPWDD,R3                                                        
RPID120  MVC   PIDNUM,SAPWDNUM     SAVE PERSON PASSWORD NUMBER                  
         MVC   PIDPWD,SAPWDCOD     SAVE PERSON PASSWORD CODE                    
         MVC   PIDPWDU,PIDPWD      PID PASSWORD COPY                            
         LA    RE,PIDPWDU+MPWDL-1                                               
         LA    RF,MPWDL                                                         
         CLI   0(RE),C' '          FIND LENGTH OF PID PASSWORD COPY             
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         STC   RF,PIDPWDL                                                       
         XC    DMCB(20),DMCB                                                    
         LA    RE,PIDPWDU                                                       
         ST    RE,DMCB                                                          
         MVC   DMCB+0(1),PIDPWDL                                                
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
         XC    0(MPWDL,RE),0(RE)                                                
         MVI   DMCB+4,0            RULE#0 TO CONVERT TO UPPER                   
         MVC   DMCB+8(4),VSSB                                                   
         MVI   DMCB+8,2            PROCESS ACTION                               
         GOTO1 =V(PWDVAL),DMCB,RR=RELO                                          
         CLC   PIDPWD,PIDPWDU                                                   
         BNE   *+8                                                              
         OI    PIDPWDF,X'02'       SET PIDPWD IS UPPER CASE                     
         B     RPIDOK                                                           
*                                                                               
RPIDOK   SR    RC,RC               EXIT CC .EQ.                                 
RPIDNO   LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET PASSWORD AUTHORISATION RECORD PERSONAL ID ELEMENT DATA          *         
* IF ELEMENT NOT FOUND RETURN CC .NE.                                 *         
* ELSE PERSONAL ID IN PERSONID AND CC .EQ.                            *         
***********************************************************************         
GETPID   NTR1                                                                   
         L     R4,ASECREC                                                       
         USING SA0REC,R4                                                        
         LA    R3,SA0DATA          FIND PERSONAL ID ELEMENT                     
         SR    R1,R1                                                            
GETPID2  CLI   0(R3),0                                                          
         BE    GETPIDNO                                                         
         USING SAPALD,R3                                                        
         CLI   SAPALEL,SAPALELQ                                                 
         BE    GETPID4                                                          
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GETPID2                                                          
GETPID4  MVC   PERSONID,SAPALPID   SAVE PERSONAL ID                             
         B     GETPIDOK                                                         
*                                                                               
GETPIDOK SR    RC,RC               EXIT CC .EQ.                                 
GETPIDNO LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET LIMIT ACCESS VALUE FROM DATA ACCESS SYSTEM ELEMENTS             *         
* RETURN IN LAGVALUE, ELSE CC .NE. IF ELEMENT NOT FOUND               *         
* SYSTEM NUMBER IN OVSYS, PROGRAM NUMBER IN PROGNUM                   *         
***********************************************************************         
GETLAS   NTR1                                                                   
         XC    LAGVALUE,LAGVALUE                                                
         L     R4,ALAGREC                                                       
         USING SALAREC,R4                                                       
         LA    R3,SALADATA                                                      
         SR    R1,R1                                                            
GETLAS2  CLI   0(R3),0                                                          
         BE    GETLASNO                                                         
         USING SALASD,R3                                                        
         CLI   SALASEL,SALASELQ                                                 
         BNE   *+14                                                             
         CLC   OVSYS,SALASNUM                                                   
         BE    GETLAS4                                                          
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GETLAS2                                                          
GETLAS4  MVC   LAGVALUE,SALASALL   SET DEFAULT ALL PROGRAM VALUE                
         LA    R1,SALASPGM                                                      
         LLC   RE,SALASLN                                                       
GETLAS6  CH    RE,=Y(SALASLNQ)     SEARCH PROGRAM CODE IN ELEMENT               
         BNH   GETLASOK                                                         
         TM    PROGIND4,PGMILINK   TEST SWITCHED TO LINK PROGRAM                
         BZ    GETLAS7A                                                         
         CLC   PROGNLNK,0(R1)      IF SO USE EXTERNAL PROGRAM NUMBER            
         BE    GETLAS8                                                          
         B     GETLAS7B                                                         
GETLAS7A CLC   PROGNUM,0(R1)                                                    
         BE    GETLAS8                                                          
GETLAS7B SH    RE,=Y(L'SALASPGM)                                                
         B     GETLAS6                                                          
GETLAS8  MVC   LAGVALUE,1(R1)      SAVE LIMIT ACCESS CODE FOR PROGRAM           
         B     GETLASOK                                                         
*                                                                               
GETLASOK SR    RC,RC               EXIT CC .EQ.                                 
GETLASNO LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET PROGRAM SECURITY FLAG FROM ACCESS RECORD SYSTEM ELEMENT         *         
* RETURN Y/N FLAG IN PSECFLAG                                         *         
***********************************************************************         
GETPSEC  NTR1                                                                   
         MVI   PSECFLAG,C'N'       DEFAULT FLAG NO                              
         TM    PROGIND2,PGMISECA   CHECK PROGRAM LIST INDICATORS                
         BZ    GPSECNO                                                          
         TM    PROGIND2,PGMISECB                                                
         BZ    GPSECYES                                                         
*                                                                               
         L     R4,ASACREC          GET SYS EL FROM SECURITY ACCESS REC          
         LA    R1,ASACSYS                                                       
         BAS   RE,GETSYS                                                        
         BZ    GPSECNO                                                          
         LR    R3,RF               POINT TO SYSTEM ELEMENT                      
         USING CTSYSD,R3                                                        
         CLI   CTSYSLEN,X'18'      TEST SECURITY FLAGS PRESENT                  
         BNE   GPSECNO                                                          
         LA    RE,1                SECURITY FLAGS IN 8 BYTES AT END OF          
         SLL   RE,31               OF SYSTEM ELEMENT CONTAINING                 
         SR    RF,RF               64 BITS, ONE PER PROGNUM                     
         SR    R1,R1               THEREFORE BUILD AND TEST BIT MASKS           
         TM    PROGIND4,PGMILINK   TEST SWITCHED TO LINK PROGRAM                
         BZ    *+12                                                             
         IC    R1,PROGNLNK         IF SO USE EXTERNAL PROGRAM NUMBER            
         B     *+8                                                              
         IC    R1,PROGNUM                                                       
         BCTR  R1,0                                                             
         SRDL  RE,0(R1)                                                         
         LTR   RE,RE                                                            
         BZ    GPSEC010                                                         
         ICM   RF,15,CTSYSPGM      FIRST GROUP OF FOUR BYTES                    
         NR    RE,RF                                                            
         BZ    GPSECNO                                                          
         B     GPSECYES                                                         
*                                                                               
GPSEC010 ICM   RE,15,CTSYSPGM+4    SECOND GROUP OF FOUR BYTES                   
         NR    RF,RE                                                            
         BZ    GPSECNO                                                          
         B     GPSECYES                                                         
*                                                                               
GPSECNO  B     EXIT                FLAG IS OFF FOR NO                           
*                                                                               
GPSECYES MVI   PSECFLAG,C'Y'       FLAG IS ON FOR YES                           
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET SECRET CODE FROM AUTH RECORD GIVEN NUMBER IN SECNO              *         
***********************************************************************         
GETSCOD  NTR1                                                                   
         L     R4,ASECREC                                                       
         USING CT0REC,R4                                                        
         XC    CT0KEY,CT0KEY       BUILD AUTH(NUMERIC) KEY                      
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,PIDSEC                                                   
         MVC   CT0KNUM,SECNO                                                    
         BAS   RE,CTREAD                                                        
         BNE   GSCODNO                                                          
         LA    R4,CT0DATA                                                       
         SR    R1,R1                                                            
GSCOD010 CLI   0(R4),0             FIND AUTH(ALPHA) ELEMENT                     
         BE    GSCODNO                                                          
         CLC   0(2,R4),=X'030C'                                                 
         BE    *+14                                                             
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GSCOD010                                                         
         MVC   SECCODE,2(R4)       SAVE SECRET CODE IN W/S                      
         MVC   SECCODU,2(R4)                                                    
         DROP  R4                                                               
*                                                                               
         BAS   RE,GETPID           GET PERSONAL ID FROM AUTH RECORD             
         BNE   GSCODOK                                                          
         L     R4,APERREC          GET CURRENT PASSWORD FROM PERSIN REC         
         USING SAPEREC,R4                                                       
         XC    SAPEKEY,SAPEKEY     BUILD PERSON KEY                             
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,PIDSEC                                                   
         MVC   SAPEPID,PERSONID                                                 
         MVC   SAPEDEF,=X'FFFF'                                                 
         XC    SAPEDEF,TODAYB                                                   
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI '),=C'CTFILE ',(R4),(R4)              
         CLI   8(R1),0                                                          
         BNE   GSCODNO                                                          
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BNE   GSCODNO                                                          
*                                                                               
         LA    R3,SAPEDATA         GET ELEMENT DATA                             
         SR    R1,R1                                                            
GSCOD020 CLI   0(R3),0                                                          
         BE    GSCODNO                                                          
         CLI   0(R3),SAPWDELQ                                                   
         BE    *+14                                                             
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GSCOD020                                                         
*                                                                               
         USING SAPWDD,R3                                                        
         MVC   SECCODE,SAPWDCOD    SAVE SECRET CODE IN W/S                      
         MVC   SECCODU,SECCODE                                                  
         B     GSCODOK                                                          
*                                                                               
GSCODOK  SR    RC,RC               RETURN CC .EQ.                               
GSCODNO  LTR   RC,RC               RETURN CC .NE.                               
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* LOAD USER ID TWA FIELD USING CURRENT USER ID NUMBER IN UTL          *         
* EXIT WITH CC .NE. IF NO ENTRY IN UTL OR INVALID VALUE               *         
***********************************************************************         
LOADUID  NTR1                                                                   
         CLI   REUSFLAG,C'N'       ERROR EXIT REUSFLAG SET TO NO                
         BE    LUIDNO                                                           
         OC    TUSER,TUSER         TEST IF USERID NUMBER IN UTL                 
         BZ    LUIDNO                                                           
         L     R4,AUIDREC                                                       
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD ID(NUMERIC) KEY                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TUSER                                                
         BAS   RE,CTREAD                                                        
         BNE   LUIDNO                                                           
         LA    R4,CTIDATA                                                       
         SR    R1,R1                                                            
LUID020  CLI   0(R4),0             FIND ID(ALPHA) ELEMENT                       
         BE    LUIDNO                                                           
         CLI   0(R4),X'02'                                                      
         BNE   *+14                                                             
         MVC   CNTID(L'USERID),2(R4)  MOVE USER ID INTO SCREEN FIELD            
         B     LUID030                                                          
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     LUID020                                                          
         DROP  R4                                                               
*                                                                               
LUID030  LA    R1,CNTID+L'USERID-1                                              
         CLI   0(R1),C' '          GET L'USER-ID                                
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTID                                                         
         SR    R1,RE                                                            
         STC   R1,CNTIDH+5         SET L'USER-ID IN TWA HEADER                  
         GOTO1 FVAL,CNTIDH         RELOAD FLDH                                  
         BZ    LUIDNO                                                           
*                                  LOAD PERSONAL ID IF IN UTL                   
         OC    TPERSON,TPERSON                                                  
         BZ    LUIDOK                                                           
         L     R4,ASECREC                                                       
         USING CT0REC,R4                                                        
         XC    CT0KEY,CT0KEY       BUILD AUTH(NUMERIC) KEY                      
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TAGYPER                                                  
         MVC   CT0KNUM,TPERSON                                                  
         BAS   RE,CTREAD                                                        
         BNE   LUIDNO                                                           
         DROP  R4                                                               
         BAS   RE,GETPID           GET PERSONAL ID FROM AUTH RECORD             
         BNE   LUIDNO                                                           
         MVI   PIDENTER,C'N'       SET PERSONALID ENTERED FLAG OFF              
         LLC   R1,CNTIDH+5                                                      
         LA    R1,CNTID(R1)                                                     
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'PERSONID,R1),PERSONID                                        
         LA    R1,L'PERSONID-1(R1)                                              
         CLI   0(R1),C' '          ADD L'PERSON ID                              
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTID                                                         
         SR    R1,RE                                                            
         STC   R1,CNTIDH+5         SET L'USER-ID IN TWA HEADER                  
         GOTO1 FVAL,CNTIDH         RELOAD FLDH                                  
         BZ    LUIDNO                                                           
         B     LUIDOK                                                           
*                                                                               
LUIDOK   SR    RC,RC               EXIT CC .EQ.                                 
LUIDNO   LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOAD SYSTEM TWA FIELD USING CURRENT SYSTEM NUMBER IN UTL            *         
* EXIT WITH CC .NE. IF NO ENTRY IN UTL OR INVALID VALUE               *         
***********************************************************************         
LOADSYS  NTR1                                                                   
         CLI   REUSPFLG,C'N'       ERROR EXIT REUSPFLG SET TO NO                
         BE    LSYSNO                                                           
         OC    TOVSYS,TOVSYS       TEST IF SYSTEM NUMBER IN UTL                 
         BZ    LSYSNO                                                           
         L     RE,ASYSLST                                                       
         USING SYSLSTD,RE                                                       
         LA    RE,SYSLLEN(RE)                                                   
LSYS010  CLI   SYSLNUM,0                                                        
         BE    LSYSNO                                                           
         CLC   SYSLNUM,TOVSYS                                                   
         BE    LSYS020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     LSYS010                                                          
LSYS020  MVC   CNTSYS(L'SYSLNAME),SYSLNAME                                      
         LA    R1,CNTSYS+L'SYSLNAME-1                                           
         CLI   0(R1),C' '          GET L'SYSTEM                                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTSYS                                                        
         SR    R1,RE                                                            
         STC   R1,CNTSYSH+5        SET L'SYSTEM IN TWA HEADER                   
         GOTO1 FVAL,CNTSYSH        RELOAD FLDH                                  
         BZ    LSYSNO                                                           
         B     LSYSOK                                                           
*                                                                               
LSYSOK   SR    RC,RC               EXIT CC .EQ.                                 
LSYSNO   LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PROGRAM TWA FIELD USING CURRENT SYSTEM NUMBER IN UTL           *         
* EXIT WITH CC .NE. IF NO ENTRY IN UTL OR INVALID VALUE               *         
***********************************************************************         
LOADPRG  NTR1                                                                   
         CLI   REUSPFLG,C'N'       ERROR EXIT REUSPFLG SET TO NO                
         BE    LPRGNO                                                           
         OC    TPRG,TPRG           TEST IF PROGRAM NUMBER IN UTL                
         BZ    LPRGNO                                                           
         L     R3,APGMLIST                                                      
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
LPRG010  CLC   PGMNUM,TPRG                                                      
         BE    LPRG020                                                          
         BXLE  R3,R4,*-10                                                       
LPRG020  MVC   CNTPGM(L'PGMNAME),PGMNAME                                        
         LA    R1,CNTPGM+L'PGMNAME-1                                            
         CLI   0(R1),C' '          GET L'PROGRAM                                
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTPGM                                                        
         SR    R1,RE                                                            
         STC   R1,CNTPGMH+5        SET L'PROGRAM IN TWA HEADER                  
         GOTO1 FVAL,CNTPGMH        RELOAD FLDH                                  
         BZ    LPRGNO                                                           
         B     LPRGOK                                                           
*                                                                               
LPRGOK   SR    RC,RC               EXIT CC .EQ.                                 
LPRGNO   LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PASSWORD TWA FIELD USING CURRENT TPASSWD VALUE IN UTL          *         
* EXIT WITH CC .NE. IF NO ENTRY IN UTL OR INVALID VALUE               *         
***********************************************************************         
LOADPWD  NTR1                                                                   
         CLI   REUSFLAG,C'N'       ERROR EXIT REUSFLAG SET TO NO                
         BE    LPWDNO                                                           
         OC    TPASSWD,TPASSWD     RESTORE SECRET CODE FROM UTL                 
         BZ    LPWDNO              IF PRESENT AND PERSONAL AUTH PASSWD          
         TM    TFLAG,TFLAGSEC                                                   
         BZ    LPWDNO                                                           
         OC    TAGYSEC,TAGYSEC     GET SECURITY AGENCY IF PRESENT               
         BNZ   *+14                                                             
         MVC   AGYSEC,TAGY                                                      
         B     *+10                                                             
         MVC   AGYSEC,TAGYSEC                                                   
         MVC   SECNO,TPASSWD                                                    
         BAS   RE,GETSCOD          GET SECRET CODE FROM AUTH RECORD             
         BNE   LPWDNO              EXIT IF NOT FOUND                            
*                                                                               
         MVC   CNTPWD(L'SECCODE),SECCODE                                        
         BAS   RE,DDSPWD           TEST/SET DDS PASSWORD                        
*                                                                               
         LA    R1,CNTPWD+L'SECCODE-1                                            
         CLI   0(R1),C' '          GET L'PASSWORD/SECRET CODE                   
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTPWD                                                        
         SR    R1,RE                                                            
         STC   R1,CNTPWDH+5        SET L'PASSWORD IN TWA HEADER                 
*                                                                               
         GOTO1 FVAL,CNTPWDH        RELOAD FLDH                                  
         BZ    LPWDNO                                                           
         B     LPWDOK                                                           
*                                                                               
LPWDOK   SR    RC,RC               EXIT CC .EQ.                                 
LPWDNO   LTR   RC,RC               EXIT CC .NE.                                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* MAIN LOGIC CONSTANTS AND LITERALS                                  *          
**********************************************************************          
DDSSECP  DS    0CL2                                                             
*&&UK*&& DC    C'#E'                                                            
*&&US*&& DC    C'#N'                                                            
*                                                                               
SSACBITS DC    X'0102040810204080'                                              
SSNABITS DC    X'FEFDFBF7EFDFBF7F'                                              
*&&UK                                                                           
SHORTY   DC    CL8'/       '                                                    
SHORTPID DC    CL8'READDDLO'                                                    
SHORTPWD DC    CL8'READONLY'                                                    
*&&                                                                             
*&&US                                                                           
SHORTY   DC    CL8'/       '                                                    
SHORTPID DC    CL8'READDDNY'                                                    
SHORTPWD DC    CL8'READONLY'                                                    
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ANYTHING AFTER THIS POINT SHOULD BE ADDRESSED BY AN ADCON ONLY     *          
**********************************************************************          
                                                                                
**********************************************************************          
* TABLE OF VALID KEYWORDS INPUTTABLE IN PROGRAM FIELD                *          
* 0-7    KEYWORD NAME                                                *          
* 8&9    MIN/MAX KEYWORD LENGTH                                      *          
* 10&11  MIN/MAX VALUE LENGTH                                        *          
* 12     BIT 0 ON=DDS ONLY, BITS 1-7=OPTION NUMBER                   *          
* 13-15  A(VALIDATION ROUTINE)                                       *          
**********************************************************************          
OPTTAB   DS    0CL16                                                            
         DC    C'PROGRAM ',AL1(01,08,01,07,01+000),AL3(VALPROG)                 
         DC    C'TEST    ',AL1(01,04,01,03,02+128),AL3(VALTEST)                 
*&&UK*&& DC    C'UPDATE  ',AL1(01,06,01,03,03+000),AL3(VALUPDT)                 
*&&US*&& DC    C'UPDATE  ',AL1(01,06,01,03,03+128),AL3(VALUPDT)                 
         DC    C'IAM     ',AL1(01,03,01,04,04+128),AL3(VALIAM)                  
         DC    C'CIL     ',AL1(01,03,01,03,05+128),AL3(VALCIL)                  
         DC    C'DDS     ',AL1(01,03,01,03,06+128),AL3(VALUSER)                 
         DC    C'AUTH    ',AL1(01,04,04,04,07+128),AL3(VALAUTH)                 
         DC    C'MAPID   ',AL1(01,05,01,08,08+000),AL3(VALMAP)                  
         DC    C'SVS     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVU     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVP     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVA     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVB     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVC     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVD     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVE     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVF     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVG     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SVH     ',AL1(03,03,00,00,09+000),AL3(VALSAVE)                 
         DC    C'SV      ',AL1(02,02,00,00,09+000),AL3(VALSAVE)                 
         DC    C'LANG    ',AL1(01,04,01,07,12+000),AL3(VALLANG)                 
         DC    C'CTRY    ',AL1(02,04,01,07,13+128),AL3(VALCTRY)                 
         DC    C'BC      ',AL1(01,02,01,03,11+000),AL3(VALBC)                   
         DC    C'LIMIT   ',AL1(02,05,01,08,10+128),AL3(VALLIMA)                 
         DC    C'STEREO  ',AL1(01,06,01,03,14+000),AL3(VALSTRO)                 
         DC    C'PROTECT ',AL1(03,07,01,01,15+128),AL3(VALPROT)                 
         DC    C'32      ',AL1(02,02,01,03,18+000),AL3(VAL32)                   
         DC    C'X       ',AL1(01,01,01,08,19+128),AL3(VALXXX)                  
         DC    C'TICKET  ',AL1(02,06,01,16,20+128),AL3(VALTKT)                  
         DC    C'TKT     ',AL1(02,03,01,16,20+128),AL3(VALTKT)                  
         DC    C'BUILD   ',AL1(02,05,01,03,21+128),AL3(VALBLD)                  
         DC    C'OSIN    ',AL1(02,04,08,08,22+128),AL3(VALOSI)                  
*                                                                               
* THESE ARE SYNONYMS FOR VALUES NORMALLY INPUT IN THE SYSTEM FIELD              
* UNIQUE FIRST CHR SO NO CLASH WITH EXISTING PROGRAM FIELD ENTRIES              
*                                                                               
         DC    C'QCP     ',AL1(01,03,01,10,23+000),AL3(VALPCP) PCP              
         DC    C'RPP     ',AL1(01,03,01,10,24+000),AL3(VALAPP) APP              
         DC    C'VERSION ',AL1(01,07,01,08,25+000),AL3(VALXVN) VERSION          
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE WORKING STORAGE VALUES                                   *         
* TIA IS USED BOTH AS A WORK AREA AND TO RETURN DATA BACK TO MONITOR  *         
* TIA+000(20) CONNECT SRVREQ FIELD INFO                               *         
* TIA+020     SETDATA BLOCK                                           *         
* TIA+500(60) PROGRAM VERSION MESSAGE (PROVER)                        *         
* TIA+560(1)  PROVER HIGHLIGHT MESSAGE FLAG (Y/N)                     *         
* TIA+600     USED TO READ TEMPSTR (ATWA2) AND AS A WORK AREA         *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         L     R1,ATIA             CLEAR 600 BYTE RETURN AREA IN TIA            
         XC    000(250,R1),000(R1)                                              
         XC    250(250,R1),250(R1)                                              
         XC    500(100,R1),500(R1)                                              
         LA    R1,600(R1)                                                       
         ST    R1,ATWA2            SET A(TWA READ AREA)                         
*                                                                               
INIT1    MVC   ATWASVR,VTWASVR     SET A(TWASVR) FROM SYSFACS                   
         ICM   R1,15,=V(TWASVR)                                                 
         BZ    *+12                                                             
         A     R1,RELO                                                          
         ST    R1,ATWASVR          TWASVR INCLUDED IN LINK                      
         LH    R1,=Y(IDREC-WORKD)  SET A(CONTROL FILE RECORDS)                  
         LA    R1,WORKD(R1)                                                     
         ST    R1,AUIDREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,APIDREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,ATRMREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,AMAPREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,ASECREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,AACCREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,APERREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,ALAGREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,ASACREC                                                       
         LA    R1,MAXRECL(R1)                                                   
         ST    R1,APRVREC                                                       
*                                                                               
INIT2    L     RE,ASRPARM          EXTRACT VALUES FROM COMFACS                  
         USING SRPARMD,RE                                                       
         L     RF,SRPARM4                                                       
         USING COMFACSD,RF                                                      
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VSCUNKEY,CSCUNKEY                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VBILLIT,CBILLIT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VPERVERT,CPERVERT                                                
         MVC   VSMFOUT,CSMFOUT                                                  
         L     RF,CGETFACT                                                      
         DROP  RE,RF                                                            
*                                                                               
INIT3    GOTO1 (RF),DMCB,0         GET A(SYSTEM LIST) FROM GETFACT              
         L     R1,0(R1)            R1=A(FAFACTS)                                
         MVC   SCRNDATE,FADATE-FACTSD(R1)                                       
         MVC   SCRNTIME,FATIME-FACTSD(R1)                                       
         L     R1,FASYSLST-FACTSD(R1)                                           
         LA    R1,6(R1)                                                         
         ST    R1,ASYSLST                                                       
*                                                                               
INIT4    XC    DMCB(12),DMCB       GET A(GETIDS) CORE RES PHASE                 
*&&UK*&& MVC   DMCB+4(4),=X'D9000AF9'                                           
*&&US*&& MVC   DMCB+4(4),=X'D9000AFA'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   AGETIDS,0(R1)                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(5,DUB),(31,TODAY)                                  
         GOTO1 VDATCON,DMCB,(5,DUB),(23,ISODATE)                                
*&&UK*&& GOTO1 (RF),(R1),(0,TODAY),(10,SCRNDATE)                                
*                                                                               
         LLC   RE,TODAY3                                                        
         SH    RE,UIDLYEAR         USERID START YEAR                            
         BM    INIT4A                                                           
         MHI   RE,12                                                            
         LLC   RF,TODAY3+1                                                      
         LA    RE,1(RE,RF)         RELATIVE MONTH NUM FROM START YEAR           
         SH    RE,UIDLMTH          USERID START MONTH                           
         BP    *+6                                                              
INIT4A   SR    RE,RE                                                            
         STC   RE,TODAYMTH         RELATIVE MONTH NUMBER SINCE START            
*                                                                               
INIT5    L     RF,VSSB             GET FACPAK SYSTEM ID INFO FROM SSB           
         USING SSBD,RF                                                          
         MVC   STRDATE(8),SSBSDATE                                              
         MVC   SYSIDNO,SSBSYSID                                                 
         MVC   SYSIDNAM,SSBSYSN4                                                
         MVC   SYSIDTY,SSBSYSFL                                                 
         MVC   SYSIDCHR,SSBSYSCH                                                
         MVC   SYSIDN1,SSBSYSN1                                                 
         MVC   SSMAX,=AL2(4)                                                    
         MVC   SSMXP,=AL2(5)                                                    
         TM    TSTATC,TSTCXSES                                                  
         BZ    INIT5A                                                           
         MVC   SSMAX,SSBSSMAX                                                   
         MVC   SSMXP,SSBSSMXP                                                   
*                                                                               
INIT5A   MVC   SSPGS,SSBSSPGS                                                   
         MVC   RECLEN,SSBTWAL                                                   
         CLC   RECLEN,=H'18432'    18K TEMPSTR DISPLACEMENTS                    
         BNE   INIT5B                                                           
         MVC   GLODSP,=Y(CHKPTGLD)                                              
         MVC   CHKDSP,=Y(CHKPTDSP)                                              
         B     INIT6                                                            
         DROP  RF                                                               
INIT5B   DC    H'0'                                                             
*                                                                               
INIT6    LR    RE,R8               POINT TO TWA SAVE AREA                       
         AH    RE,CHKDSP                                                        
         USING CHKPTD,RE                                                        
         MVC   CURSES,TSESSION     CURRENT SESSION                              
         MVC   NEWSES,TSESSION     NEW SESSION                                  
         MVC   CTSTEREO,TSTAT6     SAVE SESSION STEREO FLAGS                    
         NI    CTSTEREO,TST6STRO+TST6STFU+TST6STSS                              
         MVC   CTSTERE8,TSTAT8                                                  
         NI    CTSTERE8,TST8STSS+TST8BINT                                       
         TM    TSTAT9,TSTNVRSN                                                  
         BO    *+8                                                              
         NI    CTSTERE8,255-TST8BINT                                            
         CLC   CHTDDATE(8),STRDATE                                              
         BL    INIT10                                                           
         CLI   TSYS,0              EXTRACT AND SAVE SOME CONNECT INFO           
         BE    INIT7                                                            
         MVC   OVSYSN,CHLNSYS                                                   
         MVC   PROGNAME(L'CHLNPRG),CHLNPRG                                      
*                                                                               
INIT7    MVC   TEMPSINF,CHXTINF    SAVE TEMPEST ALLOCATION                      
         OC    CHXTNDX,CHXTNDX     TEST ANY TEMPEST USED                        
         BZ    INIT8                                                            
         TM    CHXTFLG,X'80'       TEST IF TEMPEST NOT OWNED                    
         BO    INIT8                                                            
         OI    TEMPESTF,X'01'      SET TEMPEST RESERVED FOR SESSION             
*                                                                               
INIT8    TM    TSTAT1,TSTATBIL                                                  
         BZ    INIT9                                                            
         MVC   BILLREF,CHBILL      SAVE BILLING REFERENCE                       
         DROP  RE                                                               
*                                                                               
INIT9    NI    TFLAG,255-TFLAGIRB  DEFAULT TO BC=ON                             
         TM    TSTAT1,TSTATWEB     WEB DEFAUTL TO AUTOSWAP                      
         BZ    INIT10                                                           
         OI    TSTAT8,TST8ASWP                                                  
*                                                                               
INIT10   MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVI   CTLANG,X'FF'                                                     
         MVI   CTCTRY,X'FF'                                                     
         MVI   AGYCTRY,X'FF'                                                    
         MVI   AGYCURR,X'FF'                                                    
         MVI   CTICKET,X'FF'                                                    
         MVI   COSIN,X'FF'                                                      
         MVC   SVTSYS,TSYS         SAVE TSYS AT START OF CONNECT                
*                                                                               
INIT11   TM    TSTAT1,TSTATDDS+TSTATWEB                                         
         BZ    INIT12                                                           
         CLI   CNTSREQH+5,6        TEST SWAP WITH PRE-VALIDATED P/W             
         BNE   INIT12                                                           
         CLC   CNTSREQ(6),=C'=CT,::'                                            
         BNE   INIT11A                                                          
         XC    CNTSREQ+3(3),CNTSREQ+3                                           
         MVI   CNTSREQH+5,3        RESET AND TRANSMIT S/R FIELD                 
         OI    CNTSREQH+6,X'80'                                                 
         OI    TSTATD,TSTATPVP+TSTATCSW SET SWAP WITH PRE-VAL P/W               
         B     INIT12                                                           
INIT11A  CLC   CNTSREQ(6),=C'=CT,##'                                            
         BNE   INIT12                                                           
         XC    CNTSREQ+3(3),CNTSREQ+3                                           
         MVI   CNTSREQH+5,3        RESET AND TRANSMIT S/R FIELD                 
         OI    CNTSREQH+6,X'80'                                                 
         OI    TSTATD,TSTATPVP     SET PRE-VALIDATED PASSWORD                   
*                                                                               
INIT12   DS    0H                                                               
         NI    TSTATD,255-TSTATWSP-TSTATCPV                                     
         TM    TSTATD,TSTATPVP     TEST IF PRE-VALIDATED PASSWORD               
         BZ    INIT20                                                           
         NI    TSTATD,255-TSTATPVP                                              
         OI    TSTATD,TSTATCPV     YES-SET CONNECTED WITH PRE-VALIDATED         
         OI    PWFLAG,PWPVAL                                                    
*                                                                               
         TM    TSTATD,TSTATCSW     TEST IF SWAP TYPE CONNECT                    
         BZ    INIT12B                                                          
         NI    TSTATD,255-TSTATCSW                                              
         OI    PWFLAG,PWCTSW       YES-SET CONNECT SWAP FLAG                    
INIT12B  MVC   PWINPV,CNTPWD       SAVE INPUT PASSWORD                          
         MVC   PWINPL,CNTPWDH+5                                                 
         XC    CNTPWD,CNTPWD                                                    
         MVI   CNTPWDH+5,0                                                      
         OI    CNTPWDH+6,X'80'     CLEAR AND TRANSMIT PASSWORD FIELD            
INIT20   DS    0H                                                               
*                                                                               
INITX    XIT1                                                                   
*                                  USERID LOGON MONTH NUM 01=JAN/2009           
UIDLYEAR DC    H'109'              USERID LOGON START YEAR                      
UIDLMTH  DC    H'1'                USERID LOGON START MONTH                     
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET SOX CONTROL FLAGS FOR ACCESS TO SYSTEM                          *         
* CAN OVERRIDE BY X=STRING IN PROGRAM FIELD                           *         
***********************************************************************         
SSOX     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
SSOX1    MVI   SOXFL,SOXON         SET SOX ON                                   
*&&UK*&& CLI   SYSIDNO,FAC#TST                                                  
*&&UK*&& BNE   *+8                                                              
*&&UK*&& MVI   SOXFL,0             SET SOX OFF FOR TST (UK)                     
*&&UK*&& CLI   SYSIDNO,FAC#FQA                                                  
*&&UK*&& BNE   *+8                                                              
*&&UK*&& MVI   SOXFL,0             SET SOX OFF FOR FQA                          
*                                                                               
         MVC   DDSSEC,DDSSECP      SET DDS TERMINAL SECURITY AGENCY             
*                                                                               
SSOX1A   TM    TSTAT1,TSTATDDS     DDS TERMINAL                                 
         BZ    SSOX1B                                                           
         OI    SOXFL,SOXDDS                                                     
         OI    SOXFL2,SOXDDSTR                                                  
*                                                                               
SSOX1B   TM    SYSIDTY,FACITST     TEST SYSTEM                                  
         BZ    SSOX1C                                                           
         OI    SOXFL,SOXTST                                                     
         OI    SOXFL2,SOXTSTAD                                                  
         CLI   SYSIDNO,FAC#CSC     CSC USES REAL DDS SECURITY AGENCY            
         BE    SSOX1C                                                           
*&&UK*&& MVC   DDSSEC,=C'D1'                                                    
*&&US*&& MVC   DDSSEC,=C'SJ'                                                    
*                                                                               
SSOX1C   TM    TSTAT6,TST6SCRP     SCRIPT IN PROCESS                            
         BZ    *+8                                                              
         OI    SOXFL,SOXSCRP                                                    
*                                                                               
SSOX2    TM    TSTAT1,TSTATDDS     DDS TERMINALS CAN SET SOX VIA X=             
         BZ    SSOXX                                                            
         LA    RF,CNTPGM           LOOK FOR X=... IN PROGRAM FIELD              
         SR    R0,R0                                                            
         ICM   R0,1,CNTPGMH+5                                                   
         BZ    SSOXX                                                            
SSOX2A   CLC   0(2,RF),=C'X='                                                   
         BE    SSOX2B                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,SSOX2A                                                        
         B     SSOXX                                                            
SSOX2B   LA    RF,2(RF)            POINT TO X=STRING                            
         AHI   R0,-2                                                            
         BNP   SSOXX                                                            
*                                                                               
SSOX3    CLI   0(RF),C','          TEST END OF LIST                             
         BE    SSOXA                                                            
         CLI   0(RF),C' '                                                       
         BNH   SSOXA                                                            
*                                                                               
SSOX4    CLI   0(RF),C'S'          SOX ON                                       
         BNE   SSOX4A                                                           
         OI    SOXFL,SOXON                                                      
SSOX4A   CLI   0(RF),C'O'          SOX OFF                                      
         BNE   SSOX5                                                            
         NI    SOXFL,255-SOXON                                                  
*                                                                               
SSOX5    CLI   0(RF),C'T'          TEST SYSTEM                                  
         BNE   SSOX5A                                                           
         OI    SOXFL,SOXTST                                                     
         OI    SOXFL2,SOXTSTAD                                                  
*&&UK*&& MVC   DDSSEC,=C'D1'                                                    
*&&US*&& MVC   DDSSEC,=C'SJ'                                                    
SSOX5A   CLI   0(RF),C'P'          PRODUCTION SYSTEM                            
         BNE   SSOX6                                                            
         NI    SOXFL,255-SOXTST                                                 
         NI    SOXFL2,255-SOXTSTAD                                              
         MVC   DDSSEC,DDSSECP                                                   
*                                                                               
SSOX6    CLI   0(RF),C'D'          DDS TERMINAL AND DDS SECURITY                
         BNE   SSOX6A                                                           
         OI    SOXFL,SOXDDS                                                     
         OI    SOXFL1,SOXUSEDD                                                  
         OI    SOXFL2,SOXDDSTR                                                  
SSOX6A   CLI   0(RF),C'C'          CLIENT TERMINAL                              
         BNE   SSOX7                                                            
         NI    SOXFL,255-SOXDDS                                                 
         NI    SOXFL2,255-SOXDDSTR                                              
*                                                                               
SSOX7    CLI   0(RF),C'R'          SCRIPT                                       
         BNE   SSOX8                                                            
         OI    SOXFL,SOXSCRP                                                    
*                                                                               
SSOX8    EQU   *                                                                
*                                                                               
SSOX9    LA    RF,1(RF)            BUMP TO NEXT CHR IN STRING                   
         BCT   R0,SSOX3                                                         
*                                                                               
SSOXA    EQU   *                                                                
*                                                                               
SSOXX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOG SECURITY VIOLATION DATA - SECWHY HAS THE REASON CODE            *         
***********************************************************************         
LOGSEC   NTR1  BASE=*,LABEL=*                                                   
         L     RE,VSSB             BUMP FACPAK SECURITY VIOLATION COUNT         
         LLC   R0,SSBSECV-SSBD(RE)                                              
         AHI   R0,1                                                             
         CHI   R0,255                                                           
         BH    *+8                                                              
         STC   R0,SSBSECV-SSBD(RE)                                              
         MVC   TSSVNUM,SECWHY      SET SUSPECT SECURITY VIOLATION NUM           
         OI    TSSVNUM,X'80'       SET CURRENTLY ACTIVE                         
         CHI   R0,255                                                           
         BH    LOGSX               ONLY LOG FIRST 255 VIOLATIONS                
*                                                                               
         SR    R0,R0               WRITE TWA TO TEMPSTR                         
         ICM   R0,3,TNUM                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,(R0)),SRCONFFD            
*                                                                               
         LA    R4,WORK             R4=A($CT LOG RECORD)                         
         USING LOGRECD,R4                                                       
         MVC   LOGID,=CL4'$CT0'    IDENTIFIER FOR SECURITY VIOLATION            
         MVC   LOGID+3(1),SECWHY   REASON NUMBER                                
         OI    LOGID+3,C'0'                                                     
         MVC   LOGLUID,TLUID                                                    
         GOTO1 VTICTOC,DMCB,C'SGET'                                             
         MVC   LOGTIME,DMCB        TIME P'0HHMMSS+'                             
         MVC   LOGTEXT+00(16),CNTID                                             
         OC    LOGTEXT+00(16),SPACES                                            
         MVC   LOGTEXT+16(16),CNTSYS                                            
         OC    LOGTEXT+16(16),SPACES                                            
         MVC   LOGTEXT+32(16),CNTPGM                                            
         OC    LOGTEXT+32(16),SPACES                                            
         MVC   LOGTEXT1,SPACES                                                  
         CLI   PASSWD,C' '         INPUT PASSWORD                               
         BNH   *+10                                                             
         MVC   LOGPWD,PASSWD                                                    
         CLI   SECCODE,C' '                                                     
         BNH   *+10                                                             
         MVC   LOGPWD,SECCODE                                                   
         L     RF,VSSB                                                          
         MVC   LOGDAYNO,SSBDAYNO-SSBD(RF) DAY NUMBER                            
         MVC   LOGSYSIX,SSBSYSIX-SSBD(RF) AOR/TOR ID                            
*                                                                               
         GOTO1 VLOGGER,LOGREC      OUTPUT LOGREC TO ADRFILE                     
*                                                                               
LOGSX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK WE ARE CONNECTED TO THE CORRECT ADV FOR THIS SENUM            *         
***********************************************************************         
CHKADV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VDMOD000,DMCB,VFINDSYS,(1,0)                                     
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         AHI   R1,-4                                                            
         L     R1,0(R1)            EXTRACT SYSSTAB                              
         LLC   RF,SESYSN                                                        
         AR    RF,R1                                                            
         MVC   UPDSYSID,0(RF)      EXTRACT UPDATIVE FACPAK ID                   
*                                                                               
         TM    SYSIDTY,FACITST                                                  
         BO    CHKADVEQ            EXIT IF TEST FACPAK                          
         CLC   SYSIDNO,0(RF)                                                    
         BE    CHKADVEQ            EXIT IF CORRECT SYSTEM                       
*&&UK                                                                           
         CLI   SYSIDNO,10                                                       
         BE    CHKADVEQ            UK ADVA SYSTEM                               
         CLI   SYSIDNO,12                                                       
         BE    CHKADVEQ            UK ADVB SYSTEM                               
         CLI   SYSIDNO,14                                                       
         BE    CHKADVEQ            UK ADVC SYSTEM                               
*&&                                                                             
*&&US                                                                           
         CLI   SYSIDNO,13          US ADV8 SYSTEM                               
         BE    CHKADVEQ                                                         
*&&                                                                             
         CLI   0(RF),X'00'                                                      
         BE    CHKADVEQ            UNKNOWN SO IGNORE                            
         CLI   0(RF),X'FF'                                                      
         BNE   *+14                GLOBAL SYSTEM ANY ADV IS VALID               
         MVC   UPDSYSID,SYSIDNO                                                 
         B     CHKADVEQ                                                         
         OI    SOXFL2,SOXWRONG     SET WRONG ADV                                
*                                                                               
         TM    TSTAT1,TSTATDDS+TSTATWEB                                         
         BNZ   CHKAD1                                                           
         TM    TSTAT8,TST8ASWP     ARE WE SET FOR AUTO SWAP                     
         BO    CHKAD1                                                           
*&&US*&& B     CHKADVEQ            NO-IGNORE ERROR FOR US                       
         SR    RE,RE               NO-RETURN ERROR                              
         ICM   RE,1,0(RF)                                                       
         AHI   RE,209              WITH INDEX TO REQUIRED ADV                   
         L     RD,SAVERD                                                        
         B     ERROR                                                            
*                                                                               
CHKAD1   OI    TSTAT4,TST4SWAP     SET SWAP FLAG                                
         MVC   TSWAPLID,0(RF)      SET SWAP SYS FROM TABLE                      
*                                                                               
         ICM   R1,15,TBUFF         BUILD PASS MESSAGE IN TBUFF                  
         MVI   0(R1),0                                                          
         LA    R1,1(R1)            R1=A(FIRST CHR IN CONNECT STRING)            
         MVC   0(3,R1),=C'=CT'                                                  
         LA    R1,3(R1)            SET =CT                                      
         TM    TSTATD,TSTATCPV                                                  
         BZ    CHKAD3                                                           
         MVC   0(3,R1),=C',::'     SET PRE-VALIDATE PASSWORD STRING             
         LA    R1,3(R1)                                                         
*                                                                               
CHKAD3   MVI   0(R1),C';'          TERMINATE SERVICE REQUEST FIELD              
         LA    R1,1(R1)                                                         
         LA    RF,CNTIDH           LOOP THROUGH =CT FIELDS                      
*                                                                               
CHKAD4   SR    RE,RE               SET USER;SYS;PROG;PASS;DATA                  
         ICM   RE,1,5(RF)                                                       
         BZ    CHKAD6              END IF NO MORE DATA                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),8(RF)       COPY FIELD                                   
         AR    R1,RE                                                            
         MVI   0(R1),C';'          INSERT ;                                     
         LA    R1,1(R1)                                                         
CHKAD5   LLC   R0,0(RF)            NEXT FIELD                                   
         AR    RF,R0                                                            
         TM    1(RF),X'20'         CHECK UNPROTECTED                            
         BO    CHKAD5                                                           
         B     CHKAD4                                                           
*                                                                               
CHKAD6   MVI   0(R1),C' '                                                       
         ICM   RE,15,TBUFF         SET MESSAGE LENGTH                           
         SR    R1,RE                                                            
         STC   R1,0(RE)                                                         
*                                                                               
         OI    TSTATU,TSTATDNE     SET CLSDEST REQUIRED                         
         MVC   TSVCREQ,=X'01EE'    SET TO =BYE TO CANCEL CONNECT                
         TM    PWFLAG,PWPVAL       TEST IF PRE-VALIDATED PASSWORD               
         BZ    CHKADVNE                                                         
*                                                                               
CHKAD10  OC    PIDLLOEL,PIDLLOEL   MUST HAVE LAST LOGGED ON ELEMENT             
         BZ    CHKADERR                                                         
         TM    SYSIDTY,FACITST     DO NOTHING FOR TEST SYSTEMS                  
         BO    CHKADVEQ                                                         
         ICM   RF,15,=XL4'0AFFFFFF'                                             
         GOTO1 VSWITCH,DMCB,(RF),0 SWITCH TO CONTROL SYSTEM                     
         CLI   4(R1),0                                                          
         BNE   CHKADERR                                                         
         L     R3,APERREC          READ FOR UPDATE THE PERSON RECORD            
         USING SAPEREC,R3                                                       
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMRDHI '),=C'CTFILE ',APERREC,  *        
               APERREC                                                          
         CLI   8(R1),0                                                          
         BNE   CHKADERR            CONTROL FILE READ PROBLEM                    
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BNE   CHKADERR            PERSON RECORD MISSING                        
*                                                                               
CHKAD12  L     R3,APERREC                                                       
         AH    R3,PIDLLOEL         R3=A(LAST LOGGED ON ELEMENT)                 
         USING SALLOD,R3                                                        
         CLI   SALLOEL,SALLOELQ    TEST ELEMENT STILL EXISTS                    
         BNE   CHKAD30                                                          
         CLI   SALLOLEN,SALLOLNQ                                                
         BNE   CHKAD30                                                          
         STAM  ARE,AR1,DMCB                                                     
         TIME  BIN                                                              
         LAM   ARE,AR1,DMCB                                                     
         STCM  R0,7,SALLOTME       SET TIME IN 1/100 SEC UNITS                  
         OI    SALLOFLG,SALLOSWP   SET SWAP WITH PRE-VALIDATED PASSWORD         
*                                                                               
CHKAD18  EQU   *                   WRITE PERSON RECORD - NO RECOVERY            
         GOTO1 VDATAMGR,DMCB,(X'26',=C'DMWRT'),=C'CTFILE ',APERREC,    *        
               APERREC                                                          
         CLI   8(R1),0                                                          
         BNE   CHKADERR                                                         
         OI    PWDSTAT,PSTUPDT     SET UPDATED FLAG                             
*                                                                               
CHKAD30  ICM   RF,15,=XL4'01FFFFFF'                                             
         GOTO1 VSWITCH,DMCB,(RF),0 SWITCH TO SERVICE SYSTEM                     
         CLI   4(R1),0                                                          
         BNE   CHKADERR                                                         
         MVI   TSYS,1                                                           
         B     CHKADVNE                                                         
*                                                                               
CHKADERR B     CHKADVNE            NOT THIS FACPAK                              
*                                                                               
CHKADVEQ CR    RF,RF                                                            
         B     CHKADVX                                                          
CHKADVNE LTR   RF,RF                                                            
CHKADVX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FIND IF INPUT USER-ID IS COMPATIBLE WITH A RECORD                             
* ON ENTRY R4=A(RECORD) (TERMINAL OR PERSON AUTH)                               
***********************************************************************         
VALID    NTR1  BASE=*,LABEL=*                                                   
         MVC   DUB(1),0(R4)        SAVE RECORD TYPE                             
         MVI   DUB+1,0                                                          
         CLI   DUB,C'T'            TERMINAL REC?                                
         BNE   VALID0                                                           
         CLC   CTTKTID-CTTREC(5,R4),=C'DDTNQ'                                   
         BE    *+14                                                             
         CLC   CTTKTID-CTTREC(4,R4),=C'DDQA'                                    
         BNE   *+8                                                              
         MVI   DUB+1,C'Q'          REMEMBER THAT                                
VALID0   MVI   GETOVER,0           CLEAR GETID OVERFLOW FLAG                    
         LAY   RF,GETIDSWK                                                      
         GOTO1 AGETIDS,DMCB,(C'L',(R4)),(RF),(C'S',VDATAMGR),          X        
               USERID,IDOSID                                                    
         TM    4(R1),X'08'         TEST GETID OUTPUT BLOCK OVERFLOW             
         BZ    *+8                                                              
         MVI   GETOVER,X'FF'       FLAG OVERFLOW ERROR IN GETIDS                
         TM    4(R1),X'01'         TEST GETIDS PARAMETER ERROR                  
         BNZ   VALID1                                                           
         TM    12(R1),X'04'        TEST SPECIAL WILDCARD MATCH                  
         BZ    VALID0A                                                          
         TM    12(R1),X'08'        TEST READ-ONLY WILDCARD MATCH                
         BZ    *+8                                                              
         OI    READONLY,X'01'                                                   
         B     VALIDY                                                           
VALID0A  CLI   8(R1),0             TEST COMPATIBLE ID COUNT BYTE 2              
         BNE   VALID4                                                           
         CLI   0(R1),0             TEST COMPATIBLE ID COUNT BYTE 1              
         BNE   VALID4                                                           
VALID1   CLI   0(R4),CT0KTEQU      IF AN AUTH RECORD                            
         BE    VALIDY              ID NEED NOT BE COMPATIBLE                    
         CLI   0(R4),C'T'          IS THIS A TERMINAL RECORD                    
         BNE   VALIDN              NO-EXIT                                      
         LA    R1,CTTDATA-CTTREC(R4)                                            
         SR    R0,R0               FIND PRINCIPAL ID ELEMENT                    
*                                                                               
VALID2   CLI   0(R1),0             TEST E-O-R                                   
         BE    VALIDN                                                           
         CLI   0(R1),X'1F'         PRINCIPAL ID                                 
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALID2                                                           
VALID2A  CLC   USERID,CTPID-CTPIDD(R1)                                          
         BE    VALIDY                                                           
         L     R4,APIDREC          READ PRINCIPAL ID RECORD                     
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,CTPID-CTPIDD(R1)                                          
         BAS   RE,CTREAD                                                        
         BNE   VALIDN              CAN'T READ - IS INVALID                      
         B     VALID0              OK GO GET COMPATIBLE ID'S                    
VALID4   L     R1,4(R1)            R1=A(COMPATIBLE ID LIST)                     
         LA    R1,0(R1)                                                         
         CLI   DUB,C'T'                                                         
         BNE   *+8                                                              
         MVI   ALLVALID,C'N'                                                    
VALID6   CLI   0(R1),X'FF'         TEST E-O-L                                   
         BE    VALIDN                                                           
         CLC   0(8,R1),=CL8'ALL'                                                
         BNE   VALID8                                                           
*                                                                               
         TM    SOXFL,SOXDDS        ALL VALID FOR DDS TERMINALS                  
         BO    VALID7                                                           
         TM    TSTAT1,TSTATWEB     ALL VALID FOR WEB TERMINALS                  
         BO    VALID7                                                           
*&&US*&& CLC   TLUID(2),=C'TP'     IS THIS TALENT PARTNERS                      
*&&US*&& BE    VALID7                                                           
*                                  ALL OKAY FOR DDQA TERM IN TEST FAC           
         TM    SYSIDTY,FACITST     TEST IF THIS A TEST FACPAK                   
         BZ    VALID10                                                          
         CLI   DUB+1,C'Q'          TEST IF DDQA TERMINAL                        
         BNE   VALID10                                                          
*                                                                               
VALID7   MVI   ALLVALID,C'Y'                                                    
         B     VALID12                                                          
*                                                                               
VALID8   CLC   0(8,R1),USERID      MATCH ID WITH TABLE                          
         BE    VALID12                                                          
VALID10  LA    R1,12(R1)                                                        
         B     VALID6                                                           
VALID12  CLI   8(R1),0             TEST NEW STYLE ENTRY                         
         BNE   VALIDY                                                           
         TM    9(R1),X'40'         TEST READ-ONLY ENTRY                         
         BZ    VALIDY                                                           
         OI    READONLY,X'01'      SET CONNECTED WITH READ-ONLY ID              
*                                                                               
VALIDY   CR    RB,RB               SET CC=EQUAL IF ID OK                        
         XIT1                                                                   
VALIDN   LTR   RB,RB               SET CC=NOT EQUAL IF ID NOT OK                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE TEXT IN SYSTEM FIELD INCLUDING OPTION KEYWORD=VALUES       *         
***********************************************************************         
VALSOP   NTR1  BASE=*,LABEL=*                                                   
         XC    SOPTIONS,SOPTIONS   CLEAR SAVE AREAS                             
         XC    SYSNAME,SYSNAME                                                  
         MVI   SYSNLEN,0                                                        
         XC    XPINFO,XPINFO                                                    
         XC    XPVERS,XPVERS                                                    
         GOTO1 VSCANNER,DMCB,FLDH,(10,SCANBLK),C',=,='                          
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R6,SCANBLK          R6=A(SCAN BLOCK ENTRY)                       
*                                                                               
         CLI   1(R6),0             CHECK SYSTEM NAME INPUT FIRST                
         BNE   EIIF                                                             
         CLI   0(R6),7                                                          
         BH    EIIF                                                             
         CLI   0(R6),0                                                          
         BE    EIIF                                                             
         LLC   R1,0(R6)                                                         
         STC   R1,SYSNLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8              SAVE INPUT SYSTEM NAME AND LENGTH            
         B     *+10                                                             
         MVC   SYSNAME(0),12(R6)                                                
         B     VALSOP12                                                         
*                                                                               
VALSOP2  CLC   FNDX,NLINES                                                      
         BH    VALSOP20                                                         
         CLI   0(R6),0             L'FIRST HALF                                 
         BE    EIIF                                                             
         L     RE,=A(SOPTAB)       LOOK-UP KEYWORD IN TABLE                     
         A     RE,RELO                                                          
         LLC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
VALSOP4  CLI   0(RE),0             END OF TABLE                                 
         BE    EIIF                                                             
         CLC   0(1,R6),8(RE)       CHECK L'KEYWORD                              
         BL    VALSOP6                                                          
         CLC   0(1,R6),9(RE)                                                    
         BH    VALSOP6                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),12(R6)                                                   
         BE    VALSOP8                                                          
VALSOP6  LA    RE,L'SOPTAB(RE)                                                  
         B     VALSOP4                                                          
VALSOP8  CLC   1(1,R6),10(RE)      CHECK L'VALUE                                
         BL    EIIF                                                             
         CLC   1(1,R6),11(RE)                                                   
         BH    EIIF                                                             
*                                                                               
VALSOP10 TM    SOXFL,SOXDDS        CHECK FOR DDS ONLY KEYWORD                   
         BNZ   *+12                                                             
         TM    12(RE),X'80'                                                     
         BNZ   EIIF                                                             
         MVC   DUB(4),12(RE)       DUB=OPTION NUMBER/RTN ADDRESS                
         NI    DUB,X'7F'                                                        
         LLC   R1,DUB                                                           
         LA    R1,SOPTIONS-1(R1)                                                
         CLI   0(R1),0             CKECK NOT PREVIOUSLY INPUT                   
         BNE   EDIF                                                             
         OI    0(R1),X'80'                                                      
         L     RF,DUB                                                           
         LA    RF,0(RF)                                                         
         A     RF,RELO             RELOCATE ADCON                               
         MVI   FERN,X'FF'                                                       
         BASR  RE,RF               GO TO VALIDATION ROUTINE                     
         CLI   FERN,X'FF'                                                       
         BNE   VALSOPX                                                          
*                                                                               
VALSOP12 LA    R6,32(R6)           BUMP TO NEXT ENTRY                           
         LLC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     VALSOP2                                                          
*                                                                               
VALSOP20 MVI   FNDX,0                                                           
*                                                                               
VALSOPX  XIT1                                                                   
                                                                                
***********************************************************************         
* TABLE OF VALID OPTION KEYWORDS INPUTTABLE IN SYSTEM FIELD           *         
* 0-7    KEYWORD NAME                                                 *         
* 8&9    MIN/MAX KEYWORD LENGTH                                       *         
* 10&11  MIN/MAX VALUE LENGTH                                         *         
* 12     BIT 0 ON=DDS ONLY, BITS 1-7=OPTION NUMBER                    *         
* 13-15  A(VALIDATION ROUTINE)                                        *         
***********************************************************************         
SOPTAB   DS    0CL16                                                            
         DC    C'PCP     ',AL1(01,03,01,10,01+000),AL3(VALPCP)                  
         DC    C'APP     ',AL1(01,03,01,10,02+000),AL3(VALAPP)                  
         DC    C'VERSION ',AL1(01,07,01,08,03+000),AL3(VALXVN)                  
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTERNAL PC PROGRAM NAME (P=10 CHR MAX NAME)               *         
***********************************************************************         
VALPCP   NTR1  BASE=*,LABEL=*                                                   
         L     R3,=V(FAXPTPC)      R3=A(PC PROGRAM TABLE)                       
         A     R3,RELO                                                          
         USING FAXPTABD,R3                                                      
         LLC   R0,FAXPFLAG         R0=ENTRY LENGTH                              
         TM    3(R6),X'80'         PCP=NNN INPUT                                
         BZ    VALPCP01                                                         
         ICM   R0,15,8(R6)         RANGE CHECK NUMBER 1-999                     
         BZ    VALPCP07                                                         
         CHI   R0,XPMAXPGQ         MAX PROGRAM NUMBER 999                       
         BH    VALPCP07                                                         
         STCM  R0,3,XPNUM          SET PROGRAM NUMBER                           
         MVI   XPTYPE,FAXPTPC      SET PC PROGRAM                               
         CH    R0,FAXPNUM          FIRST ENTRY CONTAINS MAXIMUM VALUE           
         BNH   VALPCPX                                                          
         OI    XPTYPE,TXPTXMAX     SET PROGRAM NUMBER EXCEEDS MAXIMUM           
         B     VALPCPX                                                          
*                                                                               
VALPCP01 CLI   1(R6),10            MAX PROGNAME IS 10 CHRS                      
         BNH   *+8                                                              
         MVI   1(R6),10                                                         
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
*                                                                               
VALPCP02 CLI   FAXPTYPE,0          1ST PASS - EXE NAME COMPARE                  
         BE    VALPCP04                                                         
         TM    FAXPTYPE,FAXPTPC                                                 
         BZ    VALPCP03                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),FAXPNEXE                                                
         BE    VALPCP08                                                         
VALPCP03 AR    R3,R0                                                            
         B     VALPCP02                                                         
*                                                                               
VALPCP04 L     R3,=V(FAXPTPC)      2ND PASS - APPL NAME COMPARE                 
         A     R3,RELO                                                          
VALPCP05 CLI   FAXPTYPE,0          TEST END-OF-TABLE                            
         BE    VALPCP07                                                         
         TM    FAXPTYPE,FAXPTPC                                                 
         BZ    VALPCP06                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),FAXPNAME                                                
         BE    VALPCP08                                                         
VALPCP06 AR    R3,R0                                                            
         B     VALPCP05                                                         
*                                                                               
VALPCP07 L     R3,=V(FAXPTPC)      SET UP DEFAULT PC PROGRAM                    
         A     R3,RELO                                                          
         AR    R3,R0                                                            
*                                                                               
VALPCP08 MVC   XPNUM,FAXPNUM                                                    
         MVC   XPTYPE,FAXPTYPE                                                  
VALPCPX  XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MF APPLICATION EXTERNAL PROGRAM NAME/SCRIPT (A=10 CHR NAME)*         
* EXTERNAL NAME TABLE ONLY HAS ONE NAME - FAXPNAME NOT DEFINED        *         
***********************************************************************         
VALAPP   NTR1  BASE=*,LABEL=*                                                   
         L     R3,=V(FAXPTMF)      R3=A(MF APP PROGRAM/SCRIPT TABLE)            
         A     R3,RELO                                                          
         USING FAXPTABD,R3                                                      
         LLC   R0,FAXPFLAG         R0=ENTRY LENGTH                              
         TM    3(R6),X'80'         APP=NNN INPUT                                
         BZ    VALAPP01                                                         
         ICM   R0,15,8(R6)         RANGE CHECK NUMBER 1-999                     
         BZ    VALAPP07                                                         
         CHI   R0,XPMAXPGQ         MAX PROGRAM NUMBER 999                       
         BH    VALAPP07                                                         
         STCM  R0,3,XPNUM          SET PROGRAM NUMBER                           
         MVI   XPTYPE,FAXPTMFS     SET MF PROGRAM SCRIPT                        
         CH    R0,FAXPNUM          FIRST ENTRY CONTAINS MAXIMUM VALUE           
         BNH   VALAPPX                                                          
         OI    XPTYPE,TXPTXMAX     SET PROGRAM NUMBER EXCEEDS MAXIMUM           
         B     VALAPPX                                                          
*                                                                               
VALAPP01 CLI   1(R6),10            ALLOW MAX OF 10 CHRS FOR NAME                
         BNH   *+8                                                              
         MVI   1(R6),10                                                         
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
*                                                                               
VALAPP02 CLI   FAXPTYPE,0          1ST PASS - EXE NAME COMPARE                  
         BE    VALAPP04                                                         
         TM    FAXPTYPE,FAXPTMFS                                                
         BZ    VALAPP03                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),FAXPNEXE                                                
         BE    VALAPP08                                                         
VALAPP03 AR    R3,R0                                                            
         B     VALAPP02                                                         
*                                                                               
VALAPP04 B     VALAPP07            NO 2ND PASS AS ONLY ONE NAME                 
         L     R3,=V(FAXPTMF)      2ND PASS - APPL NAME COMPARE                 
         A     R3,RELO                                                          
VALAPP05 CLI   FAXPTYPE,0          END-OF-TABLE                                 
         BE    VALAPP07                                                         
         TM    FAXPTYPE,FAXPTMFS                                                
         BZ    VALAPP06                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),FAXPNAME                                                
         BE    VALAPP08                                                         
VALAPP06 AR    R3,R0                                                            
         B     VALAPP05                                                         
*                                                                               
VALAPP07 L     R3,=V(FAXPTMF)      SET UP DEFAULT APP PROGRAM                   
         A     R3,RELO                                                          
         AR    R3,R0                                                            
*                                                                               
VALAPP08 MVC   XPNUM,FAXPNUM                                                    
         MVC   XPTYPE,FAXPTYPE                                                  
VALAPPX  XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTERNAL PROGRAM VERSION CODE (V=AABBCCDD)                 *         
* VERSION TABLE ENTRIES MUST BE IN PCP PROGRAM NUMBER ORDER           *         
* MULTIPLE ENTRIES FOR SAME PCP MUST BE IN FOLLOWING SEQUENCE         *         
* X'08'  VALID FOR DDS TERMINAL ONLY                                  *         
* X'80'  VALID FOR USER ID NUMBER ONLY                                *         
* X'40'  VALID FOR AGENCY ALPHA ONLY                                  *         
* X'20'  VALID FOR COUNTRY NUMBER ONLY                                *         
* X'00'  VALID FOR ALL (MUST BE LAST ENTRY)                           *         
***********************************************************************         
VALXVN   NTR1  BASE=*,LABEL=*                                                   
         XC    XPVERS,XPVERS                                                    
         MVI   XPVERFLG,X'80'      SET V=XXX... INPUT                           
         SR    R4,R4                                                            
         XC    FULL,FULL                                                        
         CLI   1(R6),8             CHECK V=AABBCCDD INPUT                       
         BNE   VALXVNW                                                          
         TM    3(R6),X'20'         TEST DATA IS HEXADECIMAL                     
         BZ    VALXVNW                                                          
         GOTO1 VHEXIN,DMCB,22(R6),FULL+3,2                                      
         OC    12(4,R1),12(R1)                                                  
         BZ    VALXVNW                                                          
         MVC   DUB+0(1),FULL+3                                                  
         NI    FULL+3,X'0F'        MAX VALUE FOR AA IS 0F                       
         O     R4,FULL                                                          
         SLL   R4,4                                                             
         GOTO1 VHEXIN,DMCB,24(R6),FULL+3,2                                      
         OC    12(4,R1),12(R1)                                                  
         BZ    VALXVNW                                                          
         MVC   DUB+1(1),FULL+3                                                  
         NI    FULL+3,X'0F'        MAX VALUE FOR BB IS 0F                       
         O     R4,FULL                                                          
         SLL   R4,8                                                             
         GOTO1 VHEXIN,DMCB,26(R6),FULL+3,2                                      
         OC    12(4,R1),12(R1)                                                  
         BZ    VALXVNW                                                          
         MVC   DUB+2(1),FULL+3                                                  
         O     R4,FULL                                                          
         SLL   R4,8                                                             
         GOTO1 VHEXIN,DMCB,28(R6),FULL+3,2                                      
         OC    12(4,R1),12(R1)                                                  
         BZ    VALXVNW                                                          
         MVC   DUB+3(1),FULL+3                                                  
         O     R4,FULL                                                          
         STCM  R4,7,XPVER                                                       
*                                                                               
VALXVN3  TM    XPTYPE,FAXPTPC      RESOLVE CONFLICTS IN PC APP NAMES            
         BZ    VALXVNX                                                          
*&&US                                                                           
VALXVN4  CLC   XPNUM,=Y(XPMMSPOQ)  MATCHMAKER FOR SPOTPAK V=03....              
         BE    *+14                                                             
         CLC   XPNUM,=Y(XPMMNETQ)  FOR NETPAK  V=01....                         
         BNE   VALXVN4X                                                         
         MVC   XPNUM,=Y(XPMMNETQ)                                               
         CLI   DUB,X'03'           TEST FIRST BYTE OF VERSION                   
         BL    VALXVN4X                                                         
         MVC   XPNUM,=Y(XPMMSPOQ)                                               
VALXVN4X EQU   *                                                                
*&&                                                                             
VALXVN7  TM    XPTYPE,FAXPTVT      TEST IF ENTRY IN VERSION TABLE               
         BZ    VALXVNX                                                          
         LT    R3,=V(FAXPTPV)      R3=A(PC PROGRAM VERSION TABLE)               
         BZ    VALXVNX                                                          
         A     R3,RELO                                                          
         USING FAXVTABD,R3                                                      
         LR    R1,R3               R1=A(FIRST ENTRY)                            
         LLC   R0,FAXVFLAG         R0=ENTRY LENGTH                              
         AR    R3,R0               SKIP FIRST ENTRY                             
         MVC   BYTE,AGYCTRY        BYTE HAS AGENCY COUNTRY CODE                 
         CLI   BYTE,0              IF ZERO UK=1 AND US=2                        
         BNE   *+8                                                              
*&&UK*&& MVI   BYTE,1                                                           
*&&US*&& MVI   BYTE,2                                                           
*                                                                               
VALXVN8  CLC   FAXVNUM,XPNUM       TEST IF ENTRY FOR PCP=PROGRAM                
         BE    VALXVN8A                                                         
         BH    VALXVN9             LAST ENTRY HAS HIGH NUMBER X'FFFF'           
         AR    R3,R0                                                            
         B     VALXVN8                                                          
*                                                                               
VALXVN8A TM    FAXVTYPE,FAXVDDS    TEST IF DDS TERMINAL ONLY ENTRY              
         BZ    VALXVN8B                                                         
         TM    SOXFL,SOXDDS                                                     
         BO    *+10                                                             
         AR    R3,R0               SKIP IF NOT DDS TERMINAL                     
         B     VALXVN8                                                          
*                                                                               
VALXVN8B EQU   *                                                                
*                                                                               
VALXVN8C TM    FAXVTYPE,FAXVUSR    TEST IF USERID NUMBER ENTRY                  
         BZ    VALXVN8D                                                         
         CLC   FAXVUSER,USERNO                                                  
         BE    VALXVN8M                                                         
         AR    R3,R0                                                            
         B     VALXVN8                                                          
*                                                                               
VALXVN8D TM    FAXVTYPE,FAXVAGY    TEST IF AGENCY ALPHA ENTRY                   
         BZ    VALXVN8E                                                         
         CLC   FAXVUSER,USERALPH                                                
         BE    VALXVN8M                                                         
         AR    R3,R0                                                            
         B     VALXVN8                                                          
*                                                                               
VALXVN8E TM    FAXVTYPE,FAXVCTRY   TEST IF COUNTRY ENTRY                        
         BZ    VALXVN8F                                                         
         CLC   FAXVUSER(1),BYTE                                                 
         BE    VALXVN8M                                                         
         AR    R3,R0                                                            
         B     VALXVN8                                                          
*                                                                               
VALXVN8F B     VALXVN8M            MUST BE VALID FOR ALL ENTRY (LAST)           
*                                                                               
VALXVN8G AR    R3,R0               BUMP TO NEXT ENTRY                           
         B     VALXVN8                                                          
*                                                                               
VALXVN8M LR    RF,R3               SAVE DISPLACEMENT OF MATCHING ENTRY          
         SR    RF,R1                                                            
         STH   RF,XPVERDSP                                                      
*                                                                               
VALXVN9  XR    RF,RF               LOCATE MATCHING ENTRY                        
         ICM   RF,3,XPVERDSP                                                    
         BZ    VALXVNX             NOT FOUND                                    
         LR    R3,R1                                                            
         AR    R3,RF                                                            
         OI    XPVERFLG,X'40'      SET VERSION TABLE ENTRY FOUND                
*                                                                               
         CLC   TODAY3,FAXVDATE     TEST DATE OF ENTRY                           
         BL    VALXVNX                                                          
         CLC   XPVER,FAXVVER       TEST IF VERSION VALID FOR DATE               
         BNL   VALXVNX                                                          
         OI    XPVERFLG,X'20'      SET VERSION OUT OF DATE                      
         MVC   XPVERNEW,FAXVNEW    SET NEW APPLICATION NAME                     
         B     VALXVNX                                                          
*                                                                               
VALXVNW  OI    XPVERFLG,X'01'      SET INVALID VERSION VALUE                    
VALXVNX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SAVE CURRENT SCREEN BEFORE CONNECT TO NEW PROGRAM                   *         
***********************************************************************         
VALSAV   NTR1  BASE=*,LABEL=*                                                   
         MVI   STOLEN,X'FF'        SET NO SESSION STOLEN                        
         SR    R0,R0               R0=MAXIMUM NUMBER OF SESSIONS                
         ICM   R0,3,SSMAX                                                       
         BZ    VALSAVX                                                          
*                                                                               
VALSAV0  MVC   LSTSES,TSSSWAP      LAST SESSION                                 
         NI    LSTSES,X'0F'                                                     
         UNPK  PRVSES,TSSSWAP      PREVIOUS SESSION                             
         NI    PRVSES,X'0F'                                                     
         MVC   RSVSES,TSSRSRV      RESERVED SESSION                             
         NI    RSVSES,X'0F'                                                     
         UNPK  TRASES,TSSRSRV      TRANSFER SESSION                             
         NI    TRASES,X'0F'                                                     
         MVI   NOMSES,X'FF'        NOMINATED SESSION                            
         MVC   CH1SES(4),=4X'FF'                                                
VALSAV0A CLI   14(R6),C'A'         SVA THRU SVH GIVES NOMINATED SESSION         
         BL    VALSAV1                                                          
         CLI   14(R6),C'H'                                                      
         BH    VALSAV0B                                                         
         IC    R1,14(R6)                                                        
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         CR    R1,R0               TEST WITH MAX SESSIONS                       
         BH    VALSAV1                                                          
         BCTR  R1,0                                                             
         CLM   R1,1,RSVSES         USE NOMINATED SESSION UNLESS RSRVD           
         BNE   VALSAV7                                                          
         STC   R1,NOMSES           RECORD SESSION NOMINATED                     
         B     VALSAV1                                                          
VALSAV0B CLI   14(R6),C'R'         SVR MEANS USE RESERVED SESSION               
         BNE   VALSAV0C                                                         
         LLC   R1,RSVSES                                                        
         CR    R1,R0                                                            
         BL    VALSAV7             USE RESERVED SESSION IF DEFINED              
         B     VALSAV1                                                          
VALSAV0C CLI   14(R6),C'T'         SVT MEANS USE TRANSFER SESSION               
         BNE   VALSAV0D                                                         
         LLC   R1,TRASES                                                        
         CR    R1,R0                                                            
         BL    VALSAV7             USE TRANSFER SESSION IF DEFINED              
         B     VALSAV1                                                          
VALSAV0D CLI   14(R6),C'X'         SVX MEANS USE LAST LOGICAL SESSION           
         BNE   VALSAV0E                                                         
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLM   R1,1,RSVSES         USE NOMINATED SESSION UNLESS RSRVD           
         BNE   VALSAV7                                                          
         STC   R1,NOMSES           RECORD SESSION NOMINATED                     
         B     VALSAV1                                                          
VALSAV0E CLI   14(R6),C'Z'         SVZ MEANS USE EXTRA PHYSICAL SESSION         
         BNE   VALSAV1                                                          
         CLI   XCTLTYP,C'Z'        ONLY VIA XCTL                                
         BNE   VALSAV1                                                          
         LH    R1,SSMXP            ONLY IF THERE IS AN EXTRA SESSION            
         CH    R1,SSMAX                                                         
         BNH   VALSAV1                                                          
         BCTR  R1,0                                                             
         STC   R1,XTRSES           SET EXTRA SESSION                            
         B     VALSAV7             USE EXTRA SESSION                            
*                                                                               
VALSAV1  LLC   R1,CURSES           R1=NEW SESSION NUMBER                        
VALSAV2  IC    RF,SSACBITS(R1)                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0           TEST SESSION AVAILABLE                       
         BO    VALSAV3                                                          
         CLM   R1,1,RSVSES         TEST SESSION RESERVED                        
         BE    VALSAV2A                                                         
         CLM   R1,1,TRASES         TEST TRANSFER SESSION                        
         BE    VALSAV2A                                                         
         CLI   NOMSES,X'FF'        USE FIRST AVAILABLE SESSION                  
         BE    VALSAV7                                                          
         CLM   R1,1,NOMSES         USE ALSO IF NOMINATED                        
         BE    VALSAV7                                                          
         CLI   CH1SES,X'FF'        TEST IF 1ST CHOICE SET                       
         BNE   VALSAV4                                                          
         STC   R1,CH1SES           1ST CHOICE FOR MONINATED                     
         B     VALSAV4                                                          
VALSAV2A CLM   R1,1,NOMSES         USE RESERVED/TRANSFER IF NOMINATED           
         BE    VALSAV7                                                          
         B     VALSAV4                                                          
*                                                                               
VALSAV3  EX    RF,*+8              TEST IF SESSION CANT BE USED                 
         B     *+8                                                              
         TM    TSSXBITS,0                                                       
         BO    VALSAV4                                                          
VALSAV3A CLM   R1,1,CURSES         SESSION ACTIVE (ALREADY BEING USED)          
         BNE   VALSAV3B                                                         
         CLM   R1,1,NOMSES         1ST CHOICE IS NOMINATED SESSION              
         BNE   *+8                                                              
         STC   R1,CH1SES                                                        
         B     VALSAV4                                                          
VALSAV3B CLM   R1,1,RSVSES         RESERVED SESSION                             
         BE    VALSAV4                                                          
         CLM   R1,1,TRASES         TRANSFER SESSION                             
         BE    VALSAV4                                                          
VALSAV3C CLM   R1,1,NOMSES         1ST CHOICE IS NOMINATED SESSION              
         BNE   *+12                                                             
         STC   R1,CH1SES                                                        
         B     VALSAV4                                                          
         CLI   CH4SES,X'FF'        4TH CHOICE IS ANY SESSION                    
         BNE   *+8                                                              
         STC   R1,CH4SES                                                        
         CLM   R1,1,LSTSES         3RD CHOICE IS LAST SESSION                   
         BNE   *+12                                                             
         STC   R1,CH3SES                                                        
         B     VALSAV4                                                          
         CLM   R1,1,PRVSES         2ND CHOICE IS PREVIOUS SESSION               
         BNE   *+12                                                             
         STC   R1,CH2SES                                                        
         B     VALSAV4                                                          
         CLI   CH1SES,X'FF'                                                     
         BNE   VALSAV4                                                          
         STC   R1,CH1SES           1ST CHOICE IS NON LAST OR NON PREV           
*                                                                               
VALSAV4  LA    R1,1(R1)            BUMP TO NEXT SESSION                         
         CH    R1,SSMAX                                                         
         BL    *+6                                                              
         SR    R1,R1                                                            
         BCT   R0,VALSAV2                                                       
*                                                                               
VALSAV5  CLI   14(R6),C'U'         TEST IF UNCONDITIONAL SAVE (SVU)             
         B     VALSAV6             *NOP* BE VALSAV6                             
         CLI   SSMAX+1,2           TEST IF ONLY TWO SESSIONS                    
         BE    VALSAV6                                                          
         CLC   TSYS(2),=X'0A0C'    TEST CONTROL/MAD                             
         BE    VALSAV6                                                          
         B     VSEASCIU            ERROR NO AVAILABLE FREE SESSIONS             
*                                                                               
VALSAV6  LA    RF,CH1SES           FIND BEST CHOICE ACTIVE SESSION              
         LA    R0,4                                                             
         CLI   0(RF),X'FF'                                                      
         BNE   *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         B     VSEASCIU            ERROR NO AVAILABLE FREE SESSIONS             
         IC    R1,0(RF)                                                         
         STC   R1,NEWSES           SET NEW SESSION AND DISCONNECT               
         STC   R1,STOLEN           SET SESSION STOLEN                           
         LR    R0,R1                                                            
         GOTO1 ATWASVR,DMCB,((R0),(R9)),(X'80',0)                               
         LR    R1,R0                                                            
*                                                                               
VALSAV7  STC   R1,NEWSES           SET NEW SESSION NUMBER                       
         CLM   R1,1,CURSES                                                      
         BE    VALSAVX             OK IF NEW SESS IS SAME AS CURRENT            
         LR    R0,R1                                                            
         GOTO1 ATWASVR,DMCB,((R0),(R9)),(X'00',0)                               
         LR    R1,R0                                                            
         SR    RE,RE                                                            
         ICM   RE,7,DMCB+5                                                      
         AH    RE,CHKDSP                                                        
         USING CHKPTD,RE                                                        
         CLI   14(R6),C'P'         TEST IF SVP (PROGRAM GENERATED)              
         BNE   *+12                                                             
         TM    TSTAT6,TST6STRO     THEN IF STEREO LEAVE ALONE                   
         BO    VALSAV7D                                                         
         CLI   XCTLTYP,C'L'        TEST IF XCTL TO A NEW SESSION                
         BE    *+12                                                             
         TM    TSTAT6,TST6STRO     YES-THEN IF STEREO LEAVE ALONE               
         BO    VALSAV7D                                                         
         NI    TSTAT6,255-TST6STRO-TST6STFU-TST6STSS                            
         TM    CHUTLST6,TST6STRO                                                
         BZ    *+8                                                              
         OI    TSTAT6,TST6STRO     RESTORE STEREO FLAGS FROM CHKPNT             
         TM    CHUTLST6,TST6STFU                                                
         BZ    *+8                                                              
         OI    TSTAT6,TST6STFU                                                  
         TM    CHUTLST6,TST6STSS                                                
         BZ    *+8                                                              
         OI    TSTAT6,TST6STSS                                                  
         CLI   CTSTIN1,0           WAS S=ABC INPUT                              
         BE    VALSAV7D            NO-LEAVE AS IT WAS LAST TIME                 
VALSAV7A CLI   CTSTIN1,C'*'        TEST ANY CHANGE TO 1ST STEREO FLAG           
         BE    VALSAV7B                                                         
         NI    TSTAT6,255-TST6STRO                                              
         CLI   CTSTIN1,C'Y'                                                     
         BNE   VALSAV7B                                                         
         OI    TSTAT6,TST6STRO                                                  
VALSAV7B CLI   CTSTIN2,C'*'        TEST ANY CHANGE TO 2ND STEREO FLAG           
         BE    VALSAV7C                                                         
         NI    TSTAT6,255-TST6STFU                                              
         CLI   CTSTIN2,C'Y'                                                     
         BNE   VALSAV7C                                                         
         OI    TSTAT6,TST6STFU                                                  
VALSAV7C CLI   CTSTIN3,C'*'        TEST ANY CHANGE TO 3RD STEREO FLAG           
         BE    VALSAV7D                                                         
         NI    TSTAT6,255-TST6STSS                                              
         NI    TSTAT8,255-(TST8STSS+TST8BINT)                                   
         CLI   CTSTIN3,C'Y'                                                     
         BNE   VALSAV7E                                                         
         OI    TSTAT6,TST6STSS                                                  
         OI    TSTAT8,TST8STSS                                                  
         B     VALSAV7D                                                         
VALSAV7E CLI   CTSTIN3,C'B'                                                     
         BNE   VALSAV7D                                                         
         OI    TSTAT6,TST6STSS                                                  
         OI    TSTAT8,TST8STSS                                                  
         TM    TSTAT9,TSTNVRSN                                                  
         BZ    *+8                                                              
         OI    TSTAT8,TST8BINT                                                  
*                                                                               
VALSAV7D MVC   CTSTEREO,TSTAT6     SAVE NEW SESSION STEREO FLAGS                
         NI    CTSTEREO,TST6STRO+TST6STFU+TST6STSS                              
         MVC   CTSTERE8,TSTAT8                                                  
         NI    CTSTERE8,TST8STSS+TST8BINT                                       
*                                                                               
VALSAV8  STC    R1,TSESSION         SET NEW SESSION IN UTL                      
         CLI   STOLEN,X'FF'        TEST SESSION STOLEN THIS TIME                
         BE    VALSAV9                                                          
         SR    RE,RE                                                            
         LLC   RE,STOLEN           SET SESSION STOLEN FLAG IN UTL               
         IC    RF,SSACBITS(RE)                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         OI    TNAHNAH,0                                                        
*                                                                               
VALSAV9  MVI   TEMPESTF,0          SET NO TEMPEST                               
         ICM   RE,15,ATWASES       TEST IF DISPLAYING SESSION                   
         BZ    VALSAVX                                                          
         N     R1,=X'00000007'     SESSION CHR IS X'A0'+TSESSION+1              
         LA    R1,1(R1)                                                         
         STC   R1,0(RE)                                                         
         OI    0(RE),X'80'         SESSION IS LOWER CASE A THRU H               
         B     VALSAVX                                                          
*                                                                               
VSEASCIU LA    RE,133              ALL SESSIONS CURRENTLY IN USE                
         B     VALSAVXX                                                         
*                                                                               
VALSAVX  XR    RE,RE                                                            
VALSAVXX ST    RE,RSLT             EXIT WITH RSLT=ERROR NUMBER                  
         LTR   RE,RE                                                            
         XIT1                                                                   
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGRAM (P=XX OR PROGNAME(=MAPID)) - CALLED BY VALPROG     *         
***********************************************************************         
VALPGM   NTR1  BASE=*,LABEL=*                                                   
         XC    APGMNTRY,APGMNTRY                                                
         CLI   0(R6),7             ALLOW >7 CHARACTER PROGRAM NAMES             
         BNH   *+8                 THOUGH MATCH ON FIRST 7 CHARS ONLY           
         MVI   0(R6),7                                                          
         LLC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         L     R3,APGMLIST         SET UP FOR PGMLIST BXLE                      
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
         CLI   1(R6),2             CHECK P=XX INPUT                             
         BNE   VALPGM4                                                          
         TM    3(R6),X'20'         TEST DATA IS HEXADECIMAL                     
         BZ    VALPGM4                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=C'PROGRAM'                                             
         BNE   VALPGM4                                                          
         TM    SOXFL,SOXDDS        ONLY VALID FOR DDS TERMINALS                 
         BZ    VALPGM4                                                          
         GOTO1 VHEXIN,DMCB,22(R6),PROGNUM,2                                     
         MVC   PROGANUM,PROGNUM    SET PROGRAM ACCESS NUMBER                    
*                                                                               
         CLC   PGMNUM,PROGNUM      CHECK IF PROGRAM DEFINED IN PGMLIST          
         BE    VALPGM2                                                          
         BXLE  R3,R4,*-10                                                       
         MVC   PROGNAME(2),=C'P='  NO-ASSUME CONVERTED OLAI PROGRAM             
         MVC   PROGNAME+2(2),22(R6)                                             
         MVI   PROGIND,PGMIRFU+PGMIOLA                                          
         MVI   PROGIND2,PGMISWT+PGMINOD                                         
         MVI   PROGIND3,0                                                       
         OI    CTTEST,TTESTCIL     FORCE TO CIL MODE                            
         OI    PROGOPT,X'40'                                                    
         B     VALPGM7                                                          
*                                                                               
VALPGM2  ST    R3,APGMNTRY                                                      
         MVC   PROGNAME,PGMNAME                                                 
         CLI   PGMALNUM,0          TEST PROGRAM ACCESS OVERRIDE                 
         BE    *+10                                                             
         MVC   PROGANUM,PGMALNUM                                                
         MVC   PROGIND,PGMIND                                                   
         MVC   PROGIND2,PGMIND2                                                 
         MVC   PROGIND3,PGMIND3                                                 
         MVC   PROGIND4,PGMIND4                                                 
         MVC   PROGSYS,PGMCOSYS                                                 
         OI    PROGOPT,X'40'                                                    
         B     VALPGM7                                                          
*                                  VALIDATE PROGNAME                            
VALPGM4  CLI   PGMCTRY,0           TEST IF PROGRAM DEFINED FOR CTRY             
         BNE   *+16                                                             
         TM    CTRYFLAG,X'80'      TEST IF HAVE CTRY SPECIFIC LIST              
         BZ    VALPGM5                                                          
         B     VPEIPN                                                           
         CLC   PGMCTRY,AGYCTRY     TEST IF TERMINAL SAME CTRY                   
         BNE   *+12                                                             
         OI    CTRYFLAG,X'80'      SET CTRY SPECIFIC LIST                       
         B     VALPGM5                                                          
         TM    CTRYFLAG,X'80'                                                   
         BO    VPEIPN                                                           
         BXLE  R3,R4,VALPGM4       IGNORE IF COUNTRIES DONT MATCH               
         B     VPEIPN                                                           
VALPGM5  EX    R1,*+8              COMPARE INPUT PROGRAM NAME                   
         B     *+10                                                             
         CLC   PGMNAME(0),12(R6)                                                
         BE    *+12                                                             
         BXLE  R3,R4,VALPGM4                                                    
         B     VPEIPN                                                           
VALPGM6  ST    R3,APGMNTRY                                                      
         MVC   PROGNUM,PGMNUM                                                   
         MVC   PROGANUM,PGMNUM                                                  
         CLI   PGMALNUM,0          TEST PROGRAM ACCESS OVERRIDE                 
         BE    *+10                                                             
         MVC   PROGANUM,PGMALNUM                                                
         MVC   PROGNAME,PGMNAME                                                 
         MVC   PROGIND,PGMIND                                                   
         MVC   PROGIND2,PGMIND2                                                 
         MVC   PROGIND3,PGMIND3                                                 
         MVC   PROGIND4,PGMIND4                                                 
         MVC   PROGSYS,PGMCOSYS                                                 
         TM    PROGIND4,PGMILINK   TEST EXTERNAL LINK PROGRAM OVERIDE           
         BZ    VALPGM7                                                          
         MVC   PROGNLNK,PGMNUM     OVERIDE WITH LINK PROGRAM NUMBER             
         MVC   PROGNUM,PGMLNUM                                                  
         BAS   RE,GETPLNK          EXTRACT NEW APGMNTRY FOR LINK PRG            
*                                                                               
VALPGM7  TM    PROGIND,PGMINOP     TEST PROGRAM NO-OP                           
         BO    VPEPNO                                                           
         TM    PROGIND3,PGMIPC     PC PROGRAM CANNOT CONNECT                    
         BO    VPEIPN                                                           
         TM    SOXFL,SOXDDS                                                     
         BO    VALPGM7A                                                         
         TM    PROGIND,PGMIACC     TEST RESTRICTED ACCESS                       
         BO    VPEPRA                                                           
         TM    PROGIND4,PGMIACCI   TEST INDIRECT ACCESS                         
         BZ    VALPGM7A                                                         
         CLI   XCTLTYP,0           MUST CONNECT VIA XCTL                        
         BE    VPEPRA                                                           
VALPGM7A TM    TTYPE,TTYPEWAP                                                   
         BNZ   *+12                                                             
         TM    PROGIND3,PGMIWEB    TEST WEB PROGRAM                             
         BNZ   VPEIPN                                                           
*                                                                               
VALPGM8  SR    RF,RF               TEST IF AGENCY SPECIFIC PROGRAM              
         ICM   RF,7,PGMAGYLA                                                    
         BZ    VALPGM8X                                                         
         TM    SOXFL,SOXDDS                                                     
         BO    VALPGM8X                                                         
         CLC   USERALPH,SPACES     TEST IF AGENCY KNOWN                         
         BNH   VALPGM8X                                                         
         LA    R0,9                                                             
VALPGM8A CLC   0(2,RF),SPACES      AGENCY MUST BE IN LIST FOR PROGRAM           
         BE    VPEIPN                                                           
         CLC   0(2,RF),USERALPH                                                 
         BE    VALPGM8X                                                         
         LA    RF,2(RF)                                                         
         BCT   R0,VALPGM8A                                                      
         B     VPEIPN                                                           
VALPGM8X EQU   *                                                                
*                                                                               
         TM    PROGIND3,PGMITSTF   PROGRAM VALID ONLY ON TEST FACPAK            
         BZ    *+12                                                             
         TM    SYSIDTY,FACITST     TEST IF THIS A TEST FACPAK                   
         BZ    VPEIPN                                                           
*&&US*&& TM    SOXFL,SOXDDS                                                     
*&&US*&& BO    VALPGME                                                          
         TM    SYSIDTY,FACITST     TEST FACPAK NO RESTRICTIONS                  
         BO    VALPGME                                                          
*&&UK*&& TM    TSTAT8,TST8ASWP     AUTO SWAP NO RESTRICTIONS                    
*&&UK*&& BO    VALPGME                                                          
         TM    PROGIND2,PGMINORA   TEST IF NO READ ONLY ACCESS                  
         BZ    VALPGME                                                          
         L     RE,ASENTRY          CANT USE PROGRAM IN READONLY SYSTEM          
         TM    SEIND-SELISTD(RE),SEIRONLY                                       
         BNZ   VPENORA                                                          
*                                                                               
VALPGME  CLI   PSECFLAG,C'Y'       PERSON CONVERTED TO NEW SECURITY?            
         BNE   VALPGMF                                                          
         TM    PWFLAG,PWREQD       ELSE USERID MUST HAVE FLAG FOR PWD           
         BO    VALPGMF                                                          
         LA    R1,CNTIDH                                                        
         ST    R1,FADR                                                          
         B     VPEIIF                                                           
*                                                                               
VALPGMF  TM    PROGOPT,X'40'       TEST IF P=XX INPUT                           
         BO    VALPGMX                                                          
         CLI   1(R6),0             TEST IF PGM=MAPID INPUT                      
         BE    VALPGMX                                                          
         OI    MAPOPT,X'80'        SET MAP INPUT                                
         L     R4,AMAPREC                                                       
         USING CTMREC,R4                                                        
         XC    CTMKEY,CTMKEY       BUILD KEY OF MAP RECORD                      
         MVI   CTMKTYP,C'M'                                                     
         MVC   CTMKSYS,OVSYS                                                    
         MVC   CTMKPROG,PROGNUM                                                 
         MVI   CTMKSCRN,X'FF'                                                   
         MVC   CTMKUSER,USERALPH                                                
         MVC   CTMKOTHR,22(R6)                                                  
         DROP  R4                                                               
         BAS   RE,CTREAD           READ MAP RECORD                              
         BE    VALPGMX                                                          
         NI    MAPOPT,255-X'80'    IGNORE INVALID MAP ID                        
         B     VALPGMX                                                          
         LA    RE,107              INVALID MAP ID                               
         B     VALPGMXX                                                         
*                                                                               
VPEIPN   LA    RE,SREPRG           INVALID PROGRAM                              
         B     VALPGMXX                                                         
VPEPNO   LA    RE,103              PROGRAM NOT OPERATIONAL                      
         B     VALPGMXX                                                         
VPEPRA   LA    RE,104              PROGRAM ACCESS IS RESTRICTED                 
         B     VALPGMXX                                                         
VPENORA  LA    RE,129              PROGRAM NOT AVAIL IN THIS ADV APPLIC         
         B     VALPGMXX                                                         
VPEIIF   LA    RE,SREIIF           INVALID INPUT FIELD                          
         B     VALPGMXX                                                         
*                                                                               
VALPGMX  XR    RE,RE                                                            
VALPGMXX ST    RE,RSLT             EXIT WITH RSLT=ERROR NUMBER                  
         LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET LINK PROGRAM ENTRY - LINK PROGRAM NUMBER = PROGNUM              *         
***********************************************************************         
GETPLNK  NTR1                                                                   
         L     R3,APGMLIST         SET UP FOR PGMLIST BXLE                      
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
         CLC   PGMNUM,PROGNUM                                                   
         BE    GETPLNK1                                                         
         BXLE  R3,R4,*-10                                                       
         B     GETPLNKX                                                         
*                                                                               
GETPLNK1 ST    R3,APGMNTRY                                                      
         MVC   PROGNAME,PGMNAME                                                 
GETPLNKX XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONNECT IS VALID - SET RELEVENT FIELDS IN SYSTEM CONTROL BLOCKS     *         
***********************************************************************         
CONOK    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETDATA          CHECK IF ANY DATA PASSED WITH CT             
*                                                                               
         OC    TCNVPRS,TCNVPRS     CONVERSATION PAIRS SET?                      
         BZ    *+8                                                              
         BRAS  RE,CLRDIAL          YES-CLEAR OUT ANY DIALOGUE MODE SET          
*                                                                               
         TM    PROGIND4,PGMICLRG   TEST TO CLEAR GLOBALS FOR PROGRAM            
         BZ    CONOK0                                                           
         L     RF,=A(GLOCLR)                                                    
         A     RF,RELO                                                          
         LA    R0,1                SET CLEAR BECAUSE PGMICLRG                   
         BASR  RE,RF                                                            
*                                                                               
CONOK0   MVC   XTEST,TTEST         SAVE UTL VALUES THAT MAY BE RESET            
         MVC   XACCS,TACCS                                                      
         MVC   XSTATB,TSTATB                                                    
         MVC   XSTAT5,TSTAT5                                                    
*                                                                               
         L     R1,TUTLXADR                                                      
         USING XAUTLD,R1                                                        
         SAM31                                                                  
         MVC   XTICKET,TTICKETN                                                 
         SAM24                                                                  
         DROP  R1                                                               
*                                                                               
         MVC   XPSAVE,TXPINFO                                                   
         MVC   XOSIN,TOSIN                                                      
         MVC   XOFFCODE,TOFFCODE                                                
*                                                                               
CONOK1   OC    PERSONID,PERSONID   CREATE PERSON ID GLOBAL IF DEFINED           
         BZ    CONOK1X                                                          
         GOTO1 VGLOBBER,DMCB,=C'PUTD',PERSONID,L'PERSONID,GLVCTPID              
CONOK1X  EQU   *                                                                
*                                                                               
CONOK2   L     RF,=A(REPROVE)      READ PROVER RECORD MESSAGE                   
         A     RF,RELO             INTO TIA+500                                 
         BASR  RE,RF                                                            
*                                                                               
*&&US*&& BRAS  RE,DARETO                                                        
*                                                                               
CONOK4   XC    TSTATSVC,TSTATSVC   RESET VIOLATION COUNT                        
         XC    TSTATNUL,TSTATNUL   CLEAR FAILED COUNT                           
         NI    TSSVNUM,255-X'80'   SET NO LONGER ACTIVE                         
         OI    TSTAT2,TSTATTCT     SET JUST CONNECTED FLAG                      
         TM    TSTAT2,TSTATBCP                                                  
         BO    *+8                                                              
         OI    TSTAT2,TSTATBCS     SET TO SCAN BRDCSTS AFTER CONNECT            
         XC    TCTDATA,TCTDATA                                                  
         MVC   TSYS,SESYSN                                                      
         MVC   TPRG,PROGNUM                                                     
         MVC   TPRGIND,PROGIND                                                  
         MVC   TOVSYS,OVSYS                                                     
         MVC   TCOSYS,PROGSYS                                                   
         CLI   TCOSYS,0                                                         
         BNE   *+10                                                             
         MVC   TCOSYS,OVSYS                                                     
         MVC   TUSER,USERNO                                                     
         MVC   TAUTH,PROGAUTH                                                   
*                                  RESOLVE ANY ADV TIME OUT VALUES              
         OC    TOUTCON,TOUTCON                                                  
         BZ    *+14                                                             
         MVC   TTIMEOUT,TOUTCON    RESTORE LAST CONNECTED TIMEOUT               
         B     CHKOK4B                                                          
*&&UK*&& MVI   TTIMEOUT,36         DEFAULT UK TO 3 HOURS                        
*&&US*&& MVI   TTIMEOUT,12         DEFAULT US TO 1 HOUR                         
         OC    TOUTTRM,TOUTTRM                                                  
         BZ    *+12                                                             
         LA    R1,TOUTTRM          USE TERMINAL LEVEL ADV TIMEOUT               
         B     CHKOK4A                                                          
         OC    TOUTUID,TOUTUID                                                  
         BZ    *+12                                                             
         LA    R1,TOUTUID          USE USERID LEVEL ADV TIMEOUT                 
         B     CHKOK4A                                                          
         OC    TOUTAGY,TOUTAGY                                                  
         BZ    CHKOK4B                                                          
         LA    R1,TOUTAGY          USE AGENCY LEVEL ADV TIMEOUT                 
CHKOK4A  EQU   *                                                                
         SR    RE,RE               RESOLVE TO 1 BYTE 5 MINUTE INCREMENT         
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         AHI   RF,4                MAKE SURE ROUND UP                           
         CLM   RF,3,=AL2(5*255)    MAXIMUM 21.25 HOURS                          
         BNH   *+8                                                              
         ICM   RF,3,=AL2(5*255)                                                 
         D     RE,=F'5'                                                         
         STC   RF,TTIMEOUT         SAVE ADV TIMEOUT VALUE IN UTL                
*                                                                               
CHKOK4B  LLC   R1,TSESSION         SET SESSION DATA                             
         IC    RE,SSACBITS(R1)                                                  
         EX    RE,*+8                                                           
         B     *+8                                                              
         OI    TSSBITS,0           TURN ON SESSION ACTIVE BIT                   
         IC    RE,SSNABITS(R1)                                                  
         EX    RE,*+8                                                           
         B     *+8                                                              
         NI    TSSXBITS,0          TURN OFF SESSION NOT AVAILABLE BIT           
         TM    CTSTEREO,TST6STRO                                                
         B     CHKOK4C             *NOP* BZ                                     
         IC    RE,SSACBITS(R1)                                                  
         EX    RE,*+8                                                           
         B     *+8                                                              
         OI    TSSXBITS,0          TURN ON SESSION NOT AVAIL FOR STEREO         
*                                                                               
CHKOK4C  SR    RE,RE               RE=X'0000PL.R' P=PREV/L=LAST/R=RSRV          
         ICM   RE,3,TSSSWAP                                                     
         SRDL  RE,4                                                             
         SRL   RF,4                RF=X'0R......'                               
         CLM   RF,8,TSESSION       DO NOTHING IF THIS IS RESERVED SESS          
         BE    CONOK5                                                           
         CLC   XTRSES,TSESSION     DO NOTHING IF THIS IS EXTRA SESS             
         BE    CONOK5                                                           
         SRDL  RE,8                RE=X'0000000P' PREV ACTIVE SESSION           
         SRL   RF,28               RF=X'0000000L' LAST ACTIVE SESSION           
         CLM   RF,1,TSESSION                                                    
         BE    CONOK5              DO NOTHING IS THIS SESS SAME AS LAST         
         LR    RE,RF               LAST TO PREVIOUS                             
         IC    RF,TSESSION         CURRENT TO LAST                              
         SLL   RF,28                                                            
         SLDL  RE,4                RE=X'000000PL'                               
         STC   RE,TSSSWAP                                                       
*                                                                               
CONOK5   EQU  *                                                                 
         XC    LIMITACS,LIMITACS   OLD ID/UK AUTH LIMIT ACCESS 1                
         XC    LIMITAC2,LIMITAC2   NEW PERSON AUTH LIMIT ACCESS 2               
         USING CTSYSD,RE                                                        
         ICM   RE,15,ASECSYS                                                    
         BZ    CONOK5A                                                          
         MVC   LIMITACS,CTSYSLMT   SET LIMIT ACCESS 1 FROM AUTH                 
         MVC   LIMITAC2,CTSYSLMT   AND SET LIMIT ACCESS 2 FROM AUTH             
CONOK5A  EQU  *                                                                 
         L     RE,AUIDSYS                                                       
         MVC   TAGYB,CTSYSAGB                                                   
*&&UK                                                                           
         CLI   OVSYS,X'04'         TEST IF UK MEDIA SYSTEM                      
         BNE   *+12                                                             
         CLI   TAGYB,0             DIE IF TAGYB NOT SET                         
         JE    *+2                 TO TRAP MEDIA CONNECT BUG?                   
*&&                                                                             
         OC    LIMITACS,LIMITACS   TEST ACCESS SET FROM AUTH RECORD             
         BNZ   *+10                                                             
         MVC   LIMITACS,CTSYSLMT   NO-USE LIMIT ACCESS FROM ID                  
*                                                                               
         OC    LAGVALUE,LAGVALUE   USE LIMIT ACCESS GROUP RECORD                
         BZ    CONOK5B                                                          
         MVC   LIMITACS,LAGVALUE   OVERRIDE LIMIT ACCESS 1                      
         MVC   LIMITAC2,LAGVALUE   AND OVERRIDE LIMIT ACCESS 2                  
CONOK5B  EQU   *                                                                
*                                                                               
         CLI   LIMOPT,0            TEST ACCESS GIVEN ON CONNECT                 
         BE    *+16                                                             
         MVC   LIMITACS,LIMACCS    YES-OVERRIDE USING GIVEN VALUE               
         XC    LIMITAC2,LIMITAC2                                                
         MVC   TACCS,LIMITACS                                                   
         MVC   TACCS2,LIMITAC2                                                  
         L     RF,ATSTNTRY                                                      
         USING TSTTABD,RF                                                       
         TM    CTTEST,TTESTTAC     IF CONNECTED WITH IAM=TESTID                 
         BZ    CONOK5C                                                          
         MVC   TACCS,ATSTNTRY      TACCS=A(TSTTAB ENTRY)                        
         MVC   TSTNUM,TNUM                                                      
         MVC   TSTTACCS,LIMITACS   TSTTACCS=LIMIT ACCESS                        
*                                                                               
CONOK5C  EQU   *                                                                
         MVC   TAGY,USERALPH                                                    
*&&DO                                                                           
         CLI   OVSYS,X'04'         TEST IF UK MEDIA SYSTEM                      
         BNE   *+20                                                             
         OC    MEDAGY,MEDAGY                                                    
         BZ    *+10                                                             
         MVC   TAGY,MEDAGY                                                      
*&&                                                                             
*&&US                                                                           
         CLC   TUSER,TDAREUSR                                                   
         BE    *+20                                                             
         NI    TSTATU,255-TSTATD4U RESET DARE INFO                              
         XC    TTIMEDAR,TTIMEDAR                                                
         XC    TDAREUSR,TDAREUSR                                                
*&&                                                                             
*                                                                               
CONOK5D  NI    TSTATU,X'FF'-TSTATEPR-TSTATDPR                                   
         CLI   CTPROT,C'Y'                                                      
         BNE   *+12                                                             
         OI    TSTATU,TSTATEPR     ENABLE STORAGE PROTECTION                    
         B     *+16                                                             
         CLI   CTPROT,C'N'                                                      
         BNE   *+8                                                              
         OI    TSTATU,TSTATDPR     DISSABLE STORAGE PROTECTION                  
*                                                                               
CONOK5E  MVC   TAGYSEC,AGYSEC      SAVE SECURITY AGENCY ALPHA                   
         MVC   TPERSON,PIDNUM      SAVE PERSON PASSWORD NUMBER                  
         MVC   TIDBITS+0(2),PIDSEC SAVE ID OF LAST PERSON IN THIS UTL           
         MVC   TIDBITS+2(2),PIDNUM                                              
         NI    TSTAT6,X'FF'-TST6STRO-TST6STFU-TST6STSS                          
         OC    TSTAT6,CTSTEREO     SET STEREO FLAGS                             
         NI    TSTAT8,X'FF'-TST8STSS-TST8BINT                                   
         OC    TSTAT8,CTSTERE8                                                  
*                                                                               
CONOK5F  EQU   *                                                                
*&&UK                                                                           
         NI    TSTAT9,X'FF'-TST9AGMS-TST9AGPR                                   
         TM    IDOFL2,CTIDOFMA                                                  
         BZ    *+8                                                              
         OI    TSTAT9,TST9AGMS                                                  
         TM    IDOFL2,CTIDOFPA                                                  
         BZ    *+8                                                              
         OI    TSTAT9,TST9AGPR                                                  
*&&                                                                             
*&&US                                                                           
         NI    TSTAT9,X'FF'-TST9SPTR                                            
         CLI   IDTYPE,C'T'                                                      
         BNE   *+8                                                              
         OI    TSTAT9,TST9SPTR                                                  
*&&                                                                             
CONOK5G  MVC   TTEST,CTTEST        SET TTEST FROM INPUT VALUE                   
         TM    DATABLD,X'01'       TEST AGENCY IN DATA BUILD MODE               
         BO    *+12                                                             
         NI    TSTATD,255-TSTATCBY NO-CLEAR CONNECT WITH BUILD=Y FLAG           
         B     CONOK5G4                                                         
         TM    DATABLD,X'02'       TEST BUILD=Y INPUT TO ALLOW UPDATES          
         BO    CONOK5G2                                                         
         TM    TSTATD,TSTATCBY     TEST PREVIOUS =CT HAD BUILD=Y                
         BO    CONOK5G4                                                         
         OI    TTEST,TTESTNOU      NO UPDATES IF BUILD=N                        
         B     CONOK5G4                                                         
CONOK5G2 OI    TSTATD,TSTATCBY     SAVE CONNECT WITH BUILD=Y                    
*                                                                               
CONOK5G4 CLI   XCTLTYP,0           TEST IF XCTL CONNECT                         
         BE    CONOK5G6                                                         
         CLI   XTEST,0             SET TTEST VALUES IF DEFINED                  
         BE    CONOK5G5                                                         
         TM    XTEST,TTESTNOU      CALLER WAS IN READONLY MODE                  
         BZ    *+8                                                              
         OI    TTEST,TTESTNOU                                                   
         TM    XTEST,TTESTLVL      CALLER WAS IN T=X TEST MODE                  
         BZ    CONOK5G5                                                         
         MVC   BYTE,XTEST                                                       
         NI    BYTE,TTESTLVL                                                    
         OC    TTEST,BYTE                                                       
*                                                                               
CONOK5G5 MVC   TOFFCODE,XOFFCODE   CALLERS DDS=YES/NO/OFFC VALUE                
         CLI   TOFFCODE,C'*'                                                    
         BE    *+10                                                             
         MVC   OFFICE,XOFFCODE     SIMULATE DDS=NO OR DDS=OFFC INPUT            
*                                                                               
CONOK5G6 TM    TTEST,TTESTNOU      SET UPDATE=N IN SOX INFO                     
         BZ    *+8                                                              
         OI    SOXFL2,SOXROMOD                                                  
         TM    READONLY,X'01'      TEST IF READ-ONLY USERID                     
         BZ    CONOK5H                                                          
         TM    XTEST,TTESTURQ      ALLOW TO ADD/CHG REQUEST                     
         BZ    *+8                                                              
         OI    TTEST,TTESTURQ      YES                                          
*&&UK*&& TM    SYSIDTY,FACITST                                                  
*&&UK*&& BO    CONOK5H                                                          
         OI    TTEST,TTESTNOU      NO-UPDATES ON PROD SYSTEMS                   
         OI    SOXFL2,SOXROMOD                                                  
*                                                                               
CONOK5H  NI    TSTAT5,255-TST5NONO RESET READ ONLY SYSTEM CONNECT FLAG          
         CLI   XCTLTYP,0           TEST IF XCTL CONNECT                         
         BNE   *+12                                                             
         TM    SOXFL1,SOXPGMSR     TEST IF =PROG CONNECT                        
         BZ    CONOK5I                                                          
         MVC   XPINFO,XPSAVE       SET CALLING PROGRAM PC INFO                  
         OI    XPTYPE,TXPTGL                                                    
         TM    XSTAT5,TST5NONO     DID CALLER HAVE WARNING FLAG SET             
         BZ    CONOK5I                                                          
         OI    TSTAT5,TST5NONO     YES-THEN DONT CLEAR IT                       
*                                                                               
CONOK5I  TM    SOXFL2,SOXROSYS     TEST IF READ-ONLY SYSTEM                     
         BO    *+12                                                             
         TM    TTEST,TTESTNOU+TTESTUEN TEST IF U=N INPUT                        
         BNM   CONOK5L                                                          
         TM    PROGIND,PGMIROP     TEST IF READ-ONLY PROGRAM                    
         BO    CONOK5L                                                          
CONOK5K  OI    TSTAT5,TST5NONO     SET CONNECTED IN READ ONLY MODE              
*                                                                               
CONOK5L  MVC   TPASSWD,PASSNO                                                   
         MVC   TSAGN,AGROUPNO      SAVE SECURITY ACCESS GROUP NUMBER            
         NI    TFLAG,255-TFLAGSEC                                               
         NI    TSTAT1,255-TSTATASW                                              
         TM    PWFLAG,PWREQD                                                    
         BZ    *+14                                                             
         MVC   TPASSWD,SECNO       SET SECRET AUTH & FLAG                       
         OI    TFLAG,TFLAGSEC                                                   
         NI    TSTAT7,255-TST7PQPW-TST7PQPU                                     
         TM    IDOFL1,X'02'                                                     
         BZ    *+8                                                              
         OI    TSTAT7,TST7PQPW                                                  
         TM    IDOFL1,X'01'                                                     
         BZ    *+8                                                              
         OI    TSTAT7,TST7PQPU                                                  
*                                                                               
         NI    TSTAT7,255-TST7PS0-TST7PSWN SET PASSWORD EXPIRES SOON            
         TM    PWDSTAT,PSTEXPW                                                  
         BZ    *+8                                                              
         OI    TSTAT7,TST7PS0+TST7PSWN                                          
         MVC   TPASSEXP,PEXPDAYS   SET DAYS TO PASSWORD EXPIRES                 
*                                                                               
         MVC   TXPINFO,XPINFO      SAVE ANY EXTERNAL PROGRAM INFO               
         OI    TXPTYPE,TXPTCT      SET THIS IS A =CT TRANSACTION                
*                                                                               
         MVC   TPQGRPID,IDOPQI     SET PQ GROUP ID NUMBER                       
         TM    SOXFL,SOXDDS                                                     
         BZ    *+8                                                              
         MVI   TOFFCODE,C'*'                                                    
         CLI   OFFICE,0            SET OFFICE CODE IF D=X INPUT                 
         BE    *+10                                                             
         MVC   TOFFCODE,OFFICE                                                  
         MVC   TASYS,ASENTRY+1                                                  
         MVC   TARSYS,ASENTRY+1                                                 
         XR    R3,R3                                                            
         ICM   R3,7,APGMLIST+1                                                  
         XR    R4,R4                                                            
         ICM   R4,7,APGMNTRY+1                                                  
         BZ    CONOK6                                                           
*                                                                               
         L     R3,VSSB             TEST IF TAPRG IS DISPLACEMENT                
         TM    SSBSTAT4-SSBD(R3),SSBPGDSP                                       
         BZ    *+12                                                             
         XR    R3,R3                                                            
         ICM   R3,7,APGMLIST+1                                                  
         SR    R4,R3                                                            
         STCM  R4,7,TAPRG          SET DISPLACEMENT TO PROGRAM                  
*                                                                               
CONOK6   CLI   CTLANG,X'FF'                                                     
         BE    *+10                                                             
         MVC   TLANG,CTLANG        SET LANGUAGE CODE IF INPUT                   
         CLI   CTCTRY,X'FF'        TEST COUNTRY INPUT                           
         BE    *+10                                                             
         MVC   TCTRY,CTCTRY        SET COUNTRY CODE IF INPUT                    
         CLI   AGYCTRY,X'FF'       SET AGENCY COUNTRY IF RESOLVED               
         BE    *+10                                                             
         MVC   TAGCTRY,AGYCTRY                                                  
         CLI   AGYCURR,X'FF'       SET AGENCY CURRENCY IF RESOLVED              
         BE    *+10                                                             
         MVC   TAGCURR,AGYCURR                                                  
         MVC   TAGCOPT,AGYOPTS     SET AGENCY OPTIONS FROM ACCESS REC           
*                                                                               
         LA    R4,SCANBLK                                                       
         MVI   0(R4),C' '                                                       
         MVC   1(199,R4),0(R4)                                                  
         SR    R3,R3                                                            
*                                                                               
         TM    CTSTEREO,TST6STRO   TEST TERM CONNECTED STEREO=YES               
         BO    CONOK14E                                                         
         CLI   TTEST,0             TEST TERM CONNECTED IN TEST MODE             
         BNE   *+12                                                             
         CLI   OFFICE,0                                                         
         BE    CONOK14                                                          
         TM    SOXFL,SOXDDS                                                     
         BZ    CONOK14                                                          
*                                                                               
         MVI   0(R4),C'*'          FORMAT '*PROG,TSTINFO' INTO TIA              
         MVC   1(L'PROGNAME,R4),PROGNAME                                        
         LA    R4,20(R4)                                                        
         LA    R3,1(R3)                                                         
         TM    TTEST,TTESTLVL      T=A/B/C                                      
         BZ    CONOK7                                                           
         MVC   0(2,R4),=C'T='                                                   
         MVC   2(1,R4),TTEST                                                    
         NI    2(R4),TTESTLVL                                                   
         OI    2(R4),X'C0'                                                      
         LA    R4,20(R4)                                                        
         LA    R3,1(R3)                                                         
CONOK7   TM    TTEST,TTESTTAC      I=TESTID                                     
         BZ    CONOK8                                                           
         MVC   0(2,R4),=C'I='                                                   
         L     RF,ATSTNTRY                                                      
         MVC   2(L'TSTACCS,R4),TSTACCS                                          
         LA    R4,20(R4)                                                        
         LA    R3,1(R3)                                                         
CONOK8   TM    TTEST,TTESTCIL      C=Y                                          
         BZ    CONOK9                                                           
         MVC   0(3,R4),=C'C=Y'                                                  
         LA    R4,20(R4)                                                        
         LA    R3,1(R3)                                                         
CONOK9   TM    TTEST,TTESTNOU+TTESTUEN U=N/S                                    
         BZ    CONOK10                                                          
         BM    *+14                                                             
         MVC   0(3,R4),=C'U=N'     NO UPDATES BECAUSE U=N INPUT                 
         B     *+10                                                             
         MVC   0(3,R4),=C'U=S'     ONLY SOX SPECIAL UPDATES ALLOWED             
         TM    TTEST,TTESTNOU+TTESTUEN+TTESTUEV                                 
         BNO   *+10                                                             
         MVC   0(3,R4),=C'U=V'     NO UPDATES BECAUSE U=V INPUT                 
         LA    R4,20(R4)                                                        
         LA    R3,1(R3)                                                         
CONOK10  CLI   OFFICE,0            D=N/X                                        
         BE    CONOK12                                                          
         MVC   0(2,R4),=C'D='                                                   
         MVC   2(1,R4),TOFFCODE                                                 
         CLI   2(R4),C' '                                                       
         BNE   *+8                                                              
         MVI   2(R4),C'N'                                                       
         LA    R4,20(R4)                                                        
         LA    R3,1(R3)                                                         
CONOK12  MVC   FLDH,CNTSREQH                                                    
         GOTO1 VUNSCAN,DMCB,((R3),SCANBLK),FLDH,C',=,='                         
         L     RE,ATIA                                                          
         MVC   0(17,RE),FLD        MOVE TO TIA & EXIT                           
         CLI   DATASW,C'Y'         TEST FOR + SIGN                              
         BNE   *+8                                                              
         MVI   17(RE),C'+'         SET FLAG IN TIA                              
         LA    R4,FLD                                                           
         B     CONOK15                                                          
*                                                                               
CONOK14  L     RF,ASYSLST                                                       
         USING SYSLSTD,RF          RF=A(SYSTEM TABLE)                           
CONOK14A CLC   OVSYS,SYSLNUM                                                    
         BE    CONOK14B                                                         
         LA    RF,SYSLLEN(RF)                                                   
         CLI   SYSLNUM,0                                                        
         BNE   CONOK14A                                                         
         DC    H'0'                INVALID SYSTEM                               
CONOK14B MVI   0(R4),C'*'                                                       
         LA    R1,L'USERID                                                      
         MVC   1(L'USERID,R4),USERID                                            
         LA    R4,L'USERID(R4)                                                  
         CLI   0(R4),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                DECREMENT LEN OF USERID                      
         BCT   R4,*-10                                                          
         MVC   1(1,R4),SYSIDCHR                                                 
         MVC   2(1,R4),SYSLUSLT    MOVE IN SINGLE CHR SYS                       
*&&US                                                                           
         CLC   SYSLSHRT(3),=C'NET' FIX FOR NET AND STR                          
         BNE   *+8                                                              
         MVI   2(R4),C'N'                                                       
         CLC   SYSLSHRT(3),=C'STR'                                              
         BNE   *+8                                                              
         MVI   2(R4),C'T'                                                       
*&&                                                                             
         LA    R4,2(R4)                                                         
         AHI   R1,-6               TEST USERID < 6 CHRS                         
         B     *+14                *NOP* WAS BNM                                
         MVC   0(3,R4),OVSYSN      MOVE IN 3 CHR SYS                            
         LA    R4,2(R4)                                                         
         MVC   1(1,R4),SYSIDCHR                                                 
         MVC   2(3,R4),PROGNAME                                                 
*&&UK                                                                           
         CLI   OVSYS,X'04'                                                      
         BNE   *+18                                                             
         CLI   PROGNUM,X'13'                                                    
         BNE   *+10                                                             
         MVC   2(3,R4),=CL3'FLI'                                                
*&&                                                                             
         MVC   5(1,R4),SYSIDCHR                                                 
         MVC   6(1,R4),SYSIDN1                                                  
         MVI   7(R4),C'*'                                                       
         CLC   TSYS(2),=X'0A0C'    NO SESSIONS FOR CONTROL/MAD                  
         BE    CONOK14F                                                         
CONOK14D IC    R1,TSESSION         SESSION CHR IS X'A0'+TSESSION+1              
         N     R1,=X'00000007'                                                  
         LA    R1,1(R1)                                                         
         STC   R1,7(R4)                                                         
         OI    7(R4),X'80'         SESSION IS LOWER CASE A THRU H               
         MVI   8(R4),C'*'                                                       
         TM    PWDSTAT,PSTEXPW     TEST IF PASSWORD ABOUT TO EXPIRE             
         BZ    CONOK14F                                                         
         CLI   PEXPDAYS,9                                                       
         BH    CONOK14F                                                         
         MVC   8(1,R4),PEXPDAYS    SET DAYS LEFT IN LAST CHR                    
         OI    8(R4),C'0'                                                       
         B     CONOK14F                                                         
*                                                                               
CONOK14E B     CONOK14             DO NOTHING SPECIAL HERE                      
*                                                                               
CONOK14F L     RE,ATIA                                                          
         LA    R4,SCANBLK                                                       
         MVC   0(17,RE),SCANBLK    MOVE CONNECT DATA TO TIA                     
         CLI   DATASW,C'Y'         TEST FOR + SIGN                              
         BNE   *+8                                                              
         MVI   17(RE),C'+'         SET FLAG IN TIA                              
*                                                                               
CONOK15  L     R1,TUTLXADR                                                      
         USING XAUTLD,R1                                                        
         SAM31                                                                  
         MVC   TSTATB,SOXFL2       SET SOX FLAGS FOR FAXTRAINF                  
         MVC   TAGYPER,PIDSEC      SET AGENCY CODE FOR TPERSON                  
         MVC   TUPDFAC,UPDSYSID    SET UPDATIVE FACPAK ID                       
         XC    TTICKETN,TTICKETN                                                
CONOK15A CLI   CTICKET,X'FF'       SET TICKET NUM IF INPUT THIS TIME            
         BE    CONOK15B                                                         
         MVC   TTICKETN,CTICKET                                                 
         B     CONOK16                                                          
CONOK15B TM    SOXFL1,SOXPGMSR     SET TICKET NUM IF =PROG CONNECT              
         BZ    CONOK15C                                                         
         MVC   TTICKETN,XTICKET                                                 
         B     CONOK16                                                          
CONOK15C CLI   XCTLTYP,0           SET TICKET NUM IF XCTL CONNECT               
         BE    CONOK16                                                          
         MVC   TTICKETN,XTICKET                                                 
*                                                                               
CONOK16  DS    0H                                                               
         SAM24                                                                  
         DROP  R1                                                               
*                                                                               
         XC    TOSIN,TOSIN         SET ORIGINAL SIN IF INPUT THIS TIME          
         CLI   COSIN,X'FF'                                                      
         BE    *+10                                                             
         MVC   TOSIN,COSIN                                                      
         NI    TSTATC,255-TSTCDDLP                                              
         XC    TRUNTAB,TRUNTAB                                                  
*                                                                               
CONOK17  L     RF,VSSB             BUILD SYSTEM TABLE IN TCB FOR CHKPT          
         L     R3,SSBTKADR-SSBD(RF)                                             
         USING TCBD,R3             R3=A(TCB)                                    
         MVC   TCBLNSYS,OVSYSN     SET LOGON SYSTEM AND PROGRAM                 
         MVC   TCBLNPRG,PROGNAME                                                
         MVC   TCBLNUID,USERID     SET ALPHA LOGON USERID AND PERSON            
         MVC   TCBLNPID,PERSONID                                                
         MVC   TCBSRMSG,0(R4)      SET SERVICE FIELD                            
         TM    TSTAT2,TSTATTCT                                                  
         BZ    *+8                                                              
         OI    TCBINDS2,TCBFCTC    SET CONNECT TRANSACTION                      
         TM    READONLY,X'01'                                                   
         BZ    *+8                                                              
         OI    TCBINDS2,TCBFCRO    SET CONNECT WITH READ-ONLY USERID            
         TM    DATABLD,X'01'                                                    
         BZ    *+8                                                              
         OI    TCBINDS2,TCBFCDB    SET CONNECT IN DATA BUILD MODE               
*                                                                               
         CLI   XCTLTYP,C'L'        DONT CLEAR TEMPEST IF SAME SESS              
         BE    CONOK17A                                                         
         L     RF,=A(CLRTEMP)                                                   
         A     RF,RELO                                                          
         BASR  RE,RF               UNALLOCATE TEMPEST                           
*                                                                               
CONOK17A OC    BILLREF,BILLREF     END BILLING IF STILL ACTIVE                  
         BZ    CONOK17B                                                         
         MVC   TCBBILL,BILLREF                                                  
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,C'E'                                                        
         GOTO1 VBILLIT,DMCB                                                     
CONOK17B NI    TSTAT1,255-TSTATBIL TURN OFF BILLING FLAG                        
*                                                                               
CONOK17C LA    R0,TCBSWMAX         CLEAR TCBSWTAB TO BINARY ZEROS               
         LA    R1,TCBSWLEN                                                      
         MR    R0,R0                                                            
         LA    R0,TCBSWTAB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   TCBSWNUM,1          BUILD CONNECTED SYSTEM TCB ENTRY             
         MVC   TCBSWSYS,SESYSN                                                  
         MVC   TCBSWSOV,OVSYS                                                   
         MVC   TCBSWAGB,TAGYB                                                   
         MVC   TCBSWACS,LIMITACS                                                
         MVC   TCBSWAC2,LIMITAC2                                                
         OC    TCBSWAGB(TCBSWRVF-TCBSWAGB),TCBSWAGB                             
         BNZ   *+8                                                              
         MVI   TCBSWACS+3,1        FORCE NON-ZERO AGENCY/ACCESS                 
         TM    PROGIND2,PGMISWT    TEST PROGRAM AUTHORISED FOR SWITCH           
         NOP   CONOK30             **NOP** BZ                                   
*                                  PROCESS ALL SYSELS ON ID RECORD              
         L     RF,AUIDSYS                                                       
         MVI   0(RF),X'FF'         SET TO IGNORE CONNECTED SYSEL                
         L     RE,AUIDREC                                                       
         LA    RE,CTIDATA-CTIREC(RE)                                            
CONOK18  CLI   0(RE),0             TEST E-O-R                                   
         BE    CONOK30                                                          
         CLI   0(RE),X'21'         TEST SYSTEM ELEMENT                          
         BE    CONOK20                                                          
         B     *+8                                                              
CONOK19  L     RE,AUIDSYS          BUMP TO NEXT ELEMENT ON ID RECORD            
         LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     CONOK18                                                          
*                                  PROCESS SYSTEM ELEMENT ON ID RECORD          
         USING CTSYSD,RE                                                        
CONOK20  ST    RE,AUIDSYS                                                       
         MVC   OVSYS,CTSYSNUM      SET OVERLAY SYSTEM NUMBER                    
         MVC   SESYSN,CTSYSSE      SET SYSTEM NUMBER                            
         XC    ASECSYS,ASECSYS     CLEAR PASSWORD AUTH REC SYS POINTER          
*                                                                               
         OC    SECNO,SECNO         SECRET CODE NUM IS ZERO IF TERMINAL          
         BZ    CONOK20B                                                         
*                                                                               
CONOK20A L     R4,ASECREC          CHECK FOR SYSTEM AUTHORISATION               
         LA    R1,ASECSYS          ON AUTH RECORD                               
         BAS   RE,GETSYS                                                        
         BZ    CONOK19             SYSTEM NOT DEFINED ON AUTH REC               
         LR    RE,RF                                                            
         B     CONOK22             SYSTEM OK - BUILD TCB ENTRY                  
*                                                                               
CONOK20B CLI   ALLVALID,C'Y'       TEST ALL ID'S VALID                          
         BE    CONOK22             IF SO OVERIDE SYSTEM CHECK                   
         L     R4,ATRMREC          CHECK FOR SYSTEM AUTHORISATION               
         LA    R1,ATRMSYS          ON TERMINAL RECORD                           
         BAS   RE,GETSYS                                                        
         BZ    CONOK19             SYSTEM NOT DEFINED ON TERM REC               
         LR    RE,RF                                                            
*                                                                               
CONOK22  CLI   CTSYSNUM,X'0B'      SKIP GAMES                                   
         BE    CONOK19                                                          
         CLI   CTSYSNUM,X'0C'      SKIP CPP                                     
         BE    CONOK19                                                          
         CLI   CTSYSNUM,X'0E'      SKIP PERSON                                  
         BE    CONOK19                                                          
*                                                                               
CONOK22A LLC   R1,TCBSWNUM         BUILD CHKPT ENTRY FOR SYSTEM                 
         LA    R1,1(R1)            BUMP TCB SYSTEM COUNT                        
         LA    R0,TCBSWMAX                                                      
         CR    R1,R0                                                            
         BH    CONOK30                                                          
         STC   R1,TCBSWNUM                                                      
         LA    R0,TCBSWLEN                                                      
         MR    R0,R0                                                            
         LA    R1,TCBSWTAB-TCBSWLEN(R1)                                         
         USING TCBSWTAB,R1         BUILD TCB CHKPT ENTRY FOR SYSTEM             
         L     RE,AUIDSYS                                                       
         MVC   TCBSWSYS,CTSYSSE                                                 
         MVC   TCBSWSOV,CTSYSNUM                                                
         MVC   TCBSWAGB,CTSYSAGB                                                
         MVC   TCBSWACS,CTSYSLMT   SET LIMIT ACCESS CODE 1                      
         XC    TCBSWAC2,TCBSWAC2                                                
         ICM   RE,15,ASECSYS       IF PASSWORD AUTH RECORD SYSTEM FOUND         
         BZ    CONOK23             SET LIMIT ACCESS                             
         OC    CTSYSLMT,CTSYSLMT                                                
         BZ    CONOK23                                                          
         MVC   TCBSWACS,CTSYSLMT   OVERRIDE LIMIT ACCESS CODE 1                 
         MVC   TCBSWAC2,CTSYSLMT   AND SET LIMIT ACCESS CODE 2                  
*                                                                               
CONOK23  OC    LGROUPNO,LGROUPNO   DATA ACCESS GROUP RECORD                     
         BZ    CONOK24             IF PRESENT                                   
         BAS   RE,GETLAS                                                        
         BNE   CONOK24                                                          
         OC    LAGVALUE,LAGVALUE   BYPASS IF NULL                               
         BZ    CONOK24                                                          
         MVC   TCBSWACS,LAGVALUE   OVERRIDE LIMIT ACCESS CODE 1                 
         MVC   TCBSWAC2,LAGVALUE   AND SET LIMIT ACCESS CODE 2                  
*                                                                               
CONOK24  OC    TCBSWAGB(TCBSWRVF-TCBSWAGB),TCBSWAGB                             
         BNZ   *+8                                                              
         MVI   TCBSWACS+3,1        FORCE NON-ZERO AGENCY/ACCESS                 
         B     CONOK19                                                          
*                                                                               
CONOK30  CLI   XCTLTYP,0           TEST IF XFR CONTROL TYPE CONNECT             
         BE    CONOK40                                                          
         XC    CNTSREQ,CNTSREQ                                                  
         MVC   CNTSREQ(8),=C'=GOBACK,' RETURN XFR CONTROL TYPE CONNECT          
         MVC   CNTSREQ+8(1),XCTLTYP                                             
*                                                                               
CONOK40  TM    SOXFL,SOXSCRP       SCRIPTS SET TPERSON FROM TPASSWD             
         BO    *+14                                                             
         OC    TPERSON,TPERSON     TEST IF PERSON ID NUMBER SET                 
         BNZ   CONOK50                                                          
         MVC   TPERSON,TPASSWD     NO-NON PPS SO USE PASSWORD NUMBER            
         ICM   RF,15,TBUFF                                                      
         BZ    CONOK50             TEST IF SCRIPT RECORD WELL DEFINED           
         LA    RE,TBHSCREC-TBHDATA(RF)                                          
         C     RE,TBHSCAS-TBHDATA(RF)                                           
         BNE   CONOK50                                                          
         CLI   0(RE),CT7KTYPQ      TEST IF SCRIPT RECORD IN TBUFF               
         BNE   CONOK50                                                          
         MVC   DUB,CT7KCODE-CT7REC(RE) EXTRACT SCRIPT ID FROM RECORD            
*                                                                               
CONOK45  L     RF,=V(FAXPTMF)      RF=A(MF APP PROGRAM/SCRIPT TABLE)            
         A     RF,RELO                                                          
         USING FAXPTABD,RF                                                      
         LLC   R0,FAXPFLAG         R0=ENTRY LENGTH                              
CONOK45A CLI   FAXPTYPE,0          SEARCH TABLE FOR SCRIPT NAME                 
         BE    CONOK45C                                                         
         TM    FAXPTYPE,FAXPTMFS                                                
         BZ    CONOK45B                                                         
         CLC   DUB,FAXPNEXE                                                     
         BE    CONOK45D                                                         
CONOK45B AR    RF,R0                                                            
         B     CONOK45A                                                         
CONOK45C L     RF,=V(FAXPTMF)      SET UP DEFAULT SCRIPT NUMBER                 
         A     RF,RELO                                                          
         AR    RF,R0                                                            
CONOK45D MVC   XPNUM,FAXPNUM                                                    
         MVC   XPTYPE,FAXPTYPE                                                  
*                                                                               
CONOK50  TM    PWFLAG,PWPVAL       TEST PASSWORD HAS BEEN PRE-VALIDATED         
         BO    CONOK50A                                                         
         CLI   PIDREQD,C'N'        TEST IF PERSON ID NUMBER REQUIRED            
         BNE   CONOK50A                                                         
         MVC   TPERSON,TPASSWD     NO-NON PPS SO USE PASSWORD NUMBER            
CONOK50A OC    TAGYPER,TAGYPER                                                  
         BNZ   *+10                                                             
         MVC   TAGYPER,TAGYSEC                                                  
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         XC    WORK,WORK                                                        
SMF      USING METD,WORK           CONNECT SMF RECORD                           
         MVI   SMF.METLN+1,METLNQ                                               
         MVI   SMF.METTYP,METTYPQ                                               
         MVC   SMF.METDSP,SSBDSPAC                                              
         MVC   SMF.METAGY,TAGY                                                  
         MVC   SMF.METUIDN,TUSER                                                
         MVC   SMF.METSAGY,TAGYSEC                                              
         MVC   SMF.METPAGY,TAGYPER                                              
         MVC   SMF.METPIDN,TPERSON                                              
         MVC   SMF.METUID,USERID   ALPHA USER ID NAME                           
         MVC   SMF.METPID,PERSONID ALPHA PERSON ID NAME                         
         MVC   SMF.METSYS,TOVSYS                                                
         MVC   SMF.METMFP,TPRG                                                  
         MVC   SMF.METXPT,XPTYPE                                                
         TM    TSTAT1,TSTATWEB     WEB TERMINAL                                 
         BZ    *+8                                                              
         OI    SMF.METXPT,TXPTWEB                                               
         MVC   SMF.METXPP,XPNUM                                                 
         MVC   SMF.METXPV,XPVER                                                 
         DROP  SMF,RE                                                           
*                                                                               
         TM    TSTAT6,TST6SCRP     SCRIPT                                       
         BO    CONOK60             YES-SKIP SMF                                 
         TM    TXPTYPE,TXPTGL      GLOBBER CALL                                 
         BO    CONOK60             YES-SKIP SMF                                 
*                                                                               
         GOTO1 VSMFOUT,DMCB,METCON,WORK  CONNECT SMF RECORD                     
*                                                                               
CONOK60  OC    PIDLLO,PIDLLO       EXIT IF NOT YET UPDATING PERSON              
         BZ    CONOK60X                                                         
         TM    PWDSTAT,PSTRESET    TEST TO RESET STATUS FOR PVP P/W             
         BO    *+14                                                             
         CLC   PIDLLO,TODAY3       TEST IF LAST LOGGED ON TODAY                 
         BE    CONOK60X                                                         
         TM    PWDSTAT,PSTUPDT     TEST IF UPDATED ALREADY                      
         BO    CONOK60X                                                         
*                                                                               
         TM    SYSIDTY,FACICSC     TEST IF CSC FACPAK                           
         BO    CONOK60X            YES-SO DONT UPDATE                           
         TM    READONLY,X'01'      TEST IF READ-ONLY                            
         BO    CONOK60A            YES-NO SMF RECORD                            
         TM    DATABLD,X'01'       TEST DATA BUILD MODE                         
         BO    CONOK60A            YES-NO SMF RECORD                            
*                                                                               
         GOTO1 VSMFOUT,DMCB,METLOG,WORK  LOGON SMF RECORD                       
*                                                                               
CONOK60A MVC   WORK+40(16),TNUM    SAVE UTL DATA BEFORE FASWITCH                
         ICM   RF,15,=XL4'0AFFFFFF'                                             
         GOTO1 VSWITCH,DMCB,(RF),0 SWITCH TO CONTROL SYSTEM                     
         CLI   4(R1),0                                                          
         BNE   CONOK60V            CANT SWITCH                                  
*                                                                               
         L     R3,APERREC          PERSON READ FOR UPDATE                       
         USING SAPEREC,R3                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,PIDSEC                                                   
         MVC   SAPEPID,PERSONID                                                 
         MVC   SAPEDEF,=X'FFFF'                                                 
         XC    SAPEDEF,TODAYB                                                   
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'CTFILE',(R3),(R3)            
         CLI   8(R1),0                                                          
         BNE   CONOK60V            READ ERROR                                   
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BNE   CONOK60V            KEYS DONT MATCH                              
         LA    R3,SAPEDATA                                                      
         SR    R1,R1                                                            
*                                                                               
CONOK60B CLI   0(R3),SALLOELQ      FIND LAST LOGON DATE ELEMENT                 
         BE    CONOK60C                                                         
         CLI   0(R3),0                                                          
         BE    CONOK60V            CANT FIND IT NOW - WHERE DID IT GO           
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CONOK60B                                                         
*                                                                               
         USING SALLOD,R3                                                        
CONOK60C LR    R0,R3               R0=A(ELEMENT) IN PERSON RECORD               
         LA    R3,WORK                                                          
         XC    WORK(SALLOLNQ),WORK                                              
         MVI   SALLOEL,SALLOELQ    BUILD NEW LAST LOGON DATA ELEMENT            
         MVI   SALLOLEN,SALLOLNQ                                                
         MVC   SALLODT,TODAY3                                                   
         MVC   SALLOADV,SYSIDNO                                                 
         LA    RF,WORK+40          RF=A(SAVED UTL DATA)                         
         MVC   SALLOUSR,TUSER-UTLD(RF)                                          
         MVC   SALLOSYS,TOVSYS-UTLD(RF)                                         
         MVC   SALLOPRG,TPRG-UTLD(RF)                                           
         LR    R3,R0               R3=A(ELEMENT IN RECORD)                      
*                                                                               
         CLC   SALLOLEN,WORK+1     ADJUST NEW LEN TO MATCH OLD LEN              
         BE    *+10                                                             
         MVC   WORK+1(1),SALLOLEN                                               
         IC    R1,SALLOLEN                                                      
         CHI   R1,5                                                             
         BL    CONOK60V            ELEMENT LOOKS FUNNY                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
*                                                                               
CONOK60D L     R3,APERREC          MAINTENANCE WRITE WITH NO RECOVERY           
         MVI   ACTN,X'26'                                                       
         IC    RF,ACTN                                                          
         GOTO1 VDATAMGR,DMCB,((RF),=C'DMWRT'),=C'CTFILE',(R3),(R3)              
         CLI   8(R1),0                                                          
         BE    CONOK60W                                                         
*                                                                               
CONOK60V EQU   *                   IGNORE ERRORS ON PERSON UPDATE               
*                                                                               
CONOK60W ICM   RF,15,=XL4'01FFFFFF'                                             
         GOTO1 VSWITCH,DMCB,(RF),0 SWITCH TO SERVICE SYSTEM                     
*                                                                               
CONOK60X EQU   *                                                                
*                                                                               
CONOK70  CLI   TODAYMTH,0          EXIT IF NOT YET UPDATING USERID              
         BE    CONOK70X                                                         
         OC    UIDLLON,UIDLLON     No date to update for now                    
         BZ    CONOK70X                                                         
         CLC   UIDLLON,TODAY3      Save as before?                              
         BE    CONOK70X                                                         
         CLC   UIDLLO,TODAYMTH     TEST IF USERID USED THIS MONTH               
         BE    CONOK70X                                                         
         TM    SYSIDTY,FACITST     IS THIS A TEST SYSTEM                        
         BO    CONOK70X            YES-SO DONT UPDATE                           
*                                                                               
CONOK70A CLC   PIDSEC,DDSSECP      TEST DDS PERSON CONNECTING TO LIVE           
         BE    CONOK70X            YES-SO DONT UDPATE                           
*                                                                               
CONOK70B ICM   RF,15,=XL4'0AFFFFFF'                                             
         GOTO1 VSWITCH,DMCB,(RF),0 SWITCH TO CONTROL SYSTEM                     
         CLI   4(R1),0                                                          
         BNE   CONOK70V            CANT SWITCH                                  
*                                                                               
         L     R4,AUIDREC          READ USER ID RECORD                          
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD ID(ALPHA) KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,USERID                                                    
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMREAD'),=C'CTFILE',(R4),(R4)            
         CLI   8(R1),0                                                          
         BNE   CONOK70V            READ ERROR                                   
*                                                                               
*        MVC   CTISTAT,TODAYMTH    STATUS BYTE HAS LAST LOGON MONTH NUM         
         NI    CTISTAT,X'7F'                                                    
*                                                                               
CONOK70D L     R4,AUIDREC          MAINTENANCE WRITE WITH NO RECOVERY           
         MVI   ACTN,X'26'                                                       
         IC    RF,ACTN                                                          
         GOTO1 VDATAMGR,DMCB,((RF),=C'DMWRT'),=C'CTFILE',(R4),(R4)              
         CLI   8(R1),0                                                          
         BE    CONOK70W                                                         
*                                                                               
CONOK70V EQU   *                   IGNORE ERRORS ON USERID UPDATE               
*                                                                               
CONOK70W ICM   RF,15,=XL4'01FFFFFF'                                             
         GOTO1 VSWITCH,DMCB,(RF),0 SWITCH TO SERVICE SYSTEM                     
*                                                                               
CONOK70X EQU   *                                                                
*                                                                               
CONOKX   XIT1                                                                   
         DROP  R1,R3,R4,RF                                                      
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* C'EST LA VIE, GOOD BYE TO SMG, DU GONE, H9 HAS SOME BROMLEY IDS     *         
* ADDING DF,DT         - READONLY  MAY   5, 2014 (SPT/STR/PRT/NET)    *         
* ADDING DF,DT,DW,TH,PC- NO ACCESS NOV   3, 2014                      *         
*        PC,TH         - Modified        ?  201x READ-only print      *         
* ADDING GD,GX         - NO ACCESS MAY   5, 2011                      *         
* ADDING GZ            - NO ACCESS SEP   1, 2013                      *         
* RESTOR GZ            - READONLY  APR  14, 2017                      *         
* ADDING HH,HK         - READONLY  JULY  1, 2011                      *         
* ADDING JW,OH         - NO ACCESS JULY  8, 2011 EXTEND TO JUL29/2011 *         
* ADDING HH,HK         - NO ACCESS JAN   1, 2015 WAS JAN01/2014       *         
* ADDING HH,HK         - READONLY  FEB   1, 2016 WAS FEB01/2015       *         
* ADDING H8,HL         - NO ACCESS JULY  1, 2014 -> CHG AUG01/2014    *         
* ADDING GZ            - NO ACCESS APRIL 1, 2012 AS JAN1,FEB1,MAY1/12 *         
* CHANGE GZ FOR NET    - READONLY  SEPT  1, 2013                      *         
* ADDING WI,WJ,WR,MC   - NO ACCESS TO POD,NPOD,DEM32,LINK(AUDI. EST)  *         
*        E6,WU           JAN 2, 2012                                  *         
* ADDING WI,WJ,WR,MC   - NO ACCESS JAN   1, 2013                      *         
*        E6,WU                                                        *         
* ADDING H3            - READONLY  SEP   1, 2015 Not for PRINT        *         
*                        NO ACCESS FEB   1, 2016                      *         
*                        Restore   JUN   8, 2017 Restore Read-only    *         
* ADDING YP            - READONLY  APR   1, 2017                      *         
*                        NO ACCESS JUL   1, 2017                      *         
* ADDING YR            - READONLY  2016  ALSO NO REQUESTING REPORTS   *         
* ADDING MR Security   - READONLY  JAN  26, 2016 (KATZ)               *         
*                      - NO ACCESS FEB  15, 2016                      *         
* ADDING AG            - READONLY  APR   8, 2016                      *         
*                        NO ACCESS MAR  31, 2019                      *         
* ADDING NE,EZ,DM,MK,BD- TBD at some point in time                    *         
* ADDING D9            - READONLY  SEP  12, 2016 (Except control)     *         
***********************************************************************         
CESTLAVI NTR1  BASE=*,LABEL=*                                                   
         MVI   CONERR,0                                                         
         MVC   HALF(1),OVSYS       OVERLAY SYSTEM ID                            
         MVC   HALF+1(1),PROGNUM                                                
         TM    SOXFL,SOXDDS        DDS TERMINAL                                 
         NOP   CLAVOKAY            YES-LEFT IN FOR PATCHING                     
         BZ    CLAV001             NO                                           
*&&US*&& CLC   =X'002F',AGROUPNO                                                
*&&US*&& BE    *+10                                                             
         CLC   =C'KCAPDDNY',PERSONID                                            
         BNE   CLAV001                                                          
         CLC   DDSSECP,PIDSEC                                                   
         BE    CLAVOKAY            ALLOW ACCESS                                 
*                                                                               
CLAV001  LHI   R3,-1               SETS CC LOW FOR RESTRICTED ACCESS            
*                                                                               
         CLI   SERVTEAM,INACTIVE   ALPHA ID INACTIVE?                           
         BE    NOACCESS            YES: NO ACCESS ALLOWED                       
*----------------------------------                                             
* AGENCY ACCESS DATES ON RECORD                                                 
*----------------------------------                                             
         MVI   BYTE,0                                                           
*                                                                               
         USING CTACCD,RF                                                        
         ICM   RF,15,ACTACC        DO WE HAVE AN ACCESS DATES ELEMENT?          
         BZ    CLAV002             NO                                           
         CLI   CTACCEL,CTACCELQ    AGENCY ACCESS DATES ON RECORD?               
         BNE   CLAV002             NO                                           
*                                                                               
         OC    CTACCNAD,CTACCNAD   ANY NO ACCESS DATE DEFINED                   
         BZ    CLAV01              NO: SKIP THE CHECK                           
         CLC   CTACCNAD,TODAY3     NO ACCESS DATE                               
         BNH   NOACCESS            NO ACCESS                                    
*                                                                               
CLAV01   OC    CTACCROD,CTACCROD   ANY READ-ONLY DATE DEFINED                   
         BZ    CLAV01A             NO: SKIP THE CHECK                           
         CLC   CTACCROD,TODAY3     READ-ONLY ACCESS?                            
         BH    CLAV01A             NO                                           
         MVI   BYTE,C'R'                                                        
*                                                                               
CLAV01A  LLC   R0,CTACCLEN                                                      
         AR    R0,RF               R0=A(END OF ELEMENT)                         
CLAV001A CR    RF,R0               ANYTHING LEFT IN THIS ELEMENT?               
         BNL   CLAV001C            NO: CONTINUE                                 
         CLC   CTACCSYS,HALF       SYSTEM ACCESS DATES FOR THIS SYSTEM          
         BNE   CLAV001B            NO: NEXT                                     
         OC    CTACCSNA,CTACCSNA   ANY NO ACCESS DATE DEFINED?                  
         BZ    CLAV01B             NO: SKIP THE CHECK                           
         CLC   CTACCSNA,TODAY3     NO ACCESS FOR THIS SYSTEM                    
         BNH   NOACCESS            NO ACCESS                                    
CLAV01B  OC    CTACCSRO,CTACCSRO   ANY READ-ONLY DATE DEFINED?                  
         BZ    CLAV001C            NO: SKIP THE CHECK                           
         CLC   CTACCSRO,TODAY3     READ-ONLY FOR THIS SYSTEM                    
         BH    CLAV001C            NO                                           
         MVI   BYTE,C'R'                                                        
         B     CLAV001C                                                         
CLAV001B LA    RF,CTACCSYL(,RF)    NEXT SYSTEM                                  
         B     CLAV001A                                                         
         DROP  RF                                                               
*                                                                               
CLAV001C CLI   BYTE,C'R'           READ-ONLY ACCESS?                            
         BNE   CLAV002                                                          
         CLC   =X'0A0D',HALF       CONTROL/SECURITY CAN UPDATE FOR R/O          
         BNE   ROACCESS            NO: READ-ONLY ACCESS                         
                                                                                
*----------------------------------                                             
* HARDCODED ACCESS RESTRICTIONS                                                 
*----------------------------------                                             
CLAV002  DS    0H                                                               
*&&DO                                                                           
         CLC   =C'GZ',USERALPH     AS OF APR 14th 2017 (temporary)              
         BE    ROACCESS                                                         
         CLC   =C'GD',USERALPH     AS OF MAY 5TH, 2014                          
         BE    NOACCESS                                                         
         CLC   =C'GX',USERALPH     AS OF MAY 5TH, 2014                          
         BE    NOACCESS                                                         
         CLC   =C'JW',USERALPH     AS OF JULY 29, 2011                          
         BE    NOACCESS                                                         
         CLC   =C'OH',USERALPH     AS OF JULY 29, 2011                          
         BE    NOACCESS                                                         
         CLC   =C'QU',USERALPH     2100 ROSS COMMUNICATIONS                     
         BE    NOACCESS                                                         
         CLC   =C'S$',USERALPH     KSL                                          
         BE    NOACCESS            AS OF MAY 5TH, 2014                          
         CLC   =C'JT',USERALPH     AS OF AUG 1ST, 2012 R/O                      
         BE    NOACCESS            AS OF MAY 5TH, 2014                          
         CLC   =C'FB',USERALPH     FOX                                          
         BE    NOACCESS            AS OF JUL 1ST, 2014                          
         CLC   =C'BL',USERALPH     Petry agencies                               
         BE    NOACCESS            AS OF JUN 29th, 2015                         
         CLC   =C'PV',USERALPH     Petry agencies                               
         BE    NOACCESS            AS OF JUN 29th, 2015                         
         CLC   =C'HK',USERALPH     Hill & Knowlton                              
         BE    NOACCESS            AS OF Feb 1st,  2016                         
         CLC   =C'HH',USERALPH     Hill & Knowlton                              
         BE    NOACCESS            AS OF Feb 1st,  2016                         
         CLC   =C'HL',USERALPH     Hill & Knowlton                              
         BE    NOACCESS            AS OF Aug 1st,  2014                         
         CLC   =C'H8',USERALPH     Hill & Knowlton                              
         BE    NOACCESS            AS OF Aug 1st,  2014                         
         CLC   =C'CA',USERALPH     Hill & Knowlton                              
         BE    NOACCESS            AS OF Feb 1st,  2015                         
         CLC   =C'MR',AGYSEC       Check Security Agency MR (KATZ)              
         BE    NOACCESS            AS of Feb 15th, 2016                         
         CLC   =C'DF',USERALPH                                                  
         BE    NOACCESS            AS of Nov,3rd, 2014                          
         CLC   =C'DW',USERALPH                                                  
         BE    NOACCESS            AS of Nov,3rd, 2014                          
         CLC   =C'DT',USERALPH                                                  
         BE    NOACCESS            AS of Nov,3rd, 2014                          
         CLC   =C'DR',USERALPH     SMG ids                                      
         BE    NOACCESS                                                         
         CLC   =C'DU',USERALPH     SMG ids                                      
         BE    NOACCESS                                                         
         CLC   =C'J2',USERALPH     SMG ids                                      
         BE    NOACCESS                                                         
*&&                                                                             
CLAV003  CLI   SYSIDNO,FAC#TST     TST SYSTEM?                                  
         BNE   CLAV003B            NO                                           
         CLC   =C'UB',USERALPH     ACCC ON TST                                  
         BE    ROACCESS                                                         
         CLC   =C'FM',USERALPH     NETW ON TST (FMNY)                           
*        BE    CLAV003A                                                         
*        CLC   =C'TH',USERALPH     ZENITH (SPECIAL OLD FILE)                    
         BNE   CLAV003B            NO                                           
CLAV003A TM    SOXFL,SOXDDS        DDS TERMINAL                                 
         BZ    ROACCESS            NO,  READ ACCESS ONLY                        
         B     CLAVOKAY            YES, FULL ACCESS                             
*                                                                               
CLAV003B DS    0H                                                               
*&&DO                                                                           
         CLC   =C'YP',USERALPH     Sudler                                       
         BNE   CLAV003E                                                         
         CLC   =X'0A0D',HALF       ACCESS TO CONTROL/SECURITY                   
         BE    CLAVOKAY                                                         
         CLC   TODAYB,=X'EA81'     APR. 1ST, 2017                               
         BL    CLAVOKAY            OKAY FOR NOW                                 
         CLC   TODAYB,=X'EAE1'     JUL. 1ST, 2017                               
         BL    ROACCESS            READ-ONLY                                    
         B     NOACCESS            NO ACCESS                                    
*                                                                               
CLAV003E CLC   =C'H3',USERALPH     HILL & KNOWLTON CANADA                       
         BNE   CLAV003J            NO SO OKAY, ELSE READ-ONLY                   
         CLC   =X'0A0D',HALF       ACCESS TO CONTROL/SECURITY                   
         BE    CLAVOKAY                                                         
         CLC   TODAYB,=X'FFFF'     ???                                          
         BNL   NOACCESS            NO ACCESS                                    
         B     ROACCESS            Read-only for all                            
*                                                                               
CLAV003J CLC   =C'D9',USERALPH                                                  
         BNE   CLAV003M                                                         
         CLC   =X'0A0D',HALF       ACCESS TO CONTROL/SECURITY                   
         BE    CLAVOKAY                                                         
         CLC   TODAYB,=X'E92C'     Sep 12th, 2016                               
         BL    CLAVOKAY            Normal access prior to this date             
         B     ROACCESS            Read-only starting Aug. 22nd                 
*&&                                                                             
CLAV003M CLC   USERNO,=AL2(10823)  YRTOME   YR                                  
         BE    RO_NORPT            Read-only , can't request reports            
*                                                                               
*                                  Omnicom group                                
*AH3     CLI   OVSYS,7             TALENT SYSTEM?                               
*AH3     BE    CLAV003U                                                         
*&&DO                                                                           
         CLC   =C'NE',USERALPH     DDB                                          
         BE    CLAV003T                                                         
         CLC   =C'MK',USERALPH     Merkly                                       
         BE    CLAV003T                                                         
         CLC   =C'BD',USERALPH     BBDO                                         
         BE    CLAV003T                                                         
         CLC   =C'EZ',USERALPH     Doremus                                      
         BE    CLAV003T                                                         
         CLC   =C'DM',USERALPH     Doremus                                      
         BNE   CLAV003U                                                         
CLAV003T CLC   TODAYB,=X'FFFF'     Future use                                   
         BNL   NOACCESS                                                         
         CLC   TODAYB,=X'FFFF'     Future use                                   
         BNL   ROACCESS                                                         
*                                                                               
CLAV003U DS    0H                                                               
         CLC   =C'AG',USERALPH     Arnold                                       
         BNE   CLAV004                                                          
         CLC   TODAYB,=X'EE7F'     March 31st, 2019                             
         BH    NOACCESS                                                         
         CLC   TODAYB,=X'E888'     April 8th,2016                               
         BNL   ROACCESS                                                         
*&&                                                                             
CLAV004  CLC   =C'TH',USERALPH     ZENITH                                       
         BE    *+10                                                             
         CLC   =C'PC',USERALPH                                                  
         BNE   CLAV005                                                          
         CLC   DDSSECP,PIDSEC      MEDIAOCEAN SECURITY ID                       
         BE    CLAVOKAY                                                         
         CLI   HALF,X'0A'          CONTROL SYSTEM?                              
         BE    *+8                 READ-ONLY ACCESS                             
         CLI   HALF,X'04'          PRINT SYSTEM?                                
         BNE   NOACCESS            No then no access                            
         CLC   USERNO,=AL2(10403)  ZENYRAD                                      
         BNE   CLAV004A                                                         
         CLC   =C'RADICEJ ',PERSONID ONLY FOR THIS PERSON                       
         BE    ROACCESS                                                         
*                                                                               
CLAV004A CLC   USERNO,=AL2(11739)  OszRAD                                       
         BNE   NOACCESS                                                         
         CLC   =C'MUBRIA01 ',PERSONID ONLY FOR THIS PERSON                      
         BE    ROACCESS                                                         
         CLC   =C'RADICEJ ',PERSONID ONLY FOR THIS PERSON                       
         BE    ROACCESS                                                         
         B     NOACCESS                                                         
*                                                                               
CLAV005  CLI   OVSYS,7             TALENT SYSTEM?                               
         BE    CLAV009                                                          
*&&DO                                                                           
         CLC   =C'MC',USERALPH     MEDIABRANDS UNIVERSAL MCANN (SEC ID)         
         BE    CLAV005A            ALLOW CON/SEC                                
         CLC   =C'1M',USERALPH     MEDIABRANDS UNIVERSAL MCANN                  
         BE    CLAV005B                                                         
         CLC   =C'WI',USERALPH     MEDIABRANDS INITIATIVE      (SEC ID)         
         BE    CLAV005A            ALLOW CON/SEC                                
         CLC   =C'WJ',USERALPH     MEDIABRANDS INITIATIVE                       
         BE    CLAV005B                                                         
         CLC   =C'WR',USERALPH     MEDIABRANDS INITIATIVE                       
         BE    CLAV005B                                                         
         CLC   =C'EB',USERALPH     MEDIABRANDS INITIATIVE                       
         BE    CLAV005B                                                         
         CLC   =C'EC',USERALPH     MEDIABRANDS INITIATIVE                       
         BE    CLAV005B                                                         
         CLC   =C'E6',USERALPH     MEDIABRANDS INITIATIVE                       
         BE    CLAV005B                                                         
         CLC   =C'WU',USERALPH     MEDIABRANDS INITIATIVE                       
         BE    CLAV005B                                                         
*&&                                                                             
         B     CLAV009                                                          
*&&DO                                                                           
CLAV005A CLC   =X'0A0D',HALF       ACCESS TO CONTROL/SECURITY                   
         BE    CLAVOKAY                                                         
*                                                                               
CLAV005B MVI   CONERR,1            NO PROGRAM ACCESS                            
         CLC   =X'021B',HALF       DEMO (DEM32)                                 
         BE    NOACCESS                                                         
         CLC   =X'0225',HALF       POD  (RESEARCH WRITER)                       
         BE    NOACCESS                                                         
         CLC   =X'020F',HALF       RSR  (RESEARCH)                              
         BE    NOACCESS                                                         
         CLC   =X'0302',HALF       LINK (RESEARCH WRITER)                       
         BE    NOACCESS                                                         
         CLC   =X'0325',HALF       NPOD (RESEARCH WRITER)                       
         BE    NOACCESS                                                         
         CLC   =X'0321',HALF       RSR  (RESEARCH)                              
         BE    NOACCESS                                                         
         MVI   CONERR,0            RESET                                        
         CLC   TODAYB,=X'E23F'     READ-ONLY AFTER JAN 31, 2013                 
         BNH   CLAVOKAY            OKAY FOR NOW                                 
         B     ROACCESS            READ-ONLY                                    
*&&                                                                             
CLAV009  CLC   =C'H9',USERALPH                                                  
         BNE   CLAVOKAY            EVERY ONE ELSE                               
         CLC   USERNO,=AL2(13997)  STAREAD  H9                                  
         BE    CLAV040                                                          
         CLC   USERNO,=AL2(13998)  MVREAD   H9                                  
         BE    CLAV040                                                          
         CLC   USERNO,=AL2(13999)  SPAREAD  H9                                  
         BE    CLAV040                                                          
*                                                                               
         LA    RE,CPYGONE                                                       
         LA    R0,CPYGONE#         NUMBER TO CHECK AGAINST                      
CLAV020  CLC   USERNO,0(RE)        USER ID NUM                                  
         BE    NOACCESS            GOT ONE, NO ACCESS                           
         LA    RE,2(RE)                                                         
         BRCT  R0,CLAV020                                                       
         B     CLAVOKAY            SET TO OKAY, EXCEPTION CASE                  
*                                                                               
*LAV040  OI    TTEST,TTESTURQ      ALLOW TO MAKE REQUEST                        
*        OI    READONLY,X'01'      SET CALLER INTO READ-ONLY MODE               
CLAV040  MVC   HALF(1),OVSYS       OVERLAY SYSTEM ID                            
         MVC   HALF+1(1),PROGNUM                                                
*                                                                               
CLAV041  LA    RE,CPYPROG          A(READ-ONLY PROGRAMS)                        
         LA    R0,CPYPROG#         NUMBER TO CHECK                              
CLAV042  CLC   HALF,0(RE)                                                       
         BE    ROACCESS            OKAY FOR READ-ONLY                           
         LA    RE,2(RE)                                                         
         BRCT  R0,CLAV042                                                       
         B     NOACCESS            RESTRICTED ACCESS MESSAGE                    
*                                                                               
CLAVOKAY SR    R3,R3               GIVE THEM ACCESS TO THESE                    
*                                                                               
CLAVXIT  CHI   R3,0                SET CC HIGH,LOW OR EQUAL                     
         J     CONOKX              HIGH=NO ACCESS, LOW=R/O, EQUAL=OK            
*                                                                               
ROACCESS TM    SOXFL,SOXDDS        DDS TERMINAL                                 
         BZ    RO10                NO, READ-ONLY ACCESS                         
         CLI   SYSIDNO,FAC#CSC     CSC USES REAL DDS SECURITY AGENCY            
         BE    CLAVOKAY            ACCESS OKAY                                  
RO10     OI    TTEST,TTESTURQ      ALLOW TO MAKE REQUEST                        
RO_NORPT OI    READONLY,X'01'      SET CALLER INTO READ-ONLY MODE               
         B     CLAVOKAY                                                         
*                                                                               
NOACCESS LA    R3,1                NO ACCESS IF ID DIDN'T MATCH                 
         TM    SOXFL,SOXDDS        DDS TERMINAL                                 
         BZ    CLAVXIT             NO, NO ACCESS                                
         CLI   SYSIDNO,FAC#CSC     CSC USES REAL DDS SECURITY AGENCY            
         BE    CLAVOKAY            ACCESS OKAY                                  
         B     CLAVXIT             NO ACCESS                                    
                                                                                
***********************************************************************         
* LTORGS AND TABLE OF IDS TO NOT LET IN. (DISABLE THESE IDS)          *         
* ALLOWED READ-ONLY ACCESS TO THESE PROGRAMS                          *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
CPYPROG  DS    0D                                                               
         DC    XL2'0A03'           CONTROL $PROF                                
         DC    XL2'0A08'           CONTROL $PIP                                 
         DC    XL2'0A0C'           CONTROL $MAD                                 
         DC    XL2'0A36'           CONTROL $PCPAK                               
*                                                                               
         DC    XL2'0204'           SPOT    $WRI                                 
         DC    XL2'0208'           SPOT    $REQ                                 
         DC    XL2'0209'           SPOT    $FIS                                 
         DC    XL2'020B'           SPOT    $MIS                                 
         DC    XL2'0210'           SPOT    $NINV                                
         DC    XL2'0211'           SPOT    $BUY                                 
         DC    XL2'0217'           SPOT    $SFM                                 
         DC    XL2'021A'           SPOT    $INFO                                
         DC    XL2'021E'           SPOT    $LINK                                
         DC    XL2'0228'           SPOT    $MAK                                 
         DC    XL2'0236'           SPOT    $PCPAK                               
         DC    XL2'02FE'           SPOT    $USDESK (NOT NEEDED?)                
*                                                                               
         DC    XL2'0D16'           TRAFFIC $TRAFFIC                             
*                                                                               
         DC    XL2'0302'           NET     $LINK   (STEWARD)                    
         DC    XL2'0308'           NET     $REQ                                 
         DC    XL2'0310'           NET     $NINV                                
         DC    XL2'0311'           NET     $NBUY                                
         DC    XL2'0316'           NET     $TRAFFIC                             
         DC    XL2'0318'           NET     $NNAV                                
         DC    XL2'031A'           NET     $INFO                                
         DC    XL2'031C'           NET     $SFM                                 
         DC    XL2'0320'           NET     $WRI                                 
         DC    XL2'0336'           NET     $PCPAK                               
*                                                                               
         DC    XL2'0405'           PRINT   $WRI                                 
         DC    XL2'0406'           PRINT   $PUBFILE                             
         DC    XL2'0411'           PRINT   $BUY                                 
         DC    XL2'0412'           PRINT   $REQ                                 
         DC    XL2'0414'           PRINT   $ADBUYER                             
         DC    XL2'041A'           PRINT   $INFO                                
         DC    XL2'041B'           PRINT   $FIS                                 
         DC    XL2'041C'           PRINT   $SFM                                 
         DC    XL2'0436'           PRINT   $PCPAK                               
*                                                                               
         DC    XL2'060B'           ACCOUNT $PROD                                
         DC    XL2'060C'           ACCOUNT $SCRIBE                              
         DC    XL2'0620'           ACCOUNT $FIS                                 
         DC    XL2'0623'           ACCOUNT $AFM                                 
         DC    XL2'0636'           ACCOUNT $PCPAK                               
CPYPROG# EQU   (*-CPYPROG)/2                                                    
*                                                                               
* BLOCKED USER IDS                                                              
*                                                                               
CPYGONE  DS    0D                                                               
*        DC    AL2(11905)          BROMCK   (H9 IDS)                            
*        DC    AL2(13503)          BROMRE                                       
*        DC    AL2(13497)          BROMSEC                                      
*        DC    AL2(09486)          BRSA                                         
*        DC    AL2(09488)          BRSAA                                        
*        DC    AL2(09487)          BRSAP                                        
*        DC    AL2(09860)          BRSAT                                        
*        DC    AL2(09625)          MVBROM   (ALLOW THIS ONE)                    
         DC    AL2(11060)          DDSMON                                       
         DC    AL2(11901)          HALCK                                        
         DC    AL2(11913)          KLCCK                                        
         DC    AL2(11544)          KLCNY                                        
         DC    AL2(11793)          KLCNYRE                                      
         DC    AL2(11910)          KLKCK                                        
         DC    AL2(11531)          KLKNYRE                                      
         DC    AL2(11500)          KLNY                                         
         DC    AL2(11503)          KLNYRE                                       
         DC    AL2(11546)          KLSCHNY                                      
         DC    AL2(11912)          KLSCK                                        
         DC    AL2(11547)          KLSHNY                                       
         DC    AL2(11873)          KLSINYRE                                     
         DC    AL2(11529)          KLSNYRE                                      
         DC    AL2(11549)          KLSSNY                                       
         DC    AL2(11911)          KLVCK                                        
         DC    AL2(11545)          KLVNY                                        
         DC    AL2(11530)          KLVNYRE                                      
         DC    AL2(11525)          KLZNYRE                                      
         DC    AL2(10455)          LAPIZ                                        
         DC    AL2(10454)          LAPIZNY                                      
         DC    AL2(10822)          LBWKS                                        
         DC    AL2(11904)          MEDCK                                        
         DC    AL2(11903)          MSLCK                                        
         DC    AL2(09627)          MVAYER                                       
         DC    AL2(11899)          MVCAP                                        
         DC    AL2(09509)          MVCHFC                                       
         DC    AL2(09344)          MVCHI                                        
         DC    AL2(09510)          MVCHJW                                       
         DC    AL2(09511)          MVCHLB                                       
         DC    AL2(09630)          MVDARCY                                      
         DC    AL2(09632)          MVDARLA                                      
         DC    AL2(12784)          MVDEU                                        
         DC    AL2(09936)          MVDR                                         
         DC    AL2(11642)          MVDRNY                                       
         DC    AL2(11783)          MVFAS                                        
         DC    AL2(11140)          MVHALG                                       
         DC    AL2(13165)          MVHO                                         
         DC    AL2(09628)          MVHONE                                       
         DC    AL2(09888)          MVINT                                        
         DC    AL2(12322)          MVJWT                                        
         DC    AL2(09533)          MVLA                                         
         DC    AL2(11246)          MVLADIT                                      
         DC    AL2(10109)          MVLAP                                        
         DC    AL2(09514)          MVLG                                         
         DC    AL2(11020)          MVLIFE                                       
         DC    AL2(11019)          MVLIFENY                                     
         DC    AL2(09626)          MVMAS                                        
         DC    AL2(09629)          MVMED                                        
         DC    AL2(09979)          MVMEDNY                                      
         DC    AL2(11311)          MVMEDT                                       
         DC    AL2(11312)          MVMEDTNY                                     
         DC    AL2(09512)          MVNC                                         
         DC    AL2(09210)          MVNY                                         
         DC    AL2(10096)          MVNYC                                        
         DC    AL2(11900)          MVNYCK                                       
         DC    AL2(09508)          MVNYFC                                       
         DC    AL2(11080)          MVNYHD                                       
         DC    AL2(12240)          MVNYK                                        
         DC    AL2(09631)          MVNYLA                                       
         DC    AL2(09984)          MVNYNS                                       
         DC    AL2(09213)          MVNYP                                        
         DC    AL2(09407)          MVNYPT                                       
         DC    AL2(11331)          MVNYQ                                        
         DC    AL2(09214)          MVNYR                                        
         DC    AL2(10334)          MVNYS                                        
         DC    AL2(09212)          MVNYT                                        
         DC    AL2(10100)          MVNYTP                                       
         DC    AL2(09513)          MVOM                                         
         DC    AL2(09891)          MVPRISM                                      
         DC    AL2(10372)          MVREV                                        
         DC    AL2(12620)          MVSCWRG                                      
         DC    AL2(09211)          MVSEC                                        
         DC    AL2(09240)          MVSL                                         
         DC    AL2(10772)          MVSMGD                                       
         DC    AL2(13294)          MVSNS                                        
         DC    AL2(10631)          MVST                                         
         DC    AL2(10641)          MVSTNY                                       
         DC    AL2(11534)          MVTAP                                        
         DC    AL2(10099)          MVTAPCH                                      
         DC    AL2(11029)          MVTAPNY                                      
         DC    AL2(10097)          MVTAPSA                                      
         DC    AL2(10426)          MVVER                                        
         DC    AL2(09515)          MVYR                                         
         DC    AL2(13352)          MV42                                         
*        DC    AL2(12677)          SMG  - DITECH RELATED                        
         DC    AL2(11902)          SMGCK                                        
         DC    AL2(12720)          SMGSRCH                                      
         DC    AL2(09710)          SMGTRA                                       
         DC    AL2(13321)          SMGTRB                                       
         DC    AL2(12088)          SPRCH                                        
         DC    AL2(12090)          SPRNY                                        
         DC    AL2(12089)          SPRSF                                        
         DC    AL2(12110)          SPRSFT                                       
         DC    AL2(08601)          STACH                                        
         DC    AL2(11908)          STACHCK                                      
         DC    AL2(10262)          STACHNY                                      
         DC    AL2(09067)          STACHO                                       
         DC    AL2(10363)          STACHPS                                      
         DC    AL2(08629)          STACHSEC                                     
         DC    AL2(09203)          STAR                                         
         DC    AL2(09335)          STARA                                        
         DC    AL2(10261)          STARANY                                      
         DC    AL2(10364)          STARAPS                                      
         DC    AL2(11332)          STARCHI                                      
         DC    AL2(11906)          STARCK                                       
         DC    AL2(12074)          STARCY                                       
         DC    AL2(12667)          STARET                                       
         DC    AL2(12628)          STARGM                                       
         DC    AL2(10277)          STARLA                                       
         DC    AL2(09997)          STARM                                        
         DC    AL2(11907)          STARMCK                                      
         DC    AL2(10170)          STARMNS                                      
         DC    AL2(09497)          STARNS                                       
         DC    AL2(09332)          STARPG                                       
         DC    AL2(10429)          STARPGO                                      
         DC    AL2(09638)          STARPMT                                      
         DC    AL2(09204)          STARSEC                                      
         DC    AL2(12678)          STARSRCH                                     
         DC    AL2(09341)          START                                        
         DC    AL2(11855)          STARYR                                       
         DC    AL2(13287)          STAWDCK                                      
         DC    AL2(10831)          STBOZ                                        
         DC    AL2(10858)          STBROM                                       
         DC    AL2(11124)          STCORE                                       
         DC    AL2(12304)          STLAMC                                       
*                                                                               
*        DC    AL2(10799)          STARMREV  (J2 ALPHAS)                        
*        DC    AL2(09171)          STARMSEC                                     
*        DC    AL2(09167)          STARMY                                       
*        DC    AL2(11972)          STARMYCK                                     
*        DC    AL2(09669)          STARMYNS                                     
*        DC    AL2(10263)          STARMYNY                                     
*        DC    AL2(10373)          STARMYRE                                     
CPYGONE# EQU   (*-CPYGONE)/2                                                    
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAR GLOBALS                                            *         
* R0=0 CLEAR GLOBALS IF $CT/=CT/+CT INPUT IN S/R FIELD                *         
* R0=1 CLEAR GLOBALS IF PGMIND4=PGMICLRG                              *         
***********************************************************************         
GLOCLR   NTR1  BASE=*,LABEL=*                                                   
         LTR   R0,R0               TEST TO SEE WHY CALLED                       
         BNZ   GLOCLR1                                                          
GLOCLR0  CLC   CNTSREQ+1(2),=C'CT' TEST $CT/=CT/+CT IN SRVREQ FIELD             
         BNE   GLOCLRX                                                          
         CLI   CNTSREQ+3,C','                                                   
         BE    GLOCLRX                                                          
         CLI   CNTIDH+5,0                                                       
         BNE   GLOCLRX                                                          
         B     GLOCLR3                                                          
*                                                                               
GLOCLR1  EQU   *                                                                
*                                                                               
GLOCLR3  L     R5,VSSB             GET A(TASK GLOBAL AREA)                      
         L     R5,SSBTKADR-SSBD(R5)                                             
         L     R5,TCBWRKA-TCBD(R5) FIND GLOBAL STORAGE IN MNTR'S W/S            
         LA    R0,8                                                             
GLOCLR4  CLC   0(4,R5),=C'MNTR'    FIND MONITORS REGISTERS                      
         BE    GLOCLR5                                                          
         L     R5,8(R5)                                                         
         BCT   R0,GLOCLR4                                                       
         B     GLOCLRX             EXIT WITH ERROR IF CANT FIND IT              
GLOCLR5  ICM   R5,15,104(R5)       PICK UP SPECIAL STORAGE LOCATION             
         BZ    *+8                                                              
         MVI   11(R5),X'20'        SET CLEAR GLOBALS FLAG                       
*                                                                               
GLOCLRX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ ANY PROGRAM VERSION CONTROL RECORD FOR CONNECT SYSTEM/PROGRAM  *         
* AND MOVE ANY MESSAGE INTO TIA+500(60)                               *         
* IF NO RECORD/MESSAGE CLEAR THIS AREA TO ZEROES                      *         
***********************************************************************         
REPROVE  NTR1  BASE=*,LABEL=*                                                   
         CLI   AGYCTRY,0           DO FOR GBR AGENCY ONLY                       
         BNZ   REPRX                                                            
         ICM   RE,15,APGMNTRY      TEST IF PROGRAM VERSION MESSAGE              
         BZ    REPRX                                                            
         TM    PGMVFLG-PGMLSTD(RE),PGMVDATE+PGMVHEAD                            
         BNO   REPRX                                                            
*                                                                               
         L     R4,APRVREC          BUILD KEY OF PROVER RECORD AT RF             
         USING CTPRREC,R4                                                       
         XC    CTPRKEY,CTPRKEY                                                  
         MVI   CTPRKTYP,CTPRKTYQ                                                
         MVI   CTPRKPTY,CTPRKPFQ                                                
         MVC   CTPRKSYS,OVSYS                                                   
         MVC   CTPRKPRG,PROGNUM                                                 
         CLI   CTLANG,X'FF'                                                     
         BE    *+10                                                             
         MVC   CTPRKLAN,CTLANG                                                  
*NOP*    MVC   CTPRKSYS,TOVSYS                                                  
*NOP*    MVC   CTPRKPRG,TPRG                                                    
*NOP*    MVC   CTPRKLAN,TLANG                                                   
         XI    CTPRKLAN,X'FF'                                                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',(R4),(R4)              
         CLI   8(R1),0                                                          
         BNE   REPRX                                                            
*                                  HEADLINE TEXT TO TIA+500(60)                 
         LA    R3,CTPRDATA         POINT TO FIRST ELEMENT                       
REPR010  CLI   0(R3),0             SEARCH FOR MESSAGE ELEMENT                   
         BE    REPRX                                                            
         CLI   0(R3),CTPRVELQ                                                   
         BE    REPR030                                                          
         CLI   0(R3),GMSGELC                                                    
         BE    REPR020                                                          
REPR012  LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     REPR010                                                          
*                                                                               
         USING GMSGEL,R3                                                        
REPR020  EQU   *                   PROCESS PROVER MESSAGE ELEMENT               
         L     RE,ATIA             RE=A(MESSAGE RETURN AREA IN TIA)             
         LA    RE,500(RE)                                                       
         MVI   0(RE),C' '          SPACE FILL                                   
         MVC   1(59,RE),0(RE)                                                   
         LLC   R1,GMSGELL          MOVE IN MESSAGE ELEMENT DATA                 
         SH    R1,=Y(GMSGFXDL+1)                                                
         CLM   R1,3,=Y(59)                                                      
         BNH   *+8                                                              
         ICM   R1,3,=Y(59)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),GMSGTXT                                                  
         B     REPR012                                                          
         DROP  R3                                                               
*                                                                               
         USING CTPRVD,R3                                                        
REPR030  EQU   *                   PROCESS PROVER CONTROL ELEMENT               
         L     RE,ATIA             RE=A(MESSAGE RETURN AREA IN TIA)             
         MVI   560(RE),C'N'        SAVE MESSAGE HIGHLIGHT FLAG                  
         TM    CTPRVFL1,CTPRVFHI                                                
         BZ    REPR012                                                          
         MVI   560(RE),C'Y'                                                     
         B     REPR012                                                          
         DROP  R3                                                               
*                                                                               
REPRX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MOVE DATA (IF ANY) FROM CONNECT FIELDS TO THE DATA FIELD            *         
* TO SHOW THAT THERE IS MORE DATA TO COME LAST CHR OF FIELD MUST BE > *         
***********************************************************************         
ANYDATA  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,INPFLDS          R3=A(FIRST INPUT FIELD CHR)                  
         XC    INPFLDS,INPFLDS                                                  
         LA    RF,INPFLDS+L'INPFLDS                                             
         ST    RF,AINPFLDX                                                      
*                                                                               
ANYDAT1  LA    RF,CNTIDH           EXTRACT DATA FROM MAX OF FIVE FLDS           
         BAS   RE,ANYDATR                                                       
         BZ    ANYDAT2                                                          
         LA    RF,CNTSYSH                                                       
         BAS   RE,ANYDATR                                                       
         BZ    ANYDAT2                                                          
         LA    RF,CNTPGMH                                                       
         BAS   RE,ANYDATR                                                       
         BZ    ANYDAT2                                                          
         LA    RF,CNTPWDH                                                       
         BAS   RE,ANYDATR                                                       
         BZ    ANYDAT2                                                          
         LA    RF,CNTDATAH                                                      
         BAS   RE,ANYDATR                                                       
         BZ    ANYDAT2                                                          
*                                                                               
ANYDAT2  XC    CNTDATA,CNTDATA     CLEAR INPUT DATA FIELD                       
         MVI   CNTDATAH+5,0                                                     
         LA    R1,INPFLDS                                                       
         SR    R3,R1               R3=NUM OF CHRS IN INPUT DATA                 
         BNP   ANYDAT3                                                          
         MVC   CNTDATA,INPFLDS                                                  
         LA    R1,L'CNTDATA                                                     
         CR    R3,R1                                                            
         BNH   *+6                                                              
         LR    R3,R1                                                            
         STC   R3,CNTDATAH+5                                                    
*                                                                               
ANYDAT3  XC    CNTID,CNTID         CLEAR ID/SYS/PRG/PWD FIELDS                  
         MVI   CNTIDH+5,0                                                       
         XC    CNTSYS,CNTSYS                                                    
         MVI   CNTSYSH+5,0                                                      
         XC    CNTPGM,CNTPGM                                                    
         MVI   CNTPGMH+5,0                                                      
         XC    CNTPWD,CNTPWD                                                    
         MVI   CNTPWDH+5,0                                                      
         B     EXIT                                                             
*                                                                               
ANYDATR  SR    R1,R1               RF=A(FIELD HEADER)                           
         ICM   R1,1,5(RF)                                                       
         BZR   RE                                                               
         SR    R0,R0                                                            
         LA    R4,7(R1,RF)         POINT TO LAST DATA CHR                       
         CLI   0(R4),C'>'                                                       
         BNE   ANYDATS                                                          
         LA    R0,1                SET MORE DATA TO COME FLAG                   
*&&UK*&& MVI   0(R4),C''''         SET FIELD SEPERATOR CHR                      
*&&US*&& MVI   0(R4),C'.'                                                       
ANYDATS  BCTR  R1,0                                                             
         LA    R4,1(R1,R3)                                                      
         C     R4,AINPFLDX         TEST IF ROOM IN INPFLDS                      
         BNH   ANYDATT                                                          
         SR    R0,R0               NO SET NO MORE DATA TO COME                  
         B     ANYDATX                                                          
ANYDATT  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(RF)                                                    
         LA    R3,1(R1,R3)                                                      
ANYDATX  LTR   R0,R0               SET CC ZERO IF NO MORE DATA                  
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UNALLOCATE TEMPEST FOR SESSION (BOTH IF DONESW=Y) R3=A(TCB)         *         
***********************************************************************         
         USING TCBD,R3                                                          
CLRTEMP  NTR1  BASE=*,LABEL=*                                                   
         TM    TCBXTFLG,X'80'      TEST IF TEMPEST NOT OWNED BY SESSION         
         BO    CLRTEMPX                                                         
         XC    TCBXTINF,TCBXTINF                                                
*                                                                               
         TM    TEMPESTF,X'01'      DOES SESSION HAVE ANY TEMPEST                
         BZ    CLRTEMPX                                                         
         MVC   TCBXTINF,TEMPSINF   YES-SET ALLOCATION IN TCB                    
*                                                                               
         OI    TFLAG,TFLAGRTS                                                   
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRLSE'),=C'TEMPEST',(255,0),0               
CLRTEMPX B     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CLEAR OUT DIALOGUE MODE                                             *         
***********************************************************************         
CLRDIAL  NTR1  BASE=*,LABEL=*                                                   
         IC    R0,TSESSION                                                      
         N     R0,=X'0000000F'     MASK FOR TO / FROM THIS SESSION              
         STC   R0,HALF                                                          
         SLL   R0,4                                                             
         STC   R0,HALF+1                                                        
*                                                                               
         LHI   R0,L'TCNVPRS                                                     
         LA    RF,TCNVPRS                                                       
*                                                                               
CLRD02   MVC   BYTE,0(RF)          CHECK FOR EITHER TO OR FROM                  
         NI    BYTE,X'0F'                                                       
         CLC   BYTE,HALF                                                        
         BE    CLRD04                                                           
         MVC   BYTE,0(RF)                                                       
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,HALF+1                                                      
         BE    CLRD04                                                           
         B     CLRD06                                                           
*                                                                               
CLRD04   MVI   0(RF),0             CLEAR THIS LINK                              
*                                                                               
CLRD06   AHI   RF,1                                                             
         BCT   R0,CLRD02                                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD DATA BLOCK INTO TIA+20                                        *         
***********************************************************************         
SETDATA  NTR1  BASE=*,LABEL=*                                                   
         TM    PROGIND,PGMIOLA                                                  
         BZ    SETDATAX                                                         
         L     R3,ATIA                                                          
         CLI   CNTDATAH+5,0                                                     
         BE    SETDATA2                                                         
         MVC   20(88,R3),CNTDATAH                                               
         B     SETDATAX                                                         
*                                                                               
SETDATA2 L     R4,AMAPREC          R4=A(MAP RECORD)                             
         USING CTMREC,R4                                                        
         LA    R3,20(R3)           R3=A(OUTPUT FIELD)                           
         TM    MAPOPT,X'80'        TEST IF MAP RECORD ALREADY READ              
         BO    SETDATA4                                                         
*&&US*&& CLI   DATASW,C'Y'         +PGMNAME MUST BE INPUT IN S/R                
*&&US*&& BNE   SETDATAX                                                         
         XC    CTMKEY,CTMKEY                                                    
         MVI   CTMKTYP,C'M'                                                     
         MVC   CTMKSYS,OVSYS                                                    
         MVC   CTMKPROG,PROGNUM                                                 
         MVI   CTMKSCRN,X'FF'                                                   
         MVC   CTMKUSER,USERALPH                                                
         MVC   FLD(L'CTMKEY),CTMKEY                                             
         BAS   RE,CTREAD           READ FOR AGENCY DEFAULT                      
         BE    SETDATA4                                                         
         MVC   CTMKEY,FLD                                                       
         XC    CTMKUSER,CTMKUSER                                                
         BAS   RE,CTREAD           READ FOR SYSTEM DEFAULT                      
         BNE   SETDATAX                                                         
*                                  EXTRACT VALUES FROM PREVIOUS TWA             
SETDATA4 XC    DATAPOOL(3),DATAPOOL                                             
         CLI   DATASW,C'Y'                                                      
         BNE   SETDATA6                                                         
         LH    R0,TNUM                                                          
         MVC   DMCB+20(2),=C'L='   SET TO READ PREV TWA LOGICAL LEN             
         MVC   DMCB+22(2),CHKDSP                                                
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R0),ATWA2              
         CLI   8(R1),0                                                          
         BNE   SETDATA6            IGNORE IF CAN'T READ TWA                     
         LA    R1,TSCRNUM          CALL EXTRACT FOR EACH OVERLAY SCREEN         
         LA    R0,3                                                             
         BAS   RE,EXTRACT                                                       
         LA    R1,3(R1)                                                         
         BCT   R0,*-8                                                           
*                                  PROCESS 'TO' MAP RECORD                      
SETDATA6 LA    R4,CTMDATA          R4=A(MAP ELEMENT)                            
         LA    RF,8(R3)            RF=A(OUTPUT STRING)                          
         MVI   FLDNUM,1                                                         
         MVI   FLDTYP,0                                                         
SETDATA8 CLI   0(R4),0             E-O-R                                        
         BE    SETDATAM                                                         
         CLI   0(R4),X'B1'         MAP ELEMENT                                  
         BE    SETDATAC                                                         
SETDATAA LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     SETDATA8                                                         
*                                  PROCESS A MAP ELEMENT                        
         USING CTMAPD,R4                                                        
SETDATAC CLC   CTMAPFLD,FLDNUM     CHECK FOR CHANGE OF FIELD                    
         BE    SETDATAE                                                         
         LLC   RE,CTMAPFLD         INSERT FIELD DELIMITERS BETWEEN THIS         
         LLC   R0,FLDNUM           FIELD AND LAST FIELD                         
         SR    RE,R0                                                            
         MVI   0(RF),DELIM                                                      
         LA    RF,1(RF)                                                         
         BCT   RE,*-8                                                           
         MVC   FLDNUM,CTMAPFLD     SAVE FIELD NUMBER                            
         MVI   FLDTYP,0            AND SET FIELD WANTED SWITCH                  
SETDATAE CLI   FLDTYP,X'FF'        IGNORE ELEMNT IF FIELD NOT WANTED            
         BE    SETDATAA                                                         
         CLI   CTMAPNUM,0                                                       
         BNE   SETDATAG                                                         
SETDATAF LLC   R1,CTMAPLEN         HANDLE LITERALS                              
         AHI   R1,-6                                                            
         LA    RE,CTMAPLIT                                                      
         B     SETDATAK                                                         
*                                  LOOK FOR DATA FIELD IN DATA POOL             
SETDATAG LA    RE,DATAPOOL+2                                                    
         SR    R1,R1                                                            
SETDATAI CLI   0(RE),0                                                          
         BE    SETDATAJ                                                         
         CLC   0(1,RE),CTMAPNUM                                                 
         BE    *+14                                                             
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     SETDATAI                                                         
         LLC   R1,1(RE)                                                         
         AHI   R1,-3                                                            
         LA    RE,2(RE)                                                         
         B     SETDATAK                                                         
SETDATAJ CLI   CTMAPLEN,5          IF FIELD HAS A LITERAL VALUE USE IT          
         BH    SETDATAF                                                         
         MVI   FLDTYP,X'FF'        SET FIELD NOT WANTED                         
         B     SETDATAA                                                         
*                                  MOVE DATA TO OUTPUT STRING                   
SETDATAK EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)                                                    
         LA    RF,1(R1,RF)                                                      
         B     SETDATAA                                                         
*                                  BUILD A FIELD HEADER AT R3 & EXIT            
SETDATAM LA    RE,8(R3)                                                         
         SR    RF,RE                                                            
         BZ    SETDATAX                                                         
         CLI   0(RE),DELIM         EXIT IF FIELD CONTAINS DELIMS ONLY           
         BNE   SETDATAO                                                         
         AHI   RF,-1                                                            
         BZ    SETDATAX                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),1(RE)                                                    
         BE    SETDATAX                                                         
         LA    RF,2(RF)                                                         
SETDATAO CHI   RF,247              CHECK FOR MAX FIELD LENGTH EXCEEDED          
         BNH   *+8                                                              
         LHI   RF,247                                                           
         STC   RF,5(R3)            SET INPUT LENGTH                             
         LA    RF,8(RF)                                                         
         STC   RF,0(R3)            SET TOTAL FIELD LENGTH                       
         B     SETDATAX                                                         
         DROP  R4                                                               
SETDATAX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* EXTRACT DATA FROM PREVIOUS TWA USING A MAP RECORD INTO DATA POOL    *         
* R1=A(UTL SCREEN MAP ENTRY)                                          *         
* R0=SCREEN MAP ENTRY NUMBER (3'S COMPLEMENT)                         *         
***********************************************************************         
EXTRACT  NTR1  WORK=(R4,125)                                                    
         USING CTMREC,R4           R4=A(MAP RECORD)                             
         CLI   0(R1),0             IGNORE UNUSED ENTRIES                        
         BE    EXTRACTX                                                         
         MVC   SCRNUM,0(R1)                                                     
         MVC   LODISP,1(R1)        SET LOW & HIGH TWA DISPLACEMENTS             
         MVC   HIDISP,CHKDSP                                                    
         CHI   R0,1                                                             
         BE    *+10                                                             
         MVC   HIDISP,4(R1)                                                     
         OC    HIDISP,HIDISP                                                    
         BNZ   *+10                                                             
         MVC   HIDISP,CHKDSP                                                    
*                                                                               
         XC    CTMKEY,CTMKEY                                                    
         MVI   CTMKTYP,C'M'                                                     
         MVC   CTMKSYS,TOVSYS                                                   
         MVC   CTMKPROG,TPRG                                                    
         MVC   CTMKSCRN,SCRNUM                                                  
         BAS   RE,CTREAD                                                        
         BNE   EXTRACTX            EXIT IF NOT FOUND                            
*                                                                               
         LA    R4,CTMDATA          R4=A(MAP ELEMENT)                            
         MVI   FLDNUM,0                                                         
EXTRACT2 CLI   0(R4),0                                                          
         BE    EXTRACTX                                                         
         CLI   0(R4),X'B1'                                                      
         BE    EXTRACT6                                                         
EXTRACT4 LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     EXTRACT2                                                         
*                                  PROCESS A MAP ELEMENT                        
         USING CTMAPD,R4                                                        
EXTRACT6 CLC   CTMAPFLD,FLDNUM     IGNORE MULTIPLE SUB-FIELDS                   
         BE    EXTRACT4                                                         
         MVC   FLDNUM,CTMAPFLD                                                  
         CLI   CTMAPNUM,0          IGNORE LITERALS                              
         BE    EXTRACT4                                                         
         MVC   FLDTYP,CTMAPNUM                                                  
*                                  LOCATE FIELD IN TWA                          
         L     R1,ATWA2                                                         
         LR    RF,R1                                                            
         AH    R1,LODISP           R1=A(FIRST FIELD)                            
         AH    RF,HIDISP                                                        
         BCTR  RF,0                RF=A(LAST FIELD)                             
         SR    RE,RE                                                            
         LLC   R0,FLDNUM           R0=RELATIVE FIELD NUMBER                     
         CLI   SCRNUM,X'FF'                                                     
         BNE   *+8                                                              
         AHI   R0,1                FIELD IS N+1 FOR X'FF' SCREENS               
EXTRACT8 CLI   0(R1),0             E-O-T                                        
         BE    EXTRACT4                                                         
         TM    1(R1),X'20'         IGNORE PROTECTED FIELDS                      
         BO    *+12                                                             
         AHI   R0,-1                                                            
         BZ    *+16                                                             
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,EXTRACT8                                                   
         B     EXTRACT4            EXIT IF FIELD NOT FOUND                      
         LLC   RE,0(R1)                                                         
         AHI   RE,-8               RE=MAX FIELD LENGTH                          
         TM    1(R1),X'02'         TEST EXTENDED FLDHDR                         
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         LA    RF,7(RE,R1)         RF=A(END OF FIELD)                           
         CLI   0(RF),C' '          LOOP BACK TO FIND LAST CHARACTER             
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         LTR   R0,RE               EXIT IF NOTHING IN FIELD                     
         BZ    EXTRACT4                                                         
         LLC   R3,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R3,R4                                                            
         CLI   0(R3),X'B1'         CHECK FOR SINGLE SUB-FIELD                   
         BNE   EXTRACTC                                                         
         CLC   CTMAPFLD-CTMAPD(L'CTMAPFLD,R3),FLDNUM                            
         BNE   EXTRACTC                                                         
         CLI   CTMAPNUM-CTMAPD(R3),0                                            
         BNE   EXTRACT4                                                         
         MVC   DUB(1),CTMAPLIT-CTMAPD(R3)                                       
         SR    RE,RE               LOOK DOWN FIELD FOR DELIMITER                
         LA    RF,8(R1)                                                         
EXTRACTA CLC   0(1,RF),DUB                                                      
         BE    EXTRACTC                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,EXTRACTA                                                      
*                                                                               
EXTRACTC AHI   RE,-1               ADD DATA ELEMENT TO DATA POOL                
         BM    EXTRACT4                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLD+2(0),8(R1)      SET DATA/LENGTH/CODE                         
         LA    RE,3(RE)                                                         
         STC   RE,FLD+1                                                         
         MVC   FLD(1),FLDTYP                                                    
         SR    RF,RF                                                            
         ICM   RF,3,DATAPOOL                                                    
         LA    R1,DATAPOOL+2(RF)   R1=A(END OF POOL)                            
         AR    RF,RE                                                            
         STCM  RF,3,DATAPOOL       SET NEW POOL LENGTH                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),FLD         MOVE IN DATA ELEMENT                         
         AR    R1,RE                                                            
         MVI   0(R1),0             SET END OF POOL                              
         B     EXTRACT4                                                         
EXTRACTX XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MOVE DDS LOGO TO CONNECT VIRGIN SCREEN                              *         
***********************************************************************         
SLOGO    NTR1  BASE=*,LABEL=*                                                   
         XC    ATWASES,ATWASES     CLEAR LOCATION OF SESSION ID                 
         TM    TTYPE,TTYPE327      NO LOGOS FOR NON-3270 TERMINALS              
         BNZ   *+14                                                             
SLOGO0   MVC   CNTTAB+1(3),=X'000100'                                           
         B     SLOGOX                                                           
*&&US*&& TM    SOXFL,SOXDDS        NO DDS LOGO FOR NON DDS TRMS IN USA          
*&&US*&& BZ    SLOGO2                                                           
*                                                                               
SLOGO1   L     R1,=A(DDSLOGO)      R1=A(DDS LOGO)                               
*&&UK                                                                           
         L     RF,VSSB             CHECK FOR LP2 SYSTEM                         
         TM    SSBSTAT5-SSBD(RF),SSB5LP2                                        
         BZ    *+8                                                              
         L     R1,=A(LP2LOGO)                                                   
*&&                                                                             
         A     R1,RELO                                                          
         LA    RF,CNTC01           POINT TO FIRST LOGO LINE                     
         LA    R0,12               NUMBER OF LINES IN LOGO                      
SLOGO1A  MVC   0(L'CNTC01,RF),0(R1)                                             
         LA    R1,L'DDSLOGO(R1)                                                 
         LA    RF,CNTC02-CNTC01(RF)                                             
         BCT   R0,SLOGO1A                                                       
*                                                                               
SLOGO2   DS    0H                                                               
*&&US*&& XR    RF,RF               LOGOS all the same in the US                 
*&&UK*&& LLC   RF,SYSIDNO          R1=A(FACPAK SYSTEM LOGO)                     
         MHI   RF,88                                                            
         L     R1,=A(SYSLOGO)                                                   
         A     R1,RELO                                                          
         AR    R1,RF                                                            
         LA    R0,2                NUMBER OF LINES IN PANEL                     
         LA    RF,CNTC11           POINT TO LAST TWO LOGO LINES                 
SLOGO2A  MVC   00(44,RF),0(R1)                                                  
         LA    R1,44(R1)                                                        
         LA    RF,CNTC02-CNTC01(RF)                                             
         BCT   R0,SLOGO2A                                                       
*                                                                               
         LA    RF,CNTC11           POINT TO LAST TWO LOGO LINES                 
         USING CPYRGHTD,RF                                                      
         MVC   CPYRTDTE,ISODATE    Move in copy right year                      
         DROP  RF                                                               
*&&US*&& TM    SOXFL,SOXDDS                                                     
*&&US*&& BZ    SLOGOX                                                           
*                                                                               
SLOGO4   SR    RF,RF               GET CURRENT LANGUAGE CODE                    
         CLI   TLANG,7                                                          
         BH    *+8                                                              
         IC    RF,TLANG                                                         
*&&US*&& IC    RF,SYSIDNO          USA USE SYSTEM ID NUM FOR SLOGAN             
         MHI   RF,48                                                            
         L     R1,=A(SLOGAN)       POINT TO SLOGAN TABLE                        
         A     R1,RELO                                                          
         AR    R1,RF                                                            
         MVC   CNTSLOG,0(R1)       MOVE SLOGAN TO CONNECT SCREEN                
*&&UK                                                                           
SLOGO5   L     R1,=A(LONADDR)      POINT TO LONDON ADDRESS LINE                 
         CLI   TCTRY,3                                                          
         BNE   *+8                                                              
         L     R1,=A(DUSADDR)      POINT TO DUESSELDORF                         
         CLI   TCTRY,4                                                          
         BNE   *+8                                                              
         L     R1,=A(PARADDR)      POINT TO PARIS                               
         CLI   TCTRY,7                                                          
         BNE   *+8                                                              
         L     R1,=A(AMSADDR)      POINT TO AMSTERDAM                           
         CLI   TCTRY,9                                                          
         BNE   *+8                                                              
         L     R1,=A(DUBADDR)      POINT TO DUBLIN                              
         A     R1,RELO                                                          
         MVC   CNTADDR,0(R1)       MOVE ADDRESS TO CONNECT SCREEN               
         MVC   CNTWWW,78(R1)       MOVE WWW INFO                                
*&&                                                                             
*&&US                                                                           
SLOGO5   L     R1,=A(NYCADDR)      POINT TO NYC ADDRESS LINE                    
         CLI   TCTRY,8                                                          
         BNE   *+8                                                              
         L     R1,=A(TORADDR)      POINT TO TORONTO                             
         A     R1,RELO                                                          
         MVC   CNTADDR,0(R1)       MOVE ADDRESS TO CONNECT SCREEN               
         MVC   CNTWWW,78(R1)       MOVE WWW INFO                                
*&&                                                                             
SLOGO6   SR    RF,RF               GET CURRENT LANGUAGE CODE                    
         CLI   TLANG,7                                                          
         BH    *+8                                                              
         IC    RF,TLANG                                                         
         MHI   RF,32                                                            
         L     R3,=A(XTRATXT)      R3=A(EXTRA DATA TEXT)                        
         A     R3,RELO                                                          
         AR    R3,RF                                                            
         LA    R4,CNTX1            R4=A(SCREEN AREA)                            
         MVI   BYTE,B'00001000'    SHOW SESSION IF DEFINED                      
*&&US                                                                           
         TM    SOXFL,SOXDDS        DDS SHOWS EXTRA DATA                         
         BZ    SLOGO6A                                                          
         TM    SYSIDTY,FACITST     FOR TEST SYSTEMS ONLY                        
         BZ    SLOGO6A                                                          
         OI    BYTE,B'10000111'    SHOW DATE/TIME/LUID/TRMNUM                   
*&&                                                                             
*&&UK                                                                           
         OI    BYTE,B'00000111'    SHOW DATE/TIME/LUID                          
         TM    SOXFL,SOXDDS        DDS SHOWS EXTRA DATA                         
         BZ    SLOGO6A                                                          
         TM    SYSIDTY,FACITST     FOR TEST SYSTEMS ONLY                        
         BZ    SLOGO6A                                                          
         OI    BYTE,B'10000000'    SHOW TRMNUM                                  
*&&                                                                             
SLOGO6A  TM    BYTE,B'00000001'    TEST TO SHOW DATE                            
         BZ    SLOGO6AX                                                         
         MVC   0(8,R4),0(R3)                                                    
         MVC   8(8,R4),SCRNDATE                                                 
         CLI   TCTRY,3                                                          
         BNE   *+12                                                             
         MVI   10(R4),C'.'                                                      
         MVI   13(R4),C'.'                                                      
SLOGO6AX LA    R3,8(R3)                                                         
         LA    R4,CNTX2-CNTX1(R4)                                               
*                                                                               
SLOGO6B  TM    BYTE,B'00000010'    TEST TO SHOW TIME                            
         BZ    SLOGO6BX                                                         
         MVC   0(8,R4),0(R3)                                                    
*&&US                                                                           
         THMS  DDSTIME=YES                                                      
         ST    R0,FULL                                                          
         AP    SCRNTIME,FULL       U.S. CLOCK ADJUST                            
*&&                                                                             
*&&UK*&& CLI   TCTRY,2                                                          
*&&UK*&& BNH   *+10                                                             
*&&UK*&& AP    SCRNTIME,=P'10000'  EUROPE UK+1 (P'10000' MOST OF TIME)          
         OI    SCRNTIME+3,X'0F'                                                 
         UNPK  DUB(6),SCRNTIME                                                  
         MVC   08(2,R4),DUB+0                                                   
         MVI   10(R4),C':'                                                      
         MVC   11(2,R4),DUB+2                                                   
         MVI   13(R4),C':'                                                      
         MVC   14(2,R4),DUB+4                                                   
SLOGO6BX LA    R3,8(R3)                                                         
         LA    R4,CNTX2-CNTX1(R4)                                               
*                                                                               
SLOGO6C  TM    BYTE,B'00000100'    TEST TO SHOW LUID                            
         BZ    SLOGO6CX                                                         
         MVC   0(8,R4),0(R3)       LUID                                         
         MVC   8(8,R4),TSYM                                                     
SLOGO6CX LA    R3,8(R3)                                                         
         LA    R4,CNTX2-CNTX1(R4)                                               
*                                                                               
SLOGO6D  MVC   0(8,R4),SPACES      TEST TO SHOW SESSION AND/OR TRMNUM           
         LA    RE,8(R4)                                                         
         MVI   BYTE1,C' '                                                       
         TM    BYTE,B'00001000'                                                 
         BZ    SLOGO6D4                                                         
SLOGO6D1 IC    R1,TSESSION         SESSION CHR IS X'A0'+TSESSION+1              
         N     R1,=X'00000007'                                                  
         LA    R1,1(R1)                                                         
         STC   R1,BYTE1                                                         
         OI    BYTE1,X'80'         SESSION IS LOWER CASE A THRU H               
SLOGO6D3 MVC   0(8,R4),0(R3)                                                    
         LA    R1,8(R4)                                                         
         ST    R1,ATWASES          SAVE LOCATION OF SESSION ID                  
         MVC   0(1,R1),BYTE1                                                    
         MVC   9(2,R4),SPACES                                                   
         LA    RE,11(R4)                                                        
SLOGO6D4 TM    BYTE,B'10000000'    TEST TO SHOW TERMINAL NUMBER                 
         BZ    SLOGOX                                                           
         CLC   0(8,R4),SPACES                                                   
         BNE   *+10                                                             
         MVC   4(4,R4),=C'Trm#'                                                 
         MVI   0(RE),C'#'                                                       
         LH    R0,TNUM                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(4,RE),DUB+5(3)                                                 
*                                                                               
SLOGOX   XIT1                                                                   
         LTORG                                                                  
*&&UK                                                                           
DDSLOGO  DS    0CL44                                                            
         DC    CL44'                                            '               
         DC    CL44'                                            '               
         DC    CL44'                                    /\\ /\\ '               
         DC    CL44'                                      \\/   '               
         DC    CL44' _ _   ___  __|''  __   __   __  ___  __  __ '              
         DC    CL44'| | | /__/ /  || /  | /  \ /   /__/ /  |l  |'               
         DC    CL44'|   | \___ \__|| \__\ \__/ \__ \___ \__\|  |'               
         DC    CL44'                                            '               
         DC    CL44'                                            '               
         DC    CL44'                                            '               
         DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
         DC    X'00'                                                            
*                                                                               
LP2LOGO  DS    0CL44                                                            
         DC    CL44'      000        0000000     000000         '               
         DC    CL44'      000        00000000   00000000        '               
         DC    CL44'      000        000  000   000  000        '               
         DC    CL44'      000        00000000       000         '               
         DC    CL44'      000        0000000       000          '               
         DC    CL44'      000      I 000      I   000    I      '               
         DC    CL44'      00000000 I 000      I  0000000 I      '               
         DC    CL44'      00000000 I 000      I 00000000 I      '               
         DC    CL44'               I          I          I      '               
         DC    CL44'                                            '               
         DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
         DC    X'00'                                                            
*&&                                                                             
*&&US                                                                           
DDSLOGO  DS    0CL44                                                            
         DC    CL44'                                            '               
         DC    CL44'                                            '               
         DC    CL44'                                    /\\ /\\ '               
         DC    CL44'                                      \\/   '               
         DC    CL44' _ _   ___  __|''  __   __   __  ___  __  __ '              
         DC    CL44'| | | /__/ /  || /  | /  \ /   /__/ /  |l  |'               
         DC    CL44'|   | \___ \__|| \__\ \__/ \__ \___ \__\|  |'               
         DC    CL44'                                            '               
         DC    CL44'                                            '               
         DC    CL44'                                            '               
         DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
         DC    X'00'                                                            
*&&                                                                             
*                                                                               
SYSLOGO  DS    0CL88                                                            
*&&UK                                                                           
SYSL0    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'           All rights reserved.             '               
*                                                                               
SYSL1    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  TST #### All rights reserved. ####  TST   '               
*                                                                               
SYSL2    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  ADV1     All rights reserved.      ADV1   '               
*                                                                               
SYSL3    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  TTS >>>> All rights reserved. >>>>  TTS   '               
*                                                                               
SYSL4    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  ADV2     All rights reserved.      ADV2   '               
*                                                                               
SYSL5    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  NEW <<<< All rights reserved. <<<<  NEW   '               
*                                                                               
SYSL6    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  ADV3     All rights reserved.      ADV3   '               
*                                                                               
SYSL7    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'           All rights reserved.             '               
*                                                                               
SYSL8    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  ADV4     All rights reserved.      ADV4   '               
*                                                                               
SYSL9    DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  BAR >>>> All rights reserved. >>>>  BAR   '               
*                                                                               
SYSL10   DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  ADVA     All rights reserved.      ADVA   '               
*                                                                               
SYSL11   DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  CSC \\\\ All rights reserved. \\\\  CSC   '               
*                                                                               
SYSL12   DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  ADVB     All rights reserved.      ADVB   '               
*                                                                               
SYSL13   DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  FQA //// All rights reserved. ////  FQA   '               
*                                                                               
SYSL14   DC    CL44'  (C) Copyright Mediaocean Ltd. 2000-####   '               
         DC    CL44'  ADVC     All rights reserved.      ADVC   '               
*&&                                                                             
*&&US                                                                           
SYSL0    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*&&                                                                             
*&&DO                                                                           
SYSL1    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL2    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL3    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL4    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL5    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL6    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL7    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL8    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL9    DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL10   DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL11   DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL12   DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL13   DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL14   DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*                                                                               
SYSL15   DC    CL44'  (C) Copyright Mediaocean LLC. 2000-####.  '               
         DC    CL44'            All rights reserved.            '               
*&&                                                                             
*&&UK                                                                           
SLOGAN   DC    CL48'     "ADVERTISING. POWERED BY MEDIAOCEAN."      '           
         DC    CL48'     "ADVERTISING. POWERED BY MEDIAOCEAN."      '           
         DC    CL48'     "ADVERTISING. POWERED BY MEDIAOCEAN."      '           
         DC    CL48'  "DIE NR.1 IM COMPUTER-SERVICE F!R AGENTUREN"  '           
         DC    CL48'  "LE SYSTEME NO.1 POUR AGENCES DE PUBLICITE"   '           
         DC    CL48'  "EL SISTEMA NO.1 PARA AGENCIAS DE PUBLICIDAD" '           
         DC    CL48'     "ADVERTISING. POWERED BY MEDIAOCEAN."      '           
         DC    CL48'  "DE COMPUTER-SERVICE VOOR DE RECLAMEWERELD"   '           
*&&                                                                             
*&&US                                                                           
SLOGAN   DC    CL48'                                                '           
         DC    CL48'  *************** TEST SYSTEM ***************   '           
         DC    CL48'  ---------- ADVERTISER ONE SYSTEM ----------   '           
         DC    CL48'  --------- ADVERTISER FIVE SYSTEM ----------   '           
         DC    CL48'  --------------- REPA SYSTEM ---------------   '           
         DC    CL48'  ---------- ADVERTISER TWO SYSTEM ----------   '           
         DC    CL48'  ************* MEL TEST SYSTEM *************   '           
         DC    CL48'  --------- ADVERTISER THREE SYSTEM ---------   '           
         DC    CL48'  --------- ADVERTISER FOUR SYSTEM ----------   '           
         DC    CL48'  --------------- REPC SYSTEM ---------------   '           
         DC    CL48'  ---------- ADVERTISER SIX SYSTEM ----------   '           
         DC    CL48'  *************** CSC SYSTEM ****************   '           
         DC    CL48'  --------- ADVERTISER SEVEN SYSTEM ---------   '           
         DC    CL48'  --------- ADVERTISER EIGHT SYSTEM ---------   '           
         DC    CL48'  --------------- REPB SYSTEM ---------------   '           
         DC    CL48'  ************** QA TEST SYSTEM *************   '           
*&&                                                                             
XTRATXT  DC    CL32'  Date    Time    Luid    Sess  '                           
         DC    CL32'  Date    Time    Luid    Sess  '                           
         DC    CL32'  Date    Time    Luid    Sess  '                           
         DC    CL32'  Datum   Zeit    Luid    Sess  '                           
         DC    CL32'  Date    Heure   Luid    Sess  '                           
         DC    CL32'  Fecha   Hora    Luid    Sess  '                           
         DC    CL32'  Date    Time    Luid    Sess  '                           
         DC    CL32'  Datum   Tijd    Luid    Sess  '                           
*                                                                               
LONADDR  DC    CL15'110 Southwark S'                                            
         DC    CL48't. London SE1 0TA. 020 7255 7000. Support Centre'           
         DC    CL15' 020 7255 7001 '                                            
LONWWW   DC    CL15' '                                                          
         DC    CL48'               www.mediaocean.com               '           
         DC    CL15' '                                                          
*                                                                               
DUBADDR  DC    CL15'110 Southwark S'                                            
         DC    CL48't. London SE1 0TA. 020 7255 7000. Support Center'           
         DC    CL15' 020 7255 7001 '                                            
DUBWWW   DC    CL15' '                                                          
         DC    CL48'               www.mediaocean.com               '           
         DC    CL15' '                                                          
*                                                                               
DUSADDR  DC    CL15' '                                                          
         DC    CL48'Bilker Stra~e 29a, 40213 D}sseldorf 0211/36810-0'           
         DC    CL15' '                                                          
DUSWWW   DC    CL15' '                                                          
         DC    CL48'               www.mediaocean.com               '           
         DC    CL15' '                                                          
*                                                                               
PARADDR  DC    CL15'    86-90 Rue d'                                            
         DC    CL48'u Dome, 921000 Boulogne Billancourt  Tel: 00 33 '           
         DC    CL15'1 4609 9800    '                                            
PARWWW   DC    CL15' '                                                          
         DC    CL48'               www.mediaocean.com               '           
         DC    CL15' '                                                          
*                                                                               
AMSADDR  DC    CL15'            7 F'                                            
         DC    CL48'arm Street, London W1J 5RX  Tel: 00 44 20 7255 7'           
         DC    CL15'000            '                                            
AMSWWW   DC    CL78' '                                                          
*                                                                               
NYCADDR  DC    CL15' '                                                          
         DC    CL48' 120 Broadway, New York, NY 10271  212-633-8100 '           
         DC    CL15' '                                                          
NYCWWW   DC    CL78' '                                                          
*                                                                               
TORADDR  DC    CL15' '                                                          
         DC    CL48'1835 Yonge St.,Toronto M4S 1X8,Can  416-487-2115'           
         DC    CL15' '                                                          
TORWWW   DC    CL78' '                                                          
*                                                                               
PWDWARNT DC    AL1(21),CL36'(Password expires in NNN days)'                     
         DC    AL1(21),CL36'(Password expires in NNN days)'                     
         DC    AL1(21),CL36'(Password expires in NNN days)'                     
         DC    AL1(19),CL36'(Kennwort lauft in NNN Tagen ab)'                   
         DC    AL1(26),CL36'(Mot de passe expire dans NNN jours)'               
         DC    AL1(21),CL36'(Password expires in NNN days)'                     
         DC    AL1(21),CL36'(Password expires in NNN days)'                     
         DC    AL1(21),CL36'(Password expires in NNN days)'                     
*                                                                               
PWDMAXWT DS    0CL22                                                            
         DC    AL1(16),CL21'One attempt left'                                   
         DC    AL1(16),CL21'One attempt left'                                   
         DC    AL1(16),CL21'One attempt left'                                   
         DC    AL1(21),CL21'ein Versuch verbleibt'                              
         DC    AL1(16),CL21'Un essai restant'                                   
         DC    AL1(16),CL21'One attempt left'                                   
         DC    AL1(16),CL21'One attempt left'                                   
         DC    AL1(16),CL21'One attempt left'                                   
         EJECT                                                                  
***********************************************************************         
* CHECK SPECIFIC TERMINALS FOR EXTRA SECURITY                         *         
***********************************************************************         
CHKIT    NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0               SET OK RETURN                                
*&&UK                                                                           
CHKIT0   CLC   PIDSEC,DDSSECP      CHECK IF DDS PERSON                          
         BNE   CHKIT0A             NO                                           
         CLC   USERID,=CL10'SSCTIME' TEST SSC TIMESHEET USER ID                 
         BE    CHKIT1W             YES-MUSTN'T USE DDS PID                      
         B     CHKIT0X             ELSE ALWAYS XTRA SECURITY IF DDS PID         
CHKIT0A  CLI   OVSYS,X'06'         CHECK IF ACC/BRAND                           
         BNE   CHKIT0B                                                          
         CLI   PROGNUM,X'24'                                                    
         BE    CHKIT1X             YES-ALLOW ANY PID ON USER ID                 
CHKIT0B  TM    TTYPE,TTYPEWAP      CHECK IF WEB TERM AND CON/MAD                
         BZ    CHKIT0X                                                          
         CLI   OVSYS,X'0A'                                                      
         BNE   CHKIT0X                                                          
         CLI   PROGNUM,X'0C'                                                    
         BE    CHKIT1X             YES-ALLOW ANY PID ON USER ID                 
CHKIT0X  EQU   *                                                                
*&&                                                                             
CHKIT1   MVC   DUB,TLUID           SAVE TERMINAL LUID                           
         TM    TSTAT6,TST6SCRP     VALID IF DUMMY TERMINAL                      
         BO    CHKIT1X                                                          
         L     RE,=A(MISCINFO)                                                  
         A     RE,RELO                                                          
*                                                                               
CHKIT1A  CLI   0(RE),X'00'         END OF LIST                                  
         BE    CHKIT1X                                                          
CHKIT1B  CLI   0(RE),X'01'         USERID ENTRY                                 
         BE    *+12                                                             
CHKIT1C  LA    RE,9(RE)                                                         
         B     CHKIT1A                                                          
         CLC   1(8,RE),USERID                                                   
         BNE   CHKIT1C                                                          
*                                                                               
CHKIT1D  LA    RE,9(RE)            BUMP TO NEXT PERSON/TERMINAL ENTRY           
         CLI   0(RE),0                                                          
         BE    CHKIT1W                                                          
CHKIT1E  CLI   0(RE),X'81'         LUID ENTRY                                   
         BNE   CHKIT1F                                                          
         MVI   FNDX,1              SET FIELD NUMBER                             
         CLC   1(8,RE),DUB                                                      
         BE    CHKIT1X                                                          
         B     CHKIT1D                                                          
CHKIT1F  CLI   0(RE),X'41'         PERSONID ENTRY                               
         BNE   CHKIT1G                                                          
         MVI   FNDX,2              SET FIELD NUMBER                             
         CLC   1(8,RE),PERSONID                                                 
         BE    CHKIT1X                                                          
         B     CHKIT1D                                                          
CHKIT1G  EQU   *                                                                
CHKIT1W  LA    R0,1                SET ERROR RETURN                             
CHKIT1X  LTR   R0,R0                                                            
         XIT1                                                                   
         LTORG                                                                  
MISCINFO DS    0C                  LIST OF PIDS FOR SPECIAL ACCESS              
*&&UK                                                                           
       ++INCLUDE SRCON00MUK                                                     
*&&                                                                             
*&&US                                                                           
       ++INCLUDE SRCON00MUS                                                     
*&&                                                                             
MISCINFX DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PASSWORD EXPIRY FOR SPECIAL USERID + PASSWORD INPUT        *         
***********************************************************************         
VALUSPW  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
VUSPW01  LLC   R1,PMAXERR          R1=MAX NUMBER OF TRYS                        
         LLC   R0,PIDLLOE          R0=CURRENT ERROR COUNT                       
         SR    R1,R0                                                            
         BP    *+6                                                              
         SR    R1,R1                                                            
         STC   R1,PWDTRYS          R1=NUM OF TRYS LEFT                          
         BNZ   *+8                                                              
         OI    PWDSTAT,PSTMAX      SET MAXIMUM HAS BEEN EXCEEDED                
         CHI   R1,2                                                             
         BNE   *+8                                                              
         OI    PWDSTAT,PSTMAXW     SET MAXIMUM ATTEMPTS WARNING                 
*                                                                               
VUSPW02  LA    RF,SECCODE+L'SECCODE-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RE,SECCODE                                                       
         SR    RF,RE                                                            
         STC   RF,BYTE             BYTE=LEN-1 OF PASSWORD INPUT                 
*                                                                               
         L     R4,ASECREC                                                       
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY       BUILD AUTH(ALPHA) KEY                        
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,PIDSEC                                                   
         MVC   SA0KNUM,PIDNUM                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',(R4),(R4)                  
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         TM    TSTAT7,TST7PS1      FIRST TIME FOR NEW PASSWORD?                 
         BO    VUSPW13                                                          
         TM    TSTAT7,TST7PS2      SECOND TIME FOR NEW PASSWORD?                
         BO    VUSPW22                                                          
*                                                                               
         LA    R4,SA0DATA                                                       
         USING SAPWHD,R4                                                        
LATEST   USING SAPWHD,R3                                                        
LATEMP   USING SAPWHD,R5                                                        
         XR    R3,R3               R3=A(LAST PASSORD)                           
         XR    R5,R5               R5=A(LAST TEMPORARY PASSORD)                 
         XR    RF,RF                                                            
*                                                                               
VUSPW06  CLI   SAPWHEL,0           RECORD END                                   
         BE    VUSPW08                                                          
         CLI   SAPWHEL,SAPWHELQ                                                 
         BNE   VUSPW06C                                                         
         CLC   SAPWHDTE,TODAYB     EFFECTIVE AFTER TODAY?                       
         BH    VUSPW06C                                                         
         TM    SAPWHFLG,SAPWHEXP+SAPWHFOR                                       
         BO    VUSPW06B                                                         
*                                                                               
VUSPW06A LTR   R3,R3               TEST FIRST PASSWORD FOUND                    
         BNZ   *+6                                                              
         LR    R3,R4               R3=FIRST PASSWORD                            
         CLC   LATEST.SAPWHDTE,SAPWHDTE                                         
         BH    VUSPW06C                                                         
         BL    *+14                                                             
         CLC   LATEST.SAPWHTME,SAPWHTME                                         
         BH    *+6                                                              
         LR    R3,R4               R3=MOST RECENT PASSWORD                      
         B     VUSPW06C                                                         
*                                                                               
VUSPW06B LTR   R5,R5               TEST FIRST TEMPORARY PASSWORD                
         BNZ   *+6                                                              
         LR    R5,R4               R5=FIRST TEMPORARY PASSWORD                  
         CLC   LATEMP.SAPWHDTE,SAPWHDTE                                         
         BH    VUSPW06C                                                         
         BL    *+14                                                             
         CLC   LATEMP.SAPWHTME,SAPWHTME                                         
         BH    *+6                                                              
         LR    R5,R4               R5=MOST RECENT TEMPORARY PASSWORD            
*                                                                               
VUSPW06C IC    RF,SAPWHLN          BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     VUSPW06                                                          
*                                                                               
VUSPW08  ST    R3,PWDALAST         END OF RECORD - SAVE A(ELEMENTS)             
         ST    R5,PWDATEMP                                                      
         LTR   R5,R5               TEST IF TEMPORARY PASSWORD FOUND             
         BNZ   VUSPW08B                                                         
         TM    PWDSTAT,PSTMAX      NO-TEST MAXIMUM HAS BEEN EXCEEDED            
         BO    VUSPMAX                                                          
         B     VUSPW09                                                          
VUSPW08B LTR   R3,R3               TEST PASSWORD FOUND                          
         BNZ   VUSPW08C                                                         
         LR    R3,R5               NO-SET LAST TO TEMPORARY                     
         ST    R3,PWDALAST                                                      
         TM    PWDSTAT,PSTMAX      TEST MAXIMUM HAS BEEN EXCEEDED               
         BO    VUSPMAX                                                          
         B     VUSPW09                                                          
VUSPW08C CLC   LATEMP.SAPWHDTE,LATEST.SAPWHDTE                                  
         BL    VUSPW08D                                                         
         BH    VUSPW09                                                          
         CLC   LATEMP.SAPWHTME,LATEST.SAPWHTME                                  
         BH    VUSPW09                                                          
VUSPW08D XR    R5,R5               IGNORE OLD TEMPORARY PASSWORD                
         ST    R5,PWDATEMP                                                      
         TM    PWDSTAT,PSTMAX      TEST MAXIMUM HAS BEEN EXCEEDED               
         BO    VUSPMAX                                                          
         DROP  LATEST                                                           
         DROP  LATEMP                                                           
*                                                                               
VUSPW09  LTR   R4,R3               R4=A(PASSWORD ELEMENT)                       
         BNZ   VUSPW10                                                          
         OI    PWDSTAT,PSTEXP      SET EXPIRED IF NO PASSWORD FOUND             
         B     VUSPINV             TREAT AS INVALID PASSWORD                    
*                                                                               
VUSPW10  TM    SAPWHFLG,SAPWHEXP+SAPWHFOR TEST IF TEMPORARY PASSWORD            
         BNO   VUSPW10A                                                         
         LHI   R0,2                TEMPORARY VALID FOR TWO DAYS ONLY            
         OI    PWDSTAT,PSTEXP      NEEDS TO BE CHANGED                          
         B     VUSPW10C                                                         
VUSPW10A CLI   PWDCNTL,SAPERPNQ    TEST NO EXPIRY PASSWORD                      
         BE    VUSPW11                                                          
*NOP*    CLI   PWDCNTL,SAPERPAQ                                                 
*NOP*    BE    VUSPW11                                                          
         XR    R0,R0               TEST IF PASSWORD EXPIRATION DEFINED          
         ICM   R0,1,PEXPIRY                                                     
         BZ    VUSPW11                                                          
         TM    SOXFL,SOXSCRP       DONT TEST PSWD EXPIRY FOR SCRIPTS            
         BO    VUSPW11                                                          
         MVC   PWDEXDAT,TODAY                                                   
*                                                                               
VUSPW10B TM    SAPWHFLG,SAPWHEXP   TEST IF RESET BY SECURITY MANAGER            
         BO    VUSPW10D            YES-TREAT AS EXPIRED TODAY                   
*                                                                               
VUSPW10C GOTO1 VDATCON,DMCB,(2,SAPWHDTE),(0,DUB)                                
         GOTO1 VADDAY,DMCB,(C'D',DUB),PWDEXDAT,(R0)                             
         CLC   TODAY,PWDEXDAT                                                   
         BL    VUSPW10E                                                         
*                                                                               
VUSPW10D TM    TTEST,TTESTNOU      TEST IF UPDATES INHIBITED                    
         BO    VUSPW10E                                                         
         OI    PWDSTAT,PSTEXP      SET PASSWORD HAS EXPIRED                     
         B     VUSPW11                                                          
*                                                                               
VUSPW10E GOTO1 VPERVERT,DMCB,TODAY,PWDEXDAT                                     
         SR    RF,RF                                                            
         ICM   RF,3,DMCB+8                                                      
         AHI   RF,-1               GET NUMBER OF DAYS TO EXPIRATION             
         BNP   VUSPW11                                                          
         CHI   RF,255                                                           
         BNH   *+8                                                              
         LHI   RF,255                                                           
         STC   RF,PEXPDAYS         SAVE NUMBER OF DAYS TO EXPIRATION            
         TM    SAPWHFLG,SAPWHEXP+SAPWHFOR                                       
         BO    VUSPW11             NO WARNINGS FOR TEMPORARY PASSWORD           
         XR    RE,RE               TEST IF PASSWORD WARNING DEFINED             
         ICM   RE,1,PEXPWARN                                                    
         BZ    VUSPW11                                                          
         SR    R0,RE                                                            
         BNP   VUSPW11                                                          
         GOTO1 VADDAY,DMCB,(C'D',DUB),DUB1,(R0)                                 
         CLC   TODAY,DUB1          TEST IF WITHIN WARNING PERIOD                
         BL    VUSPW11                                                          
         OI    PWDSTAT,PSTEXPW     SET PASSWORD EXPIRATION WARNING              
*                                                                               
VUSPW11  LLC   RF,SAPWHLN          TEST PASSWORD WITH VALUE IN RECORD           
         SH    RF,=Y(SAPWHLNQ+1)   RF  =L'-1 OF PSWRD IN ELEMENT                
         CLM   RF,1,BYTE           BYTE=L'-1 OF PSWRD FIELD                     
         BNE   VUSPW11B            MUST BE EQUAL                                
         CLI   BYTE,MPWDL-1                                                     
         BH    VUSPW11B            INPUT CANT BE GREATER THAN MAX               
         CLI   PASSLEN,MPWDL                                                    
         BH    VUSPW11B            ACTUAL INPUT CANT BE GREATER EITHER          
         EX    RF,VUSPW11I                                                      
         BE    VUSPW11A            INPUT FIELD MATCH                            
         EX    RF,VUSPW11L                                                      
         BE    VUSPW11A            LOWER CASE MATCH                             
         EX    RF,VUSPW11U                                                      
         BE    VUSPW11A            UPPER CASE MATCH                             
         B     VUSPW11B                                                         
VUSPW11I CLC   CNTPWD(0),SAPWHPWD                                               
VUSPW11L CLC   SECCODE(0),SAPWHPWD                                              
VUSPW11U CLC   SECCODU(0),SAPWHPWD                                              
*                                                                               
VUSPW11A TM    PWDSTAT,PSTMAX      ALWAYS ERROR IF MAX ATTEMPTS                 
         BO    VUSPMAX                                                          
         TM    SAPWHFLG,SAPWHEXP+SAPWHFOR TEST MATCH ON TEMP P/W                
         BNO   VUSPW1AA                   NO: MATCHED ON NON-TEMP               
         CLC   TODAY,PWDEXDAT             IS TEMPORARY STILL VALID?             
         BH    VUSPW11C                   NO: TEMP IS TOO OLD                   
         OI    PWDSTAT,PSTRESET                                                 
         B     VUSPW12                                                          
*                                                                               
VUSPW1AA CLI   PWDCNTL,SAPERPNQ    TEST NO EXPIRY PASSWORD                      
         BE    VUSPW1AB                                                         
         XR    R0,R0               TEST IF PASSWORD EXPIRATION DEFINED          
         ICM   R0,1,PEXPIRY                                                     
         BZ    VUSPW1AB                                                         
         TM    SOXFL,SOXSCRP       DONT TEST PSWD EXPIRY FOR SCRIPTS            
         BO    VUSPW1AB                                                         
         TM    PIDLLFL,SALLOMLC+SALLOFPW  HIT MAX AFTER TEMP P/W REQT?          
         BO    VUSPW11B            YES: CAN'T USE OLD PASSWORD                  
VUSPW1AB CLI   PIDLLOE,0           CHECK IF PREVIOUS ERRORS                     
         BE    VUSPW1CC                                                         
         OI    PWDSTAT,PSTRESET    RESET PASSWORD ERROR COUNT                   
         B     VUSPW11D                                                         
*                                                                               
VUSPW11B CR    R4,R5               NO MATCH                                     
         BE    VUSPW11C                                                         
         LTR   R5,R5               IS THERE A NEW TEMPORARY P/W                 
         BZ    VUSPW11C                                                         
         LR    R4,R5               YES-NOW TRY NEW TEMPORARY P/W                
         B     VUSPW10                                                          
*                                                                               
VUSPW11C OI    PWDSTAT,PSTBUMP     NO MATCH - BUMP PASSWORD ERROR COUNT         
         B     VUSPW11D                                                         
*                                                                               
VUSPW1CC LTR   R5,R5               IS THERE A NEW TEMP P/W                      
         BZ    VUSPW11D            NO                                           
         BAS   RE,VUSPWSWA         SWITCH TO CONTROL SYSTEM                     
         BAS   RE,VUSPRD           READ RECORD FOR UPDATE                       
LATEMP   USING SAPWHD,R5           YES: NEED TO REMOVE FORGOT TEMP P/W          
         MVC   SAELEM(L'SAPWHDTE+L'SAPWHFLG+L'SAPWHTME),LATEMP.SAPWHDTE         
         BAS   RE,VUSPDEL          DELETE THIS FORGOT P/W TEMP                  
         MVI   ACTN,0                                                           
         BAS   RE,VUSPWT           WRITE RECORD BACK TO FILE                    
         BAS   RE,VUSPWSW1         SWITCH BACK TO SERVICE SYSTEM                
         MVI   TSYS,1                                                           
*                                                                               
         ICM   RF,15,VDMRCVR       COMMIT RECOVERY WRITE                        
         JZ    *+2                                                              
         GOTO1 (RF),DMCB,0,0,X'00FF0000'                                        
*                                                                               
         OI    PWDSTAT,PSTRESET    RESET PASSWORD ERROR COUNT                   
         DROP  LATEMP                                                           
*                                                                               
VUSPW11D TM    SAPWHFLG,SAPWHEXP+SAPWHFOR TEST IF TEMPORARY PASSWORD            
         BO    *+12                                                             
         CLI   PWDCNTL,SAPERPNQ    TEST NO EXPIRY PASSWORD                      
         BE    VUSPW11E                                                         
         TM    SYSIDTY,FACITST     ALWAYS EXPIRE ZERO ON TST SYSTEMS            
         BO    *+12                                                             
         CLI   PEXPIRY,0           TEST NO EXPIRY VALUE FOR AGENCY              
         BE    VUSPW11E                                                         
         TM    PWDSTAT,PSTBUMP     TEST PASSWORD ERROR                          
         BO    VUSPW12                                                          
         TM    PWDSTAT,PSTEXP      TEST PASSWORD EXPIRED                        
         BO    VUSPW12                                                          
*                                                                               
VUSPW11E TM    PWDSTAT,PSTRESET+PSTBUMP                                         
         BZ    VALUSPWX            EXIT IF DONT NEED TO SET ERROR COUNT         
*                                                                               
VUSPW12  L     R1,VSELIST          TEST CONTROL SYSTEM UPDATIVE                 
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
*                                                                               
         CLI   SESYS,X'0A'         SEE IF SAFE TO SWITCH                        
         BE    *+10                                                             
         BXLE  R1,RE,*-8                                                        
         DC    H'0'                                                             
*                                                                               
         TM    TTEST,TTESTNOU      TEST IF UPDATES INHIBITED                    
         BO    VUSPW12A                                                         
         TM    SEIND,SEISTRT       TEST IF CONTROL SYSTEM UPDATIVE              
         BZ    VUSPW12A                                                         
         TM    SEIND,SEINOP+SEIRONLY+SEISETRO                                   
         BZ    VUSPW12B                                                         
VUSPW12A TM    PWDSTAT,PSTBUMP     CONTROL SYSTEM NOT UPDATIVE                  
         BZ    *+12                                                             
         MVI   PWDSTAT,0           TREAT AS ERROR IF CAN'T CHANGE               
         B     VUSPINV                                                          
         MVI   PWDSTAT,0           TREAT AS OK IF CAN'T CHANGE                  
         B     VALUSPWX                                                         
*                                                                               
VUSPW12B TM    PWDSTAT,PSTRESET+PSTBUMP                                         
         BNZ   VUSPW60             NEED TO SET/RESET PASSWORD ERROR CNT         
*                                                                               
VUSPW12C OI    TSTAT7,TST7PS1      PASSWORD HAS EXPIRED                         
         MVC   TIDBITS+0(2),PIDSEC SAVE ID OF LAST PERSON IN THIS UTL           
         MVC   TIDBITS+2(2),PIDNUM                                              
         MVC   TSYS,SVTSYS         RESTORE TSYS                                 
         B     VUSPEXP                                                          
                                                                                
***********************************************************************         
* NEW PASSWORD HAS BEEN ENTERED - SEE IF TRYING REUSE                 *         
***********************************************************************         
VUSPW13  NI    TSTAT7,255-(TST7PS0+TST7PS1+TST7PS2+TST7PSWN)                    
         LLC   RF,BYTE                                                          
         LA    RF,1(RF)                                                         
         CLI   SECCODEL,MPWDL      TEST IF TOO LONG                             
         BNH   *+12                                                             
         OI    TSTAT7,TST7PS1                                                   
         B     VUSPPTL                                                          
         CLM   RF,1,PMINLEN        ENSURE LENGTH INPUT IS SUFFICIENT            
         BNL   VUSPW14                                                          
         OI    TSTAT7,TST7PS1                                                   
         B     VUSPPTS                                                          
*                                                                               
VUSPW14  XR    RF,RF                                                            
         CLI   BYTE,2                                                           
         BNE   VUSPW15                                                          
         CLC   SECCODU(3),=CL10'DDS'                                            
         BNE   VUSPW15                                                          
         OI    TSTAT7,TST7PS1                                                   
         B     VUSPPDR             INVALID DUE TO RESERVED DDS PASSWORD         
*                                                                               
VUSPW15  L     R4,ASECREC                                                       
         USING SA0REC,R4                                                        
         LA    R4,SA0DATA                                                       
         USING SAPWHD,R4                                                        
         XR    RF,RF                                                            
*                                                                               
VUSPW16  CLI   SAPWHEL,0           RECORD END                                   
         BE    VUSPW20             YES-NO MATCH, SO NO REUSE TRIED              
         CLI   SAPWHEL,SAPWHELQ                                                 
         BNE   VUSPW18                                                          
         CLC   SAPWHDTE,=X'FFFF'   IGNORE TEMPORARY ENTRIES                     
         BE    VUSPW18                                                          
         IC    RF,SAPWHLN                                                       
         SH    RF,=Y(SAPWHLNQ+1)   RF  =L'-1 OF PSWRD IN ELEMENT                
         CLM   RF,1,BYTE           BYTE=L'-1 OF PSWRD FIELD                     
         BNE   VUSPW18             MUST BE EQUAL                                
         EX    RF,VUSP16I                                                       
         BE    VUSPW17             MATCH ON INPUT FIELD                         
         EX    RF,VUSP16L                                                       
         BE    VUSPW17             MATCH ON LOWER CASE                          
         EX    RF,VUSP16U                                                       
         BE    VUSPW17             MATCH ON UPPER CASE                          
         B     VUSPW18                                                          
VUSP16I  CLC   CNTPWD(0),SAPWHPWD                                               
VUSP16L  CLC   SECCODE(0),SAPWHPWD                                              
VUSP16U  CLC   SECCODU(0),SAPWHPWD                                              
*                                                                               
VUSPW17  OI    TSTAT7,TST7PS1      SET INVALID DUE TO PREVIOUS USE              
         B     VUSPPUS                                                          
*                                                                               
VUSPW18  IC    RF,SAPWHLN                                                       
         AR    R4,RF                                                            
         B     VUSPW16                                                          
         DROP  R4                                                               
*                                                                               
VUSPW20  BAS   RE,VUSPWSWA         SWITCH TO CONTROL SYSTEM                     
         BAS   RE,VUSPRD           READ RECORD FOR UPDATE                       
*                                  DELETE ANY TEMPORARY HISTORY ELS             
         GOTO1 VHELLO,DMCB,(C'D',CTFBIG),(X'E4',ASECREC),=X'FFFF',0             
*                                                                               
         XC    SAELEM,SAELEM       ADD NEW TEMORARY COPY OF PASSWORD            
TMP      USING SAPWHD,SAELEM                                                    
         MVI   TMP.SAPWHEL,SAPWHELQ                                             
         MVC   TMP.SAPWHDTE,=XL2'FFFF'                                          
         LLC   RE,BYTE                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TMP.SAPWHPWD(0),SECCODE                                          
         AH    RE,=Y(SAPWHLNQ+1)                                                
         STC   RE,TMP.SAPWHLN                                                   
         DROP  TMP                                                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',CTFBIG),ASECREC,SAELEM,0                       
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*                                                                               
         MVI   ACTN,X'26'          WRITE RECORD BACK - NO RECOVERY              
         BAS   RE,VUSPWT                                                        
         BAS   RE,VUSPWSW1         SWITCH BACK TO SERVICE SYSTEM                
         OI    TSTAT7,TST7PS2      CONFIRM PASSWORD                             
*                                                                               
         B     VUSPREN                                                          
                                                                                
***********************************************************************         
* MAKE SURE PASSWORD IS CONFIRMED                                     *         
***********************************************************************         
VUSPW22  NI    TSTAT7,255-(TST7PS0+TST7PS1+TST7PS2+TST7PSWN)                    
         L     R4,ASECREC          SECOND TIME FOR NEW PASSWORD                 
         USING SA0REC,R4                                                        
         LA    R4,SA0DATA                                                       
         USING SAPWHD,R4                                                        
         XR    RF,RF                                                            
*                                                                               
VUSPW24  CLI   SAPWHEL,0           RECORD END                                   
         JE    *+2                                                              
         CLI   SAPWHEL,SAPWHELQ                                                 
         BNE   *+14                                                             
         CLC   SAPWHDTE,=X'FFFF'                                                
         BE    VUSPW26                                                          
         IC    RF,SAPWHLN                                                       
         AR    R4,RF                                                            
         B     VUSPW24                                                          
*                                                                               
VUSPW26  IC    RF,SAPWHLN                                                       
         SH    RF,=Y(SAPWHLNQ+1)   RF  =L'-1 OF PSWRD IN ELEMENT                
         CLM   RF,1,BYTE           BYTE=L'-1 OF PSWRD FIELD                     
         BNE   VUSPW28             MUST BE EQUAL                                
         EX    RF,VUSPW26I                                                      
         BE    VUSPW30             MATCH ON INPUT FIELD                         
         EX    RF,VUSPW26L                                                      
         BE    VUSPW30             MATCH ON LOWER                               
         EX    RF,VUSPW26U                                                      
         BE    VUSPW30             MATCH ON UPPER                               
         B     VUSPW28                                                          
VUSPW26I CLC   CNTPWD(0),SAPWHPWD                                               
VUSPW26L CLC   SECCODE(0),SAPWHPWD                                              
VUSPW26U CLC   SECCODU(0),SAPWHPWD                                              
*                                                                               
VUSPW28  BAS   RE,VUSPWSWA         SWITCH TO CONTROL SYSTEM                     
         BAS   RE,VUSPRD           READ RECORD FOR UPDATE                       
         XC    SAELEM,SAELEM                                                    
         MVC   SAELEM(2),=XL2'FFFF'                                             
         BAS   RE,VUSPDEL          DELETE ALL TEMPORARY ELEMENTS                
         MVI   ACTN,0                                                           
         BAS   RE,VUSPWT           WRITE RECORD BACK TO FILE                    
         BAS   RE,VUSPWSW1         SWITCH BACK TO SERVICE SYSTEM                
         OI    TSTAT7,TST7PS1                                                   
         B     VUSPNSM             ERROR PASSWORD NOT SAME                      
         DROP  R4                                                               
*                                                                               
VUSPW30  BAS   RE,VUSPWSWA         SWITCH TO CONTROL SYSTEM                     
         BAS   RE,VUSPRD           READ PASSWORD NUM RECORD FOR UPDATE          
         BAS   RE,VUSPRDP          READ PERSON RECORD FOR UPDATE                
         MVC   PEXPDAYS,PEXPIRY    SET NEW EXPIRATION DAYS                      
*                                                                               
         MVC   SAELEM(2),=XL2'FFFF'                                             
         BAS   RE,VUSPGET          GET TEMPORARY ELEMENT                        
*                                                                               
         ICM   R4,7,13(R1)         SET CHANGE DATE TO TODAY                     
         MVC   SAPWHDTE-SAPWHD(L'SAPWHDTE,R4),TODAYB                            
         MVI   SAPWHFLG-SAPWHD(R4),0                                            
         STAM  ARE,AR1,DMCB                                                     
         TIME  BIN                                                              
         LAM   ARE,AR1,DMCB                                                     
         LTR   R0,R0                                                            
         JZ    *+2                                                              
         STCM  R0,7,SAPWHTME-SAPWHD(R4)                                         
*                                                                               
VUSPW32  L     R4,ASECREC          COUNT NUMBER OF PASSWORD ELEMENTS            
         LA    R4,SA0DATA-SA0REC(R4)                                            
         USING SAPWHD,R4                                                        
         XR    R3,R3               R3=A(OLDEST PASSWORD)                        
OLDEST   USING SAPWHD,R3                                                        
         XR    RE,RE               RE=COUNT OF PASSWORDS ON RECORD              
         XR    RF,RF                                                            
*                                                                               
VUSPW34  CLI   SAPWHEL,0           EOR?                                         
         BE    VUSPW38             YES-SEE IF NEED TO LOSE AN ELEMENT           
         CLI   SAPWHEL,SAPWHELQ    PASSWORD ELEMENT?                            
         BNE   VUSPW36             NO                                           
*                                                                               
         LTR   R3,R3               R3=A(OLDEST PASSWORD)                        
         BNZ   *+6                                                              
         LR    R3,R4                                                            
         CLC   OLDEST.SAPWHDTE,SAPWHDTE                                         
         BL    VUSPW35                                                          
         BH    *+14                                                             
         CLC   OLDEST.SAPWHTME,SAPWHTME                                         
         BL    *+6                                                              
         LR    R3,R4               OLDER PASSWORD FOUND                         
*                                                                               
VUSPW35  LA    RE,1(RE)            BUMP COUNT                                   
*                                                                               
VUSPW36  IC    RF,SAPWHLN          NEXT ELEMENT                                 
         AR    R4,RF                                                            
         B     VUSPW34                                                          
         DROP  R4                                                               
*                                                                               
VUSPW38  L     R1,ASECREC          CHECK WE ARE NOT EXCEEDING MAXREC            
         CLC   SA0LEN-SA0KEY(2,R1),=Y(MAXRECL)                                  
         BH    VUSPW39             IF WE DO THEN DROP OLD PW HISTORY            
         CLM   RE,1,PREUSE         ELEMENT COUNT EXCEEDS REUSE NUMBER           
         BNH   VUSPW40             NO                                           
*                                                                               
VUSPW39  MVC   SAELEM(L'SAPWHDTE+1+L'SAPWHTME),OLDEST.SAPWHDTE                  
         BAS   RE,VUSPDEL          DELETE OLDEST ELEMENT                        
         B     VUSPW32             RETRY LOOP                                   
         DROP  OLDEST                                                           
*                                                                               
VUSPW40  L     R4,ASECREC          UPDATE OLD STYLE PASSWORD ELEMENTS           
         LA    R4,SA0DATA-SA0REC(R4)                                            
         USING SAPASD,R4                                                        
         XR    RF,RF                                                            
*                                                                               
VUSPW42  CLI   SAPASEL,0           EOR?                                         
         BE    VUSPW44             ADD OLD PASSWORD ELEMENT                     
         CLI   SAPASEL,SAPASELQ    OLD PASSWORD ELEMENT FOUND                   
         BE    *+14                                                             
         IC    RF,SAPASLEN         NEXT ELEMENT                                 
         AR    R4,RF                                                            
         B     VUSPW42                                                          
         MVC   SAPASDTA(MPWDL),SECCODE   MOVE IN NEW PASSWORD CODE              
         B     VUSPW46                                                          
         DROP  R4                                                               
*                                                                               
VUSPW44  XC    SAELEM,SAELEM       ADD OLD STYLE PASSWORD ELEMENT               
         LA    R4,SAELEM                                                        
         USING SAPASD,R4                                                        
         MVI   SAPASEL,SAPASELQ                                                 
         MVI   SAPASLEN,X'0C'                                                   
         MVC   SAPASDTA(MPWDL),SECCODE                                          
         GOTO1 VHELLO,DMCB,(C'P',CTFBIG),ASECREC,SAELEM,0                       
         CLI   12(R1),0                                                         
         JNE   *+2                 ELEMENT ADD FAILED                           
         DROP  R4                                                               
*                                                                               
VUSPW46  BAS   RE,VUSPUPP          UPDTE PERSON RECORD PASSWORD ELEMENT         
         MVI   ACTN,0                                                           
         BAS   RE,VUSPWT           WRITE RECORD BACK TO FILE                    
         MVI   ACTN,0                                                           
         BAS   RE,VUSPWTP          WRITE PERSON RECORD BACK TO FILE             
         BAS   RE,VUSPWSW1         SWITCH BACK TO SERVICE SYSTEM                
         MVI   TSYS,1                                                           
*                                                                               
         ICM   RF,15,VDMRCVR                                                    
         JZ    *+2                                                              
         GOTO1 (RF),DMCB,0,0,X'00FF0000'                                        
*                                                                               
         B     VALUSPWX            EXIT                                         
                                                                                
***********************************************************************         
* UPDATE PASSWORD ERROR COUNT IN PERSON RECORD                        *         
***********************************************************************         
VUSPW60  OC    PIDLLOEL,PIDLLOEL   MUST HAVE LAST LOGGED ON ELEMENT             
         BZ    VUSPW70                                                          
*                                                                               
         BAS   RE,VUSPWSWA         SWITCH TO CONTROL SYSTEM                     
         BAS   RE,VUSPRDP          READ FOR UPDATE PERSON RECORD                
*                                                                               
         L     R3,APERREC                                                       
         AH    R3,PIDLLOEL         R3=A(LAST LOGGED ON ELEMENT)                 
         USING SALLOD,R3                                                        
         CLI   SALLOEL,SALLOELQ    TEST ELEMENT STILL EXISTS                    
         BNE   VUSPW68                                                          
         CLI   SALLOLEN,SALLOLNQ                                                
         BNE   VUSPW68                                                          
*&&UK*&& NOP   VUSPW68             *NOPID* B VUSPW68                            
         MVC   SALLODT,TODAY3      SET DATE                                     
         MVC   SALLOADV,SYSIDNO    SET FACPAK ID                                
         MVC   SALLOUSR,USERNO     SET USER/SYSTEM/PROGRAM                      
         MVC   SALLOSYS,OVSYS                                                   
         MVC   SALLOPRG,PROGNUM                                                 
         STAM  ARE,AR1,DMCB                                                     
         TIME  BIN                                                              
         LAM   ARE,AR1,DMCB        SET TIME IN 1/100 SEC UNITS                  
         STCM  R0,7,SALLOTME                                                    
*                                                                               
VUSPW64  TM    PWDSTAT,PSTRESET    TEST TO RESET COUNT                          
         BZ    VUSPW65                                                          
         MVI   SALLOCNT,0                                                       
         NI    SALLOFLG,255-SALLOMLC-SALLOFPW-SALLOSWP                          
         B     VUSPW67                                                          
VUSPW65  TM    PWDSTAT,PSTBUMP     TEST TO BUMP COUNT                           
         BZ    VUSPW67                                                          
*                                                                               
         TM    SYSIDTY,FACIFQA     IS THIS A QA FACPAK?                         
         BO    *+12                YES: NEED TO HANDLE ERRORS                   
         TM    SYSIDTY,FACITST     IS THIS A TEST FACPAK?                       
         BO    VUSPW66             YES: IGNORE ERRORS                           
         LLC   R0,SALLOCNT                                                      
         AHI   R0,1                                                             
         STC   R0,SALLOCNT         BUMP INVALID PASSWORD COUNT                  
*                                                                               
VUSPW66  CLI   PWDTRYS,1           TEST IF JUST USED LAST TRY                   
         BNE   VUSPW67                                                          
         OI    SALLOFLG,SALLOMLC   SET MAX LOGIN COUNT REACHED                  
*                                                                               
         CLI   PWDCNTL,SAPERPNQ    TEST NO EXPIRY PASSWORD                      
         BNE   VUSPW67                                                          
         MVI   SALLOCNT,0                                                       
         NI    SALLOFLG,255-SALLOMLC-SALLOFPW-SALLOSWP                          
         OI    PWDSTAT,PSTNOP      NO-OP THE MAXIMUM COUNT ERROR                
*                                                                               
VUSPW67  MVI   ACTN,X'26'          WRITE PERSON RECORD - NO RECOVERY            
         BAS   RE,VUSPWTP                                                       
         OI    PWDSTAT,PSTUPDT     SET UPDATED FLAG                             
VUSPW68  BAS   RE,VUSPWSW1         SWITCH BACK TO SERVICE SYSTEM                
         MVI   TSYS,1                                                           
*                                                                               
VUSPW70  TM    PWDSTAT,PSTRESET+PSTEXP  TEST RESET COUNT AND EXPIRED            
         BO    VUSPW12C                                                         
         TM    PWDSTAT,PSTBUMP     ERROR EXIT IF HAVE BUMPED COUNT              
         BO    VUSPINV                                                          
         B     VALUSPWX            OK EXIT IF WE HAVE RESET COUNT               
                                                                                
***********************************************************************         
* PASSWORD ERROR EXITS                                                *         
***********************************************************************         
VUSPPUS  LA    R0,SE$PPRE          PASSWORD INVALID DUE TO PREVIOUS USE         
         B     VUSPWERR                                                         
VUSPPDR  LA    R0,SE$PDRS          PASSWORD RESERVED FOR DDS                    
         B     VUSPWERR                                                         
VUSPPTL  LA    R0,SE$PTL           PASSWORD TOO LONG                            
         B     VUSPWERR                                                         
VUSPPTS  LA    R0,SE$PTS           PASSWORD TOO SHORT                           
         B     VUSPWERR                                                         
VUSPINV  LA    R0,SE$INPAS         PASSWORD INVALID                             
         B     VALUSPER                                                         
VUSPMAX  LA    R0,SE$INPAS         PASSWORD MAXIMUM ATTEMPTS EXCEEDED           
         B     VALUSPER                                                         
VUSPNSM  LA    R0,SE$NSME          PASSWORD NOT SAME                            
         B     VUSPWERR                                                         
VUSPEXP  LA    R0,SE$EXPR          PASSWORD EXPIRED                             
         B     VUSPWERR                                                         
VUSPREN  LA    R0,SI$REEN          RE-ENTER NEW PASSWORD                        
         B     VUSPWINF                                                         
*                                                                               
VUSPWERR MVI   BYTE,GTMERR         ERROR MESSAGE                                
         B     VUSPWMSG                                                         
VUSPWINF MVI   BYTE,GTMINF         INFO MESSAGE                                 
         B     VUSPWMSG                                                         
*                                                                               
VUSPWMSG MVC   TUSER,USERNO        NEED THESE VALUES TO STAY THE SAME           
         MVC   TPERSON,PIDNUM      DURING PASSWORD VALIDATION/CHANGE            
*                                                                               
         LA    R1,GTB              POINT TO GETTXT BLOCK                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FNDX         SET FIELD NUMBER                             
         STCM  R0,3,GTMSGNO        SET ERROR NUMBER                             
         MVI   GTMAXL,60           SET L'MESSAGE AREA                           
         LA    RF,FLD                                                           
         STCM  RF,7,GTAOUT         SET A(MESSAGE AREA)                          
         MVC   GTMTYP,BYTE         SET MESSAGE TYPE                             
         OI    GT1INDS,GT1OWRK     SET T'MESSAGE AREA TO WORK                   
         MVI   GTMSYS,1            SET SERVICE SYSTEM MESSAGES                  
         GOTO1 VGETTXT,(R1)                                                     
         DROP  R1                                                               
*                                                                               
         XC    CNTPWD,CNTPWD       CLEAR PASSWORD FIELD                         
         MVI   CNTPWDH+5,0                                                      
         OI    CNTPWDH+6,X'80'                                                  
*                                                                               
         MVC   CNTMSG,FLD          MOVE ERROR MESSAGE TO FIRST FIELD            
         LA    R1,CNTPWDH                                                       
         ST    R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD IN ERROR                 
*                                                                               
         L     RD,SAVERD                                                        
         XMOD1 1                                                                
*                                                                               
VALUSPWX SR    RC,RC               EXIT CC .EQ.                                 
VALUSPER LTR   RC,RC               EXIT CC .NE.                                 
         XIT1                                                                   
                                                                                
***********************************************************************         
* SUBROUTINES TO READ/WRITE AND PROCESS PERSON AND PASSWORD RECORDS   *         
***********************************************************************         
VUSPWSWA LR    R0,RE                                                            
         ICM   RF,15,=XL4'0AFFFFFF'                                             
         GOTO1 VSWITCH,DMCB,(RF),0 SWITCH TO CONTROL SYSTEM                     
         LR    RE,R0                                                            
         CLI   4(R1),0                                                          
         BER   RE                                                               
         DC    H'0'                                                             
*                                                                               
VUSPWSW1 LR    R0,RE                                                            
         ICM   RF,15,=XL4'01FFFFFF'                                             
         GOTO1 VSWITCH,DMCB,(RF),0 SWITCH TO SERVICE SYSTEM                     
         LR    RE,R0                                                            
         CLI   4(R1),0                                                          
         BER   RE                                                               
         DC    H'0'                                                             
*                                                                               
VUSPRD   LR    R0,RE               READ PASSWORD NUM RECORD FOR UPDATE          
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMREAD '),=C'CTFILE ',ASECREC,  *        
               ASECREC                                                          
         LR    RE,R0                                                            
         CLI   8(R1),0                                                          
         BER   RE                                                               
         DC    H'0'                CONTROL FILE READ PROBLEM                    
*                                                                               
VUSPWT   LR    R0,RE               WRITE BACK UPDATED PASSWORD NUM REC          
         MVC   RVPID#,TPERSON                                                   
         MVC   RVUSER,TUSER                                                     
         MVC   RVAGYB,TAGYB                                                     
         MVC   RVAGYSEC,TAGYSEC                                                 
         MVC   TPERSON,PIDNUM      SET FOR RECOVERY ON PASSWORD CHANGE          
         MVC   TUSER,USERNO                                                     
         MVC   TAGYSEC,AGYSEC                                                   
*                                                                               
         USING CTSYSD,RE                                                        
         L     RE,AUIDSYS                                                       
         MVC   TAGYB,CTSYSAGB                                                   
         DROP  RE                                                               
*                                                                               
         IC    RF,ACTN                                                          
         GOTO1 VDATAMGR,DMCB,((RF),=C'DMWRT'),=C'CTFILE ',ASECREC,     *        
               ASECREC                                                          
         MVC   TPERSON,RVPID#                                                   
         MVC   TUSER,RVUSER                                                     
         MVC   TAGYSEC,RVAGYSEC                                                 
         MVC   TAGYB,RVAGYB                                                     
         LR    RE,R0                                                            
         CLI   8(R1),0                                                          
         BER   RE                                                               
         DC    H'0'                CONTROL FILE WRITE PROBLEM                   
*                                                                               
VUSPDEL  LR    R0,RE               DELETE ELEMENT DESCRIBED IN SAELEM           
         GOTO1 VHELLO,DMCB,(C'D',CTFBIG),('SAPWHELQ',ASECREC),         *        
               (L'SAPWHDTE+1+L'SAPWHTME,SAELEM)                                 
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
*                                                                               
VUSPGET  LR    R0,RE               GET ELEMENT DESCRIBED IN SAELEM              
         GOTO1 VHELLO,DMCB,(C'G',CTFBIG),('SAPWHELQ',ASECREC),         *        
               (L'SAPWHDTE,SAELEM)                                              
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BER   RE                                                               
         DC    H'0'                                                             
*                                                                               
VUSPRDP  NTR1                      READ PERSON RECORD FOR UPDATE                
         L     R3,APERREC                                                       
         USING SAPEREC,R3                                                       
         XC    SAPEKEY,SAPEKEY     BUILD PERSON KEY                             
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,PIDSEC                                                   
         MVC   SAPEPID,PERSONID                                                 
         MVC   SAPEDEF,=X'FFFF'                                                 
         XC    SAPEDEF,TODAYB                                                   
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMRDHI '),=C'CTFILE ',APERREC,  *        
               APERREC                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CONTROL FILE READ PROBLEM                    
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BE    *+6                                                              
         DC    H'0'                PERSON RECORD MISSING                        
         XIT1                                                                   
*                                                                               
VUSPWTP  LR    R0,RE               WRITE UPDATED PERSON RECORD BACK             
         MVC   RVPID#,TPERSON                                                   
         MVC   RVUSER,TUSER                                                     
         MVC   RVAGYB,TAGYB                                                     
         MVC   RVAGYSEC,TAGYSEC                                                 
         MVC   TPERSON,PIDNUM      SET FOR RECOVERY ON PASSWORD CHANGE          
         MVC   TUSER,USERNO                                                     
         MVC   TAGYSEC,AGYSEC                                                   
*                                                                               
         USING CTSYSD,RE                                                        
         L     RE,AUIDSYS                                                       
         MVC   TAGYB,CTSYSAGB                                                   
         DROP  RE                                                               
*                                                                               
         IC    RF,ACTN                                                          
         GOTO1 VDATAMGR,DMCB,((RF),=C'DMWRT'),=C'CTFILE ',APERREC,     *        
               APERREC                                                          
         MVC   TPERSON,RVPID#                                                   
         MVC   TUSER,RVUSER                                                     
         MVC   TAGYSEC,RVAGYSEC                                                 
         MVC   TAGYB,RVAGYB                                                     
         LR    RE,R0                                                            
         CLI   8(R1),0                                                          
         BER   RE                                                               
         DC    H'0'                CONTROL FILE WRITE PROBLEM                   
*                                                                               
VUSPUPP  LR    R0,RE               UPDATE PASSWORD POINTER ELEMENT              
         GOTO1 VHELLO,DMCB,(C'G',CTFBIG),('SAPWDELQ',APERREC),0                 
         CLI   12(R1),0                                                         
         JNE   *+2                 PASSWORD POINTER ELEMENT MISSING             
         ICM   RE,7,13(R1)         CHANGE PASSWORD CODE TO NEW VALUE            
         MVC   SAPWDCOD-SAPWDD(L'SAPWDCOD,RE),SECCODE                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SE$INPAS EQU   34                  INVALID PASSWORD                             
SE$NSME  EQU   128                 NOT SAME AS BEFORE                           
SE$INVPP EQU   131                 INVALID PERSONAL PASSWORDD                   
SE$EXPR  EQU   318                 EXPIRED PASSWORD                             
SE$PTL   EQU   303                 PASSWORD TOO LONG                            
SE$PTS   EQU   304                 PASSWORD TOO SHORT                           
SE$PPRE  EQU   305                 PASSWORD INVALID DUE TO PREVIOUS USE         
SI$REEN  EQU   307                 RE-ENTER NEW PASSWORD                        
SE$PDRS  EQU   308                 PASSWORD RESERVED                            
*                                                                               
CTFBIG   DC    CL8'CTFBIG'                                                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK PASSWORD EXPIRY WARNING TIME                                  *         
***********************************************************************         
PWDWARN  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ASECREC                                                       
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY       BUILD AUTH(ALPHA) KEY                        
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,PIDSEC                                                   
         MVC   SA0KNUM,PIDNUM                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',(R4),(R4)                  
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         OI    PWDSTAT,PSTEXP      TREAT ERROR AS EXPIRED PASSWORD              
         B     PWARYES                                                          
*                                                                               
         LA    R4,SA0DATA                                                       
         USING SAPWHD,R4                                                        
LASTEQU  USING SAPWHD,R3                                                        
         XR    R3,R3                                                            
         XR    RF,RF                                                            
*                                                                               
PWAR010  CLI   SAPWHEL,0           RECORD END                                   
         BE    PWAR030                                                          
         CLI   SAPWHEL,SAPWHELQ                                                 
         BNE   PWAR020                                                          
         CLC   SAPWHDTE,TODAYB     EFFECTIVE AFTER TODAY?                       
         BH    PWAR020                                                          
         LTR   R3,R3               FIRST TIME?                                  
         BNZ   *+6                                                              
         LR    R3,R4                                                            
         CLC   LASTEQU.SAPWHDTE,SAPWHDTE                                        
         BH    PWAR020                                                          
         BL    *+14                                                             
         CLC   LASTEQU.SAPWHTME,SAPWHTME                                        
         BH    *+6                                                              
         LR    R3,R4               R3=MOST RECENT PASSWORD                      
*                                                                               
PWAR020  IC    RF,SAPWHLN                                                       
         AR    R4,RF                                                            
         B     PWAR010                                                          
         DROP  LASTEQU                                                          
*                                                                               
PWAR030  LTR   R4,R3               TEST IF PASSWORD SET UP OK                   
         BNZ   PWAR040                                                          
         OI    PWDSTAT,PSTEXP      SET PASSWORD HAS EXPIRED                     
         B     PWARYES                                                          
*                                                                               
PWAR040  XR    R0,R0               TEST IF PASSWORD EXPIRATION DEFINED          
         ICM   R0,1,PEXPIRY                                                     
         BZ    PWARNO                                                           
         TM    SOXFL,SOXSCRP       DONT TEST PSWD EXPIRY FOR SCRIPTS            
         BO    PWARNO                                                           
         GOTO1 VDATCON,DMCB,(2,SAPWHDTE),(0,DUB)                                
         GOTO1 VADDAY,DMCB,(C'D',DUB),PWDEXDAT,(R0)                             
         CLC   TODAY,PWDEXDAT                                                   
         BL    PWAR050                                                          
         TM    TTEST,TTESTNOU      TEST IF UPDATES INHIBITED                    
         BO    PWAR050                                                          
         OI    PWDSTAT,PSTEXP      SET PASSWORD HAS EXPIRED                     
         B     PWARYES                                                          
*                                                                               
PWAR050  GOTO1 VPERVERT,DMCB,TODAY,PWDEXDAT                                     
         SR    RF,RF                                                            
         ICM   RF,3,DMCB+8                                                      
         AHI   RF,-1               GET NUMBER OF DAYS TO EXPIRATION             
         BNP   PWARNO                                                           
         CHI   RF,255                                                           
         BNH   *+8                                                              
         LHI   RF,255                                                           
         STC   RF,PEXPDAYS         SAVE NUMBER OF DAYS TO EXPIRATION            
*                                                                               
         XR    RE,RE               TEST IF PASSWORD WARNING DEFINED             
         ICM   RE,1,PEXPWARN                                                    
         BZ    PWARNO                                                           
         SR    R0,RE                                                            
         BNP   PWARNO                                                           
         GOTO1 VADDAY,DMCB,(C'D',DUB),DUB1,(R0)                                 
         CLC   TODAY,DUB1          TEST IF WITHIN WARNING PERIOD                
         BL    PWARNO                                                           
         OI    PWDSTAT,PSTEXPW     SET PASSWORD EXPIRATION WARNING              
         B     PWARYES                                                          
*                                                                               
PWARYES  SR    RC,RC               WARNING REQUIRED OR EXPIRED                  
PWARNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REBULD ORIGINAL CONNECT SCREEN AFTER VALSRV                         *         
***********************************************************************         
REBUILD  NTR1  BASE=*,LABEL=*                                                   
         MVC   USERNO,TUSER        BUILD ORIGINAL CONNECT SCREEN                
         MVC   PIDNUM,TPERSON                                                   
         MVC   TOUTCON,TTIMEOUT    SAVE LAST CONNECTED TIMEOUT                  
         MVC   AGYCTRY,TAGCTRY                                                  
         XC    PASSINPT,PASSINPT                                                
         XC    USERID,USERID                                                    
         XC    USERALPH,USERALPH                                                
         MVI   DDSACC,0                                                         
         MVI   AGYOPTS,0                                                        
         XC    MEDAGY,MEDAGY                                                    
         TM    TFLAG,TFLAGSEC      SET SECRET CODE OR PASSWORD NUMBER           
         BZ    *+14                                                             
         MVC   SECNO,TPASSWD                                                    
         B     *+10                                                             
         MVC   PASSNO,TPASSWD                                                   
         MVC   PIDSEC,TAGYPER      SET PERSON SECURITY AGENCY                   
*                                                                               
         L     R4,AUIDREC                                                       
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD ID(NUMERIC) KEY                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERNO                                               
         BAS   RE,CTREAD                                                        
         BNE   RBLDNO                                                           
         LA    R4,CTIDATA                                                       
         SR    R1,R1                                                            
RBLD010  CLI   0(R4),0             FIND ID(ALPHA) ELEMENT                       
         BE    RBLD030                                                          
         CLI   0(R4),X'02'                                                      
         BNE   *+14                                                             
         MVC   USERID,2(R4)        SAVE ALPHA ID IN W/S                         
         B     RBLD020                                                          
         CLI   0(R4),X'06'                                                      
         BNE   *+14                                                             
         MVC   USERALPH,2(R4)      SAVE AGENCY ALPHA ID                         
         B     RBLD020                                                          
         CLI   0(R4),X'07'                                                      
         BNE   RBLD020                                                          
         CLI   1(R4),3                                                          
         BL    RBLD020                                                          
         IC    R1,1(R4)                                                         
         CLI   1(R4),L'IDOPTS+2                                                 
         BNH   *+8                                                              
         LA    R1,L'IDOPTS+2                                                    
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     RBLD020                                                          
         MVC   IDOPTS(0),2(R4)     SAVE ID OPTIONS IN W/S                       
RBLD020  IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     RBLD010                                                          
         DROP  R4                                                               
*                                                                               
RBLD030  EQU    *                                                               
         MVC   AGYSEC,USERALPH                                                  
         L     R4,AACCREC          READ ACCESS RECORD                           
         USING CT5REC,R4           BUILD KEY OF ACCESS RECORD                   
         MVI   DDSACC,0            CLEAR DDS AGENCY ACCESS LEVEL FLAGS          
         MVI   AGYOPTS,0           CLEAR AGENCY OPTIONS FLAG                    
         XC    MEDAGY,MEDAGY       CLEAR UK MEDIA AGENCY ALPHA ID               
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         OC    CT5KALPH,USERALPH                                                
         BAS   RE,CTREAD           READ ACCESS RECORD                           
         BNE   RBLDNO                                                           
         LA    R4,CT5DATA                                                       
         SR    R1,R1                                                            
RBLD040  CLI   0(R4),0                                                          
         BE    RBLD050                                                          
         CLI   0(R4),CTAGDELQ      AGENCY GROUP DETAILS ELEMENT                 
         BE    RBLD044                                                          
*&&DO*&& CLI   0(R4),CTMAGELQ      UK MEDIA AGENCY ELEMENT                      
*&&DO*&& BE    RBLD046                                                          
         CLI   0(R4),CTSEAELQ      SECURITY AGENCY ELEMENT                      
         BE    RBLD048                                                          
         CLI   0(R4),CTTOUELQ                                                   
         BE    RBLD045                                                          
         CLI   0(R4),CTAADELQ                                                   
         BE    RBLD049                                                          
         CLI   0(R4),CTACCELQ      X'1C' AGENCY ACCESS DATES                    
         BE    RBLD049A                                                         
RBLD042  IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     RBLD040                                                          
*                                                                               
         USING CTAGDD,R4                                                        
RBLD044  MVC   DDSACC,CTAGDDA      SAVE DDS AGENCY ACCESS LEVEL FLAGS           
         MVC   AGYOPTS,CTAGOPTS    SAVE AGENCY OPTIONS FLAGS                    
         B     RBLD042                                                          
*                                                                               
         USING CTTOUD,R4                                                        
RBLD045  MVC   TOUTAGY,CTTOUADV    SAVE AGENCY LEVEL ADV TIME OUT               
         B     RBLD042                                                          
*&&DO                                                                           
         USING CTMAGD,R4                                                        
RBLD046  MVC   MEDAGY,CTMAGAID     SAVE UK MEDIA AGENCY ID                      
         B     RBLD042                                                          
*&&                                                                             
         USING CTSEAD,R4                                                        
RBLD048  MVC   AGYSEC,CTSEAAID     SAVE SECURITY AGENCY ALPHA ID                
         B     RBLD042                                                          
*                                                                               
         USING CTAADD,R4                                                        
RBLD049  MVC   SERVTEAM,CTAACSTN   CLIENT SERVICE TEAM                          
         TM    CTAADFLG,CTAADDAB   TEST DATA BUILD MODE                         
         BZ    RBLD042                                                          
         TM    SYSIDTY,FACITST     TEST IF THIS A TEST FACPAK                   
         BZ    RBLD042                                                          
         TM    SYSIDTY,FACIFQA     TEST IF THIS IS FQA                          
         BO    RBLD042                                                          
         OI    DATABLD,X'01'       SET DATA BUILD MODE                          
         B     RBLD042                                                          
*                                                                               
         USING CTACCD,R4                                                        
RBLD049A ST    R4,ACTACC           SAVE A(ACCESS DATE ELEMENT)                  
         B     RBLD042                                                          
*                                                                               
RBLD050  EQU   *                                                                
         OC    USERID,USERID       ERROR IF ALPHA-ID ELEMENT NOT FOUND          
         BZ    RBLDNO                                                           
         TM    TVIFLAG,TVIVIRT     NO TERMINAL IF VIRTUAL                       
         BO    RBLD058                                                          
*                                                                               
         L     R4,ATRMREC                                                       
         USING CTTREC,R4                                                        
         XC    CTTKEY,CTTKEY       BUILD TERMINAL (ALPHA/NUMERIC) KEY           
         MVI   CTTKTYP,C'T'                                                     
         OC    PASSNO,PASSNO                                                    
         BNZ   RBLD052                                                          
*                                                                               
         MVC   CTTKTID,TLUID                                                    
         BAS   RE,CTREAD                                                        
         BE    RBLD058                                                          
*                                                                               
         XC    CTTKEY,CTTKEY       BUILD GENERIC KEY                            
         MVI   CTTKTYP,C'T'                                                     
         MVC   CTTKTID,TLUID                                                    
         MVC   CTTKTID+4(4),=C'%%%T'                                            
         BAS   RE,CTREAD                                                        
         BE    RBLD058                                                          
         B     RBLDNO                                                           
*                                                                               
RBLD052  MVC   CTTKPASS+8(2),PASSNO                                             
         BAS   RE,CTREAD                                                        
         BNE   RBLDNO                                                           
         LA    R4,CTTDATA                                                       
         SR    R1,R1                                                            
*                                  READ SECRET CODE FROM AUTH RECORD            
RBLD054  CLI   0(R4),0             FIND TERMINAL(ALPHA) ELEMENT                 
         BE    RBLDNO                                                           
         CLI   0(R4),X'03'                                                      
         BE    *+14                                                             
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     RBLD054                                                          
         MVC   PASSWD,10(R4)       SAVE PASSWORD IN W/S                         
         MVC   PASSWDU,PASSWD                                                   
         DROP  R4                                                               
*                                  READ SECRET CODE FROM AUTH RECORD            
RBLD058  OC    SECNO,SECNO                                                      
         BZ    RBLD070                                                          
         L     R4,ASACREC          READ SECURITY AGENCY ACCESS RECORD           
         USING CT5REC,R4           BUILD KEY OF ACCESS RECORD                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         OC    CT5KALPH,AGYSEC                                                  
         BZ    RBLDNO                                                           
         BAS   RE,CTREAD           READ ACCESS RECORD                           
         BNE   RBLDNO                                                           
         MVI   PIDREQD,C'N'                                                     
         LA    R3,CT5DATA                                                       
         USING CTAADD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
RBLD060  CLI   CTAADEL,0                                                        
         BE    RBLD063                                                          
         CLI   CTAADEL,CTAADELQ                                                 
         BE    RBLD062                                                          
         IC    RF,CTAADLEN                                                      
         AR    R3,RF                                                            
         B     RBLD060                                                          
*                                                                               
RBLD062  TM    CTAADFLG,CTAADPRQ   TEST FOR PERSONAL ID REQUIRED                
         BZ    RBLD063                                                          
         MVI   PIDREQD,C'Y'                                                     
         OI    SOXFL2,SOXPPSAG                                                  
         B     RBLD064                                                          
RBLD063  TM    SOXFL,SOXON+SOXDDS  PID NOT REQUIRED FOR THIS AGENCY             
         BNO   RBLD064                                                          
         MVI   PIDREQD,C'Z'        SET DDS PID REQUIRED FOR THIS AGENCY         
RBLD064  BAS   RE,GETSCOD                                                       
         BNE   RBLDNO              EXIT IF NOT FOUND                            
*                                                                               
RBLD070  MVC   CNTID(L'USERID),USERID                                           
         LA    R1,CNTID+L'USERID-1                                              
         CLI   0(R1),C' '          GET L'USER-ID                                
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTID                                                         
         SR    R1,RE                                                            
         STC   R1,CNTIDH+5         SET L'USER-ID IN TWA HEADER                  
         CLI   PIDREQD,C'N'        MOVE OUT ",PERSONID" IF REQD.                
         BE    RBLD080                                                          
         MVI   PIDENTER,C'N'       SET PERSONALID ENTERED FLAG OFF              
         LA    RE,CNTID                                                         
         AR    RE,R1                                                            
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         LA    RF,PERSONID                                                      
         LA    R0,8                                                             
RBLD072  CLI   0(RF),0                                                          
         BE    RBLD074                                                          
         CLI   0(RF),C' '                                                       
         BE    RBLD074                                                          
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,RBLD072                                                       
RBLD074  LA    R1,CNTID                                                         
         SR    RE,R1                                                            
         STC   RE,CNTIDH+5         SET LEN IN TWA HEADER                        
RBLD076  TM    SOXFL,SOXDDS                                                     
         BZ    RBLD081                                                          
         TM    TSTATB,TSTATDPO     TEST IF CONNECTED WITH DDS SEC AGY           
         BZ    RBLD081                                                          
         OI    SOXFL1,SOXUSEDD                                                  
         AR    RE,R1               USERID,PERSONID,DDS                          
         MVI   0(RE),C','                                                       
         MVC   1(3,RE),=CL10'DDS'                                               
         LA    RE,4(RE)                                                         
         SR    RE,R1                                                            
         STC   RE,CNTIDH+5         SET LEN IN TWA HEADER                        
         B     RBLD081                                                          
*                                                                               
RBLD080  EQU   *                                                                
         XC    PERSONID,PERSONID   CLEAR PERSONID SET BY GETSCOD                
RBLD081  EQU   *                                                                
*                                                                               
         OC    PASSWD,PASSWD       MOVE PASSWORD/SECRET CODE WORD               
         BZ    *+14                                                             
         MVC   CNTPWD(L'PASSWD),PASSWD                                          
         B     RBLD082                                                          
         OC    SECCODE,SECCODE                                                  
         BZ    RBLD090                                                          
         MVC   CNTPWD(L'SECCODE),SECCODE                                        
         BAS   RE,DDSPWD           TEST/SET DDS PASSWORD                        
RBLD082  LA    R1,CNTPWD+L'SECCODE-1                                            
         CLI   0(R1),C' '          GET L'PASSWORD/SECRET CODE                   
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTPWD                                                        
         SR    R1,RE                                                            
         STC   R1,CNTPWDH+5        SET L'PASSWORD IN TWA HEADER                 
*                                                                               
RBLD090  L     RE,ASYSLST          FIND SYSTEM ENTRY IN SYSTAB                  
         USING SYSLSTD,RE                                                       
         CLI   XCTLTYP,0           TEST AND SET XFR CONTROL SYS/PRG             
         BE    RBLD100                                                          
         MVC   CNTSYS(3),XCTLSYS                                                
         MVI   CNTSYSH+5,3                                                      
         MVC   CNTPGM(3),XCTLPGM                                                
         MVI   CNTPGMH+5,3                                                      
         B     RBLDOK              PROCEED TO STANDARD VALIDATION               
*                                                                               
RBLD100  CLI   SYSLNUM,0           TEST END-OF-LIST                             
         BE    RBLDNO                                                           
         CLC   TOVSYS,SYSLNUM      MATCH ON OVSYS NUMBER                        
         BNE   RBLD110                                                          
         OC    OVSYSN,OVSYSN       AND SHORT NAME (IF SET)                      
         BZ    *+14                                                             
         CLC   OVSYSN,SYSLSHRT                                                  
         BNE   RBLD110                                                          
         MVC   CNTSYS(L'SYSLNAME),SYSLNAME                                      
         MVI   CNTSYSH+5,L'SYSLNAME                                             
         B     RBLDOK              PROCEED TO STANDARD VALIDATION               
RBLD110  LA    RE,SYSLLEN(RE)                                                   
         B     RBLD100                                                          
         DROP  RE                                                               
RBLDOK   SR    RC,RC                                                            
RBLDNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET AGENCY SECURITY RECORD AND READ PERSON RECORD IF REQUIRED       *         
* IF PPS AGENCY THEN READ PERSON RECORD FOR THAT AGENCY               *         
* IF NO MATCH AND A DDS TERMINAL READ PERSON RECORD FOR DDS AGENCY    *         
***********************************************************************         
CHKSAC   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRNUM,0            SET ALL OK                                   
         MVI   FNDX,1                                                           
         MVI   PIDREQD,C'N'        SET PERSONAL ID NOT REQUIRED                 
         XC    PIDDATA,PIDDATA                                                  
         MVC   PIDSEC,AGYSEC       SET AGENCY PID TO READ PERSON                
         MVI   PONCE,C'N'          SET PERSON ID ONCE ONLY                      
*                                                                               
CSAC1    L     R4,ASACREC          READ SECURITY AGENCY ACCESS RECORD           
         USING CT5REC,R4           BUILD KEY OF ACCESS RECORD                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         OC    CT5KALPH,PIDSEC                                                  
         BZ    CSACEIID                                                         
         BAS   RE,CTREAD           READ ACCESS RECORD                           
         BNE   CSACEIID                                                         
         LA    R3,CT5DATA                                                       
         USING CTAADD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
CSAC2    CLI   CTAADEL,0           FIND AGENCY ACCESS DETAILS ELEMENT           
         BE    CSAC4                                                            
         CLI   CTAADEL,CTAADELQ                                                 
         BE    CSAC2A                                                           
         IC    RF,CTAADLEN                                                      
         AR    R3,RF                                                            
         B     CSAC2                                                            
CSAC2A   TM    CTAADFLG,CTAADCON   TEST FOR USE ONCE ONLY FLAG                  
         BZ    *+8                                                              
         MVI   PONCE,C'Y'                                                       
         TM    CTAADFLG,CTAADPRQ   TEST FOR PERSONAL ID REQUIRED                
         BZ    CSAC4                                                            
*                                                                               
CSAC3    MVI   PIDREQD,C'Y'        PID REQUIRED FOR THIS AGENCY                 
         OI    SOXFL2,SOXPPSAG                                                  
         MVC   PEXPIRY,CTAADPTO    PASSWORD TIMEOUT PERIOD FOR ADDAY            
         MVC   PEXPWARN,CTAADPTW   PASSWORD TIMEOUT WARNING PERIOD              
         MVC   PREUSE,CTAADPRU     REUSE COUNT                                  
         MVC   PMINLEN,CTAADPML    MINIMUM INPUT LENGTH                         
         MVC   PRULENUM,CTAADPVR   VALIDATION RULE NUMBER                       
         MVC   PMAXERR,CTAADPME    MAXIMUM NUMBER OF PASSWORD ERRORS            
         CLI   PMAXERR,0                                                        
         BNE   *+8                                                              
         MVI   PMAXERR,MPWDERR     SET DEFAULT IF NOT DEFINED                   
         B     CSAC5                                                            
*                                                                               
CSAC4    TM    SOXFL,SOXON+SOXDDS  PID NOT REQUIRED FOR THIS AGENCY             
         BNO   CSAC5                                                            
         MVI   PIDREQD,C'Z'        SET DDS PID REQUIRED FOR THIS AGENCY         
*                                                                               
CSAC5    MVI   FNDX,2              VALIDATE PERSON ID                           
         CLI   PERSONID,0          TEST PERSON ID INPUT                         
         BNE   CSAC20              YES                                          
         CLI   PIDREQD,C'N'        TEST PERSON ID REQUIRED                      
         BNE   CSAC7                                                            
*                                                                               
CSAC6    EQU   *                   PID NOT INPUT AND PID NOT REQUIRED           
         TM    SOXFL,SOXDDS                                                     
         BZ    CSACX               OK IF NOT DDS TERMINAL                       
         TM    SOXFL,SOXON                                                      
         BZ    CSACX               OK IF SOX INACTIVE                           
         TM    SOXFL,SOXTST                                                     
         BZ    CSACEMPI            MUST INPUT PID ON PROD SYSTEM                
         B     CSACX                                                            
*                                                                               
CSAC7    EQU   *                   PID NOT INPUT AND PID REQUIRED               
*                                                                               
CSAC8    CLI   AUTOCON,C'Y'        TEST IF AUTO CONNECT                         
         BE    CSACX                                                            
*                                                                               
CSAC9    TM    SOXFL,SOXSCRP       TEST IF SCRIPT                               
         BZ    CSAC10                                                           
         BAS   RE,CSACSS                                                        
         BNE   CSACX                                                            
         OI    SOXFL1,SOXDDPSC     SET SCRIPT PERSON/PASSWORD SET               
         B     CSAC20                                                           
*                                                                               
CSAC10   TM    TTYPE,TTYPEWAP      TEST IF SPECIAL WEB APPL                     
         BZ    CSAC11                                                           
         BAS   RE,CSACSD                                                        
         BNE   CSACX                                                            
         OI    SOXFL1,SOXDDPST     DDS/DDS SET FOR A SPECIAL TERMINAL           
         B     CSAC20                                                           
*                                                                               
CSAC11   CLI   CNTSYS,C'C'         TEST IF CON/MAD                              
         BNE   CSAC12                                                           
         CLI   CNTPGM,C'M'                                                      
         BNE   CSAC12                                                           
         BAS   RE,CSACSS                                                        
         BNE   CSACX                                                            
         OI    SOXFL1,SOXDDPCM     SET CON/MAD PERSON/PASSWORD SET              
         B     CSAC20                                                           
*                                                                               
CSAC12   TM    SOXFL,SOXDDS        ERROR IF CLIENT TERMINAL                     
         BZ    CSACEMPI                                                         
         TM    SOXFL,SOXTST        ERROR IF PROD ADV                            
         BZ    CSACEMPI                                                         
         BAS   RE,CSACSD           OK IF TEST ADV                               
         BNE   CSACX                                                            
         OI    SOXFL1,SOXDDPTS     DDS/DDS SET FOR A TEST SYSTEM                
         B     CSAC20                                                           
*                                                                               
CSAC20   DS    0H                                                               
*NOP*    CLC   PERSONID,=CL10'DDS' TEST IF DDS PID INPUT                        
*NOP*    BNE   CSAC20A                                                          
CSAC20A  TM    SOXFL1,SOXUSEDD     TEST IF DDS TERM WANTS DDS SEC               
         BO    CSAC21                                                           
         BAS   RE,READPID          READ PID REC FOR AGY PIDSEC                  
         BNE   CSAC20B                                                          
         CLI   PIDREQD,C'Z'        TEST IF REQUIRED BECAUSE DDS TERM            
         BNE   *+8                                                              
         MVI   PIDREQD,C'N'        YES-RESET PID REQUIRED FLAG                  
         CLC   PIDSEC,DDSSEC                                                    
         BNE   CSACX                                                            
         OI    SOXFL1,SOXAGDDS     SET FLAG TO SHOW DDS PERSON                  
         OI    SOXFL2,SOXDDSPE                                                  
         B     CSACX                                                            
*                                                                               
CSAC20B  DS    0H                                                               
*&&US                                                                           
         TM    TSTAT1,TSTATWEB     TEST WEB TERMINAL                            
         BZ    CSAC20C                                                          
         CLI   CNTSYS,C'N'         TEST NETWORK SYSTEM                          
         BNE   CSAC20C                                                          
         CLC   CNTPGM(2),=C'NN'    TEST NETWORK NAVIGATOR PROGRAM               
         BNE   CSAC20C                                                          
         CLC   PERSONID,=CL10'PRSMTVCV'                                         
         BNE   CSAC20C             NOT SPECIAL PID                              
         OI    PASSDDS,PASSWSPD    SET SPECIAL PID IN USE                       
         B     CSAC21                                                           
*&&                                                                             
CSAC20C  TM    SOXFL,SOXDDS        PID MUST EXIST IF CLIENT TERM                
         BZ    CSACEIPI                                                         
         TM    SOXFL,SOXON         PID MUST EXIST IF SOX INACTIVE               
         BZ    CSACEIPI                                                         
         CLC   PIDSEC,DDSSEC       PID MUST EXIST IF DDS SECURITY AGY           
         BE    CSACEIPI                                                         
*                                                                               
CSAC21   MVC   PIDSEC,DDSSEC       SET DDS SECURITY AGENCY                      
         BAS   RE,READPID          READ PID REC FOR DDS SECURITY AGY            
         BNE   CSACEIPI                                                         
         TM    PASSDDS,PASSWSPD    WEB SPECIAL PID/PASSWORD                     
         BZ    CSAC21C                                                          
         OI    READONLY,X'01'      SET CALLER INTO READ-ONLY MODE               
         OI    TSTATD,TSTATWSP     SET WEB TERM SPECIAL PID/PASSWORD            
         B     CSAC22                                                           
*                                                                               
CSAC21C  OI    SOXFL1,SOXAGDDS     SET FLAG TO SHOW DDS PERSON ORIDE            
         OI    SOXFL2,SOXDDSPE+SOXDDSOV                                         
         OI    PASSDDS,PASSDPID    SET FLAG TO SHOW A DDS PID                   
*&&US*&& CLC   PERSONID,=C'DDSSDARE'                                            
*&&UK*&& CLC   PERSONID,=C'SCR1DDLO'                                            
         BNE   CSAC22                                                           
         OI    PASSDDS,PASSSPID    SPECIAL PID                                  
*                                                                               
CSAC22   L     R4,ATRMREC          READ DDS SEC AGY REC INTO TRM REC            
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         OC    CT5KALPH,PIDSEC                                                  
         BZ    CSACEIID                                                         
         BAS   RE,CTREAD           READ DDS ACCESS RECORD                       
         BNE   CSACEIID                                                         
         LA    R3,CT5DATA                                                       
         USING CTAADD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
CSAC23   CLI   CTAADEL,0           FIND AGENCY ACCESS DETAILS ELEMENT           
         BE    CSACEIID                                                         
         CLI   CTAADEL,CTAADELQ                                                 
         BE    CSAC23A                                                          
         IC    RF,CTAADLEN                                                      
         AR    R3,RF                                                            
         B     CSAC23                                                           
CSAC23A  MVI   PONCE,C'N'                                                       
         TM    CTAADFLG,CTAADCON   TEST FOR USE ONCE ONLY FLAG                  
         BZ    *+8                                                              
         MVI   PONCE,C'Y'                                                       
         TM    CTAADFLG,CTAADPRQ   TEST FOR PERSONAL ID REQUIRED                
         BZ    CSACEIID                                                         
*                                                                               
CSAC24   MVC   PEXPIRY,CTAADPTO    PASSWORD TIMEOUT PERIOD FOR ADDAY            
         MVC   PEXPWARN,CTAADPTW   PASSWORD TIMEOUT WARNING PERIOD              
         MVC   PREUSE,CTAADPRU     REUSE COUNT                                  
         MVC   PMINLEN,CTAADPML    MINIMUM INPUT LENGTH                         
         MVC   PRULENUM,CTAADPVR   VALIDATION RULE NUMBER                       
         MVC   PMAXERR,CTAADPME    MAXIMUM NUMBER OF PASSWORD ERRORS            
         CLI   PMAXERR,0                                                        
         BNE   *+8                                                              
         MVI   PMAXERR,MPWDERR     SET DEFAULT IF NOT DEFINED                   
         B     CSACX                                                            
*                                                                               
CSACSD   MVC   DUB,=CL10'DDS'      SET DDS TERM PERSON/PASSWORD                 
         MVC   DUB1,=CL10'DDS'                                                  
         LA    R1,3                                                             
         B     CSACSS1                                                          
CSACSS   EQU   *                   SET SCRIPT PERSON/PASSWORD                   
*&&UK*&& MVC   DUB,=CL10'SCR1DDLO'                                              
*&&UK*&& MVC   DUB1,=CL10'SCR1DDLO'                                             
*&&UK*&& LA    R1,8                                                             
*&&US*&& MVC   DUB,=CL10'DDSSDARE'                                              
*&&US*&& MVC   DUB1,=CL10'SPOTDARE'                                             
*&&US*&& LA    R1,8                                                             
*                                                                               
         OI    PASSFLAG,PFSCRIPT   INDICATE SCRIPT PID/PW SET                   
*                                                                               
CSACSS1  CLI   PIDREQD,C'Y'        PPS PASSWORD GETS RESET                      
         BE    CSACSS2                                                          
         CLI   CNTPWDH+5,3         DDS PASSWORD GETS RESET                      
         BNE   *+14                                                             
         CLC   CNTPWD(3),=CL10'DDS'                                             
         BE    CSACSS2                                                          
         CLI   CNTPWDH+5,0         MISSING PASSWORD GETS RESET                  
         BE    CSACSS2                                                          
         CLI   PIDREQD,C'Z'        TEST IF REQUIRED BECAUSE DDS TERM            
         BNE   *+8                                                              
         MVI   PIDREQD,C'N'        YES-RESET PID REQUIRED FLAG                  
         CR    RB,RE                                                            
         BR    RE                  EXIT WITH CC NEQ IF PASSWORD OK              
*                                                                               
CSACSS2  MVC   PERSONID,DUB        SET PERSON ID AND PASSWORD                   
         XC    CNTPWD,CNTPWD                                                    
         STC   R1,CNTPWDH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CNTPWD(0),DUB1                                                   
         CR    RE,RE               EXIT WITH CC EQL IF SET PID/PWD              
         BR    RE                                                               
*                                                                               
CSACEIID MVI   ERRNUM,1            INVALID USERID                               
         B     CSACX                                                            
CSACEMPI MVI   ERRNUM,2            MISSING PERSON ID                            
         B     CSACX                                                            
CSACEIPI MVI   ERRNUM,3            INVALID PERSON ID                            
         B     CSACX                                                            
*                                                                               
CSACX    CLI   ERRNUM,0            EXIT WITH CC EQL IF ALL OK                   
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE PASSWORD NUMBER OR PERSON ID IS CONNECTED ONLY ONCE        *         
* RETURN CC .NE. IF INVALID                                           *         
***********************************************************************         
VALPONCE NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
         L     R3,VUTL                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         DROP  R7                                                               
         USING UTLD,R3                                                          
         L     R0,TTIMETU-UTLD(R7) GET TIME NOW FOR THIS TERMINAL               
*                                                                               
VPON1    CLI   PIDREQD,C'N'        TEST SAME PERSON CONNECTED                   
         BNE   VPON3                                                            
*                                                                               
VPON2    CLC   TPASSWD,SECNO       TEST SAME PASSWORD CONNECTED                 
         BNE   VPON2A                                                           
         BAS   RE,VPONTO           CHECK IF CONNECT HAS TIMED OUT               
         BE    VPONNO              MATCH                                        
VPON2A   BXLE  R3,R4,VPON2                                                      
         B     VPONOK              NO MATCH                                     
*                                                                               
VPON3    CLC   TAGYPER,PIDSEC      TEST SAME PERSON CONNECTED                   
         BNE   VPON3A                                                           
         CLC   TPERSON,PIDNUM                                                   
         BNE   VPON3A                                                           
         BAS   RE,VPONTO           CHECK IF CONNECT HAS TIMED OUT               
         BE    VPONNO              MATCH                                        
VPON3A   BXLE  R3,R4,VPON3                                                      
         B     VPONOK              NO MATCH                                     
*                                                                               
* ROUTINE WITHIN VALPONCE                                                       
*                                                                               
VPONTO   ST    RE,SAVERE           COMPUTE TIME OUT FOR TERMINAL                
         ICM   R1,15,TTIME         R1=TIME OF LAST TRANSACTION IN TUS           
         BZ    VPONTOX                                                          
         CLR   R0,R1               R0=TIME NOW FOR THIS TERM                    
         BNL   *+8                                                              
         AL    R0,=X'C5C10000'     ADJUST FOR MIDNIGHT (24 HRS IN TUS)          
         SLR   R0,R1               R0=TIME SINCE LAST TRANSACTION               
         SR    RF,RF                                                            
         ICM   RF,1,TTIMEOUT       RF=TERMINAL TIME OUT IN 5 MIN UNITS          
         BZ    VPONTOX                                                          
         CHI   RF,255              MAX VALUE MEANS NO TIME OUT                  
         BE    VPONTOX                                                          
         M     RE,=A(5*60*38400)   RF=MAXIMUM TIME BEFORE TIME OUT              
         CLR   R0,RF                                                            
         BNH   VPONTOX                                                          
         LHI   RE,1                CC .NE. TERMINAL HAS TIMED OUT               
         B     *+8                                                              
VPONTOX  LHI   RE,0                CC .EQ. TERMINAL STILL CONNECTED             
         LTR   RE,RE                                                            
         L     RE,SAVERE                                                        
         BSM   0,RE                RETURN IN MODE CAME FROM                     
         DROP  R3                                                               
*                                                                               
         USING UTLD,R7                                                          
VPONOK   SR    RC,RC               EXIT CC .EQ.                                 
VPONNO   LTR   RC,RC               EXIT CC .NE.                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCAN USER ID FIELD AT FLDH FOR USER ID NUMBER FOR SCRIPT IO         *         
* RETURN USER ID IN FIELD IF VALID INPUT                              *         
***********************************************************************         
SCRIPUID NTR1  BASE=*,LABEL=*                                                   
         CLI   FLDH+5,0            EXIT IF NULL INPUT                           
         BE    SCRUOK                                                           
         CLI   FLD,C'#'            CHECK FOR NUMERIC ENTRY FOR SCRIPTS          
         BNE   SCRUOK                                                           
         TM    TSTAT6,TST6SCRP     TEST SCRIPT BIT SET IN UTL                   
         BZ    SCRUNO              ELSE EXIT WITH ERROR                         
         LLC   R1,FLDH+5                                                        
         AHI   R1,-2               R1=L'NUMBER-1                                
         BM    SCRUNO                                                           
         MVC   DUB,=8C'0'          FIELD MUST BE NUMERIC                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),FLD+1                                                     
         CLC   DUB,=8C'0'                                                       
         BNE   SCRUNO                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD+1(0)                                                     
         CVB   R1,DUB                                                           
         LTR   R1,R1               TEST NUMBER IS ZERO                          
         BNZ   SCRU010                                                          
         ICM   R1,3,TUSER          IF SO USE EXISTING UTL ENTRY VALUE           
         BZ    SCRUNO                                                           
*                                                                               
SCRU010  L     R4,AUIDREC                                                       
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD ID(NUMERIC) KEY                        
         MVI   CTIKTYP,C'I'                                                     
         STCM  R1,3,CTIKID+8       STORE ID NUMBER IN KEY                       
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',(R4),(R4)              
         CLI   8(R1),0                                                          
         BNE   SCRUNO                                                           
         LA    R4,CTIDATA                                                       
         SR    R1,R1                                                            
SCRU020  CLI   0(R4),0             FIND ID(ALPHA) ELEMENT                       
         BE    SCRUNO                                                           
         CLI   0(R4),X'02'                                                      
         BNE   *+14                                                             
         MVC   CNTID(L'USERID),2(R4)  MOVE USER ID INTO SCREEN FIELD            
         B     SCRU030                                                          
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     SCRU020                                                          
         DROP  R4                                                               
*                                                                               
SCRU030  LA    R1,CNTID+L'USERID-1                                              
         CLI   0(R1),C' '          GET L'USER-ID                                
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTID                                                         
         SR    R1,RE                                                            
         STC   R1,CNTIDH+5         SET L'USER-ID IN TWA HEADER                  
         GOTO1 FVAL,CNTIDH         RELOAD FLDH                                  
         BZ    SCRUNO                                                           
         B     SCRUOK                                                           
*                                                                               
SCRUOK   SR    RC,RC               EXIT CC .EQ.                                 
SCRUNO   LTR   RC,RC               EXIT CC .NE.                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCAN PASSWORD FIELD AT FLDH FOR PASSWORD NUMBER FOR SCRIPT PASSWORD *         
* RETURN PASSWORD IN FIELD IF VALID INPUT                             *         
***********************************************************************         
SCRIPPWD NTR1  BASE=*,LABEL=*                                                   
         CLI   FLDH+5,0            EXIT IF NULL INPUT                           
         BE    SCRPOK                                                           
         CLI   FLD,C'#'            CHECK FOR NUMERIC ENTRY FOR SCRIPTS          
         BNE   SCRPOK                                                           
         TM    TSTAT6,TST6SCRP     TEST SCRIPT BIT SET IN UTL                   
         BZ    SCRPNO              ELSE EXIT WITH ERROR                         
         LLC   R1,FLDH+5                                                        
         AHI   R1,-2               R1=L'NUMBER-1                                
         BM    SCRPNO                                                           
         MVC   DUB,=8C'0'          FIELD MUST BE NUMERIC                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),FLD+1                                                     
         CLC   DUB,=8C'0'                                                       
         BNE   SCRPNO                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD+1(0)                                                     
         CVB   R1,DUB                                                           
         LTR   R1,R1               TEST NUMBER IS ZERO                          
         BNZ   SCRP010                                                          
         ICM   R1,3,TPASSWD        IF SO USE EXISTING UTL ENTRY VALUE           
         BZ    SCRPNO                                                           
*                                                                               
SCRP010  L     R4,ASECREC                                                       
         USING CT0REC,R4                                                        
         XC    CT0KEY,CT0KEY       BUILD PASSWORD AUTH(NUMERIC) KEY             
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,AGYSEC                                                   
         STCM  R1,3,CT0KNUM        STORE PASSWORD NUMBER IN KEY                 
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',(R4),(R4)              
         CLI   8(R1),0                                                          
         BNE   SCRPNO                                                           
         LA    R4,CT0DATA                                                       
         SR    R1,R1                                                            
SCRP020  CLI   0(R4),0             FIND ID(ALPHA) ELEMENT                       
         BE    SCRPNO                                                           
         CLC   0(2,R4),=X'030C'                                                 
         BNE   *+14                                                             
         MVC   CNTPWD(L'SECCODE),2(R4) LOAD FIELD WITH PASSWORD CODE            
         B     SCRP030                                                          
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     SCRP020                                                          
         DROP  R4                                                               
*                                                                               
SCRP030  LA    R1,CNTPWD+L'SECCODE-1                                            
         CLI   0(R1),C' '          GET L'PASSWORD CODE                          
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RE,CNTPWD                                                        
         SR    R1,RE                                                            
         STC   R1,CNTPWDH+5        SET L'PASSWORD IN TWA HEADER                 
         GOTO1 FVAL,CNTPWDH        RELOAD FLDH                                  
         B     SCRPOK                                                           
*                                                                               
SCRPOK   SR    RC,RC               EXIT CC .EQ.                                 
SCRPNO   LTR   RC,RC               EXIT CC .NE.                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*NOTE THAT BIAS ANS DARE BOTH USE R2 (MAIN BASE REGISTER) SO LEAVE    *         
*THESE DUDES AS THE LAST BASE=* TYPE SUBROUTINES                      *         
***********************************************************************         
*&&US                                                                           
BIAS     NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D90110'                                             
         MVI   DMCB+7,X'FF'                                                     
         GOTO1 VCALLOV,DMCB,CNTMSGH                                             
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
*                                                                               
         LA    R2,CNTSREQH                                                      
         MVC   8(3,R2),=C'=CT'                                                  
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,CNTIDH                                                        
         MVC   8(5,R2),=C'BIAS1'   SET FOR FACREP1 SIGNON                       
         TM    SYSIDTY,FACITST     IS IT A TEST FACPAK                          
         BZ    *+10                                                             
         MVC   8(5,R2),=C'BIAST'                                                
         MVI   5(R2),5                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,CNTSYSH                                                       
         MVC   8(3,R2),=C'REP'                                                  
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,CNTPGMH                                                       
         MVC   8(8,R2),=C'CFI,BC=N'                                             
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         XIT1                                                                   
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* BUILD DARE ASSIST TABLE IN DATASPACE FOR THIS LUID (US ONLY)        *         
* BE CAREFUL - THIS CRAPS ON 2 IO AREAS                               *         
***********************************************************************         
DARETO   NTR1  BASE=*,LABEL=*                                                   
         ICM   R2,15,TAINITS       HAVE DARE ENTRY IN DATASPACE?                
         BZ    DRTO02              NO-TRY TO GET ONE                            
*                                                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         XR    R0,R0               MAKE SURE NO SPURIOUS MATCH                  
         L     R1,VSSB                                                          
         LAM   AR2,AR2,SSBTBLET-SSBD(R1)                                        
         SAC   512                                                              
         USING ASSISTD,R2                                                       
*                                                                               
         CLC   ASSLUID,TLUID       MATCH LUID?                                  
         BE    *+8                 YES                                          
         LHI   R0,1                SET CC FOR MATCH                             
         REAR  ARS=OFF                                                          
*                                                                               
         CHI   R0,1                                                             
         BE    DARETOX             WE ARE OK - DO NOTHING                       
*                                                                               
DRTO02   XC    TAINITS,TAINITS     RESET THIS                                   
         L     R5,APRVREC          USE THIS FOR KEY READ                        
         USING DASRECD,R5                                                       
         XC    DASKEY,DASKEY                                                    
         MVI   DASKTYP,DASKTYPQ                                                 
         MVC   DASKCPY,USERALPH                                                 
         MVC   DASKINIT,TLUID                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'GENDIR',(R5),(R5)                    
         CLI   8(R1),0                                                          
         BNE   DARETOX                                                          
*                                                                               
         L     R6,ASACREC                                                       
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'GENFIL',DASKDA,(R6),DMWORK           
         CLI   8(R1),0                                                          
         BNE   DARETOX                                                          
         DROP  R5                                                               
*                                                                               
         USING DASRECD,R6                                                       
         GOTO1 VHELLO,DMCB,(C'G',=C'GENFIL'),('DASELQ',(R6)),0,0                
         CLI   12(R1),0                                                         
         BNE   DARETOX                                                          
*                                                                               
         L     R5,12(R1)                                                        
         USING DASELD,R5                                                        
         CLI   DASLN,2                                                          
         BNH   DARETOX                                                          
*                                                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTASS)                                               
         GOTO1 VLOCKSPC,DUB        LOCK DATASPACE                               
         ICM   R2,15,4(R1)                                                      
         JZ    *+2                                                              
         REAR  ARS=OFF                                                          
*                                                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         USING DMSPACED,R2                                                      
         ICM   RF,15,DSPTEND       RF=A(END-1)                                  
         XR    RE,RE                                                            
         ICM   RE,3,DSPTWIDE       RE=WIDTH                                     
         ICM   R2,15,DSPTFRST      R2=A(START)                                  
         L     R1,VSSB                                                          
         LAM   AR2,AR2,SSBTBLET-SSBD(R1)                                        
         SAC   512                                                              
         USING ASSISTD,R2                                                       
*                                                                               
DRTO03   DS    0H                                                               
         OC    ASSLUID,ASSLUID     FIND FREE SLOT                               
         BZ    DRTO03A                                                          
         CLC   ASSLUID,TLUID       ENTRY FROM A PREVIOUS CONNECTION?            
         BE    DRTO03A                                                          
         BXLE  R2,RE,DRTO03                                                     
         DC    H'0'                TABLE FULL                                   
*                                                                               
DRTO03A  DS    0H                                                               
         MVC   ASSLUID,TLUID       USE LUID TO RESERVE SLOT                     
         MVC   ASSFAC,SYSIDNO                                                   
         MVI   ASSTYPE,ASSTDARE    SET LIST TYPE                                
         STCM  R2,15,TAINITS       SAVE ADDRESS IN UTL                          
         SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTASS)                                               
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        FREE DATASPACE                               
*                                                                               
         LAM   AR0,ARF,=16F'0'                                                  
         ICM   R2,15,TAINITS                                                    
         ICM   R1,15,VSSB                                                       
         LAM   AR2,AR2,SSBTBLET-SSBD(R1)                                        
         SAC   512                                                              
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,DASLN                                                       
         LA    RF,DASINIT                                                       
         LA    R2,ASSINITS                                                      
         XR    R1,R1                                                            
*                                                                               
DRTO04   CHI   R0,2                                                             
         BNH   DRTO06                                                           
         MVC   0(DASINITL,R2),0(RF)                                             
         AHI   R0,-(DASINITL)                                                   
         AHI   R2,DASINITL                                                      
         AHI   RF,DASINITL                                                      
         AHI   R1,1                                                             
         B     DRTO04                                                           
*                                                                               
DRTO06   ICM   R2,15,TAINITS                                                    
         STCM  R1,1,ASSCNT                                                      
         REAR  ARS=OFF                                                          
*                                                                               
DARETOX  XIT1                                                                   
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* SET VSAM OVERRIDES IN TSTATV ACCORDING TO VSAM VERSION TABLE IN     *         
* DMGR DATASPACE                                                      *         
***********************************************************************         
SETVSM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   TSTATV,0            PRESET NO OVERRIDES                          
         STAR  CLEAR=Y,ARS=OFF                                                  
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         LAM   AR2,AR2,SSBALET                                                  
         SAC   512                                                              
         SR    R2,R2                                                            
         L     R2,DHASSBG-DMDHDR(,R2)                                           
         USING FASSBG,R2                                                        
         LH    R0,SSGDMVST         GET COUNT OF ENTRIES                         
         LTR   R0,R0                                                            
         JZ    SETVSMX                                                          
         LA    R2,SSGDMVST+2       FIRST ENTRY                                  
         USING DMVSVSNT,R2                                                      
*                                                                               
SETVSM10 OC    DMVSVTAL,DMVSVTAL   SPECIFIC ALPHA ID?                           
         JZ    SETVSM12                                                         
         CLC   DMVSVTAL,TAGY       YES, OURS?                                   
         JNE   SETVSM90            NO, NEXT                                     
SETVSM12 CLI   DMVSVTTY,0          FURTHER QUALIFIERS?                          
         JE    SETVSM20            NO, SO THIS MATCHES                          
         CLI   DMVSVTTY,DMVSVTFQ   FACPAK ENTRY?                                
         JNE   SETVSM90            NO, NEXT                                     
         CLI   DMVSVTFI,0          SPECIFIC FACPAK ID?                          
         JE    SETVSM14                                                         
         CLC   DMVSVTFI,SSBSYSID   YES, OURS?                                   
         JNE   SETVSM90            NO, NEXT                                     
SETVSM14 CLI   DMVSVTSY,0          SPECIFIC SYSTEM?                             
         JE    SETVSM20            NO, NO FURTHER QUALIFIERS SO MATCHES         
         CLC   DMVSVTSY,TOVSYS     YES, OURS?                                   
         JNE   SETVSM90            NO, NEXT                                     
         CLI   DMVSVTPR,0          SPECIFIC PROGRAM?                            
         JE    SETVSM20            NO, SO THIS MATCHES                          
         CLC   DMVSVTPR,TPRG       YES, OURS?                                   
         JNE   SETVSM90            NO, NEXT                                     
SETVSM20 MVC   TSTATV,DMVSVTFL     COPY OVERRIDES                               
         J     SETVSMX                                                          
SETVSM90 LA    R2,DMVSVTLQ(,R2)    NEXT ENTRY                                   
         JCT   R0,SETVSM10                                                      
         DROP  R2,RE                                                            
SETVSMX  REAR  ARS=OFF                                                          
         XIT1                                                                   
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* SYSTEM EQUATES                                                      *         
***********************************************************************         
* DDGLOBEQUS                                                                    
       ++INCLUDE DDGLOBEQUS                                                     
                                                                                
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
                                                                                
* SRERREQUS                                                                     
       ++INCLUDE SRERREQUS                                                      
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
RSLT     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
ACTN     DS    X                                                                
SECWHY   DS    X                                                                
DMCB     DS    6F                                                               
GTB      DS    6F                                                               
SPACES   DS    CL20                                                             
WORK     DS    XL256                                                            
*                                                                               
ASRPARM  DS    A                   A(SRPARM LIST ON ENTRY)                      
ATIA     DS    A                   A(TIA)                                       
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
AGETIDS  DS    V                                                                
ATWASVR  DS    V                                                                
VGETTXT  DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VSCANNER DS    V                                                                
VSCUNKEY DS    V                                                                
VUNSCAN  DS    V                                                                
VDATCON  DS    V                                                                
VSWITCH  DS    V                                                                
VGLOBBER DS    V                                                                
VBILLIT  DS    V                                                                
VHELLO   DS    V                                                                
VADDAY   DS    V                                                                
VPERVAL  DS    V                                                                
VPERVERT DS    V                                                                
VSMFOUT  DS    V                                                                
*                                                                               
AUIDREC  DS    A                   A(IDREC)                                     
APIDREC  DS    A                   A(PIDREC)                                    
ATRMREC  DS    A                   A(TRMREC)                                    
AMAPREC  DS    A                   A(MAPREC)                                    
ASECREC  DS    A                   A(SECREC)                                    
AACCREC  DS    A                   A(ACCREC)                                    
APERREC  DS    A                   A(PERREC)                                    
ALAGREC  DS    A                   A(LAGREC)                                    
ASACREC  DS    A                   A(SACREC)                                    
APRVREC  DS    A                   A(PRVREC)                                    
*                                                                               
ASYSLST  DS    A                   A(GETFACT SYSTEM LIST)                       
AUIDSYS  DS    A                   A(SYS ELEMENT IN ID RECORD)                  
ATRMSYS  DS    A                   A(SYS ELEMENT IN TERMINAL RECORD)            
ASECSYS  DS    A                   A(SYS ELEMENT IN AUTH RECORD)                
ASACSYS  DS    A                   A(SYS ELEMENT IN ACCESS RECORD)              
ASENTRY  DS    A                   A(SELIST ENTRY)                              
APGMLIST DS    A                   A(PGMLIST)                                   
APGMNTRY DS    A                   A(PGMLIST ENTRY)                             
ATSTNTRY DS    A                   A(TSTTAB ENTRY)                              
ATWA2    DS    A                   A(TWA READ AREA)                             
ATWASES  DS    A                   A(SESSION ID CHR IN TWA)                     
ATIOB    DS    A                   A(TIOB)                                      
*                                                                               
STRDATE  DS    PL4                 FACPAK START DATE (JULIAN)                   
STRTIME  DS    XL4                 FACPAK START TIME (1/100 SEC)                
RECLEN   DS    H                   LENGTH OF TWA RECORD                         
CHKDSP   DS    H                   LENGTH OF TWA TO START OF CHKPNT             
GLODSP   DS    H                   LENGTH OF TWA TO START OF GLOBALS            
SSMAX    DS    H                   MAXIMUM LOGICAL SESSIONS                     
SSMXP    DS    H                   MAXIMUM PHYSICAL SESSIONS                    
SSPGS    DS    H                   PAGES PER SESSION                            
CURSES   DS    X                   CURRENT SESSION                              
NEWSES   DS    X                   NEW SESSION                                  
RSVSES   DS    X                   RESERVED SESSION                             
LSTSES   DS    X                   LAST SESSION                                 
PRVSES   DS    X                   PREVIOUS SESSION                             
NOMSES   DS    X                   NOMINATED SESSION                            
TRASES   DS    X                   TRANSFER SESSION                             
STOLEN   DS    X                   STOLEN SESSION                               
XTRSES   DS    X                   EXTRA SESSION                                
*                                                                               
CH1SES   DS    X                   1ST CHOICE                                   
CH2SES   DS    X                   2ND CHOICE                                   
CH3SES   DS    X                   3RD CHOICE                                   
CH4SES   DS    X                   4TH CHOICE                                   
*                                                                               
EXPIDEL  DS    XL528               NEW SYSTEM AUTHORIZATION ELEMENT             
SAVESREQ DS    CL17                SAVED FROM INPUT CNTSREQ                     
DATASW   DS    C                   C'Y' IF '+PGM' INPUT                         
CTSW     DS    C                   C'Y' IF '$CT' INPUT                          
REUSFLAG DS    C                   C'Y' TO REUSE USERID/PASSWORD IN UTL         
REUSPFLG DS    C                   C'Y' TO REUSE SYSTEM/PROGRAM IN UTL          
AUTOCON  DS    C                   C'Y' IF AUTOMATIC CONNECT REQUESTED          
PWDCNTL  DS    C                   PASSWORD CONTROL VALUE FROM PERSONID         
PWDEXDAT DS    CL6                 PASSWORD EXPIRY DATE                         
PWDERROR DS    X                   PASSWORD ERROR FOUND                         
DONESW   DS    C                   C'Y' IF $CT INPUT TO DISCONNECT              
ALLVALID DS    C                   C'Y' IF 'ALL' IDS COMPAT WITH TERM           
ERRFLAG  DS    X                   FLAG IF ERROR ENCOUNTERED                    
ERRNUM   DS    X                   ERROR NUMBER                                 
SESYSN   DS    X                   SYSTEM NUMBER                                
OVSYS    DS    X                   SYSTEM OVERLAY NUMBER                        
OVSYSN   DS    CL3                 OVERLAY SYSTEM NAME                          
USERID   DS    CL10                ALPHA USER-ID                                
USERNO   DS    XL2                 NUMERIC USER-ID                              
PIDOFF   DS    XL1                 OFFSET TO PERSONAL ID ENTERED                
READONLY DS    XL1                 READ-ONLY CONNECT                            
UPDSYSID DS    XL1                 UPDATIVE FACPAK ID FOR SYSTEM                
*                                                                               
SOXFL    DS    XL1                 SOX FLAG                                     
SOXON    EQU   X'80'               SOX CONTROLS ON                              
SOXDDS   EQU   X'40'               SOX DDS TERMINAL                             
SOXTST   EQU   X'20'               SOX TEST SYSTEM                              
SOXSCRP  EQU   X'10'               SOX SCRIPT TSTAT6=TST6SRPT                   
*                                                                               
SOXFL1   DS    XL1                 SOX FLAG1                                    
SOXDDPSC EQU   X'80'               SOX SCRIPT SET DDS PSWD                      
SOXDDPST EQU   X'40'               SOX SPECIAL TERM SET DDS PSWD                
SOXDDPTS EQU   X'20'               SOX TEST SYSTEM SET DDS PSWD                 
SOXAGDDS EQU   X'10'               SOX DDS PERSON ON DDS SECURITY AGY           
SOXDDPCM EQU   X'08'               SOX CON/MAD SET DDS PSWD                     
SOXPGMSR EQU   X'04'               SOX PROGRAM NAME IN S/R FIELD                
SOXUSEDD EQU   X'02'               SOX USE DDS SEC AGENCY FOR PERSON            
*                                                                               
SOXFL2   DS    XL1                 SOX FLAG2 (TSTATB AND XIFLAG1)               
SOXROSYS EQU   X'80'               SOX CONNECTED TO READ ONLY SYSTEM            
SOXROMOD EQU   X'40'               SOX CONNECTED IN READ ONLY MODE              
SOXWRONG EQU   X'20'               SOX CONNECTED TO WRONG FACPAK                
SOXTSTAD EQU   X'10'               SOX CONNECTED TO TEST FACPAK                 
SOXDDSPE EQU   X'08'               SOX DDS PERSON                               
SOXDDSTR EQU   X'04'               SOX DDS TERMINAL                             
SOXPPSAG EQU   X'02'               SOX PPS AGENCY                               
SOXDDSOV EQU   X'01'               SOX DDS PERSON OVERRIDE                      
*                                                                               
XTEST    DS    XL1                 SAVED UTL VALUES FOR XCT CONNECT             
XACCS    DS    XL4                                                              
XSTATB   DS    XL1                                                              
XSTAT5   DS    XL1                                                              
XTICKET  DS    CL16                                                             
XPSAVE   DS    XL6                                                              
XOSIN    DS    XL4                                                              
XOFFCODE DS    CL1                                                              
*                                                                               
IDOPTS   DS    0XL16               ID OPTIONS                                   
IDOFL1   DS    XL1                 X'80'=PSWD REQD,X'02'=PQPQWD REQD            
IDOFL2   DS    XL1                 N/D                                          
IDOPQI   DS    XL2                 PQ GROUP ID NUMBER                           
IDOSNU   DS    XL2                 SYNONYM ID NUMBER                            
IDOSID   DS    CL10                SYNONYM ID NAME                              
*                                                                               
PWINPV   DS    CL10                PASSWORD INPUT VALUE                         
PWINPL   DS    XL1                 PASSWORD INPUT LEN                           
         DS    XL1                 N/D                                          
PWFLAG   DS    XL1                 PASSWORD REQUIRED FLAG                       
PWREQD   EQU   X'80'               PASSWORD IS REQUIRED                         
PWPVAL   EQU   X'40'               PASSWORD HAS BEEN PRE-VALIDATED              
PWCTSW   EQU   X'20'               CONNECT SWAPPED TO THIS FACPAK               
*                                                                               
MEDAGY   DS    CL2                 UK MEDIA AGENCY ALPHA ID                     
USERALPH DS    CL2                 AGENCY ID                                    
AGYSEC   DS    CL2                 SECURITY AGENCY ID                           
PIDSEC   DS    CL2                 SECURITY AGENCY ID FOR READPID               
DDSSEC   DS    CL2                 SECURITY AGENCY FOR DDS TERMINALS            
UIDLLO   DS    XL1                 USERID LAST LOGON RELATIVE MONTH             
UIDLLON  DS    XL2                 YM binary new last logon                     
PASSDDS  DS    XL1                                                              
PASSDPWD EQU   X'01'               DDS PSW                                      
PASSDPID EQU   X'02'               DDS PID                                      
PASSSPID EQU   X'04'               SPECIAL PID USED FOR CONTROL MAD             
PASSWSPD EQU   X'08'               SPECIAL WEB PID USED FOR PRISMA              
*                                                                               
PASSFLAG DS    XL1                                                              
PFSCRIPT EQU   X'01'               SCRIPT PID/PW SET BY SRCON00                 
*                                                                               
ACTERR#  DS    H                                                                
*                                                                               
         DS    0F                                                               
PASSINPT DS    0XL48               PASSWORD DATA FROM INPUT FIELD               
PASSWD   DS    CL10                ALPHA PASSWORD                               
PASSWDU  DS    CL10                ALPHA PASSWORD IN UPPER CASE                 
PASSNO   DS    XL2                 NUMERIC PASSWORD                             
SECCODE  DS    CL10                ALPHA SECRET CODE                            
SECCODU  DS    CL10                ALPHA SECRET CODE IN UPPER CASE              
SECCODEL DS    AL1                 PASSWORD FIELD LENGTH                        
SECCODEF DS    XL1                 PASSWORD FIELD FLAG X'02'=UPPER              
SECNO    DS    XL2                 NUMERIC SECRET CODE                          
PASSLEN  DS    XL1                 ACTUAL INPUT LENGTH                          
         DS    XL1                                                              
*                                                                               
MPWDL    EQU   10                  MAXIMUM PASSWORD LENGTH                      
MPWDERR  EQU   6                   MAXIMUM PASSWORD ERRORS                      
*                                                                               
         DS    0F                                                               
PIDDLE   DS    0XL48               PERSON DATA EXTRACTED FROM RECORD            
PIDPWD   DS    CL10                PERSON PASSWORD CODE                         
PIDPWDU  DS    CL10                PERSON PASSWORD CODE IN UPPER CASE           
PIDDLE1  DS    0XL28                                                            
PIDPWDL  DS    AL1                 PERSON PASSWORD CODE LENGTH                  
PIDPWDF  DS    XL1                 PERSON PASSWORD FLAG X'02'=UPPER             
PIDNUM   DS    XL2                 PERSON PASSWORD NUMBER                       
PIDLLOEL DS    H                   PERSON LAST LOGIN ELEMENT DISP               
PIDLLO   DS    XL3                 PERSON LAST LOGON DATE                       
PIDLLOT  DS    XL3                 PERSON LAST ERROR TIME                       
PIDLLOE  DS    XL1                 PERSON LAST ERROR COUNT                      
PIDLLFL  DS    XL1                 PERSON LAST PASSWORD STATUS                  
         DS    XL3                                                              
PWDTRYS  DS    XL1                 PERSON TRYS LEFT TO INPUT PASSWORD           
PEXPDAYS DS    XL1                 COUNT OF DAYS TO EXPIRY                      
PWDSTAT  DS    XL1                 PASSWORD STATUS                              
PSTEXP   EQU   X'80'               PASSWORD EXPIRED                             
PSTEXPW  EQU   X'40'               PASSWORD ABOUT TO EXPIRE WARNING             
PSTMAX   EQU   X'20'               PASSWORD MAX ERRORS REACHED                  
PSTMAXW  EQU   X'10'               PASSWORD ABOUT TO REACH MAX ERRORS           
PSTNOP   EQU   X'08'               PASSWORD ERROR COUNT NOP                     
PSTUPDT  EQU   X'04'               PASSWORD ERROR COUNT UPDATED                 
PSTBUMP  EQU   X'02'               PASSWORD ERROR COUNT BUMP                    
PSTRESET EQU   X'01'               PASSWORD ERROR COUNT RESET                   
PWDALAST DS    A                   A(LAST PASSWORD)                             
PWDATEMP DS    A                   A(LAST TEMPORARY PASSWORD)                   
*                                                                               
ACTACC   DS    A                   A(ACCESS DATES ELEMENT)                      
*                                                                               
PROGAUTH DS    XL2                 COMPOSITE PROGRAM AUTHORISATION              
AUTHVAL  DS    XL2                 OVERIDE PROGRAM AUTHORISATION                
AUTHFNDX DS    X                   OVERIDE AUTHVAL SUB FIELD INDEX              
PSECFLAG DS    X                   PROGRAM SECURITY FLAG IN ACCESS REC.         
LIMACCS  DS    XL4                 LIMIT ACCESS VALUE (LIMIT=)                  
LIMITACS DS    XL4                 DEDUCED LIMIT ACCESS VALUE (USER)            
LIMITAC2 DS    XL4                 LIMIT ACCESS 2 VALUE (AUTH PERSON)           
OFFICE   DS    C                   OFFICE CODE                                  
CTTEST   DS    X                   TTEST VALUE                                  
CTSTEREO DS    X                   STEREO FLAGS (SAME AS TSTAT6 BITS)           
CTSTERE8 DS    X                   STEREO FLAGS (SAME AS TSTAT8 BITS)           
CTSTIN1  DS    C                   STEREO INPUT VALUE 1                         
CTSTIN2  DS    C                   STEREO INPUT VALUE 2                         
CTSTIN3  DS    C                   STEREO INPUT VALUE 3                         
CTPROT   DS    C                   STORAGE PROTECTION OVERRIDE FLAG             
CTLANG   DS    X                   LANGUAGE CODE                                
CTCTRY   DS    X                   COUNTRY CODE                                 
CTICKET  DS    CL16                TICKET VALUE                                 
COSIN    DS    XL4                 OSIN VALUE                                   
AGYCTRY  DS    XL1                 AGENCY COUNTRY  (FROM ACCESS RECORD)         
AGYCURR  DS    CL3                 AGENCY CURRENCY (FROM ACCESS RECORD)         
AGYOPTS  DS    XL1                 AGENCY OPTIONS  (FROM ACCESS RECORD)         
         DS    XL1                 N/D                                          
DDSACC   DS    XL1                 DDS AGENCY ACCESS LEVEL FLAGS                
CTRYFLAG DS    XL1                 FLAGS FOR COUNTRY VALIDATION                 
TOUTAGY  DS    XL2                 ADV TIMEOUT AT AGENCY LEVEL (MINS.)          
TOUTUID  DS    XL2                 ADV TIMEOUT AT USERID LEVEL (MINS.)          
TOUTTRM  DS    XL2                 ADV TIMEOUT AT TERM. LEVEL (MINS.)           
TOUTCON  DS    XL1                 LAST CONNECTED ADV TIMEOUT (=PGM)            
DATABLD  DS    XL1                 DATA BUILD X'01' BUILD=Y X'02'               
SERVTEAM DS    XL1                 CLIENT SERVICE TEAM / INACTIVE FLAG          
INACTIVE EQU   10                  . INACTIVE AGENCY INDICATOR                  
*                                                                               
RVPID#   DS    XL2                 PASSWORD CHANGE SAVE BLOCK                   
RVAGYB   DS    CL2                                                              
RVAGYSEC DS    CL2                                                              
RVUSER   DS    XL2                                                              
*                                                                               
GETOVER  DS    XL1                 GETIDS OUTPUT BLOCK OVERFLOW ERROR           
*                                                                               
PERSONID DS    CL8                 PERSONAL ID CODE                             
AGROUPNO DS    XL2                 PERSON ACCESS GROUP CODE NUMBER              
LGROUPNO DS    XL2                 PERSON DATA ACCESS GROUP CODE                
LAGVALUE DS    XL4                 PERSON DATA ACCESS GROUP VALUE               
*                                                                               
ISODATE  DS    CL10                ISO DATE YYYY-MM-DD                          
*                                                                               
SCRNDATE DS    CL8                 TODAYS DATE FOR SCREEN                       
SCRNTIME DS    PL4                 TIME NOW FOR SCREEN                          
TODAYMTH DS    XL1                 TODAYS RELATIVE MONTH NUMBER                 
*                                                                               
TODAY    DS    CL6                 TODAYS DATE C'YYMMDD'                        
TODAYP   DS    PL3                 TODAYS DATE PACKED                           
TODAYB   DS    XL2                 TODAYS DATE BINARY COMPRESSED                
TODAY3   DS    XL3                 TODAYS DATE BINARY YMD                       
*                                                                               
SYSIDTAB DS    0CL8                FACPAK SYSTEM ID BLOCK                       
SYSIDNAM DS    CL4                 FACPAK SYSTEM NAME (4 CHR)                   
SYSIDNO  DS    X                   FACPAK SYSTEM NUMBER                         
SYSIDTY  DS    X                   FACPAK SYSTEM TYPE X'80'=TST                 
SYSIDCHR DS    C                   FACPAK SYSTEM CHARACTER                      
SYSIDN1  DS    C                   FACPAK SYSTEM NAME (1 CHR)                   
*                                                                               
PROGINFO DS    0CL12               PROGRAM INFO BLOCK                           
PROGNAME DS    CL4                 PROGRAM NAME                                 
PROGNUM  DS    X                   PROGRAM NUMBER                               
PROGSYS  DS    X                   PROGRAM CALLOV SYSTEM NUMBER                 
PROGANUM DS    X                   PROGRAM ACCESS OVERRIDE NUMBER               
PROGIND  DS    X                   PROGRAM INDICS                               
PROGIND2 DS    X                   PROGRAM INDICS 2                             
PROGIND3 DS    X                   PROGRAM INDICS 3                             
PROGIND4 DS    X                   PROGRAM INDICS 4                             
PROGNLNK DS    X                   PROGRAM LINK NUMBER                          
*                                                                               
XPINFO   DS    0XL6                EXTERNAL PROGRAM INFO                        
XPTYPE   DS    XL1                 EXTERNAL PROGRAM TYPE (PC OR MF APP)         
XPNUM    DS    XL2                 EXTERNAL PROGRAM INTERNAL NUMBER             
XPVER    DS    XL3                 EXTERNAL PROGRAM VERSION CODE                
*                                                                               
         DS    0H                                                               
XPVERS   DS    0XL14               EXTERNAL VERSION INFO                        
XPVERFLG DS    XL1                 EXTERNAL VERSION FLAG                        
         DS    XL1                 N/D                                          
XPVERDSP DS    H                   EXTERNAL VERSION TABLE DISPLACEMENT          
XPVERNEW DS    CL10                EXTERNAL VERSION NEW NAME                    
XPVERERR EQU   373                 EXTERNAL VERSION ERROR NUMBER                
*                                                                               
SYSNAME  DS    CL7                 SYSTEM NAME SAVED FROM SCREEN FLD            
SYSNLEN  DS    XL1                 SYSTEM NAME LENGTH                           
*                                                                               
SVTSYS   DS    X                   TSYS ON ENTRY TO CONNECT                     
FERN     DS    X                   FIELD ERROR NUMBER                           
FNDX     DS    X                   MULTIPLE FIELD INDEX VALUE                   
FSUB     DS    X                   FIELD SUB INDEX VALUE (REASON CODE)          
FADR     DS    A                   A(CURRENT TWA FIELD HEADER)                  
FLDH     DS    CL8                 CURRENT FIELD HEADER                         
FLD      DS    CL80                CURRENT FIELD VALUE                          
AINPFLDX DS    A                   A(END OF INPUT FIELDS)                       
INPFLDS  DS    CL80                INPUT DATA FIELDS                            
*                                                                               
LODISP   DS    H                   LOW TWA DISP FOR OVERLAY SCREEN              
HIDISP   DS    H                   HIGH TWA DISP FOR OVERLAY SCREEN             
FLDNUM   DS    X                   RELATIVE TWA FIELD NUMBER                    
FLDTYP   DS    X                   DATA TYPE                                    
SCRNUM   DS    X                   OVERLAY SCREEN NUMBER                        
CONERR   DS    X                   CHECK ERROR NUMBER                           
*                                                                               
SAELEM   DS    CL20                FOR SAPWHEL BUILD                            
IDTYPE   DS    CL1                 CTAGYIDT                                     
PIDENTER DS    X                   PERSONAL ID ENTERED THIS TIME                
PIDREQD  DS    X                   PERSONAL ID REQUIRED FLAG                    
PONCE    DS    X                   PERSONAL ID ONCE ONLY                        
*                                                                               
PIDDATA  DS    0XL6                PID DATA SAVED FROM ACCESS RECORD            
PEXPIRY  DS    X                   COUNT OF DAYS FOR EXPIRY                     
PEXPWARN DS    X                   COUNT OF DAYS FOR EXPIRY WARNING             
PREUSE   DS    X                   COUNT OF NUMBER OF REPEATS                   
PMINLEN  DS    X                   MINIMUM LENGTH OF PASSWORD                   
PRULENUM DS    X                   VALIDATION RULE NUMBER                       
PMAXERR  DS    X                   MAXIMUM NUMBER OF PASSWORD ERRORS            
*                                                                               
OPTIONS  DS    0CL32                                                            
PROGOPT  DS    X                                                                
TESTOPT  DS    X                                                                
UPDTOPT  DS    X                                                                
IAMOPT   DS    X                                                                
CILOPT   DS    X                                                                
USEROPT  DS    X                                                                
AUTHOPT  DS    X                                                                
MAPOPT   DS    X                                                                
SAVEOPT  DS    X                                                                
LIMOPT   DS    X                                                                
BCOPT    DS    X                                                                
LANGOPT  DS    X                                                                
         DS    X                                                                
         DS    X                                                                
         DS    X                                                                
         DS    X                                                                
         DS    X                                                                
S32OPT   DS    X                                                                
         ORG   OPTIONS+L'OPTIONS                                                
*                                                                               
SOPTIONS DS    0CL3                                                             
PCPOPT   DS    X                                                                
APPOPT   DS    X                                                                
XPVOPT   DS    X                                                                
         ORG   SOPTIONS+L'SOPTIONS                                              
*                                                                               
BILLREF  DS    XL12                SAVE BILLING REFERENCE                       
TEMPESTF DS    X                   FLAG FOR TEMPEST STATUS                      
*                                                                               
TEMPSINF DS    0XL7                SAVE TEMPEST RESERVE INFO                    
TEMPSFLG DS    X                                                                
TEMPSNUM DS    XL2                                                              
TEMPSNDX DS    XL4                                                              
*                                                                               
XCTLTYP  DS    CL1                                                              
XCTLSYS  DS    CL3                                                              
XCTLPGM  DS    CL3                                                              
*                                                                               
NLINES   DS    X                   NUMBER OF INPUT SCAN LINES                   
SCANBLK  DS    10CL32              SCANNER BLOCK                                
*                                                                               
IOKEY    DS    XL40                SAVE AREA FOR CTFILE RECORD KEY              
DMWORK   DS    8D                                                               
*                                                                               
DATAPOOL DS    1000C               EXTRACT OF TWA SAVED HERE                    
MAXRECL  EQU   1000                                                             
IDREC    DS    (MAXRECL)X          ID RECORD                                    
PIDREC   DS    (MAXRECL)X          PRINCIPLE ID RECORD                          
TRMREC   DS    (MAXRECL)X          TERMINAL RECORD                              
MAPREC   DS    (MAXRECL)X          MAP RECORD                                   
SECREC   DS    (MAXRECL)X          SECRET AUTHORISATION RECORD                  
ACCREC   DS    (MAXRECL)X          ACCESS RECORD                                
PERREC   DS    (MAXRECL)X          PERSONAL ID RECORD                           
LAGREC   DS    (MAXRECL)X          LIMIT ACCESS GROUP RECORD                    
SACREC   DS    (MAXRECL)X          SECURITY AGENCY ACCESS RECORD                
PRVREC   DS    (MAXRECL)X          PROVER RECORD                                
GETIDSWK DS    (30*K)X                                                          
WORKX    EQU   *                                                                
*                                                                               
ALLBITS  EQU   X'FF'                                                            
K        EQU   1024                                                             
*&&UK                                                                           
DELIM    EQU   C''''               DATA STRING DELIMITER (UK)                   
*&&                                                                             
*&&US                                                                           
DELIM    EQU   C'.'                DATA STRING DELIMITER (US)                   
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT to cover logo to change copy right date                       *         
***********************************************************************         
CPYRGHTD DSECT                                                                  
         DS    CL37                                                             
CPYRTDTE DS    CL4                                                              
         DS    CL3                                                              
CPYRTLNQ EQU   *-CPYRGHTD                                                       
*                                                                               
***********************************************************************         
* DSECT TO COVER OPTTAB TABLE                                         *         
***********************************************************************         
OPTD     DSECT                                                                  
OPTKYW   DS    CL8                 KEYWORD                                      
OPTMINKL DS    AL1                 MINIMUM LENGTH OF KEYWORD                    
OPTMAXKL DS    AL1                 MAXIMUM LENGTH OF KEYWORD                    
OPTMINDL DS    AL1                 MINIMUM LENGTH OF DATA                       
OPTMAXDL DS    AL1                 MAXINUM LENGTH OF DATA                       
OPTIND1  DS    X                   INDICATORS                                   
OPTIDDS  EQU   X'80'               DDSONLY                                      
         ORG   OPTIND1                                                          
OPTNUM   DS    AL1                 OPTION NUMBER 1-127                          
OPT@RTN  DS    AL3                                                              
OPTLNQ   EQU   *-OPTD              LENGTH                                       
                                                                                
***********************************************************************         
* DSECT TO COVER LOG RECORD                                           *         
***********************************************************************         
LOGRECD  DSECT ,                   LOG RECORD FOR SECURITY VIOLATIONS           
LOGREC   DS    0CL80               CURRENT ADRFILE RECORD SIZE                  
LOGID    DS    CL4                 $CT. IDENTIFIES A SRCON LOG REC              
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGTEXT  DS    CL48                FREE FORM TEXT STARTS HERE                   
LOGTEXT1 DS    0CL15                                                            
LOGPWD   DS    CL10                INPUT PASSWORD                               
         DS    CL4                                                              
LOGDAYNO DS    XL1                 DAY NUMBER FROM SSB MON=1,SUN=7              
LOGSYSIX DS    XL1                 FACPAK AOR/TOR ID                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TWA                                                  *         
***********************************************************************         
SRCONFFD DSECT                                                                  
         DS    XL64                                                             
       ++INCLUDE SRCONFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER SYSTEM DSECTS                                                 *         
***********************************************************************         
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
                                                                                
* GEGENMSG                                                                      
       ++INCLUDE GEGENMSG                                                       
                                                                                
*&&US                                                                           
* DDASSISTD                                                                     
       ++INCLUDE DDASSISTD                                                      
*&&                                                                             
* SEACSFILE                                                                     
       ++INCLUDE SEACSFILE                                                      
                                                                                
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
                                                                                
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
                                                                                
* FAGETTXTD                                                                     
       ++INCLUDE FAGETTXTD                                                      
                                                                                
*&&US                                                                           
* GEGENASS                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENASS                                                       
         PRINT ON                                                               
*&&                                                                             
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
                                                                                
* FATBHD                                                                        
       ++INCLUDE FATBHD                                                         
                                                                                
* FACHKPT                                                                       
       ++INCLUDE FACHKPT                                                        
                                                                                
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
                                                                                
* FASYSLSTD                                                                     
       ++INCLUDE FASYSLSTD                                                      
                                                                                
* FACIDTABD                                                                     
       ++INCLUDE FACIDTABD                                                      
                                                                                
* FAXPTABD                                                                      
       ++INCLUDE FAXPTABD                                                       
                                                                                
* FAXPEQUS                                                                      
       ++INCLUDE FAXPEQUS                                                       
                                                                                
* DDSMFMETD                                                                     
       ++INCLUDE DDSMFMETD                                                      
                                                                                
*&&US                                                                           
* DMDSHDR                                                                       
       ++INCLUDE DMDSHDR                                                        
*&&                                                                             
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SRCON00   08/14/20'                                      
         END                                                                    
