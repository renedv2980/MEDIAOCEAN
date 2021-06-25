*          DATA SET TAREP5E    AT LEVEL 002 AS OF 10/04/16                      
*PHASE T7035EB,*                                                                
*INCLUDE DLFLD                                                                  
*===============================================================                
* MVS INPUT DATASET IS A COPY OF THE WORKER FILE PROCESSED                      
* BY SINTER. THE DSN COMES FROM A SYSTEM RECORD WITH TODAY'S                    
* DATE.                                                                         
* THE FIRST ELEMENT IS A X'D7' TANXEL                                           
* FOR EACH RECORD ON THE WORKER FILE, CONSTRUCT THE KEY OF A HOLD               
* RECORD AND VERIFY IT IS THERE.                                                
*===============================================================                
         TITLE 'T7035E - CLARUS AUDIT DOWNLOAD'                                 
T7035E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7035E                                                         
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         USING MYD,R7                                                           
         EJECT                                                                  
*==================================================                             
*        MODE CONTROLLED ROUTINES                                               
*==================================================                             
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   YES                                                              
         BRAS  RE,VOPT             VALIDATION OPTIONS                           
         BRAS  RE,PROC             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     EQU   *                                                                
XIT      XIT1                                                                   
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================                               
*        VALIDATE OPTIONS                                                       
*================================================                               
                                                                                
VOPT     NTR1                                                                   
         MVI   PROSTAT,0                                                        
                                                                                
         CLI   SPLOPTH+5,0                                                      
         JE    XIT                                                              
                                                                                
         LA    R2,SPLOPTH                                                       
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         LLC   R0,4(R1)                                                         
                                                                                
VOPT10   CLC   =C'TRACE',SCDATA1                                                
         BNE   VOPT12                                                           
         CLI   SCDATA2,C'N'                                                     
         BE    VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    PROSTAT,PSTRACE                                                  
         B     VOPT20                                                           
*                                                                               
VOPT12   CLC   =C'PRINT',SCDATA1                                                
         BNE   VOPT14                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPT20                                                           
         CLI   SCDATA2,C'N'                                                     
         BNE   INVERR                                                           
         OI    PROSTAT,PSNOPRT                                                  
         B     VOPT20                                                           
*                                                                               
VOPT14   CLC   =C'EMAIL',SCDATA1                                                
         BNE   INVERR                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPT20                                                           
         CLI   SCDATA2,C'N'                                                     
         BNE   INVERR                                                           
         OI    PROSTAT,PSNOEML                                                  
         B     VOPT20                                                           
*                                                                               
VOPT20   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT10                                                        
         J     XIT                                                              
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* READ SYSTEM RECORD AND OPEN ASSOCIATED MVS DATASET                            
*=============================================================                  
                                                                                
PROC     NTR1                                                                   
*                                                                               
         L     RF,TWADCONS                                                      
         MVC   DYNALLOC,TDYNALLO-TWADCOND(RF)                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,0,X'D9000AFE'   LOAD TRUNPK                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTRPACK,0(R1)                                                    
                                                                                
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN         INITIALIZE DOWNLOAD                          
*                                                                               
         MVC   PDATA(PCOLUMNL),SPACES    DOWNLOAD COLUMN HEADERS                
         MVC   PCUSERID,=C'USER ID'                                             
         MVC   PCAGY,=C'AGENCY'                                                 
         MVC   PCCMML,=C'COMMERCIAL'                                            
         MVC   PCSLN,=C'LENGTH'                                                 
         MVC   PCCLI,=C'CLIENT'                                                 
         MVC   PCPRD,=C'PRODUCT'                                                
         MVC   PCMED,=C'MEDIA'                                                  
         MVC   PCUSE,=C'USE'                                                    
         MVC   PCSTA,=C'STATION'                                                
         MVC   PCDATE,=C'DATE'                                                  
         MVC   PCPNME,=C'PROGRAM NAME'                                          
         MVC   PCNWK,=C'NETWORK'                                                
         MVC   PCMKT,=C'MARKET'                                                 
         MVC   PCLCS,=C'LOCAL CABLE STATION'                                    
         MVC   PCRNA,=C'REASON NOT AIRED'                                       
         MVC   PCERR,=C'ERROR'                                                  
*                                                                               
         MVI   DWNFLG,DWNFLCOL     DOWNLOAD COLUMN HEADERS                      
         BRAS  RE,OUTPDOWN                                                      
         MVI   DWNFLG,DWNFLFLD                                                  
*                                                                               
         XC    KEY,KEY                                                          
K        USING TLSYD,KEY                                                        
         MVI   K.TLSYCD,TLSYCDQ                                                 
         MVI   K.TLSYTYPE,TLSYSNFN                                              
         L     RE,TWAMASTC                                                      
         MVC   DUB,MCDATE-MASTD(RE)                                             
         GOTO1 DATCON,DMCB,(4,DUB),(1,K.TLSYSNDT)                               
         DROP  K                                                                
*                                                                               
PROC2    MVI   RDUPDATE,C'N'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(5),KEYSAVE      SAME TYPE/DATE                               
         BNE   PROCX                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              READ THE RECORD                              
*                                                                               
         L     R6,AIO                                                           
         MVC   SVSYSKEY,0(R6)      SAVE KEY OF SYSTEM RECORD                    
*                                                                               
         MVI   ELCODE,TACMELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,SVSYSEL          SAVE ELEMENT ADDRESS                         
*                                                                               
         USING TACMD,R6                                                         
* X'40' IN P2 REQUESTS RETURN ERROR CODE IN P4                                  
*                                                                               
PROC4    GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'FILEIN'),(X'40',TACMCOMM)              
         OC    12(4,R1),12(R1)     TEST FOUND DATASET OK                        
         BNZ   PROCERR                                                          
         DROP  R6                                                               
*                                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GET   FILEIN,RECINLEN     GET FIRST RECORD = WKFILE KEY                
         MVC   SVWRKRID,RECIN      AND SAVE THE WRKR FILE KEY                   
         MVC   SVWRKUID,RECIN+20                                                
*                                                                               
PROC10   GET   FILEIN,RECINLEN                                                  
         CLC   =C'/*',RECIN        TEST END OF WRKR FILE                        
         BE    PROC10              YES - GET NEXT WRKR FILE                     
*                                                                               
         LA    R6,RECIN            POINT TO TANXEL IN MVS FILE RECORD           
         BRAS  RE,FNDREC           FIND THE DATA RECORD                         
         B     PROC10                                                           
*                                                                               
PROC100  DS    0H                                                               
         CLOSE FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,TACMELQ                                                   
         L     R6,SVSYSEL          RESTORE AIO2 ELEM ADDR                       
         BRAS  RE,NEXTEL           TRY FOR ANOTHER DSN NAME ELEM                
         BNE   PROC102                                                          
         ST    R6,SVSYSEL          SAVE ELEMENT ADDRESS                         
         B     PROC4                                                            
*                                                                               
PROC102  MVC   KEY,SVSYSKEY            LAST SYSTEM REC PROCESSED                
         LLC   RE,TLSYSEQ-TLSYD+KEY    GET SEQNUM                               
         LA    RE,1(RE)                                                         
         STC   RE,TLSYSEQ-TLSYD+KEY                                             
         B     PROC2                                                            
                                                                                
* DSN OPEN FAILED - PUT ERROR TO DOWNLOAD                                       
                                                                                
PROCERR  MVC   PDATA,SPACES                                                     
         MVC   PUSERID(9),=C'* ERROR *'                                         
         MVC   PCMML(15),=C'DSN OPEN FAILED'                                    
         USING TACMD,R6                                                         
         MVC   PERR(40),TACMCOMM                                                
         BRAS  RE,OUTPDOWN                                                      
         DROP  R6                                                               
*                                                                               
PROCX    DS    0H                                                               
         BAS   RE,ENDDOWN          WRAP UP DOWNLOAD                             
                                                                                
         TM    PROSTAT,PSNOEML                                                  
         JO    XIT                                                              
         B     PROCEML                                                          
*========================================================                       
* SEND EMAIL TO PROGRAMMERS                                                     
*========================================================                       
                                                                                
PROCEML  L     RF,TWADCONS                                                      
         L     RF,TSMTP-TWADCOND(RF)                                            
         GOTOR (RF),DMCB,('SMTPAINI',JESMAIL)                                   
*                                                                               
         TM    PROSTAT,PSERR       IF DISCREPANCY PRESENT                       
         BZ    *+10                NOTIFY PROGRAMMERS                           
         MVC   SUBJECT1(9),=C'(FAILED!)'                                        
         LA    R0,SUBJECTX-SUBJECT                                              
         GOTOR (RF),DMCB,('SMTPAPRS',TOWHO),((R0),SUBJECT)                      
*                                                                               
         L     RE,TWAMASTC                                                      
         MVC   AMCREMOT,MCVREMOT-MASTD(RE)   REMOTE BLOCK                       
*                                                                               
         MVC   LINE1+24(L'MCUSERID),MCUSERID-MASTD(RE)                          
         LA    R2,LINE1+24                                                      
         LA    R3,L'MCUSERID        USER-ID                                     
PROCE02  CLI   0(R2),C' '           FIND FIRST SPACE                            
         BE    PROCE04                                                          
         AHI   R2,1                                                             
         BCT   R3,PROCE02                                                       
PROCE04  MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
*                                                                               
         L     RE,AMCREMOT                                                      
         USING REMOTED,RE                                                       
         MVC   0(3,R2),REMOTJID     REPORT ID                                   
         AHI   R2,3                                                             
         MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
         MVC   DUB,SPACES                                                       
         EDIT  REMOTRNO,DUB,ALIGN=LEFT                                          
         MVC   0(L'DUB,R2),DUB                                                  
         DROP  RE                                                               
*                                                                               
         GOTOR (RF),DMCB,('SMTPAPTL',LINE1)                                     
         GOTOR (RF),DMCB,('SMTPASND',0)                                         
         GOTOR (RF),DMCB,('SMTPAEND',0) DETACH SMTP                             
*                                                                               
ERRCHX   XIT1                                                                   
*                                                                               
VSMTP    DC    V(SMTP)                                                          
JESMAIL  DC    CL8'JESMAIL '                                                    
TOWHO    DC    C'US-TALENT_MF_DEV_TEAM@MEDIAOCEAN.COM,US-TALENT_PRODUCT+        
               _TEAM@MEDIAOCEAN.COM:'                                           
*TOWHO    DC    C'JBAS,GHOA,MZEI,TFIT,SCHT:'                                    
SUBJECT  DC    C'CLARUS FILE AUDIT DAILY NOTICE '                               
SUBJECT1 DC    C'(SUCCESS)'                                                     
SUBJECTX EQU   *                                                                
LINE1    DC    CL80'PLEASE CHECK OUTPUT - U='                                   
         J     XIT                                                              
                                                                                
*================================================================               
* LOOK FOR THE DATA IN A HOLD RECORD                                            
*================================================================               
                                                                                
         USING TANXD,R6            R6=A(WORKER FILE RECORD)                     
FNDREC   NTR1                                                                   
*                                                                               
K        USING TLNXKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   K.TLNXCD,TLNXCDQ                                                 
         MVC   K.TLNXAGY,TANXAGY                                                
         MVC   K.TLNXNID,TANXNCID    SET CMML ID                                
*                                                                               
         MVI   K.TLNXMED,C'T'      ASSUME TV                                    
         CLI   TANXMED,0                                                        
         BE    *+10                                                             
         MVC   K.TLNXMED,TANXMED                                                
*                                                                               
         LA    R6,RECIN                                                         
         MVI   ELCODE,TACTELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TACTD,R6                                                         
         MVC   K.TLNXNCLI,TACTCLI                                               
         DROP  R6                                                               
*                                                                               
         LA    R6,RECIN                                                         
         MVI   ELCODE,TAPRELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   FNDR02                                                           
         USING TAPRD,R6                                                         
         MVC   K.TLNXNPRD,TAPRPRD    SET PRODUCT CODE                           
         DROP  R6                                                               
*                                                                               
FNDR02   LA    R6,RECIN                                                         
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TANPD,R6                                                         
         MVC   K.TLNXUSE,=C'CLA'   SET USE TO NETWORK CLASS A                   
*                                                                               
         TM    TANPSTAT,TANPPAX    IF NETWORK IS PAX                            
         BZ    FNDR05                                                           
         MVC   K.TLNXUSE,=C'PAX'   SET USE TO NETWORK PAX                       
         B     FNDR20              ***TEMPORARY CODE***                         
*                                                                               
FNDR05   TM    TANPSTAT,TANPFLAT   IF FLAT RATE PROGRAM                         
         BZ    FNDR10                                                           
*                                                                               
         MVC   K.TLNXUSE,=C'LNA'   SET USE BY NETWORK                           
         CLI   TANPNWK,C'A'        LNA FOR ABC                                  
         BE    FNDR20                                                           
*                                                                               
         MVC   K.TLNXUSE,=C'LNN'   LNN FOR NBC                                  
         CLI   TANPNWK,C'N'                                                     
         BE    FNDR20                                                           
*                                                                               
         MVC   K.TLNXUSE,=C'LNF'   LNF FOR FOX                                  
         CLI   TANPNWK,C'F'                                                     
         BE    FNDR20                                                           
*                                                                               
         MVC   K.TLNXUSE,=C'LNC'   LNC FOR CBS                                  
         B     FNDR20                                                           
*                                                                               
FNDR10   CLI   TANPLEN,TANPLNQ3    TEST ELEMENT LENGTH                          
         BNH   FNDR20              CAN'T BE ANYTHING ELSE                       
*                                                                               
         CLI   TANPTYP,TANPNET     IF CABLE NETWORK PROGRAM                     
         BNE   *+10                                                             
         MVC   K.TLNXUSE,=C'CBL'   SET USE TO CABLE                             
*                                                                               
         CLI   TANPTYP,TANPCSYS    IF LOCAL CABLE PRGRAM                        
         BNE   *+10                                                             
         MVC   K.TLNXUSE,=C'LCB'   SET USE TO LCB                               
*                                                                               
         CLI   TANPTYP,TANPSPT     IF SPOT PROGRAM                              
         BNE   FNDR20                                                           
         MVC   K.TLNXUSE,=C'WSP'   SET USE TO WILDSPOT                          
         CLI   K.TLNXMED,C'T'      TEST SPOT TV                                 
         BNE   FNDR20                                                           
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY BUILT SO FAR                        
         MVI   ELCODE,TANPELQ                                                   
         LA    R6,RECIN                                                         
         BRAS  RE,NEXTEL           (NOTE USE NEXTEL, NOT GETEL!)                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TANPLEN,TANPLNQ3     TEST IF MKT IN ELEMENT                      
         BNH   FNDR20                                                           
         CLI   TANPMKT-TANPEL(R6),C' '  TEST INVALID                            
         BH    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETALPHA                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY         THEN RESTORE KEY                  
*                                                                               
FNDR20   MVC   SVUSE,K.TLNXUSE      SAVE TO PRINT OUT LATER                     
*                                                                               
         L     RE,TWAMASTC                                                      
         MVC   DUB,MCDATE-MASTD(RE)                                             
         GOTO1 DATCON,DMCB,(4,DUB),(1,K.TLNXDATE)   SET DATE ADDED              
         XC    K.TLNXDATE,=X'FFFFFF'              COMPLEMENT                    
         MVC   K.TLNXUID,UKUSRID-UKKEY+SVWRKRID                                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(30),KEYSAVE                                                  
         BNE   FNDERR1             NO HOLD RECORD ON FILE                       
         DROP  K                                                                
*                                                                               
FNDR22   MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING TLNXD,R6                                                         
         CLI   TLNXSEQ,0          ONLY GET TANX ELEM IF SEQ=0                   
         BH    FNDR40                                                           
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,TANXELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FL       USING TANXD,R6                                                         
*                                                                               
         LA    R5,RECIN                                                         
TP       USING TANXD,R5                                                         
*                                                                               
FNDR32   CLC   FL.TANXAGY,TP.TANXAGY    TEST RIGHT AGENCY                       
         BNE   FNDR34                                                           
         CLC   FL.TANXNCID,TP.TANXNCID  TEST RIGHT CMML                         
         BNE   FNDR34                                                           
         CLC   FL.TANXSEC,TP.TANXSEC    RIGHT SLN                               
         BNE   FNDR34                                                           
         B     FNDR40                                                           
*                                                                               
FNDR34   BRAS  RE,NEXTEL                                                        
         BE    FNDR32                                                           
         B     FNDERR2             CMML NOT IN HOLD RECORD                      
         DROP  FL,TP                                                            
*                                                                               
FNDR40   MVI   ELCODE,TANPELQ                                                   
         LA    R6,RECIN                                                         
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R5,R6                                                            
TP       USING TANPD,R5                                                         
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
FL       USING TANPD,R6                                                         
*                                                                               
FNDR42   CLC   FL.TANPDATE,TP.TANPDATE  MATCH DATE                              
         BNE   FNDR50                                                           
*                                                                               
         CLI   FL.TANPLEN,TANPLNQ3      TEST ELEMENT LENGTH                     
         BH    FNDR43                                                           
*                                                                               
         CLC   FL.TANPNWK,TP.TANPNWK    TEST NETWORK                            
         BNE   FNDR50                                                           
         CLC   FL.TANPPNME,TP.TANPPNME  AND PROGRAM NAME                        
         BNE   FNDR50                                                           
         B     FNDR60                   NO MEDIA TYPE TO TEST                   
*                                                                               
FNDR43   CLC   FL.TANPTYP,TP.TANPTYP    MATCH MEDIA TYPE                        
         BNE   FNDR50                                                           
*                                                                               
FNDR44   CLI   FL.TANPTYP,C'N'          TEST NETWORK                            
         BNE   FNDR46                                                           
         OC    FL.TANPNTI,SPACES        MAKE SURE SPACE FILLED                  
         OC    TP.TANPNTI,SPACES                                                
         CLC   FL.TANPNTI,TP.TANPNTI    MATCH NTI STATION                       
         BNE   FNDR50                                                           
*                                                                               
FNDR46   CLI   FL.TANPTYP,C'S'          TEST SPOT                               
         BNE   FNDR48                                                           
         OC    FL.TANPMKT,SPACES                                                
         OC    TP.TANPMKT,SPACES                                                
         CLC   FL.TANPMKT,TP.TANPMKT    MATCH MARKET                            
         BNE   FNDR50                                                           
*                                                                               
FNDR48   CLI   FL.TANPTYP,C'C'          TEST LOCAL CABLE                        
         BNE   FNDR60                                                           
         OC    FL.TANPSYS,SPACES                                                
         OC    TP.TANPSYS,SPACES                                                
         CLC   FL.TANPSYS,TP.TANPSYS    MATCH SYSCODE                           
         BE    FNDR60                                                           
*                                                                               
FNDR50   BRAS  RE,NEXTEL                                                        
         BE    FNDR42                                                           
                                                                                
* SEE IF THERE IS ANOTHER RECORD                                                
         GOTO1 SEQ                                                              
         CLC   KEY(30),KEYSAVE                                                  
         BE    FNDR22                                                           
         B     FNDERR3                                                          
         DROP  FL,TP                                                            
*                                                                               
FNDR60   TM    PROSTAT,PSNOPRT     TEST ONLY PRINT ERRORS                       
         JO    EXIT                                                             
*                                                                               
         MVC   PERR,SPACES                                                      
         BRAS  RE,BLIN                                                          
*                                                                               
         BRAS  RE,OUTPDOWN                                                      
         J     EXIT                                                             
*                                                                               
FNDERR1  MVC   PERR(24),=CL24'NO HOLD REC ON FILE'                              
         B     FNDERRX                                                          
*                                                                               
FNDERR2  MVC   PERR(24),=CL24'CMML NOT IN HOLD REC'                             
         B     FNDERRX                                                          
*                                                                               
FNDERR3  MVC   PERR(24),=CL24'STATION NOT IN HOLD REC'                          
*                                                                               
FNDERRX  BRAS  RE,BLIN                                                          
         BRAS  RE,OUTPDOWN                                                      
         OI    PROSTAT,PSERR       SET ERROR FLAG                               
         J     EXIT                                                             
         EJECT                                                                  
*================================================================               
* GET ALPHA MARKET FOR MARKET CODE AT 0(R4)                                     
*================================================================               
                                                                                
         USING TANPD,R6                                                         
GETALPHA NTR1                                                                   
K        USING TLMTD,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   K.TLMTCD,TLMTCDQ                                                 
         MVI   K.TLMTTYPE,C'T'                                                  
         MVC   K.TLMTCODE(4),TANPMKT                                            
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLMTINUM-TLMTD),KEYSAVE                                      
         BNE   GETALPHX                                                         
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         LR    R5,R6               SAVE TANPEL POINTER                          
         L     R6,AIO                                                           
         MVI   ELCODE,TASNELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TASND,R6                                                         
         MVC   TANPMKT-TANPD(4,R5),TASNAME                                      
*                                                                               
GETALPHX XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
BLIN     NTR1                                                                   
         MVC   PDATA(PERR-PDATA),SPACES                                         
*                                                                               
         LA    R6,RECIN                                                         
         USING TANXD,R6                                                         
*                                                                               
         MVC   PUSERID,SVWRKUID                                                 
         MVC   PAGY,TANXAGY                                                     
         MVC   PCMML(8),TANXNCID                                                
         MVC   PCMML+8(4),SPACES                                                
         TM    TANXSTAT,TANXPACK   TEST CMML IS PACKED                          
         BZ    BLIN2                                                            
         GOTO1 VTRPACK,DMCB,(C'U',TANXNCID),PCMML                               
*                                                                               
BLIN2    LLC   R0,TANXSEC                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSLN,DUB                                                         
*                                                                               
         MVC   PMED,TANXMED                                                     
*                                                                               
         MVC   PUSE,SVUSE                                                       
*                                                                               
         MVI   ELCODE,TACTELQ      FIND CLIENT CODE ELEMENT                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACTD,R6                                                         
         MVC   PCLI,TACTCLI                                                     
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,TAPRELQ                                                   
         LA    R6,RECIN                                                         
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPRD,R6                                                         
         MVC   PPRD,TAPRPRD                                                     
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,TANPELQ                                                   
         LA    R6,RECIN                                                         
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TANPD,R6                                                         
         GOTO1 DATCON,DMCB,(1,TANPDATE),(11,PDATE)                              
*                                                                               
         CLC   TANPPNME,SPACES                                                  
         BNH   BLIN10                                                           
         MVC   PPNME,TANPPNME  PROGRAM NAME                                     
         MVC   PNWK,TANPNWK    NETWORK                                          
*                                                                               
BLIN10   CLI   TANPLEN,TANPLNQ4    TEST ELEMENT LENGTH                          
         BL    BLIN20                                                           
         OC    PSTA,SPACES                                                      
         OC    PMKT,SPACES                                                      
         OC    PLCS,SPACES                                                      
*                                                                               
         CLI   TANPTYP,TANPNET                                                  
         BNE   *+14                                                             
         MVC   PSTA,TANPDATA   MOVE NET                                         
         B     BLIN20                                                           
*                                                                               
         CLI   TANPTYP,TANPSPT                                                  
         BNE   *+14                                                             
         MVC   PMKT,TANPDATA   MOVE MKT                                         
         B     BLIN20                                                           
*                                                                               
         CLI   TANPTYP,TANPCSYS                                                 
         BNE   *+10                                                             
         MVC   PLCS,TANPDATA   MOVE CBLNET                                      
*                                                                               
BLIN20   CLI   TANPAIR,0       ANY REASON NOT AIRED                             
         BE    BLINX                                                            
         MVC   PRNA,=CL10'PRE-EMPTED'                                           
         CLI   TANPAIR,TANPEMPT                                                 
         BE    BLINX                                                            
         MVC   PRNA,=CL10'MISSED'                                               
         CLI   TANPAIR,TANPMISD                                                 
         BE    BLINX                                                            
         MVC   PRNA,=CL10'DELETED'                                              
*                                                                               
BLINX    J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*========================================================                       
*        INITIALIZE DOWNLOAD SETTINGS                                           
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                                      
*========================================================                       
                                                                                
         USING DLCBD,R5                                                         
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
*===============================================                                
*        USER SUPPLIED PRINT ROUTINE                                            
*===============================================                                
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
                                                                                
*===========================================================                    
* OUTPUT FIELDS IN DOWNLOAD TABLE (DOWNFLDS)                                    
* FOR NOW, SINCE THERE'S ONLY ONE, IT WILL ALWAYS SEND IT                       
*                                                                               
*        LIST ENTRIES ARE  P1, B0    = TYPE TO PASS                             
*                              B1-B3 = ADDRESS OF DATA                          
*                          P2        = LENGTH                                   
*===========================================================                    
                                                                                
         USING DLCBD,R5                                                         
OUTPDOWN NTR1                                                                   
         LA    R5,DLCB                                                          
         LA    R4,DOWNCOLS                                                      
         CLI   DWNFLG,DWNFLCOL     DOWNLOAD COLUMN HEADERS?                     
         BE    OPD2                                                             
         LA    R4,DOWNFLDS         POINT TO FIELD LIST                          
*                                                                               
OPD2     MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R4)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         SR    RF,RF                                                            
         ICM   RF,7,1(R4)          RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R4)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
*                                                                               
OPD10    BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LA    RE,1                                                             
*                                                                               
OPD20    STC   RE,DLCBLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
*                                                                               
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R4,8(R4)            NEXT LIST ENTRY                              
         CLI   0(R4),X'FF'                                                      
         BNE   OPD2                                                             
*                                                                               
         BRAS  RE,EOLDOWN                                                       
*                                                                               
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
*=========================================================                      
*        FINISH LINE OF DOWNLOAD OUPUT                                          
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                                      
*=========================================================                      
                                                                                
         USING DLCBD,R5                                                         
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
*=========================================================                      
*        END DOWNLOAD                                                           
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                                      
*=========================================================                      
                                                                                
         USING DLCBD,R5                                                         
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
*        WORKING STORAGE                                                        
*=========================================================                      
                                                                                
         PRINT GEN                                                              
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,MACRF=(GM),EODAD=PROC100,        +        
               RECFM=VB,LRECL=2048                                              
*                                                                               
PDATA    DS    CL132                                                            
         ORG   PDATA                                                            
PUSERID  DS    CL10                                                             
PAGY     DS    CL6                                                              
PCMML    DS    CL15                                                             
PSLN     DS    CL3                                                              
PCLI     DS    CL6                                                              
PPRD     DS    CL6                                                              
PMED     DS    CL1                                                              
PUSE     DS    CL3                                                              
PSTA     DS    CL6                                                              
PMKT     DS    CL4                                                              
PLCS     DS    CL4                                                              
PDATE    DS    CL8                                                              
PPNME    DS    CL15                                                             
PNWK     DS    CL1                                                              
PRNA     DS    CL10                REASON NOT AIRED                             
PERR     DS    CL32                                                             
*                                                                               
         ORG   PDATA                                                            
PCUSERID DS    CL7                 USERID                                       
PCAGY    DS    CL6                 AGENCY                                       
PCCMML   DS    CL10                COMMERCIAL                                   
PCSLN    DS    CL6                 LENGTH                                       
PCCLI    DS    CL6                 CLIENT                                       
PCPRD    DS    CL7                 PRODUCT                                      
PCMED    DS    CL5                 MEDIA                                        
PCUSE    DS    CL3                 USE                                          
PCSTA    DS    CL7                 STATION                                      
PCMKT    DS    CL6                 MARKET                                       
PCLCS    DS    CL19                LOCAL CABLE STATION                          
PCDATE   DS    CL4                 DATE                                         
PCPNME   DS    CL12                PROGRAM NAME                                 
PCNWK    DS    CL7                 NETWORK                                      
PCRNA    DS    CL16                REASON NOT AIRED                             
PCERR    DS    CL5                 ERROR                                        
PCOLUMNL EQU   *-PCUSERID                                                       
*                                                                               
         ORG                                                                    
DOWNFLDS DS    0D                                                               
         DC    C'T',AL3(PUSERID),AL4(L'PUSERID)                                 
         DC    C'T',AL3(PAGY),AL4(L'PAGY)                                       
         DC    C'T',AL3(PCMML),AL4(L'PCMML)                                     
         DC    C'T',AL3(PSLN),AL4(L'PSLN)                                       
         DC    C'T',AL3(PCLI),AL4(L'PCLI)                                       
         DC    C'T',AL3(PPRD),AL4(L'PPRD)                                       
         DC    C'T',AL3(PMED),AL4(L'PMED)                                       
         DC    C'T',AL3(PUSE),AL4(L'PUSE)                                       
         DC    C'T',AL3(PSTA),AL4(L'PSTA)                                       
         DC    C'T',AL3(PMKT),AL4(L'PMKT)                                       
         DC    C'T',AL3(PLCS),AL4(L'PLCS)                                       
         DC    C'T',AL3(PDATE),AL4(L'PDATE)                                     
         DC    C'T',AL3(PPNME),AL4(L'PPNME)                                     
         DC    C'T',AL3(PNWK),AL4(L'PNWK)                                       
         DC    C'T',AL3(PRNA),AL4(L'PRNA)                                       
         DC    C'T',AL3(PERR),AL4(L'PERR)                                       
         DC    X'FF'                                                            
                                                                                
DOWNCOLS DS    0D                                                               
         DC    C'T',AL3(PCUSERID),AL4(L'PCUSERID)                               
         DC    C'T',AL3(PCAGY),AL4(L'PCAGY)                                     
         DC    C'T',AL3(PCCMML),AL4(L'PCCMML)                                   
         DC    C'T',AL3(PCSLN),AL4(L'PCSLN)                                     
         DC    C'T',AL3(PCCLI),AL4(L'PCCLI)                                     
         DC    C'T',AL3(PCPRD),AL4(L'PCPRD)                                     
         DC    C'T',AL3(PCMED),AL4(L'PCMED)                                     
         DC    C'T',AL3(PCUSE),AL4(L'PCUSE)                                     
         DC    C'T',AL3(PCSTA),AL4(L'PCSTA)                                     
         DC    C'T',AL3(PCMKT),AL4(L'PCMKT)                                     
         DC    C'T',AL3(PCLCS),AL4(L'PCLCS)                                     
         DC    C'T',AL3(PCDATE),AL4(L'PCDATE)                                   
         DC    C'T',AL3(PCPNME),AL4(L'PCPNME)                                   
         DC    C'T',AL3(PCNWK),AL4(L'PCNWK)                                     
         DC    C'T',AL3(PCRNA),AL4(L'PCRNA)                                     
         DC    C'T',AL3(PCERR),AL4(L'PCERR)                                     
         DC    X'FF'                                                            
                                                                                
                                                                                
MYD      DSECT                                                                  
SVSYSEL  DS    F                                                                
SVSYSKEY DS    XL32                KEY OF SYSTEM RECORD IN PROCESS              
SVWRKRID DS    XL16                KEY OF WORKER FILE                           
SVWRKUID DS    CL10                                                             
SAVEKEY  DS    XL32                                                             
PROSTAT  DS    XL1                                                              
PSTRACE  EQU   X'80'                                                            
PSNOPRT  EQU   X'40'               ONLY PRINT ERRORS                            
PSNOEML  EQU   X'20'               SUPPRESS EMAIL                               
PSERR    EQU   X'01'               ERROR HAS OCCURRED                           
SVUSE    DS    CL3                                                              
         DS    XL4                                                              
*                                                                               
AMCREMOT DS    A                   A(REMOTE)                                    
DWNFLG   DS    XL1                 DOWNLOAD FLAGS                               
DWNFLCOL EQU   1                   DOWNLOAD COLUMNS                             
DWNFLFLD EQU   2                   DOWNLOAD FIELDS                              
*                                                                               
         DS    0D                                                               
DLCB     DS    CL(DLCBXLX)       DOWNLOAD BLOCK                                 
         DS    0D                                                               
DYNALLOC DS    A                                                                
VTRPACK  DS    A                                                                
RECINLEN DS    F                                                                
RECIN    DS    XL2048            ENTRY READ FROM MVS SEQ DATASET                
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
*DDSMTPD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDSMTPD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP5E   10/04/16'                                      
         END                                                                    
