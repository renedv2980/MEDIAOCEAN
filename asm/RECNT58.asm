*          DATA SET RECNT58    AT LEVEL 120 AS OF 05/01/02                      
*PHASE T80258C,*                                                                
         TITLE 'RECNT58 (T80258) - CONTRACT TAKEOVER EDIT PROGRAM'              
*                                                                               
*********************************************************************           
*                                                                   *           
*  RECNT58 (T80258) - CONTRACT PROGRAM TAKEOVER VALIDATION          *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* REFER TO RECNTHIST FOR PAST HISTORY                               *           
*                                                                   *           
* JUL01/97 (BU ) --- INITIAL ENTRY                                  *           
* JUN28/00 (BU ) --- REMOVE REFERENCE TO GLV1GOTO PER MEL HERTZIG   *           
* JAN30/01 (HWO) --- IGNORE AGENCY OFFICE WHEN READING DARE         *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T80258   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,T80258,R9                                                      
         L     RC,0(R1)            WORK AREA                                    
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         GOTO1 =A(CHK9EKEY),DMCB,(RC),RR=Y                                      
*                                  CHECK:  ORDER ALREADY TRANSFERRED?           
         BZ    MAIN0020            NO  - CONTINUE                               
         LA    R2,CONCNUMH         SET CURSOR ADDRESS                           
         LA    R3,NOREXFER         SET ERROR CODE:  ALREADY XFERRED             
         B     ERROR               ERROR RETURN:  EXIT                          
NOREXFER EQU   713                                                              
MAIN0020 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
*                                  SET UP/READ SOURCE CONTRACT                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           INSERT RECORD TYPE                           
         MVC   KEY+21(2),TWATKSRP  INSERT SOURCE REP CODE                       
***                                COMPLEMENT CONTRACT NUMBER                   
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),TWATKCON(4)                                           
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+23(4),WORK+15   INSERT COMP CON# INTO KEY                    
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 KEY FOUND?                                   
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                  RETRIEVE ORIGINAL CONTRACT RECORD            
*                                  RETURN TO ORIGINAL FILE FOR                  
*                                     MODIFIED SCREEN VALIDATION                
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                                                               
AUTOD    USING RCAUTOD,TWAGENBK                                                 
*                                                                               
*                                                                               
*        FOUT  CONAGYNH,MYSPACES,20                                             
*        FOUT  CONADVNH,MYSPACES,20                                             
*        FOUT  CONSTAMH,MYSPACES,20                                             
*        FOUT  CONSALNH,MYSPACES,19                                             
*        FOUT  CONOFFNH,MYSPACES,19                                             
*        FOUT  CONDSPNH,MYSPACES,15                                             
*        FOUT  CONDCTNH,MYSPACES,15                                             
         FOUT  CONBNUMH,MYSPACES,8                                              
         OI    CONBNUMH+4,X'20'                                                 
*                                                                               
*              VALIDATE 'AGENCY(OFFICE) CODE                                    
AGYED    EQU   *                                                                
         GOTO1 =A(AGYEDMOD),DMCB,(RC),RR=Y                                      
         BZ    ADVED               OKAY:  CONTINUE                              
         L     R2,DUB              RESET CURSOR ADDRESS                         
         L     R3,DUB+4            RESET ERROR CODE                             
         B     ERROR               ERROR RETURN:  EXIT                          
*                                                                               
         EJECT                                                                  
*                                                                               
*              VALIDATE ADVERTISER                                              
*                                                                               
ADVED    EQU   *                                                                
         MVC   SVCATADV,MYSPACES   CLEAR ADVERTISER CATEGORY                    
*        GOTO1 =A(CHKDARE),DMCB,(RC),RR=Y                                       
*                                  DARE ORDER?                                  
*        BZ    STAED               YES - SKIP EDIT TESTING                      
         LA    R2,CONADVH                                                       
         LA    R3,ADVERR                                                        
         MVC   SVADV,RCONKADV      SAVE LATER FOR PRODUCT LOCK                  
         TM    4(R2),X'20'         VALID ADVERTISER?                            
         BZ    ADVE0060                                                         
*                                                                               
         TM    CONPRDH+4,X'20'     PRD CODE CHANGED?                            
         BZ    ADVE0020                                                         
         TM    CONCATH+4,X'20'     CAT CODE CHANGED?                            
         BO    STAED               NEED ADV CAT CODE                            
*                                                                               
ADVE0020 GOTO1 VMOVE                                                            
         B     ADVE0080                                                         
*                                                                               
*              VALIDATE ADVERTISER                                              
ADVE0060 NI    CONPRDH+4,X'DF'     MAKE SURE PRODUCT GETS REVALIDATED           
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         GOTO1 VMOVE                                                            
ADVE0080 XC    IOAREA(32),IOAREA                                                
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   ADVE0100                                                         
         OI    6(R2),X'40'+X'80'   CURSOR HERE ON RETURN                        
         GOTO1 =A(GOOBR),DMCB,(RC),C'ADV ',(7,9(R2)),0,RR=Y                     
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
ADVE0100 DS    0H                                                               
         MVI   RADVKTYP,8          RECORD TYPE FOR ADVERTISER                   
         MVC   RADVKADV,WORK                                                    
         MVC   RADVKREP,REPALPHA                                                
         CLC   =C'V4',REPALPHA     FOX NETWORK:  TEST REP?                      
         BE    ADVE0120            NO  - DON'T CHECK FOR LOCKOUT                
         CLC   =C'FN',REPALPHA     FOX NETWORK?                                 
         BNE   ADVE0180            NO  - DON'T CHECK FOR LOCKOUT                
ADVE0120 EQU   *                                                                
         LA    R3,SPECCHR2                                                      
         B     ADVE0140                                                         
SPECCHR2 DC    C'#$%=*+!@><()-":'                                               
         DS    0F                                                               
ADVE0140 EQU   *                                                                
         CLI   0(R3),X'FF'         DELIMITER REACHED?                           
****     BE    ADVE0180            YES - NOT SPECIAL CHARACTER                  
*                                                                               
         B     ADVE0180            PERMIT ALL USE                               
*                                                                               
         CLC   RADVKADV+3(1),0(R3)                                              
*                                  SPECIAL CHARACTER FOUND?                     
         BE    ADVE0160            YES - SEND BACK MESSAGE                      
         LA    R3,1(R3)                                                         
         B     ADVE0140            GO BACK FOR NEXT CODE                        
ADVE0160 EQU   *                                                                
         LA    R3,NOCONCDE         SET ERROR MESSAGE                            
         B     ERROR                                                            
ADVE0180 EQU   *                                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
*                                                                               
*        CLC   KEY(25),KEYSAVE                                                  
*        BNE   ERROR                                                            
*        CLC   KEY+25(2),KEYSAVE+25                                             
*        BE    ADVE0200                                                         
*        CLC   KEY+25(2),=C'ZZ'                                                 
*        BE    ADVE0200                                                         
*        MVC   KEY+25(2),=C'ZZ'                                                 
*        GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
ADVE0200 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   WADVEXP,RADVNAME                                                 
         MVC   SVCATADV,RADVCATG   SAVE CAT CODE FOR CATED VALIDATION           
         FOUT  CONADVNH,RADVNAME,20     ADVERTISER NAME                         
*                                                                               
         MVC   RCONKADV,RADVKADV   TO K REC                                     
*                                                                               
* DELETE EXTENDED ADVERTISER NAME FOR KATZ ADDED DURING CONVERSION              
*                                                                               
         TM    RCONMODR+1,X'10'    FOR KATZ CONVERTED CONTRACT ONLY             
         BZ    STAED                                                            
         GOTO1 VDELELEM,DMCB,(X'6F',RCONREC)                                    
         EJECT                                                                  
STAED    EQU   *                                                                
**                                                                              
         B     PRDED               STATION EDIT HAS BEEN REMOVED                
**                                                                              
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
PRDED    LA    R2,CONPRDH                                                       
         CLC   8(2,R2),=C'C='                                                   
         BNE   PRD10                                                            
*                                                                               
         GOTO1 =A(VALAGFD),DMCB,(RC),RR=Y                                       
         LA    R2,CONPRDH                                                       
*                                                                               
         TM    PROFILES+CNTKTYPB,CNTKTYPA                                       
         BZ    PRD30                                                            
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    PRD05                                                            
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    PRD30               SKIP REQUIRED FIELD FOR AUTOHEADER?          
PRD05    CLI   CONTYPE,C'D'                                                     
         BE    PRD30                                                            
         CLI   CONTYPE,C'N'                                                     
         BE    PRD30                                                            
         CLI   CONTYPE,C'X'                                                     
         BE    PRD30                                                            
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTYPE?                    
         BE    PRD07               NO                                           
         CLC   CONTYPE,TWARTS      YES - CONTYPE = OPTIONAL TYPE?               
         BE    PRD30                                                            
PRD07    EQU   *                                                                
         LA    R3,442              PRD CODE ALLOWED ONLY FOR TYPES              
         B     ERROR                 D, N OR X                                  
                                                                                
PRD10    DS    0H                                                               
         TM    RCONMODR+1,X'10'    KATZ CONVERTED ORDER?                        
         BO    PRD20               YES - DON'T CHECK FOR TYPE                   
         LA    R3,67               MUST USE PRD CDE WHEN TYPE=N OR X            
*                                     OR OPTIONAL RTS TYPE                      
         LA    R4,CONTYPEH                                                      
         CLI   8(R4),C'X'                                                       
         BE    ERROR                                                            
         CLI   8(R4),C'N'                                                       
         BE    ERROR                                                            
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTYPE?                    
         BE    PRD20               NO                                           
         CLC   8(1,R4),TWARTS      YES - SCREEN TYPE = OPTIONAL TYPE?           
         BE    ERROR               YES - MUST USE PRODUCT CODE                  
*                                                                               
PRD20    DS    0H                                                               
         CLI   8(R4),C'D'                                                       
         BNE   PRD30                                                            
         TM    PROFILES+CNTPRD1B,CNTPRD1A   PRD CDE REQUIRED FOR TYPE=D         
         BZ    PRD30               NO                                           
         LA    R3,263              MUST USE PRODUCT CODE WHEN TYPE=D            
         B     ERROR                                                            
*                                                                               
PRD30    LA    R3,PRDERR                                                        
         MVC   SVPRD,RCONPRD       SAVE FOR PRODUCT LOCK                        
         TM    4(R2),X'20'         VALID?                                       
         BO    PRD40                                                            
****>>>> MVI   UPVER,1    ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM            
         B     PRD45               REVALIDATE IF CATG CHANGED                   
PRD40    TM    CONCATH+4,X'20'                                                  
         BZ    PRD45                                                            
         TM    CONADVH+4,X'20'     OR IF ADVERTISER CHANGED                     
         BO    SALED                                                            
PRD45    DS    0H                                                               
         NI    CONCATH+4,X'DF'     REVALIDATE CATG IF PRD CHANGED               
         GOTO1 VMOVE                                                            
         GOTO1 VDELELEM,DMCB,(5,RCONREC)     DELETE ELEM (FOR CHANGE)           
* CHECK FOR PRD CODE                                                            
         CLC   WORK(2),=C'C='                                                   
         BNE   PRD100                                                           
*                                                                               
* VALIDATE ACTIVE STATUS OF POINT PERSON                                        
*                                                                               
PPRSNACT EQU   460                                                              
*                                                                               
         XC    IOAREA(32),IOAREA   RETRIEVE THE PRODUCT RECORD                  
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV   ADVERTISER                                   
         MVC   RPRDKPRD,WORK+2                                                  
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR               PRODUCT RECORD NOT ON FILE                   
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  PROD FOUND - CHANGE ERROR MESSAGE            
         LA    R3,PPRSNACT         POINT PERSON INACTIVE                        
         GOTO1 =A(POINTPSN),DMCB,(RC),RR=Y                                      
         BNZ   ERROR               POINT PERSON IS INACTIVE                     
*                                  RETRIEVE PROD REC FOR UPDATE                 
         XC    IOAREA(32),IOAREA                                                
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV   ADVERTISER                                   
         MVC   RPRDKPRD,WORK+2                                                  
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH               RECORD WILL BE FOUND                         
*                                                                               
PRD50    MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   SVCAT,RPRDCATG      NEED PRD CAT FOR LATER                       
*                                  PRD LOCK ROUTINE KILLS CATE                  
         MVC   WPRDEXP,RPRDNAME                                                 
         MVC   TWAPRDNM,RPRDNAME                                                
*                                                                               
         MVC   RCONPRD,WORK+2                                                   
         CLI   RCONTYPE,C'X'                                                    
         BE    PRD52                                                            
         CLI   RCONTYPE,C'N'                                                    
         BE    PRD52                                                            
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    PRD55               NO                                           
         CLC   RCONTYPE,TWARTS     CONTYPE = OPTIONAL CONTYPE?                  
         BNE   PRD55               NO                                           
PRD52    EQU   *                                                                
         MVI   ELCODE,X'03'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BE    PRD55                                                            
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BZ    PRD60                                                            
         LA    R3,265                                                           
         B     ERROR                                                            
*                                                                               
         USING RPRDSPOT,R6                                                      
PRD55    MVC   TWASPCL,RPRDSPCL    SPOTPAK CLIENT CODE                          
         MVC   TWASPPD,RPRDSPP1    SPOTPAK PRODUCT CODE                         
         MVC   TWASPPP,RPRDSPP2    SPOTPAK PIGGY BACK RODUCT                    
         MVC   TWASPP1,RPRDSPS1    SPOTPAK PRODUCT 1 SPLIT                      
         MVC   TWASPP2,RPRDSPS2    SPOTPAK PRODUCT 2 SPLIT                      
         MVC   TWASPES,RPRDSPES    SPOTPAK ESTIMATE NUMBER                      
         DROP  R6                                                               
*                                                                               
PRD60    OC    CONPRD(6),MYSPACES                                               
         MVC   CONPRD+6(14),RPRDNAME                                            
         FOUT  CONPRDH                                                          
*                                                                               
         ZICM  RF,RPRDLOCK,2       INCREMENT LOCK COUNTER                       
*                                                                               
         CLI   TWACOMBO,0          IF COMBO ORDER, INCREMENT BY THE             
         BE    PRD70               NUMBER OF CONTRACTS THAT WILL BE             
         ZIC   RE,TWACOMBO         USING THIS PRODUCT CODE                      
         AR    RF,RE                                                            
         B     PRD75                                                            
*                                                                               
PRD70    LA    RF,1(RF)                                                         
PRD75    STCM  RF,3,RPRDLOCK                                                    
         GOTO1 VPUTREC,DMCB,RPRDREC   UPDATE LOCK COUNTER IN PRD REC            
         GOTO1 =A(PRDLOCK),DMCB,(RC),RR=Y                                       
*                                                                               
* GET DAYPART MENU                                                              
*                                                                               
         L     R6,AIO4                                                          
         GOTO1 VREGENDP,DMCB,RPRDREC,(R6),DATAMGR,ACOMFACS,CALLOV,     X        
               VMEDGET                                                          
         MVC   TWADAYPT(L'TWADAYPT),0(R6)                                       
*                                                                               
         B     CATED                                                            
*                                                                               
* NO CODE - JUST USE PRODUCT INPUT                                              
PRD100   LA    R3,1                ERROR, MISSING INPUT FIELD                   
         CLC   WORK(20),MYSPACES   FORCE SOME INPUT                             
         BE    ERROR                                                            
         MVC   WORK2+2(20),WORK    PRODUCT                                      
         MVC   WORK2(2),=X'0516'   EXPANSION ELEM CODE + LENGTH                 
*                                                                               
*              ADD PRODUCT EXPANSION ELEMENT                                    
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2   IN BASE                            
         MVC   RCONPRD,MYSPACES                                                 
         EJECT                                                                  
* VALIDATE CATEGORY                                                             
CATED    EQU   *                                                                
         LA    R2,CONCATH                                                       
         LA    R3,CATERR                                                        
         TM    4(R2),X'20'                                                      
         BO    SALED                                                            
* IF CATEGORY CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS            
         GOTO1 VMOVE                                                            
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,REPALPHA                                                
         MVC   RCTGKCTG,WORK                                                    
*                                                                               
         CLC   RCTGKCTG,MYSPACES                                                
         BNE   CATED05                                                          
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    CATED05                                                          
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    SALED               SKIP REQUIRED FIELD FOR AUTOHEADER?          
*                                                                               
CATED05  DS    0H                                                               
         CLC   RCTGKCTG,=C'XX'                                                  
         BE    ERROR                                                            
*                                                                               
         CLC   CONPRD(2),=C'C='    CHECK IF PRD CODE                            
         BNE   CATED10                                                          
         CLC   SVCAT,MYSPACES      YES, CHECK IF PRD PROVIDES A CATG CD         
         BE    CATED10                                                          
         MVC   RCTGKCTG,SVCAT      IF AVAILABLE,                                
         B     CATED20                PRD CODE ALWAYS OVERRIDE                  
*                                                                               
CATED10  CLC   SVCATADV,MYSPACES   CHECK IF ADV PROVIDES A CATG CODE            
         BE    CATED20                                                          
         OC    SVCATADV,SVCATADV                                                
         BZ    CATED20                                                          
         MVC   RCTGKCTG,SVCATADV                                                
*                                                                               
CATED20  MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   RCONCTGY,RCTGKCTG   CATEGORY                                     
         EJECT                                                                  
*              VALIDATE SALESMAN                                                
SALED    LA    R2,CONSALH                                                       
         LA    R3,SALERR                                                        
         TM    4(R2),X'20'         VALID SALESMAN?                              
         BO    RTGED               YES - NO CHECKING                            
*                                                                               
* IF SALESMAN CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS            
*                                                                               
*&&DO                                                                           
         GOTO1 =A(PROCHK),RR=Y                                                  
         BNE   *+12                                                             
         LA    R3,563                                                           
         B     ERROR               NO CHANGE W/PROPOSALS                        
*&&                                                                             
         GOTO1 VMOVE                                                            
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   SALED05                                                          
         OI    6(R2),X'40'+X'80'   CURSOR HERE ON RETURN                        
         GOTO1 =A(GOOBR),DMCB,(RC),C'SAL ',(2,9(R2)),(X'80',0),RR=Y             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
SALED05  DS    0H                                                               
*                                                                               
         MVI   RSALKTYP,6          SALESMAN REC TYPE                            
         MVC   RSALKREP,REPALPHA   REP CODE                                     
         MVC   RSALKSAL,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWASALTL,RSALTEL                                                 
         MVC   WSALEXP,RSALNAME                                                 
         FOUT  CONSALNH,RSALNAME,19                                             
*                                  SALESMAN'S NAME                              
*                                                                               
         MVC   RCONTEM,RSALTEAM    TEAM                                         
         MVC   RCONSAL,RSALKSAL    SALESMAN'S CODE                              
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(L'RSALOFF),RSALOFF                                          
*                                                                               
*  TEMPORARILY BYPASS THE OFFTEAM TESTING FOR KATZ TV.                          
*  TEMPORARILY BYPASS THE OFFTEAM TESTING FOR TEST FILES (KATZ TV)              
*                                                                               
         CLC   =C'AM',REPALPHA     KATZ AMERICAN?                               
         BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
         CLC   =C'CQ',REPALPHA     KATZ CONTINENTAL?                            
         BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
         CLC   =C'NK',REPALPHA     KATZ NATIONAL?                               
         BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
*        CLC   =C'TV',REPALPHA     KATZ TV TEST FILES?                          
*        BE    SALED30             YES - DON'T TEST OFF/TEAM SETUP              
*                                                                               
* VALIDATE IF SALESPERSON'S OFF/TEAM CODE MATCHES THE STATION'S                 
*                                                                               
         OC    TWASTOTC,TWASTOTC   THERE ARE NONE, THEN SKIP                    
         BZ    SALED30                                                          
         LA    R4,15               15 SETS MAXIMUM                              
         LA    R5,TWASTOTC                                                      
*                                                                               
SALED10  CLC   RSALOFF,0(R5)       IF OFFICE IS NOT IN                          
         BE    SALED15             OFFTEAM LIST, SKIP VALIDATION                
         LA    R5,4(R5)                                                         
         BCT   R4,SALED10                                                       
         B     SALED30                                                          
*                                                                               
SALED15  DS    0H                  OFFICE IS IN OFFTEAM LIST,                   
         CLC   RSALOFF,0(R5)       VALIDATE TEAM                                
         BNE   SALED20                                                          
         CLC   RSALTEAM,2(R5)                                                   
         BE    SALED30             SALESPERSON AND STATION OFF/TEAM             
*                                                                               
SALED20  LA    R5,4(R5)                                                         
         BCT   R4,SALED15                                                       
         LA    R3,288                                                           
         B     ERROR               CODE MISMATCH                                
*                                                                               
SALED30  EQU   *                                                                
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    OFFED               NO  - SALESPERSON STILL ACTIVE               
         GOTO1 DATCON,DMCB,(5,FULL),(3,FULL)                                    
*                                  YES - CHECK AGAINST TODAY'S DATE             
         CLC   RSALLEAV,FULL       LEAVE DATE VS TODAY'S DATE                   
         BH    OFFED               NOT GONE YET: LD > TD                        
         LA    R3,SALLEFT          GONE - REJECT ENTRY                          
         B     ERROR               GO TO ERROR RTN                              
*                                                                               
SALLEFT  EQU   407                 SALESPERSON GONE ERROR CODE                  
         EJECT                                                                  
OFFED    DS    0H                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   ROFFKTYP,4          OFFICE REC TYPE                              
         MVC   ROFFKREP,REPALPHA   REP CODE                                     
         MVC   ROFFKOFF,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    OFF20                                                            
         CLC   TWAACCS(2),=C'O='   TEST FOR OFFICE RESTRICTION                  
         BNE   OFF20                                                            
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    OFF20               TO ALL OFFICES                               
         CLC   ROFFKOFF,TWAACCS+2  ELSE, COMPARE OFFICES                        
         BE    OFF20                                                            
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         B     ERROR                                                            
*                                                                               
OFF20    GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAOFAD1,ROFFADD1                                                
         MVC   TWAOFAD2,ROFFADD2                                                
         MVC   TWAOFSTT,ROFFSTT                                                 
         MVC   TWAOFZIP,ROFFZIP                                                 
         MVC   WOFFEXP,ROFFNAME                                                 
*                                                                               
         FOUT  CONOFFNH,ROFFNAME,19                                             
*                                                                               
         MVC   WORK(2),RCONKOFF                                                 
         MVC   RCONKOFF,ROFFKOFF                                                
         CLC   RCONKOFF,WORK       DIFFERENT OFFICE??                           
         BE    RTGED               NEED TO CHANGE MAKEGOOD OFFER KEYS           
*                                                                               
         GOTO1 =A(CHGMGKEY),DMCB,(RC),RR=Y                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*              VALIDATE RATING SOURCE                                           
RTGED    LA    R2,CONRTGH                                                       
         TM    4(R2),X'20'         VALID RATING SERVICE?                        
         BO    RTG100                                                           
                                                                                
         CLI   5(R2),0             IF FIELD HAS NO INPUT                        
         BNE   RTG10                                                            
         MVC   CONRTG(3),=C'NSI'                                                
         TM    TWATIME,X'40'       IF REP PROFILE DICTATES NSI AS               
         BZ    RTG05               DEFAULT RATING SERVICE, USE NSI              
         MVC   CONRTG(3),=C'ARB'   IF ON, USE ARB                               
                                                                                
RTG05    DS    0H                                                               
         MVI   CONRTGH+5,3                                                      
         OI    CONRTGH+6,X'80'     XMIT                                         
                                                                                
* IF RATING SRC CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS          
RTG10    DS    0H                                                               
         GOTO1 VANY                                                             
         LA    R3,RTGERR                                                        
         MVI   RCONRTGS,0                                                       
         MVI   RCONRMON,0                                                       
         SPACE 1                                                                
         CLC   CONRTG(3),=C'NON'                                                
         BE    RTG100                                                           
*                                                                               
*- VALIDATE RATING SORCE & SAVE IN 'RCONRTGS'                                   
*  (ONLY STORE 3 CHARACTER RATING SOURCE IN RECORD)                             
*                                                                               
*  1) DETERMINE LENGTH OF RATING SOURCE (LEN .GT. 3 = ERROR)                    
*                                                                               
*  2) CHECK FOR VALID SOURCE VIA TABLE LOOKUP.                                  
*     ACCEPT IF SOURCE MATCHES ENTRY & MEETS MINIMUM COMPARE LENGTH.            
*     (FUTURE CONSIDERATION J.I.C. 1 CHARACTER ISN'T UNIQUE)                    
*                                                                               
*     THIS METHOD ACCEPTS 'A', 'AR', OR 'ARB' AS VALID                          
*                                                                               
*  3) SAVE 1 BYTE RATING SOURCE IN RECORD.                                      
*                                                                               
         SR    R6,R6               PUT RTG SOURCE LEN HERE                      
         IC    R6,5(R2)            INPUT LENGTH                                 
*                                                                               
         CLI   5(R2),3             MAX INPUT LENGTH                             
         BH    ERROR                                                            
*                                                                               
*- R6 = RTG SOURCE LENGTH.  LOOK FOR MATCH IN TABLE IF LENGTH                   
*  MEETS OR EXCEEDS MINIMUM TABLE MATCH LENGTH.                                 
*                                                                               
*  IF VALID RTG SOURCE FOUND, SAVE 3 BYTE SOURCE IN RECORD.                     
*  ELSE ERROR.                                                                  
RTG20    EQU   *                                                                
         LR    RE,R6               RE = RTG SOURCE LEN - 1                      
         BCTR  RE,0                                                             
*                                                                               
         LA    R5,RTGSTBL          TV RTG SOURCE TABLE                          
*                                                                               
         CLI   RCONKSTA+4,C' '     BLANK = TV                                   
         BE    RTG30                                                            
         CLI   RCONKSTA+4,C'L'     LOW POWER = TV                               
         BE    RTG30                                                            
*                                                                               
         LA    R5,RRTGSTBL         RADIO RATING SOURCE TABLE                    
*                                                                               
RTG30    CLI   0(R5),X'00'         EOT?                                         
         BE    ERROR               RTG SOURCE IS INVALID                        
*                                                                               
         SR    RF,RF               MEET MINIMUM LENGTH?                         
         IC    RF,0(R5)                                                         
         CR    R6,RF                                                            
         BL    RTG40               SCREEN SOURCE TOO SMALL.                     
*                                                                               
         EX    RE,RTG50            TBL ENTRY -VS- SCREEN COMPARE                
         BNE   RTG40               NO HIT                                       
*                                                                               
         MVC   RCONRTGS(1),1(R5)   SAVE RTG SOURCE IN REC                       
         B     RTG100                                                           
*                                                                               
RTG40    LA    R5,LRTGSTBL(R5)     POINT TO NEXT TBL ENTRY                      
         B     RTG30                                                            
*                                                                               
RTG50    CLC   1(0,R5),8(R2)       RTGSTBL ENTRY -VS- SCREEN FLD                
*                                                                               
*- RATING SOURCE TABLE.  END WITH BYTE OF X'00'   LAYOUT:                       
*    XL1'MINIMUM COMPARE LENGTH FOR ENTRY'                                      
*    CL3'RATING SOURCE CODE'                                                    
*                                                                               
RTGSTBL  EQU   *                   TELEVISION TABLE                             
         DC    X'01',CL3'ARB'      ARBITRON                                     
LRTGSTBL EQU   *-RTGSTBL           <-- ENTRY LENGTH                             
         DC    X'01',CL3'NSI'      NIELSEN                                      
         DC    X'01',CL3'SRC'      SRC                                          
         DC    X'00'               EOT                                          
         SPACE                                                                  
RRTGSTBL EQU   *                   RADIO TABLE                                  
         DC    X'01',CL3'ARB'      ARBITRON                                     
         DC    X'01',CL3'BIR'      BIRCH                                        
         DC    X'00'               EOT                                          
*                                                                               
RTG100   DS    0H                                                               
         EJECT                                                                  
*                                                                               
*  VALIDATE COMMENTS                                                            
COMEDT   EQU   *                                                                
**                                                                              
         B     END5                SKIP COMMENT EDIT                            
**                                                                              
         GOTO1 =A(CMTRNT),DMCB,(RC),RR=Y                                        
ENDEDIT  EQU   *                                                                
* CHECK STATION INVOICE CLOSE MONTH - ADD 0 INV BUCKET IF MONTH CLOSED          
END5     EQU   *                                                                
**                                                                              
         B     END50                                                            
**                                                                              
         OC    TWASTCDT,TWASTCDT   NO CLOSE DATE?                               
         BZ    END50                                                            
*                                                                               
         CLI   TWACOMBO,0          IF COMBO, DON'T CHECK FOR STATION            
         BNE   END50               CLOSE OUT.  IT'LL BE CHECKED LATER           
*                                  IN ADDCON                                    
* CHECK K START DATE                                                            
         CLC   RCONDATE(2),TWASTCDT                                             
         BH    END50                                                            
* CHECK MONTHS OF K                                                             
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
         XC    HALF,HALF                                                        
*                                                                               
* BUILD ZERO INVOICE BUCKET IN WORK2                                            
         MVC   WORK2(2),=X'040A'   ELEM CODE + LEN                              
         MVC   WORK2+4(2),MONDATE  ACTIVITY DATE                                
         XC    WORK2+6(4),WORK2+6                                               
*                                                                               
* GET BROADCAST MONTH                                                           
END10    GOTO1 VGTBROAD,(R1),(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 DATCON,(R1),WORK+18,(3,FULL)                                     
         CLC   FULL(2),TWASTCDT                                                 
         BH    END50                                                            
         OC    HALF,HALF      FIRST TIME?                                       
         BZ    END15                                                            
*                                                                               
         CLC   HALF,FULL           SAME MONTH?                                  
         BE    END40                                                            
*                                                                               
END15    MVC   HALF,FULL           SAVE                                         
         MVC   WORK2+2(2),FULL                                                  
* NEXT ELEM                                                                     
END30    EQU   *                   LOOK FOR AN INV EL FOR THIS MONTH            
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(X'04',RCONREC),          X        
               (2,WORK2+2)                                                      
         CLI   DMCB+12,0           FOUND?                                       
         BE    END40               DON'T PUT DUP ELEM                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RCONREC),(0,WORK2)              
* GET NEXT WEEK                                                                 
END40    EQU   *                                                                
         CLC   WORK(4),WORK+6                                                   
         BE    END50                                                            
         CLC   WORK2+2(2),TWASTCDT                                              
         BE    END50                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+24,7                                        
         MVC   WORK(6),WORK+24                                                  
         B     END10                                                            
*                                                                               
END50    EQU   *                                                                
         SPACE 1                                                                
         EJECT                                                                  
         TITLE 'ADD CONTRACT RECORD'                                            
ADDCON   DS    0H                                                               
*                                                                               
ADDC3    DS    0H                                                               
*        MVI   RCONKTYP,12         CONTRACT RECORD TYPE                         
*        MVC   RCONKREP,REPALPHA   ALPHA REP CODE                               
*        MVC   RCONCREA,TODAY      CREATION OR BUYLINE 1 ADDED DATE             
*        MVC   RCONHDRD,TODAY      HEADER CREATION DATE (NEVER CHANGED)         
*        TM    RCONMODR+1,X'C0'    ONLY ACE/GRAPHNET SET MOD TO -1              
*        BZ    *+8                                                              
*        MVI   RCONMOD,X'FF'                                                    
*        OI    TAREQ,1             T/A REQ IND FOR BASE                         
*                                                                               
*              GET NEXT REP CONTRACT NUMBER                                     
         ZAP   WORK(5),=P'99999999'                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           PASSIVE POINTER KEY TYPE                     
         MVC   KEY+21(2),REPALPHA  ALPHA REP CODE                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(23),KEYSAVE     SAME REP?                                    
         BNE   *+10                                                             
*                                                                               
*                                  GET NEXT CONTRACT NUMBER                     
*                                                                               
         MVO   WORK+5(5),KEY+23(4) K NUMBER                                     
         SP    WORK(5),WORK+5(5)   GET POSITIVE                                 
         AP    WORK(5),=P'1'       NEXT K NUMBER                                
         MVO   WORK+10(5),WORK(5)                                               
         MVC   RCONKCON,WORK+10    TO K KEY                                     
         SPACE                                                                  
ADDCON3  OC    TWAXCON,TWAXCON                                                  
         BNZ   *+10                                                             
         MVC   TWAXCON,RCONKCON                                                 
*                                                                               
ADDCON10 DC    0H'0'                                                            
*                                                                               
* FOR ACE/GRAPHNET CONTRACTS, MARK UNCONFIRMED, & BUILD X'20' SEND EL           
*                                                                               
*        TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
*        BZ    ADDCON30                                                         
*        SPACE 1                                                                
*        LA    R6,RCONREC                                                       
*        MVI   ELCODE,X'1F'                                                     
*        BAS   RE,GETEL                                                         
*        BNE   ADDCON15                                                         
*        SPACE 1                                                                
*        USING RCONXEL,R6          EXTENDED DESCRIPTION ELEMENT                 
*        OI    RCONCONF,X'80'      UNCONFIRMED                                  
*        TM    TWASTAST,X'02'      IS 'DON'T SEND' ALLOWED ON STATION           
*        BZ    *+8                                                              
*        OI    RCONSTAT,X'02'      YES                                          
*        DROP  R6                                                               
*        B     ADDCON20                                                         
*        SPACE 1                                                                
*ADDCON15 XC    WORK2,WORK2         BUILD NEW ELEM. IN WORK2                    
*        MVC   WORK2(2),=X'1F18'                                                
*        OI    WORK2+6,X'80'       UNCONFIRMED                                  
*        TM    TWASTAST,X'02'      IS 'DON'T SEND' ALLOWED ON STATION           
*        BZ    *+8                                                              
*        OI    WORK2+7,X'02'       YES                                          
*        GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*        SPACE 1                                                                
*ADDCON20 DC    0H'0'                ADD SEND ELEMENT                           
*        XC    WORK2(RCONSNLQ),WORK2                                            
*        MVI   WORK2,X'20'                                                      
*        MVI   WORK2+1,RCONSN2Q    NEW ELEMENT LENGTH                           
*        MVI   WORK2+4,X'10'       START STA VERS. NOT ADVANCED                 
*        MVI   WORK2+5,1           SET REP VERSION NUMBER TO 1                  
*        MVI   WORK2+14,0          SET STATION VER. NUMBER TO 0                 
*        GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         EJECT                                                                  
**********************************************************************          
* NEW COMBO CONTRACT WILL LOOP TO ADD THE NUMBER OF COMPONENT CONTRACTS         
* SPECIFIED.  THE X'17' COMBO INFO ELEMENT WILL BE ADDED TO ALL KS.             
* ALSO, CONTRACTS WILL BE ADDED HIGHEST # FIRST TO INSURED CONSECUTIVE          
* CONTRACT #S.                                                                  
**********************************************************************          
ADDCON30 DS    0H                                                               
*        OC    TWACOMBO,TWACOMBO   CHECK IF COMBO CONTRACT                      
*        BZ    ADDCON70                                                         
*                                                                               
*        XC    WORK2,WORK2         YES, ADD COMBO INFO ELEMENT                  
*        MVI   WORK2,X'17'         ELEMENT CODE                                 
*                                                                               
*        ZIC   R4,TWACOMBO         COMPUTE ELEMENT LENGTH                       
*        MH    R4,=H'9'            # OF STATIONS * 9 (LETTER + MED)             
*        LA    R4,2(R4)            PLUS 2 FOR CODE AND LENGTH                   
*        STC   R4,WORK2+1                                                       
*                                                                               
*        LA    R4,WORK2+2          BUILD REST OF ELEMENT                        
*        LA    R2,CONCMBSH                                                      
*                                                                               
*ADDCON35 MVC   0(4,R4),8(R2)       CALL LETTERS                                
*        MVC   4(1,R4),13(R2)      MEDIA                                        
*                                                                               
*        ZIC   R0,0(R2)            POINT TO SELECTION FIELD                     
*        AR    R2,R0                                                            
*                                                                               
*        CLI   8(R2),C'X'          IF STATION SELECTED, INCLUDE IN ELEM         
*        BE    ADDCON40                                                         
*                                                                               
*        B     ADDCON45                                                         
*                                                                               
ADDCON40 DS    0H                  GENERATE CONT # FOR EACH COMBO STA           
*        MVC   5(4,R4),RCONKCON                                                 
*        LA    R4,9(R4)                                                         
*                                                                               
*        ZAP   WORK(5),=P'0'                                                    
*        MVO   WORK(5),RCONKCON                                                 
*        AP    WORK(5),=P'1'                                                    
*        MVO   WORK+10(5),WORK(5)                                               
*        MVC   RCONKCON,WORK+10                                                 
*                                                                               
ADDCON45 DS    0H                                                               
*        ZIC   R0,0(R2)            BUMP TO STATION FIELD                        
*        AR    R2,R0                                                            
*        LA    RF,CONCMBLH         PASS THE END YET?                            
*        CR    R2,RF                                                            
*        BL    ADDCON35            NO, KEEP LOOPING                             
*                                                                               
ADDCON50 DS    0H                  ADD COMBO INFO ELEMENT                       
*        GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
*        ZIC   R4,TWACOMBO         BEWARE, USING R4 FOR ADDING COMBO            
*        MVC   TWACMBPT,TWACOMBO   COMBO K COUNTER, SO WE KNOW WHICH            
*                                    COMBO K WE ARE WORKING ON                  
ADDCON55 DS    0H                                                               
*        CLC   =C'ADDO',CONACT     TESTING FOR ADDO                             
*        BNE   ADDCON56                                                         
*        GOTO1 VLOAD,DMCB,(X'4A',0),=C'EDIT'                                    
*                                                                               
*ADDCON56 CLC   =C'ADDB',CONACT     TESTING FOR ADDB                            
*        BNE   ADDCON57                                                         
*        GOTO1 VLOAD,DMCB,(X'4B',0),=C'EDIT'                                    
*                                                                               
*ADDCON57 CLC   =C'ADDR',CONACT     TESTING FOR ADDR                            
*        BNE   ADDCON60                                                         
*        GOTO1 VLOAD,DMCB,(X'4B',0),=C'EDIT'                                    
*                                                                               
ADDCON60 DS    0H                  ASSIGN COMPONENT STATIONS TO KS              
*        LA    R6,RCONREC          POINT R6 TO X'17' COMBO ELEMENT              
*        MVI   ELCODE,X'17'                                                     
*        BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*        LR    RF,R4               ELEMENT X'17' IS COMBO STATIONS              
*        BCTR  RF,0                FIND CORRESPONDING STATION COMPONENT         
*        MH    RF,=H'9'              AND ADD TO EACH CONTRACT GENERATED         
*        LA    RF,2(RF)            BUMP PASS CODE AND LENGTH                    
*        AR    R6,RF               SINCE WE ARE ADDING CONTRACT #'S IN          
*        MVC   RCONKSTA,0(R6)        REVERSE, GET LAST STA FIRST                
*                                                                               
*        ZAP   WORK(5),=P'0'       PUT OUT HIGHEST K# FIRST                     
*        MVO   WORK(5),RCONKCON                                                 
*        SP    WORK(5),=P'1'                                                    
*        MVO   WORK+10(5),WORK(5)                                               
*        MVC   RCONKCON,WORK+10                                                 
*                                                                               
*        CLC   CONACT,=C'ADDR'     UPDATE BOP REFERENCE NUMBER                  
*        BE    ADDCON66            FOR ADDR/ADDB                                
*        CLC   CONACT,=C'ADDB'                                                  
*        BNE   ADDCON70                                                         
*                                                                               
ADDCON66 DS    0H                                                               
*        LA    R6,RCONREC                                                       
*        MVI   ELCODE,X'10'                                                     
*        BAS   RE,GETEL                                                         
*        BNE   ADDCON70                                                         
*                                                                               
*        USING RCONBPEL,R6                                                      
*        CLC   CONACT,=C'ADDB'                                                  
*        BE    ADDCON67                                                         
*        OC    RCONBPRF,RCONBPRF                                                
*        BNZ   ADDCON70                                                         
*        MVC   RCONBPRF,TWAXCON                                                 
*        B     ADDCON70                                                         
*                                                                               
ADDCON67 DS    0H                                                               
*        MVC   RCONBPRF,RCONKCON                                                
*        DROP  R6                                                               
*                                                                               
         EJECT                                                                  
ADDCON70 DS    0H                                                               
*                                  INSERT TAKEOVER/MOVE HIST ELT                
         XC    WORK2(RCONMMLQ),WORK2                                            
         MVI   WORK2,X'2A'                                                      
         MVI   WORK2+1,RCONMMLQ    NEW ELEMENT LENGTH                           
         GOTO1 DATCON,DMCB,(5,WORK),(2,WORK2+2)                                 
*                                  INSERT DATE OF TAKEOVER                      
         MVC   WORK2+4(4),TWATKCON INSERT ORIGINAL CONTRACT NUMBER              
         MVC   WORK2+8(2),TWATKSRP INSERT ORIGINAL SOURCE REP                   
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                  INSERT NEW ELEMENT INTO RECORD               
         MVC   RCONKREP,TWATKTRP   INSERT TARGET REP INTO RECORD                
         XC    KEY,KEY                                                          
         MVC   KEY(27),RCONKEY     SET KEY FOR ADDREC                           
         GOTO1 =A(UPDTCON),DMCB,(RC),RR=Y                                       
*                                                                               
         GOTO1 VADDREC,DMCB,RCONREC                                             
*                                                                               
ADDCON73 DS    0H                                                               
         SP    WORK+5(5),=P'1'                                                  
ADDCON75 MVO   WORK+15(5),WORK+5(5)                                             
         MVC   TWACNUM,WORK+15                                                  
         PACK  TWACNUM(1),WORK+18(1)    REVERSE THE COMPLIMENT                  
         PACK  TWACNUM+1(1),WORK+17(1)                                          
         PACK  TWACNUM+2(1),WORK+16(1)                                          
         PACK  TWACNUM+3(1),WORK+15(1)                                          
         MVC   TWACDTES,RCONDATE                                                
         MVC   TWAKADDR,KEY        SAVE CONTRACT DISK ADDRESS                   
* ADD PASSIVE PTRS                                                              
         L     R6,AIO3             OLD                                          
         LR    RE,R6                                                            
         XCEF  (RE),500                                                         
         MVC   0(27,R6),RCONREC    DO NOT ADD MASTER POINTER                    
* BUILD PASSIVE PTRS                                                            
         LA    R8,500(R6)                                                       
         GOTO1 =A(PTRS),DMCB,(RC),(R8),0,RR=Y                                   
* ADD PTRS                                                                      
         GOTO1 =A(ADDPTRS),DMCB,(RC),(R6),(R8),TWAKADDR,RR=Y                    
*                                                                               
         XC    KEY(27),KEY         CLEAR KEY:  LEAVE DISK ADDR                  
         MVI   KEY,X'9E'           ESTABLISH TAKEOVER/MOVE KEY                  
         MVC   KEY+12(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+14(5),RCONKSTA  INSERT STATION                               
         MVC   KEY+19(2),TWATKSRP  INSERT SOURCE REP CODE                       
         GOTO1 DATCON,DMCB,(5,WORK),(2,KEY+21)                                  
*                                  INSERT TAKEOVER/MOVE DATE                    
         MVC   KEY+23(4),TWATKCON  INSERT SOURCE CONTRACT NUMBER                
         GOTO1 VADD                ADD 9E KEY                                   
*                                                                               
ADDCON80 DS    0H                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,CONCNUM),ALIGN=LEFT                                 
         FOUT  CONCNUMH                                                         
         OI    CONCNUMH+4,X'20'    SET PRE-VALID BIT...                         
         GOTO1 =A(TAKEBUYS),DMCB,(RC),RR=Y                                      
****     GOTO1 =A(TAKEDARE),DMCB,(RC),RR=Y                                      
         GOTO1 =A(TAKEMGS),DMCB,(RC),RR=Y                                       
         XC    TWATKSRP,TWATKSRP   CLEAR SOURCE REP                             
         B     EXXMOD                                                           
*                                                                               
*                                                                               
CHECK    TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    1000C               SLACK TO PROVIDE ADDRESSABILITY              
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REGENPRO                                                       
         DSECT                                                                  
       ++INCLUDE REGLBRW                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    CL3                 BINARY MONTH START DATE                      
BRDEND   DS    CL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    CL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
         EJECT                                                                  
         CSECT                                                                  
***********************************************************************         
* CHECKS IF THERE ARE PROPOSALS AGAINST A CONTRACT                              
*                                                                               
* CC ON RETURN                                                                  
*    EQUAL - PROPOSALS                                                          
*    NE    - NO PROPOSALS                                                       
***********************************************************************         
         DS    0H                                                               
PROCHK   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*PROCHK*'                                                    
*                                                                               
         LA    R6,KEY                                                           
         USING RPROKEY,R6                                                       
         XC    RPROKEY,RPROKEY                                                  
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,REPALPHA                                                
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   RPROKCON,WORK                                                    
         DROP  R6                                                               
         GOTO1 VHIGH                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(RPROKPRO-RPROKEY),KEYSAVE                                    
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   UPDTCON:  MODIFY CONTRACT RECORD ON TARGET SIDE.                            
*                                                                               
UPDTCON  CSECT                                                                  
         NMOD1 0,*UPCN*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                  CLEAR REP SENDING ID                         
         MVI   ELCODE,X'20'        FIND SENDING INFO ID                         
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   UPDT0020            ELEMENT NOT FOUND                            
         XC    2(2,R6),2(R6)       FOUND:  SET SEND ID TO ZERO                  
UPDT0020 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TAKEBUYS:  READ ALL BUYS FROM ORIGINAL ORDER, MOVE TO NEW FILE              
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
TAKEBUYS CSECT                                                                  
         NMOD1 0,*TKBY*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
***>>>                                                                          
                                                                                
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),TWATKCON(4)                                           
                                                                                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0B'           INSERT BUY KEY TYPE                          
         MVC   KEY+16(2),TWATKSRP  INSERT SOURCE REP CODE                       
         MVC   KEY+18(4),WORK+32   INSERT CON#, COMP/REVERSED                   
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 VHIGH               ACCESS FIRST KEY                             
         B     TKBY0080                                                         
TKBY0040 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         MVC   KEY(27),KEYSAVE     RESTART SOURCE FILE                          
         GOTO1 VHIGH               REPOSITION TO LAST KEY ACCESSED              
*                                                                               
         GOTO1 VSEQ                ACCESS NEXT KEY                              
TKBY0080 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME REC TYPE/REP/CON#?                      
         BNE   TKBY0120            NO  - FINISHED WITH CONTRACT                 
         MVC   KEYSAVE,KEY         SAVE KEY ACCESSED FOR RESTART                
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                  RETRIEVE ORIGINAL BUY RECORD                 
*                                                                               
*   CONTRACT NUMBER REVERSAL/COMPLEMENT LOGIC (NEW CONTRACT NUMBER)             
***>>>   STILL NEED TO STORE ORIGINAL NUMBER IN RECORD                          
*                                                                               
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         MVC   RBUYKCON,WORK+32    INSERT NEW CON# INTO BUY                     
         MVC   RBUYKREP,TWATKTRP   INSERT NEW REP CODE                          
*                                                                               
*                                  SWITCH BACK TO ORIGINAL FILE TO              
*                                     ADD THE NEW BUY RECORD                    
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         GOTO1 VADDREC,DMCB,RBUYREC                                             
*                                  ADD BUY RECORD FOR NEW CONTRACT              
         B     TKBY0040            GO BACK FOR NEXT BUY                         
*                                                                               
***>>>                                                                          
TKBY0120 EQU   *                                                                
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TAKEDARE:  READ ALL DARE FOR ORIGINAL ORDER, MOVE TO NEW FILE               
*        WITH NEW CONTRACT NUMBER.                                              
*   UPDATE: INGORE AGENCY OFFICE WHEN READING DARE                              
*                                                                               
TAKEDARE CSECT                                                                  
         NMOD1 0,*TKDR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   ELCODE,X'1D'        FIND DARE ELEMENT                            
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   TKDR0140            ELEMENT NOT FOUND: NOT DARE                  
*                                  FOUND:  COPY DARE RECORD(S)                  
*>DARE*>                                                                        
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'41'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,MYSPACES                                                
         OC    RAGY2DAR,RAGY2DAR     NULL EQUIVALENCY CODE?                     
         BZ    TKDR0030                                                         
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
         B     TKDR0020                                                         
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
TKDR0018 CLC   RDARKAOF,PRVKEY.RDARKAOF  SAME OFFICE?                           
         DROP  PRVKEY                                                           
         BNE   TKDR0019               YES -- DON'T INCREMENT                    
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
TKDR0019 XC    RDARKORD(7),RDARKORD  CLEAR FELDS AFTER AGENCY OFFICE            
*                                                                               
TKDR0020 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   TKDR0021                                                         
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,RCONDRLK     MOVE IN ORDER # FOR RDHI                   
*                                                                               
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   TKDR0021                                                         
         CLC   RDARKORD,RCONDRLK     SAME ORDER NUMBER?                         
         BNE   TKDR0018              NO -- SKIP READ                            
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    TKDR0040              YES -- DARE RECORD BUILT...                
         B     TKDR0030                                                         
         DROP  R6                                                               
*                                                                               
TKDR0021 CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
*                                                                               
TKDR0022 LA    R4,5(R4)                                                         
         OC    0(3,R4),0(R4)         NULL EQUIVALENCY CODE?                     
         BZ    TKDR0030              YES                                        
         CLC   RDARKAGY,0(R4)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,TKDR0022           CHECK NEXT EQUIVALENCY CODE                
         B     TKDR0030                                                         
*                                  SPECIAL FOR SELTEL                           
         MVC   RDARKAGY,0(R4)      EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF  CLEAR FIELDS AFTER AGENCY CODE             
         BCT   R3,TKDR0020                                                      
*                                  SPECIAL FOR SELTEL                           
TKDR0030 DS    0H                                                               
         CLC   =C'SZ',REPALPHA                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'1342  ',RAGK2AGY AND AGENCY 1342 ONLY                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RDARKAGY(5),=C'ED2DE'                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    TKDR0040                                                         
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
TKDR0040 DS    0H                                                               
****>>>  MVC   SVDARKEY,KEY        SAVE OFF FOR WHEN WE DELETE                  
*                                                                               
         L     R6,AIO4                                                          
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
         USING RDARREC,R6                                                       
                                                                                
*>DARE*>                                                                        
TKDR0060 EQU   *                                                                
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),TWATKCON(4)                                           
                                                                                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0B'           INSERT BUY KEY TYPE                          
         MVC   KEY+16(2),TWATKSRP  INSERT SOURCE REP CODE                       
         MVC   KEY+18(4),WORK+32   INSERT CON#, COMP/REVERSED                   
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 VHIGH               ACCESS FIRST KEY                             
         B     TKDR0100                                                         
TKDR0080 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         MVC   KEY(27),KEYSAVE     RESTART SOURCE FILE                          
         GOTO1 VHIGH               REPOSITION TO LAST KEY ACCESSED              
*                                                                               
         GOTO1 VSEQ                ACCESS NEXT KEY                              
TKDR0100 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME REC TYPE/REP/CON#?                      
         BNE   TKDR0120            NO  - FINISHED WITH CONTRACT                 
         MVC   KEYSAVE,KEY         SAVE KEY ACCESSED FOR RESTART                
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                  RETRIEVE ORIGINAL BUY RECORD                 
*                                                                               
*   CONTRACT NUMBER REVERSAL/COMPLEMENT LOGIC (NEW CONTRACT NUMBER)             
***>>>   STILL NEED TO STORE ORIGINAL NUMBER IN RECORD                          
*                                                                               
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         MVC   RBUYKCON,WORK+32    INSERT NEW CON# INTO BUY                     
         MVC   RBUYKREP,TWATKTRP   INSERT NEW REP CODE                          
*                                                                               
*                                  SWITCH BACK TO ORIGINAL FILE TO              
*                                     ADD THE NEW BUY RECORD                    
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         GOTO1 VADDREC,DMCB,RBUYREC                                             
*                                  ADD BUY RECORD FOR NEW CONTRACT              
         B     TKDR0080            GO BACK FOR NEXT BUY                         
*                                                                               
***>>>                                                                          
TKDR0120 EQU   *                                                                
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
TKDR0140 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TAKEMGS:  READ ALL MG'S FOR ORIGINAL ORDER, MOVE TO NEW FILE                
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
TAKEMGS  CSECT                                                                  
         NMOD1 0,*TKMG*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),TWATKCON(4)                                           
                                                                                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'11'           INSERT MAKEGOOD BUY KEY TYPE                 
         MVC   KEY+06(2),TWATKSRP  INSERT SOURCE REP CODE                       
         MVC   KEY+08(2),RCONKOFF  INSERT SOURCE OFFICE CODE                    
         MVC   KEY+10(5),RCONKSTA  INSERT SOURCE STATION CODE                   
         MVC   KEY+15(4),WORK+32   INSERT CON#, COMP/REVERSED                   
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 VHIGH               ACCESS FIRST KEY                             
         B     TKMG0080                                                         
TKMG0040 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         MVC   KEY(27),KEYSAVE     RESTART SOURCE FILE                          
         GOTO1 VHIGH               REPOSITION TO LAST KEY ACCESSED              
*                                                                               
         GOTO1 VSEQ                ACCESS NEXT KEY                              
TKMG0080 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     SAME REC TYPE/REP/OFF/STA/CON#?              
         BNE   TKMG0120            NO  - FINISHED WITH CONTRACT                 
         MVC   KEYSAVE,KEY         SAVE KEY ACCESSED FOR RESTART                
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  RETRIEVE ORIGINAL M/G RECORD                 
*                                                                               
*   CONTRACT NUMBER REVERSAL/COMPLEMENT LOGIC (NEW CONTRACT NUMBER)             
*                                                                               
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         MVC   RMKGKCON,WORK+32    INSERT NEW CON# INTO BUY                     
         MVC   RBUYKREP,TWATKTRP   INSERT NEW REP CODE                          
*                                                                               
*                                  SWITCH BACK TO ORIGINAL FILE TO              
*                                     ADD THE NEW BUY RECORD                    
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         GOTO1 VADDREC,DMCB,RMKGREC                                             
*                                  ADD M/G RECORD FOR NEW CONTRACT              
         B     TKMG0040            GO BACK FOR NEXT BUY                         
*                                                                               
***>>>                                                                          
TKMG0120 EQU   *                                                                
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHK9EKEY:  PREVENTS FOLLOWING SITUATION:                                    
*        1.  USER SELECTS ORDER, GLOBBERS TO CONTRACT                           
*        2.  ORDER IS TAKEN OVER.  GENERATES 9E KEY ON ADDING.                  
*        3.  USER RETURNS TO SFM SCREEN                                         
*        4.  USER SELECTS SAME ORDER AGAIN, GLOBBERS TO CONTRACT.               
*        5.  USER TRIES TO TAKE OVER SAME ORDER.                                
*   SITUATION CAN ONLY ARISE WHEN SAME SCREEN IS RETURNED TO.                   
*        9E KEYS PROHIBIT A USED ORDER BEING PLACED ON A NEW                    
*        SCREEN.                                                                
*                                                                               
*        RETURNS CC = ZERO.  IF BAD, RETURNS CC NOT = ZERO.                     
*                                                                               
CHK9EKEY CSECT                                                                  
         NMOD1 0,*CK9E*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'9E'           SET KEY TYPE                                 
         MVC   KEY+12(2),REPALPHA  SET REP CODE                                 
         MVC   KEY+14(5),TWATKSTN  SET STATION                                  
         MVC   KEY+19(2),TWATKSRP  SET SOURCE REP                               
         GOTO1 DATCON,DMCB,(5,WORK),(2,KEY+21)                                  
*                                  INSERT TODAY'S DATE                          
         MVC   KEY+23(4),TWATKCON  INSERT CONTRACT NUMBER                       
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY ALREADY ON FILE?                         
         BE    CK9E0020            YES - DON'T REUSE                            
         SR    R0,R0               NO  - SET CC = ZERO: USE                     
         B     CK9E0040                                                         
CK9E0020 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
CK9E0040 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   AGYEDMOD:  AGENCY EDIT NMOD.  TESTS AGENCY CODE ENTERED.  IF OKAY,          
*        RETURNS CC = ZERO.  IF BAD, RETURNS CC NOT = ZERO.                     
*                                                                               
AGYEDMOD CSECT                                                                  
         NMOD1 0,*AGYE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   UPVER,0    ACE/GRAPHNET FLAG TO UP VERSION & UNCONFIRM           
         LA    R2,CONAGYH                                                       
         LA    R3,AGYERR                                                        
         TM    4(R2),X'20'                                                      
         BO    AGYE0400            EXIT CC= ZERO                                
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   AGYE0020                                                         
         OI    6(R2),X'40'+X'80'   CURSOR HERE ON RETURN                        
         GOTO1 =A(GOOBR),DMCB,(RC),C'AGY ',(7,9(R2)),0,RR=Y                     
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
AGYE0020 DS    0H                                                               
         MVI   RAGYKTYP,10         REC TYPE                                     
         GOTO1 VMOVE                                                            
         LA    RE,WORK                                                          
*                                                                               
         MVI   RAGYKAGY,C' '                                                    
         MVC   RAGYKAGY+1(5),RAGYKAGY                                           
*                                                                               
* CHECK FOR AGENCY OFFICE                                                       
         CLI   0(RE),C'-'                                                       
         BE    AGYE0040                                                         
         CLI   0(RE),C' '                                                       
         BE    AGYE0060                                                         
         LA    RE,1(RE)                                                         
         B     *-20                                                             
*              AGENCY OFFICE                                                    
AGYE0040 MVC   RAGYKAOF,1(RE)      AGENCY OFFICE                                
         LA    RF,WORK+1                                                        
         SR    RE,RF                                                            
         EX    RE,MOVEAGY                                                       
         B     AGYE0080                                                         
*                                                                               
MOVEAGY  MVC   RAGYKAGY(0),WORK                                                 
*                                                                               
AGYE0060 MVC   RAGYKAGY(4),WORK                                                 
*                                                                               
AGYE0080 DS    0H                                                               
         MVC   RAGYKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
         ST    R3,FULL             SAVE CURRENT ERROR MESSAGE                   
         CLC   =C'V4',REPALPHA     FOX NETWORK:  TEST REP?                      
         BE    AGYE0100            NO  - DON'T CHECK FOR LOCKOUT                
         CLC   =C'FN',REPALPHA     FOX NETWORK?                                 
         BNE   AGYE0160            NO  - DON'T CHECK FOR LOCKOUT                
AGYE0100 EQU   *                                                                
         LA    R3,SPECCHRS                                                      
         B     AGYE0120                                                         
SPECCHRS DC    C'#$%=*+!@><()-":'                                               
*                                  SPECIAL CONVERSION VALUES                    
         DC    X'FFFF'             DELIMITER                                    
         DS    0F                                                               
AGYE0120 EQU   *                                                                
         CLI   0(R3),X'FF'         DELIMITER REACHED?                           
****>>>> BE    AGYE0160            YES - NOT SPECIAL CHARACTER                  
*                                                                               
         B     AGYE0160            PERMIT USE IN ALL CASES                      
*                                                                               
         CLC   RAGYKAGY+3(1),0(R3)                                              
*                                  SPECIAL CHARACTER FOUND?                     
         BE    AGYE0140            YES - SEND BACK MESSAGE                      
         LA    R3,1(R3)                                                         
         B     AGYE0120            GO BACK FOR NEXT CODE                        
AGYE0140 EQU   *                                                                
         LA    R3,NOCONCDE         SET ERROR MESSAGE                            
         B     AGYE0500            EXIT CC NOT ZERO                             
NOCONCDE EQU   675                                                              
AGYE0160 EQU   *                                                                
         L     R3,FULL             RESET PRIOR ERROR MESSAGE                    
         GOTO1 VHIGH                                                            
*        CLC   KEY(25),KEYSAVE                                                  
*        BNE   AGYE0500                                                         
*        CLC   KEY+25(2),REPALPHA                                               
*        BE    AGYE0180                                                         
*        MVC   KEYSAVE+25(2),=C'ZZ'                                             
*        CLC   KEY+25(2),=C'ZZ'                                                 
*        BE    AGYE0180                                                         
*        MVC   KEY+25(2),=C'ZZ'                                                 
*        GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AGYE0500                                                         
* CHECK IF DEFAULT                                                              
AGYE0180 GOTO1 VGETREC,DMCB,IOAREA                                              
AGADMISS EQU   515                                                              
         LA    R3,AGADMISS         SET 'AGENCY ADDR MISSING' ERROR              
         CLC   RAGYADD1(15),=C'**NOT ENTERED**'                                 
         BNE   AGYE0200            AGENCY ADDRESS ENTERED                       
         CLC   RAGYADD2(15),=C'**NOT ENTERED**'                                 
         BE    AGYE0500            AGENCY ADDRESS MISSING                       
AGYE0200 EQU   *                                                                
         MVC   WAGYEXP,RAGYNAM1    SAVE EXPANSION FOR DISPLAY                   
         CLC   KEY+23(2),MYSPACES                                               
         BNE   AGYE0240                                                         
         SPACE 2                                                                
* DEFAULT - CHECK IF OFFICE EXISTS                                              
         LA    R3,AODERR                                                        
         MVC   KEY+25(2),IOAREA+25                                              
AGYE0220 GOTO1 VSEQ                                                             
         CLC   KEY(23),IOAREA      SAME AGENCY?                                 
         BNE   AGYE0240                                                         
         CLC   KEY+23(2),MYSPACES                                               
         BE    AGYE0220                                                         
         CLC   KEY+25(2),IOAREA+25                                              
         BE    AGYE0500                                                         
         B     AGYE0220                                                         
AGYE0240 FOUT  CONAGYNH,RAGYNAM1,20                                             
         MVC   RCONKAGY(6),RAGYKAGY                                             
*                                                                               
         XC    CONARSK,CONARSK     ALWAYS CLEAR CREDIT RISK FIELD               
         OC    RAGYRISK,RAGYRISK   NO RATING ASSIGNED, ASSUM OK                 
         BZ    AGYE0260                                                         
         CLI   RAGYRISK,1          RISK=OK, SHOW NOTHING                        
         BE    AGYE0260                                                         
         MVC   CONARSK(9),=C'AGY RISK='  SHOW RISK                              
         LA    R2,CONARSK+9                                                     
         EDIT  RAGYRISK,(1,(R2))                                                
AGYE0260 OI    CONARSKH+6,X'80'    XMIT                                         
*                                                                               
* DELETE EXTENDED AGENCY ADDRESSES FOR KATZ ADDED DURING CONVERSION             
*                                                                               
         TM    RCONMODR+1,X'10'    FOR KATZ CONVERTED CONTRACT ONLY             
         BZ    AGYE0400                                                         
         GOTO1 VDELELEM,DMCB,(X'70',RCONREC)                                    
         GOTO1 VDELELEM,DMCB,(X'71',RCONREC)                                    
         GOTO1 VDELELEM,DMCB,(X'72',RCONREC)                                    
         GOTO1 VDELELEM,DMCB,(X'73',RCONREC)                                    
AGYE0400 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     AGYE0600                                                         
AGYE0500 EQU   *                                                                
         ST    R2,DUB              SAVE ERROR ADDRESS                           
         ST    R3,DUB+4            SAVE ERROR RETURN CODE                       
         LA    R0,1                SET CC NOT = ZERO                            
AGYE0600 EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PROPOSAL X'02' SWITCH ELEMENTS AND THE PRIMARY STATION ELEMENT         
***********************************************************************         
UPDPROS  CSECT                                                                  
         NMOD1 0,*UPDPRO*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         USING RPROKEY,RE                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,REPALPHA                                                
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   RPROKCON,WORK                                                    
         XC    RPROKPRO,RPROKPRO                                                
         DROP  RE                                                               
*                                                                               
UPDPRO2  GOTO1 VHIGH                                                            
         CLC   KEY(RPROKPRO-RPROKMST),KEYSAVE                                   
         BNE   UPDPROX                                                          
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         MVI   ELCODE,RPRSWELQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   UPDPRO30            NO SWITCH ELEMENT TO CHANGE                  
*                                                                               
         MVC   WORK2,KEY                                                        
PK       USING RPROKEY,WORK2                                                    
*                                                                               
         LR    R4,R6                                                            
         USING RPRSWELD,R4                                                      
         CLC   RPRSWSAL,RCONSAL                                                 
         BNE   *+14                                                             
         CLC   RPRSWSTA,RCONKSTA                                                
         BE    UPDPRO9                                                          
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         USING RPROKEY,RE                                                       
         MVI   RPROPTYP,RPROPTYQ                                                
         MVI   RPROPSTY,RPROPSBQ                                                
         MVC   RPROPRCD,REPALPHA                                                
         MVC   RPROPSAL,RPRSWSAL                                                
         MVC   RPROPSTA,RPRSWSTA                                                
         MVC   RPROPCON,PK.RPROKCON                                             
         MVC   RPROPPRO,PK.RPROKPRO                                             
         DROP  RE                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     UPDPRO4                                                          
*                                                                               
         OI    KEY+RPROKCTL-RPROKEY,X'80'       DELETED                         
         GOTO1 VWRITE                                                           
*                                                                               
UPDPRO4  LA    RE,KEY              NEW PASSIVE KEY                              
         USING RPROKEY,RE                                                       
         MVC   RPROPSAL,RCONSAL                                                 
         MVC   RPROPSTA,RCONKSTA                                                
         NI    RPROKCTL,X'FF'-X'80'                                             
         OC    RPROKDA,RPROKDA                                                  
         BNZ   *+10                                                             
         MVC   RPROKDA,WORK2+(RPROKDA-RPROKEY)                                  
         DROP  RE                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BNE   UPDPRO8                                                          
         NI    KEY+(RPROKCTL-RPROKEY),X'FF'-X'80'                               
         GOTO1 VWRITE                                                           
         B     UPDPRO9                                                          
*                                                                               
UPDPRO8  MVC   KEY,KEYSAVE                                                      
         GOTO1 VADD                                                             
*                                                                               
UPDPRO9  DS    0H                                                               
         CLC   RPRSWSAL,RCONSAL                                                 
         BE    UPDPRO19                                                         
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         USING RPROKEY,RE                                                       
         MVI   RPROOTYP,RPROOTYQ                                                
         MVI   RPROOSTY,RPROOSBQ                                                
         MVC   RPROORCD,REPALPHA                                                
         MVC   RPROOOFF,RPRSWOFF                                                
         MVC   RPROOSAL,RPRSWSAL                                                
         MVC   RPROOCON,PK.RPROKCON                                             
         MVC   RPROOPRO,PK.RPROKPRO                                             
         DROP  RE,PK                                                            
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     UPDPR14                                                          
*                                                                               
         OI    KEY+RPROKCTL-RPROKEY,X'80'       DELETED                         
         GOTO1 VWRITE                                                           
*                                                                               
UPDPR14  LA    RE,KEY              NEW PASSIVE KEY                              
         USING RPROKEY,RE                                                       
         MVC   RPROOOFF,RCONKOFF                                                
         MVC   RPROOSAL,RCONSAL                                                 
         NI    RPROKCTL,X'FF'-X'80'                                             
         OC    RPROKDA,RPROKDA                                                  
         BNZ   *+10                                                             
         MVC   RPROKDA,WORK2+(RPROKDA-RPROKEY)                                  
         DROP  RE                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BNE   UPDPR18                                                          
         NI    KEY+(RPROKCTL-RPROKEY),X'FF'-X'80'                               
         GOTO1 VWRITE                                                           
         B     UPDPRO19                                                         
*                                                                               
UPDPR18  MVC   KEY,KEYSAVE                                                      
         GOTO1 VADD                                                             
*                                                                               
UPDPRO19 MVC   KEY,WORK2                                                        
*                                                                               
UPDPRO20 MVC   RPRSWSAL,RCONSAL                                                 
         MVC   RPRSWOFF,RCONKOFF                                                
         MVC   RPRSWTEM,RCONTEM                                                 
         MVC   RPRSWSTA,RCONKSTA                                                
         MVC   RPRSWADV,RCONKADV                                                
         MVC   RPRSWAGY,RCONKAGY                                                
         MVC   RPRSWAOF,RCONKAOF                                                
         MVC   RPRSWGRP,RCONKGRP                                                
         MVC   RPRSWFLT,RCONDATE                                                
         XC    RPRSWDSP,RPRSWDSP                                                
         XC    RPRSWDCT,RPRSWDCT                                                
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENTAL INVOCE ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   UPDPRO25            NOT FOUND                                    
         USING RCONDVEL,R6                                                      
         MVC   RPRSWDSP,RCONDVSP                                                
         MVC   RPRSWDCT,RCONDVCT                                                
         DROP  R4,R6                                                            
*                                                                               
UPDPRO25 GOTO1 VPUTREC,DMCB,AIO4                                                
*                                                                               
UPDPRO30 DS    0H                                                               
         LA    RE,KEY                                                           
         USING RPROKEY,RE                                                       
         ZIC   RF,RPROKPRO                                                      
         LA    RF,1(RF)                                                         
         STC   RF,RPROKPRO                                                      
         XC    RPROKMEL,RPROKMEL                                                
         DROP  RE                                                               
         B     UPDPRO2             PROCESS NEXT RECORD                          
*                                                                               
UPDPROX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO BUILD POINTER LIST IN P1                                           
*  P2 - 0=NEW POINTER                                                           
*       1=OLD POINTER                                                           
***********************************************************************         
PTRS     CSECT                                                                  
         NMOD1 0,**PTRS**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R2,4(R1)                                                         
         LR    RE,R2                                                            
         L     R3,8(R1)                                                         
         XCEF  (RE),500                                                         
* BUILD ACTIVE PTR                                                              
         MVI   0(R2),X'0C'                                                      
         MVC   02(02,R2),REPALPHA                                               
         MVC   04(02,R2),RCONKGRP                                               
         MVC   06(05,R2),RCONKSTA                                               
         MVC   11(02,R2),RCONKOFF                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 1                                                                  
         MVI   0(R2),X'8C'                                                      
         MVC   21(02,R2),REPALPHA                                               
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVC   23(4,R2),WORK                                                    
         LA    R2,32(R2)                                                        
* CREATE PTR 2                                                                  
         MVI   0(R2),X'9C'                                                      
         MVC   02(02,R2),REPALPHA                                               
         MVC   04(02,R2),RCONKOFF                                               
         MVC   06(02,R2),RCONKGRP                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKADV                                               
         MVC   17(04,R2),RCONKAGY                                               
         MVC   21(02,R2),RCONKAOF                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 3                                                                  
         MVI   0(R2),X'AC'                                                      
         MVC   01(2,R2),REPALPHA                                                
         MVC   03(2,R2),RCONKOFF                                                
         MVC   05(2,R2),RCONTEM                                                 
* INVERT SALESMAN CODE FOR LAST NAME HIGH                                       
         LA    RE,RCONSAL+2        LAST INITIAL                                 
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   07(1,R2),0(RE)                                                   
         MVC   08(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '      ONLY 2 INITIALS?                             
         BNE   *+8                                                              
         MVI   9(R2),C' '                                                       
         MVC   10(5,R2),RCONKSTA                                                
         MVC   15(4,R2),RCONKAGY                                                
         MVC   19(4,R2),RCONKADV                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
* CREATE PTR 4                                                                  
         MVI   0(R2),X'BC'                                                      
         MVC   02(02,R2),REPALPHA                                               
         MVC   04(02,R2),RCONCTGY                                               
         MVC   06(02,R2),RCONKOFF                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 5                                                                  
         MVI   0(R2),X'CC'                                                      
         MVC   01(02,R2),REPALPHA                                               
         MVC   03(05,R2),RCONKSTA                                               
         MVC   08(02,R2),RCONKOFF                                               
         MVC   10(02,R2),RCONTEM                                                
* INVERT SALESMAN                                                               
         LA    RE,RCONSAL+2                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   12(1,R2),0(RE)                                                   
         MVC   13(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '                                                   
         BNE   *+8                                                              
         MVI   14(R2),C' '                                                      
*                                                                               
         MVC   15(04,R2),RCONKADV                                               
         MVC   19(04,R2),RCONKAGY                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
*        BNE   PTREC               NO BOP                                       
         BNE   PT8D                NO BOP                                       
         SPACE 1                                                                
         USING RCONBPEL,R6                                                      
DCPRTR   MVI   0(R2),X'DC'         CREATE BOP POINTER FOR CHANGE                
         MVC   5(2,R2),REPALPHA                                                 
         MVC   7(4,R2),RCONKADV                                                 
         MVC   11(3,R2),TODAY      NEW POINTER GETS TODAYS DATE                 
         CLC   CONACT(3),=C'ADD'                                                
         BE    DC20                                                             
         LTR   R3,R3               0=NEW POINTER                                
         BZ    DC20                                                             
         MVC   11(3,R2),RCONBPDT   OLD PTR NEEDS OLD BOP CHANGE DATE            
*    (IN ADPTRS, ONLY ADD NEW POINTER IF CHANGE IS OTHER THAN DATE)             
*                                                                               
DC20     MVC   14(4,R2),RCONBPRF                                                
         MVC   18(5,R2),RCONKSTA                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
         DROP  R6                                                               
*                                                                               
*   CREATE X'8D' POINTERS                                                       
*                                                                               
PT8D     EQU   *                                                                
                                                                                
* - GET FLIGHT START/END DATES AND SAVE IN WORK+40                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,WORK+40)    START DATE               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,WORK+42)  END DATE                 
                                                                                
* - GET DEMO FROM BOP OR SAR ELEMENT AND SAVE IN WORK+45                        
* - LOOK FOR DEMO MARKED AS PRIMARY (X'40' IN 1ST BYTE)                         
* - IF NO DEMO MARKED AS PRIMARY, USE 1ST DEMO AS DEFAULT                       
*                                                                               
         XC    WORK+45(3),WORK+45                                               
         LA    R4,RCONELEM                                                      
PPC8DLP  CLI   0(R4),0                                                          
         BE    PPC8D00                                                          
         CLI   0(R4),X'12'         SAR ELEMENT                                  
         BE    PPC8DDD                                                          
         CLI   0(R4),X'10'         BOP ELEMENT                                  
         BE    PPC8DEE                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPC8D00                                                          
         AR    R4,R1                                                            
         B     PPC8DLP             GO TO LOOP                                   
         USING RSARCO,R4                                                        
PPC8DDD  LA    RE,RSARDEM            DEMO                                       
         LA    RF,8                                                             
         MVC   WORK+45(3),RSARDEM    DEMO                                       
PPC8DDE  TM    0(RE),X'40'         IS IT MARKED AS PRIMARY ?                    
         BO    PPC8DDF               YES                                        
         LA    RE,3(RE)                                                         
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PPC8DDE                                                       
         MVC   WORK+45(3),RSARDEM    NO/USE 1ST AS DEFAULT                      
PPC8DDF  NI    WORK+45,X'FF'-X'40'          CLEAR MAIN DEMO INDICATOR           
         B     PPC8D00                                                          
                                                                                
         USING RCONBPEL,R4                                                      
PPC8DEE  LA    RE,RCONBPDM+1                                                    
         LA    RF,8                                                             
         MVC   WORK+45(3),RCONBPDM+1                                            
PPC8DEF  TM    0(RE),X'40'              IS IT MARKED AS PRIMARY DEMO?           
         BO    PPC8DEG                  YES                                     
         LA    RE,3(RE)                 NO/BUMP TO NEXT DEMO                    
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PPC8DEF                                                       
         MVC   WORK+45(3),RCONBPDM+1     NO PRIMARY/USE 1ST AS DEFAULT          
PPC8DEG  NI    WORK+45,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR          
         B     PPC8D00                                                          
         DROP R4                                                                
                                                                                
* BUILD BASIC KEY IN WORK                                                       
PPC8D00  XC    WORK(32),WORK                                                    
         LA    R4,WORK                                                          
         MVI   0(R4),X'8D'                                                      
         MVC   1(2,R4),RCONKREP                                                 
         MVC   8(2,R4),WORK+40    START DATE                                    
         MVC   10(2,R4),WORK+42    END DATE                                     
         MVC   12(4,R4),RCONKCON   CONTRACT NUMBER                              
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         LA    R2,32(R2)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         LA    R2,32(R2)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45       DEMO                                      
PPC8DX   LA    R2,32(R2)                                                        
*                                                                               
* END X'8D' PASSIVE POINTERS                                                    
*                                                                               
*   CREATE X'8E' POINTERS                                                       
*   SIMILAR TO X'8D' POINTERS BUT HAVE STATION IN KEY INSTEAD OF 0'S            
*                                                                               
* WORK HAS BASIC KEY- REPLACE ZEROS OF X'8D' WITH STATION                       
PPCON8E  MVI   WORK,X'8E'                                                       
         MVC   WORK+3(5),RCONKSTA                                               
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         LA    R2,32(R2)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         LA    R2,32(R2)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45    DEMO                                         
PPC8EX   LA    R2,32(R2)                                                        
                                                                                
* END X'8E' PASSIVE POINTERS                                                    
*                                                                               
         EJECT                                                                  
* ADD X'9D' PASSIVE POINTER (RIS/PRODUCT)                                       
*                                                                               
         MVI   0(R2),X'9D'                                                      
         MVC   1(2,R2),RCONKREP                                                 
         MVC   3(5,R2),RCONKSTA                                                 
         MVC   8(4,R2),RCONKADV                                                 
         MVC   23(4,R2),RCONKCON                                                
* NOW SET PRODUCT NAME OR CODE INTO KEY                                         
         MVI   12(R2),X'FF'              SET DEFAULT TO LAST                    
         MVC   13(3,R2),RCONPRD                                                 
         LA    R6,RCONREC          NOW LOOK FOR PRODUCT NAME                    
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   PP9DX                                                            
         USING RCONEXEL,R6                                                      
         MVC   12(9,R2),RCONEXPR   SET PRODUCT NAME                             
         DROP  R6                                                               
PP9DX    LA    R2,32(R2)           9D POINTER CREATED, BUMP                     
*                                                                               
* END X'9D' PASSIVE POINTER                                                     
*                                                                               
PTREC    CLC   CONACT(3),=C'ADD'                                                
         BNE   PTRX                                                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTRX                NO SAR                                       
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         MVI   0(R2),X'EC'                                                      
         MVC   21(2,R2),REPALPHA                                                
         MVC   23(4,R2),TWACNUM                                                 
PTRX     DS    0H                                                               
         XMOD1                                                                  
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD POINTERS TO FILE                                               
*              P1=A(OLD PTR LIST)                                               
*              P2=A(NEW PTR LIST)                                               
*              P3=A(DISK ADDR)                                                  
***********************************************************************         
ADDPTRS  CSECT                                                                  
         NMOD1 0,*ADDPTR*                                                       
         L     RC,0(R1)                                                         
         LM    R2,R4,4(R1)                                                      
*                                                                               
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
AP25     CLC   0(27,R2),0(R3)      SAME?                                        
         BE    AP100                                                            
* DIFFERENT                                                                     
* DELETE OLD PTR                                                                
         CLI   0(R2),0             ADD?                                         
         BNE   AP30                                                             
* ADD                                                                           
         MVC   KEY,0(R3)           NEW KEY                                      
         B     AP50                                                             
* CHANGE                                                                        
AP30     MVC   KEY,0(R2)                                                        
         CLI   KEY,X'DC'           BOP POINTER ONLY CHANGED IF                  
         BNE   AP33                                                             
         CLC   0(11,R3),KEY        ADVERTISER OR                                
         BNE   AP33                                                             
         CLC   14(13,R3),KEY+14    REF #, STATION OR CON# CHANGES               
         BE    AP100                                                            
AP33     OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AP40                                                             
         MVI   KEY+27,X'FF'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,APCHECK                                                       
* ADD NEW PTR                                                                   
AP40     MVC   KEY,0(R3)                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE     KEY ALREADY THERE?                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     AP50                                                             
* UNDELETE OLD PTR                                                              
         MVI   KEY+27,0                                                         
         GOTO1 VWRITE                                                           
         BAS   RE,APCHECK                                                       
         B     AP100                                                            
* ADD PTR                                                                       
AP50     MVI   KEY+27,0                                                         
         MVC   KEY+28(4),0(R4)     DISK ADDR                                    
         GOTO1 VADD                                                             
         BAS   RE,APCHECK                                                       
*                                                                               
* NEXT POINTER                                                                  
AP100    LA    R2,32(R2)                                                        
         LA    R3,32(R3)                                                        
         CLI   0(R3),0             LAST?                                        
         BNE   AP25                                                             
         MVI   DMOUTBTS,X'FD'                                                   
         B     EXXIT                                                            
*                                                                               
APCHECK  TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
EXXIT    XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CHKDARE CHECKS FOR DARE ORDERS.  IF FOUND, PERMITS SKIP OF CHECKS *           
*********************************************************************           
CHKDARE  CSECT                                                                  
         NMOD1 0,*CKDA*                                                         
         L     RC,0(R1)                                                         
         LA    R2,RCONELEM         LOOK FOR DARE ELEMENT                        
CHDA0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CHDA0160            YES - NOT A DARE ORDER                       
*                                     COULD BE AN ADD(S) REQUEST                
         CLI   0(R2),X'1F'         EXTENDED DESCRIPTION ELEMENT?                
         BE    CHDA0040            YES - CHECK FOR CONFIRM STATUS               
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     CHDA0020            GO BACK FOR NEXT                             
CHDA0040 EQU   *                                                                
         USING RCONXEL,R2                                                       
*                                                                               
         TM    RCONCONF,X'40'      CONFIRMED?                                   
         BO    CHDA0160            YES - MUST BE UNCONFIRMED                    
         TM    RCONCONF,X'20'      PREVIOUSLY CONFIRMED?                        
         BO    CHDA0160            YES - MUST BE UNCONFIRMED                    
*                                                                               
         DROP  R2                                                               
*                                                                               
CHDA0060 EQU   *                                                                
         LA    R2,RCONELEM         LOOK FOR DARE ELEMENT                        
CHDA0080 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CHDA0160            YES - NO DARE ELEMENT                        
         CLI   0(R2),X'1D'         DARE ELEMENT?                                
         BE    CHDA0100            YES - CHECK FOR ACTIVITY                     
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     CHDA0080            GO BACK FOR NEXT                             
CHDA0100 EQU   *                                                                
         USING RCONDREL,R2                                                      
*                                                                               
         CLI   RCONDRFG,0          ANYTHING SET IN STATUS BYTE?                 
         BE    CHDA0160            NO  - NOT DARE CONTRACT                      
         TM    RCONDRFG,X'04'+X'02' KATZ EDI ORDER? OR ONE SHOT                 
         BNZ   CHDA0160            YES, DON'T TREAT AS DARE CONTRACT            
CHDA0140 EQU   *                   RETURN WITH CC= ZERO                         
         SR    R0,R0               SET CC = ZERO                                
         B     CHDA0180            DARE ORDER IN PROGRESS                       
CHDA0160 EQU   *                   RETURN WITH CC NOT = ZERO                    
         LTR   RB,RB               SET CC NOT = ZERO                            
*                                     NOT A DARE ORDER                          
CHDA0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* VALIDATE FILE COMMENTS, SFM COMMENTS AND/OR FREE FORM COMMENTS                
*********************************************************************           
CMTRNT   CSECT                                                                  
         NMOD1 0,*CMTE*                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         LA    R2,CONCOM1H         1ST COMMENT                                  
         TM    TWASTAOP,X'80'      STA OPTION #10?                              
         BZ    CMTRNT05            NO - SKIP CHECK                              
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    CMTRNT03                                                         
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    CMTRNT05            SKIP REQUIRED FIELD FOR AUTOHEADER?          
*                                                                               
CMTRNT03 DS    0H                                                               
         CLC   =C'C=',8(R2)        YES - REQUIRE STANDARD CMT                   
         BE    CMTRNT05                                                         
         LA    R3,600              ERROR, RETURN CONTROL TO USER                
         B     ERROR                                                            
*                                                                               
CMTRNT05 TM    4(R2),X'20'         VALID?                                       
         BO    CMTRNTX                                                          
*                                                                               
         CLC   CONACT(3),=C'ADD'   FOR ACTION ADD, RCONKREP IS NOT              
         BNE   CMTRNT10            ESTABLISHED, YET                             
         OC    RCONKREP,RCONKREP   WE'LL HAVE TO MOVE IT IN HERE                
         BNZ   CMTRNT10                                                         
         MVC   RCONKREP,REPALPHA                                                
*                                                                               
CMTRNT10 DS    0H                                                               
         CLC   =C'C=TO',CONCOM1    FLAG IF TAKEOVER CONTRACT                    
         BNE   CMTRNT20                                                         
         CLC   CONSTA(3),CONCOM1+4 CHECK CALL LETTER MATCH                      
         BNE   CMTRNT20            THREE LETTERS ARE CLOSE ENOUGH               
         OI    RCONMODR,X'18'      CONTRACT IS TAKEOVER/NOT PENDING             
                                                                                
CMTRNT20 L     R7,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,CONCOM1H),(R7),DATAMGR,RCONREC,GETTXT           
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         MVI   UPVER,1     ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM           
         GOTO1 VDELELEM,DMCB,(2,RCONREC)     DELETE COMMENTS (CHANGE)           
         CLI   5(R2),0                                                          
*        BE    CMTRNTX                                                          
         BE    CMTRNT30                                                         
         MVC   WORK2+2(60),CONCOM1                                              
         MVI   WORK2,2             COMMENT ELEM CODE                            
*                                                                               
         IC    RE,5(R2)            LENGTH                                       
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          ELEM LEN                                     
*              ADD 1ST COMMENT ELEMENT                                          
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2   ADD 1ST COMMENT ELEM               
*                                                                               
CMTRNT30 DS    0H                                                               
         CLI   CONCOM2H+5,0        2D COMMENT?                                  
         BE    CMTRNTX                                                          
*                                                                               
         L     R7,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,CONCOM2H),(R7),DATAMGR,RCONREC,GETTXT           
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
CMTRNT50 DS    0H                                                               
         IC    RE,CONCOM2H+5       LENGTH                                       
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          2D COMMENT LENGTH                            
         MVC   WORK2+2(60),CONCOM2                                              
         MVI   WORK2,2             COMMENT ELEM CODE                            
*              ADD 2D COMMENT                                                   
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
CMTRNTX  XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DEVSPRSN:  CHECK X'18' ELEMENT.  SEE NEXT NOTE                              
*                                                                               
DEVSPERR EQU   415                                                              
DEVSPACT EQU   459                                                              
DEVSTMIS EQU   469                                                              
*                                                                               
DEVSPRSN CSECT                                                                  
         NMOD1 0,*DVSP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   AN X'18' ELEMENT MAY ALREADY BE IN THE RECORD.  IT MAY CONTAIN              
*     INVOICE DATA WHICH CANNOT BE LOST.  THE ELEMENT IS SEARCHED               
*     FOR.  IF PRESENT, IT IS SAVED, AND THE DEV CODE FIELDS ARE                
*     CLEARED.  IF NOT PRESENT, A SKELETON IS INITIALIZED.  AT THE              
*     END OF VALIDATION OF THESE TWO FIELDS, THE ELEMENT AS SAVED               
*     IS CHECKED TO SEE IF ANYTHING IS PRESENT.  IF SO, THE ELEMENT             
*     IS RESTORED TO THE RECORD.                                                
*                                                                               
         XC    MYP,MYP             USE 'MY PRINT LINE' AS TEMP SPACE            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        LOOK FOR DEV/INV ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DVSP0040            NO ELEMENT                                   
         ZIC   RF,1(R6)            FOUND - MOVE TO STORAGE                      
         BCTR  RF,0                DECREMENT FOR MOVE                           
         EX    RF,DVSP0020         MOVE BY LENGTH                               
         GOTO1 VDELELEM,DMCB,(X'18',RCONREC)                                    
*                                  DELETE THE OLD ELEMENT                       
         B     DVSP0060                                                         
DVSP0020 MVC   MYP(0),0(R6)        MOVE BY LENGTH                               
DVSP0040 EQU   *                                                                
         MVC   MYP(2),=X'1808'     SET BASIS FOR ELEMENT                        
DVSP0060 EQU   *                                                                
         CLI   5(R2),0             ANY DATA?                                    
         BNE   DVSP0070            NO  - CHECK PROFILE IF REQUIRED              
                                                                                
         TM    PROFILES+CNTRDEVB,CNTRDEVA                                       
*                                  REQUIRES DEVSAL AND DEVTYPE?                 
         BZ    DVSP0160            NO - FINISHED                                
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    DVSP0090                                                         
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    DVSP0160            SKIP REQUIRED FIELD FOR AUTOHEADER?          
         B     DVSP0090            YES - ERROR!                                 
*                                                                               
DVSP0070 DS    0H                                                               
         TM    4(R2),X'20'         VALID DEVELOPMENTAL SALESMAN?                
         BO    DVSP0160            EXIT OKAY:  CC=ZERO                          
*                                  NO  - CHANGE MADE                            
         XC    MYP+2(3),MYP+2         CLEAR DEV S/P CODE                        
         XC    TWDSPEXP,TWDSPEXP      CLEAR EXPANSION AREA                      
         GOTO1 VMOVE                                                            
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RDSPKTYP,X'3A'      DEVELOPMENTAL SALESMAN REC TYPE              
         MVC   RDSPKREP,REPALPHA   REP CODE                                     
         MVC   RDSPKSAL,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DVSP0100            EXIT NG:    CC=NOT ZERO                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         OC    RDSPLEAV,RDSPLEAV   ANY LEAVE DATE?                              
         BZ    DVSP0080            NO                                           
         GOTO1 DATCON,DMCB,(5,FULL),(2,FULL)                                    
*                                  YES - CHECK LEAVE VS TODAY'S DATE            
         CLC   RDSPLEAV,FULL                                                    
         BNH   DVSP0120            INACTIVE                                     
DVSP0080 EQU   *                                                                
         MVC   TWDSPEXP,RDSPNAME                                                
*                                  SAVE EXPANSION NAME                          
         FOUT  CONDSPNH,RDSPNAME,15                                             
*                                  DEV S/P NAME                                 
         MVC   MYP+RCONDVSP-RCONDVEL(3),RDSPKSAL                                
*                                  INSERT DEV S/P CODE                          
         B     DVSP0160            EXIT OKAY:  CC = ZERO                        
DVSP0090 EQU   *                                                                
         LA    RF,DEVSTMIS         ERROR = REQUIRED FIELD IS MISSING            
         ST    RF,FULL                                                          
         B     DVSP0140                                                         
DVSP0100 EQU   *                                                                
         LA    RF,DEVSPERR         ERROR = NOT FOUND                            
         ST    RF,FULL                                                          
         B     DVSP0140                                                         
DVSP0120 EQU   *                                                                
         LA    RF,DEVSPACT         ERROR = INACTIVE                             
         ST    RF,FULL                                                          
DVSP0140 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     DVSP0180                                                         
DVSP0160 EQU   *                                                                
         SR    RF,RF               SET CC = ZERO                                
         LTR   RF,RF                                                            
DVSP0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   POINTPSN:  CHECK X'02' ELEMENT.  IF PRESENT, CHECK FOR POINT                
*        PERSON CODE.  IF PRESENT, CHECK TO ENSURE THAT IT IS IN                
*        ACTIVE STATUS.   IF NOT, RETURN AN ERROR CONDITION.                    
*                                                                               
POINTPSN CSECT                                                                  
         NMOD1 0,*PPSN*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,X'02'        LOOK FOR DEV/INV ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   PPSN0120            NO ELEMENT - RETURN CC = ZERO                
         USING RPRDNELM,R6         ELEMENT FOUND - POINT PERSON?                
         OC    RPRDNPNT,RPRDNPNT   ANY POINT PERSON?                            
         BZ    PPSN0120            NO  - RETURN CC = ZERO                       
         XC    IOAREA(32),IOAREA   YES - SET UP THE POINT PERSON KEY            
         LA    R3,IOAREA                                                        
         USING RPTPREC,R3                                                       
         MVI   RPTPKTYP,X'31'                                                   
         MVC   RPTPKREP,REPALPHA                                                
         MVC   RPTPKREC,RPRDNPNT   ADVERTISER                                   
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PPSN0080            NOT FOUND - RETURN CC NOT = ZERO             
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  FOUND - RETRIEVE RECORD                      
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BZ    PPSN0120            NO  - ACTIVE - ACCEPT IT                     
         GOTO1 DATCON,DMCB,(5,FULL),(2,FULL)                                    
*                                  GET TODAY'S DATE: COMPRESSED                 
         CLC   RPTPLDAT,FULL       LEAVE DATE VS TODAY'S DATE                   
         BNH   PPSN0080            GONE - RETURN CC NOT = ZERO                  
         B     PPSN0120            OKAY - RETURN CC = ZERO                      
PPSN0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     PPSN0160                                                         
PPSN0120 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
PPSN0160 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DEVCONTY:  VALIDATE ANY DEVELOPMENTAL CONTRACT TYPE ENTERED.                
*                                                                               
DEVCTERR EQU   416                                                              
DEVSPREQ EQU   420                                                              
*                                                                               
DEVCONTY CSECT                                                                  
         NMOD1 0,*DVCT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   NOTE: ALWAYS CHECK CONTRACT TYPE.  IGNORE PREVIOUSLY VALID BIT              
*****>   TM    4(R2),X'20'         VALID DEVELOPMENTAL CONTRACT TYPE?           
*****>   BO    DVCT0020                                                         
*                                  NO  - CHANGE MADE                            
         XC    MYP+5(2),MYP+5         CLEAR DEV CONTYPE CODE                    
         XC    TWDCTEXP,TWDCTEXP      CLEAR EXPANSION AREA                      
         CLI   5(R2),0             ANY DATA?                                    
         BNE   DVCT0010            YES - CHECK IT OUT                           
         LA    R3,DEVSTMIS                                                      
         TM    PROFILES+CNTRDEVB,CNTRDEVA                                       
*                                  NO  - REQUIRES DEVSAL AND DEVTYPE?           
         BZ    DVCT0005                                                         
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    ERROR                                                            
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    DVCT0005            SKIP REQUIRED FIELD FOR AUTOHEADER?          
         B     ERROR               YES - ERROR                                  
*                                                                               
DVCT0005 EQU   *                                                                
         TM    PROFILES+CNTDEVSB,CNTDEVSA                                       
*                                  NO  - S/P REQUIRES TYPE?                     
         BO    DVCT0008            NO  - FINISHED                               
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    DVCT0020                                                         
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    DVCT0008            SKIP REQUIRED FIELD FOR AUTOHEADER?          
         B     DVCT0020            NO  - FINISHED                               
*                                                                               
DVCT0008 EQU   *                                                                
         OC    MYP+2(3),MYP+2      YES - ANY S/P ENTERED?                       
         BZ    DVCT0020            NO  - CONTYPE NOT NEEDED                     
         XC    MYP,MYP             YES - CONTYPE NOT PRESENT                    
*                                     SET ERROR FLAG                            
         B     DVCT0060            RETURN WITH ERROR                            
*                                                                               
DVCT0010 EQU   *                                                                
         GOTO1 VMOVE                                                            
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RDSPKTYP,X'3B'      DEVELOPMENTAL SALESMAN REC TYPE              
         MVC   RDCTKREP,REPALPHA   REP CODE                                     
         MVC   RDCTKCTY,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DVCT0060            EXIT NG:    CC NOT = ZERO                    
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWDCTEXP,RDCTDESC                                                
*                                  SAVE EXPANSION NAME                          
         FOUT  CONDCTNH,RDCTDESC,15                                             
*                                  DEV CONTYPE'S NAME                           
         MVC   MYP+RCONDVCT-RCONDVEL(2),RDCTKCTY                                
*                                  INSERT DEV CONTRACT TYPE                     
         SPACE 5                                                                
DVCT0020 EQU   *                                                                
         OC    MYP+2(L'MYP-2),MYP+2 ANYTHING IN FIELD?                          
         BZ    DVCT0040            NO  - DON'T ADD TO RECORD                    
*                                     EXIT OKAY:  CC = ZERO                     
         GOTO1 VADDELEM,DMCB,RCONREC,MYP                                        
*                                  YES - ADD ELEMENT TO RECORD                  
DVCT0040 EQU   *                                                                
         SR    RF,RF               SET CC = ZERO                                
         LTR   RF,RF                                                            
         B     DVCT0080                                                         
DVCT0060 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
DVCT0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CHECK IF ANY MAKEGOOD OFFERS. IF SO, WE NEED TO CHANGET THE KEYS              
* SINCE THE OFFICE CHANGED                                                      
*********************************************************************           
CHGMGKEY CSECT                                                                  
         NMOD1 0,*CHMGK*                                                        
         L     RC,0(R1)                                                         
*                                                                               
         XC    KEY,KEY                                                          
MGKEY    USING RMKGKEY,KEY                                                      
         MVI   MGKEY.RMKGKTYP,X'11'                                             
         MVC   MGKEY.RMKGKREP,REPALPHA                                          
         MVC   MGKEY.RMKGKOFF,WORK                                              
         MVC   MGKEY.RMKGKSTA,RCONKSTA                                          
         MVC   MGKEY.RMKGKCON,TWACNUM                                           
         DROP  MGKEY                                                            
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
*                                                                               
CMGK10   DS    0H                  EXIT FOR NO MAKEGOOD OFFERS                  
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   CMGKX                                                            
*                                                                               
         MVC   WORK(27),KEY                                                     
*                                                                               
         MVI   UPDATE,C'Y'         UPDATE RECORD                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
IOD      USING RMKGREC,IOAREA                                                   
         MVC   IOD.RMKGKOFF,RCONKOFF                                            
         DROP  IOD                                                              
*                                                                               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
*                                                                               
         OI    KEY+27,X'80'        DELETE OLD KEY                               
         GOTO1 VWRITE                                                           
*                                                                               
MGKEY    USING RMKGKEY,KEY                                                      
         MVC   MGKEY.RMKGKOFF,RCONKOFF                                          
         DROP  MGKEY                                                            
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     ADD/WRITE NEW KEY                            
         BNE   CMGK20                                                           
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE                                                           
         B     CMGK30                                                           
*                                                                               
CMGK20   DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VADD                                                             
*                                                                               
CMGK30   DS    0H                                                               
         MVC   KEY(27),WORK                                                     
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VHIGH               RE-READ ORIGINAL KEY AFTER A WRITE           
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         B     CMGK10                                                           
*                                                                               
CMGKX    DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GOOBR = ROUTINE TO SWAP TO & INVOKE RECORD BROWSER                  *         
*                                                                     *         
*  P1 = RC                                                            *         
*  P2 = 4 CHAR REC TYPE (EG: C'AGY ' OR C'ADV ')                      *         
*  P3 = A(KEYWORD), HIGH ORDER BYTE = MAX LENGTH OF KEYWORD           *         
*  P4 = HIGH ORDER BYTE  = RECORD OPTIONS BYTE                        *         
*                                                                     *         
***********************************************************************         
GOOBR    CSECT                                                                  
         NMOD1 0,*GOOBR                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*  CREATE BROWSE ELEMENT FOR GLOBBER                                            
*                                                                               
         XC    IOAREA(GLBRWLNQ),IOAREA                                          
         LA    R2,IOAREA                                                        
         USING GLBRWKW,R2                                                       
         L     R4,8(R1)                                                         
         ZIC   R3,8(R1)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   GLBRWKW(0),0(R4)                                                 
         MVC   GLBRWREC,4(R1)                                                   
         MVC   GLBRWFLG,12(R1)                                                  
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),DMCB,=C'PUTD',IOAREA,GLBRWLNQ,GLRBRWSE                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
* CREATE GLOBBER CONTROL ELEMENT                                                
*                                                                               
         XC    IOAREA(GLVXLENQ),IOAREA                                          
         LA    R2,IOAREA                                                        
         USING GLVXFRSY,R2                                                      
         MVC   GLVXFRSY,=C'REP'                                                 
         MVC   GLVXFRPR,=C'CON'                                                 
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'BRO'                                                 
***>>>   MVI   GLVXFLG1,GLV1GOTO+GLV1SEPS   INVOKE BASE                         
         MVI   GLVXFLG1,GLV1SEPS            INVOKE BASE                         
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),DMCB,=C'PUTD',IOAREA,GLVXLENQ,GLVXCTL                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         L     RD,BASERD           GET ALL THE WAY OUT OF HERE                  
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRODUCT CODE MUST EXIST. IF AGENCY ENERED, ORDER'S AGENCY FIELD               
* MUST EQUAL THE PRODUCT AGENCY CODE. FLIGHT DATES SHOULD BE INCLUSIVE          
***********************************************************************         
VALAGFD  NMOD1 0,*VALAGFD*                                                      
         L     RC,0(R1)                                                         
* SEE IF PRODUCT EXISTS                                                         
         LA    R2,CONPRDH                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,CONADV     ADVERTISER CODE                              
         OC    RPRDKADV,MYSPACES                                                
         MVC   RPRDKPRD,10(R2)     PRODUCT                                      
         OC    RPRDKPRD,MYSPACES                                                
         MVC   RPRDKREP,REPALPHA   REP CODE                                     
         MVC   TWASVKEY,KEY                                                     
         MVC   KEY(27),RPRDKEY                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VALERR1            IF NOT PRESENT, ERROR                         
         MVC   KEY(27),TWASVKEY                                                 
* VALIDATE AGY AND OFFICE                                                       
         LA    R2,CONAGYH          AGENCY FIELD                                 
         CLI   5(R2),0             AGENCY CODE ENTERED?                         
         BNE   VALAG10             YES                                          
         LA    R2,CONDTESH         DATES FIELD                                  
         CLI   5(R2),0             FLIGHT DATES ENTERED?                        
         BE    VALAGFDX            NOTHING TO VALIDATE                          
VALAG10  EQU   *                                                                
         GOTO1 VGETREC,DMCB,AIO3                                                
         L     R6,AIO3                                                          
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   VALAGFDX                                                         
         USING RPRDAGFL,R6                                                      
         LA    R2,CONAGYH                                                       
         CLI   5(R2),0                                                          
         BE    VALFL20                                                          
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 SCANNER,DMCB,(R2),(1,WORK2),C',=,-'                              
         CLC   RPRDAGAG,=F'0'      AGENCY?                                      
         BE    VALFL20             NOTHING TO COMPARE                           
         CLC   RPRDAGAG,WORK2+12   AGENCY FIELD AND PRODUCT AGY CODE            
         BNE   VALERR2                                                          
         CLC   RPRDAGAO,MYSPACES   PARENT LEVEL?                                
         BE    VALFL20             YES                                          
         CLC   RPRDAGAO,WORK2+22   OFFICE FIELD AND PRODUCT OFF CODE            
         BNE   VALERR3                                                          
VALFL20  EQU   *                                                                
* VALIDATE FLIGHT DATES                                                         
         XC    WORK2,WORK2                                                      
         XC    WORK,WORK                                                        
         LA    R2,CONDTESH         DATES FIELD                                  
         CLI   5(R2),0             FLIGHT DATES ENTERED?                        
         BE    VALAGFDX            NOTHING TO VALIDATE                          
         CLI   RPRDAGDF,X'0'                                                    
         BE    VALAGFDX                                                         
         CLI   RPRDAGDT,X'0'                                                    
         BE    VALAGFDX                                                         
         GOTO1 SCANNER,DMCB,CONDTESH,(2,WORK2),C',=-='                          
         CLI   DMCB+4,2            IF NOT 2 DATES, ERROR                        
         BE    VALFL30                                                          
         LA    R3,EDTERR           MISSING END DATE                             
         B     ERROR                                                            
VALFL30  LA    R3,SDTERR                                                        
         GOTO1 DATVAL,DMCB,(0,WORK2+12),WORK                                    
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,WORK,(3,TWASVSDT)                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,WORK2+44),WORK+6                                  
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,WORK+6,(3,TWASVEDT)                                  
         CLC   RPRDAGDF,TWASVSDT                                                
         BH    VALERR4                                                          
         CLC   RPRDAGDT,TWASVEDT                                                
         BL    VALERR4                                                          
*        CLC   CONACT(3),=C'CHA'                                                
*        BNE   VALAGFDX                                                         
*        GOTO1 VGETREC,DMCB,AIO3                                                
         B     VALAGFDX                                                         
*                                                                               
VALERR1  EQU   *                                                                
         LA    R3,109              INVALID  PRODUCT CODE                        
         B     ERROR                                                            
VALERR2  EQU   *                                                                
         LA    R3,647              AGENCY != PR. AGENCY CODE                    
         B     ERROR                                                            
VALERR3  EQU   *                                                                
         LA    R3,660              OFFICE != PRODUCT OFFICE CODE                
         B     ERROR                                                            
VALERR4  EQU   *                                                                
         LA    R3,65               OVERLAPPING DATES                            
         B     ERROR                                                            
VALERR5  EQU   *                                                                
         LA    R3,2                INVALID INPUT FIELD                          
         B     ERROR                                                            
VALAGFDX EQU   *                                                                
         DROP  R6                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
STAEXCL  EQU   648                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETFIL:    SWITCHES TO SOURCE REP FILE, IF NECESSARY.                       
*                                                                               
SETFIL   NMOD1 0,*SFIL*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   TWATKSES,TWATKSET   SOURCE/TARGET ON SAME FILE?                  
         BE    SETF0040            YES - DON'T SWITCH                           
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(TWATKSES,X'FFFFFFFF'),0                               
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    SETF0040            YES - CONTINUE TO PROCESS                    
         CLI   4(R1),2             NO  - SYSTEM NOT OPENED?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO  - OTHER REASON                           
*                                                                               
*                                  ERROR: SWITCH BACK TO TARGET REP             
*        GOTO1 (RF),DMCB,(X'08',X'FFFFFFFF'),0                                  
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
*                                                                               
*   MUST EXIT WITH ERROR:  HOW TO INTERFACE?                                    
*                                                                               
****>>>  BE    SRCCLOSD            YES - EXIT WITH MESSAGE                      
         DC    H'0'                NO  - ABORT                                  
*                                                                               
SETF0040 EQU   *                                                                
         XIT1                      SWITCH FINISHED                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RESETFIL:  SWITCHES TO TARGET REP FILE, IF NECESSARY.                       
*                                                                               
RESETFIL NMOD1 0,*RSFL*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   TWATKSES,TWATKSET   SOURCE/TARGET ON SAME FILE?                  
         BE    RSFL0080            YES - NO NEED TO SWITCH BACK                 
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*        GOTO1 (RF),DMCB,(X'08',X'FFFFFFFF'),0                                  
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    *+6                                                              
         DC    H'0'                NO  - ABORT                                  
RSFL0080 EQU   *                                                                
         XIT1                      SWITCH FINISHED                              
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET RECNT10    AT LEVEL 204 AS OF 06/24/97                      
*********************************************************************           
* PRDLOCK DECREMENTS PREVIOUS PRD CODE LOCK COUNTER                 *           
*********************************************************************           
PRDLOCK  NMOD1 0,*PDLK*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         CLC   SVPRD,MYSPACES      NO PRD CODE, DON'T BOTHER                    
         BE    PRDLXIT                                                          
*                                                                               
         XC    RPRDKEY,RPRDKEY     FIND PREVIOUS PRD REC SO WE                  
         MVI   RPRDKTYP,X'09'      CAN DECREMENT LOCK COUNTER                   
         MVC   RPRDKADV,SVADV      PREVIOUS ADVERTISER                          
         MVC   RPRDKPRD,SVPRD      PREVIOUS PRD CODE                            
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,RPRDKEY                                                      
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PRDLXIT             IF NOT PRESENT, DON'T WORRY ABOUT IT         
*                                  MIGHT BE DELETED ALREADY                     
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RPRDREC                                             
         MVI   ELCODE,X'01'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BNE   PRDLXIT                                                          
*                                                                               
         OC    RPRDLOCK,RPRDLOCK   ALREADY ZERO, DON'T SUBT.. COUNTER           
         BZ    PRDLXIT             MIGHT NOT BE IN SYNC, BUT LET IT GO          
         ZICM  RF,RPRDLOCK,2       DECREMENT LOCK COUNTER                       
         SH    RF,=H'1'                                                         
         STCM  RF,3,RPRDLOCK                                                    
         GOTO1 VPUTREC,DMCB,RPRDREC                                             
         B     PRDLXIT                                                          
*                                                                               
PRDLXIT  XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         CSECT                                                                  
T80258X  DS    3000C               SIZING                                       
         ORG   T80258X                                                          
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120RECNT58   05/01/02'                                      
         END                                                                    
