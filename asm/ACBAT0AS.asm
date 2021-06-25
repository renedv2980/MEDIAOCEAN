*          DATA SET ACBAT0AS   AT LEVEL 004 AS OF 10/03/03                      
*PHASE T61B0AA                                                                  
T61B0A   TITLE 'TYPE 10- MULTIPLE BILLABLE/NON-BILLABLE EXPENSE ENTRY'          
*                                                                               
T61B0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,T61B0A,RR=R5,CLEAR=YES                              
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    R5,RELOA                                                         
         ST    RB,BASERB                                                        
         LA    R1,PROGD                                                         
         AHI   R1,EXCBLK-PROGD                                                  
         ST    R1,AEXCELD          SET POINTER TO EXCEL BLOCK                   
         LA    R1,PROGD                                                         
         AHI   R1,CATBLK-PROGD                                                  
         ST    R1,ACATD            SET POINTER TO CATCALL BLOCK                 
*                                                                               
         LA    RF,ADTAB            RESOLVE TABLE ADDRESS                        
MUL3     XR    R1,R1                                                            
         ICM   R1,3,0(RF)          R1=DISPLACEMENT TO TABLE                     
         AR    R1,RB               R1=A(TABLE)                                  
         XR    R2,R2                                                            
         ICM   R2,3,2(RF)          R2=DISPLACEMENT INTO W/S                     
         LA    R2,PROGD(R2)        R2=LOCATION IN W/S                           
         STCM  R1,15,0(R2)         SAVE ADDRESS OF TABLE                        
         LA    RF,L'ADTAB(RF)                                                   
         CLI   0(RF),X'FF'                                                      
         BNE   MUL3                                                             
*                                                                               
         LA    RF,TWAD             RESTORE SAVED VALUES                         
         AHI   RF,OSSAVE-TWAD                                                   
         MVC   STS(STSLNQ),0(RF)                                                
*                                                                               
         BRAS  RE,SETSCR           SET SCREEN PARAMETERS                        
*                                                                               
         MVI   PZERO,X'0C'         SET PACKED ZERO                              
         MVC   WC99,=C'99'                                                      
         MVC   SXVND,=C'SX'                                                     
         TM    PROCSW,PROCFST      TEST FIRST TIME                              
         BO    MUL5                NO                                           
         ZAP   TOTDLRS,PZERO       INITIALIZE TOTAL CASH                        
         MVI   PROCSW,PROCFST      SET NEXT TIME                                
         MVI   VPFSW1,VPFTAX       ALLOW PF TO TAX                              
         MVC   FRSTSCRN,TWASCRN    SAVE FIRST SCREEN NUMBER                     
*                                                                               
MUL5     NI    BUFSW,X'FF'-(BUFRES)                                             
         BRAS  RE,SETUPR           SET UPPER SCREEN FIELD ADDRESSES             
         L     R2,ATOTH                                                         
         MVC   FLD,SPACES                                                       
         BRAS  RE,MOVEFLD          CLEAR TOTAL FIELD                            
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BASSRVH          PREPARE BH MSG FLD                           
         OI    1(R2),X'01'         TURN ON MODIFIED BIT                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         GOTOR SAVTOP              SAVE UPPER PORTION OF SCREEN                 
*                                                                               
         CLI   PFKEY,PFCLRQ        TEST  CLEAR                                  
         BE    MUL15                                                            
         BAS   RE,TSTNUM           SET MUNBER OF DETAIL LINES                   
         OC    NBUF,NBUF           TEST ANY DETAILS                             
         BNZ   MUL7                YES, CAN'T SWITCH YET                        
         CLI   NITM,0              TEST ANY DETAILS                             
         BNE   MUL7                YES, MUST EDIT                               
         CLI   PFKEY,PFSWAPQ                                                    
         BE    MUL15                                                            
         CLI   PFKEY,PFTAXQ                                                     
         BE    MUL15                                                            
         CLI   PFKEY,PFINVQ                                                     
         BE    MUL15                                                            
*                                                                               
MUL7     GOTOR CLEAR,AUPNTAB       CLEAR UPPER NAME FIELDS                      
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
         GOTOR SAVTOP              SAVE ANY CHANGED FIELDS                      
         CLI   LONGINVL,0          ANY LONG INVOICE ?                           
         BE    MUL9                NO, SKIP CHECK DUP                           
         LA    R3,VENDACCT         PRODUCTION VENDOR                            
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         LA    R3,XVNDACCT         OR EXPENSE VENDOR                            
         GOTO1 ACHKINV,DMCB,2(R3),3(R3),LONGINV,DOCDATE                         
         BE    MUL9                NOT A DUPLCATE                               
         ICM   R2,15,AOVRH         OVERRIDE FIELD                               
         BZ    MUL8                                                             
         CLI   8(R2),C'Y'                                                       
         BE    MUL9                                                             
MUL8     L     R2,ADOCH                                                         
         J     EMDUPIT             DUPLICATE ITEM                               
*                                                                               
MUL9     CLI   NITM,0              TEST ANY DETAILS                             
         BNE   MUL11               YES, MUST EDIT                               
         CLI   PFKEY,PFSWAPQ                                                    
         BE    MUL15                                                            
         CLI   PFKEY,PFTAXQ                                                     
         BE    MUL15                                                            
         CLI   PFKEY,PFINVQ                                                     
         BE    MUL15                                                            
         OC    NBUF,NBUF           TEST ANYTHING IN BUFFER                      
         BNZ   MUL13               YES, OK TO TEST PF KEYS                      
         L     R2,ADETH                                                         
         J     EMMISIF             'MISSING INPUT...'                           
*                                                                               
MUL11    BAS   RE,EDT              CHECK THE DETAILS                            
*                                                                               
MUL13    CLI   PFKEY,X'FF'                                                      
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
         CLI   PFKEY,0             TEST FOR ANY PFKEY                           
         BE    MUL17               NO                                           
*                                                                               
MUL15    GOTOR PF                                                               
         TM    PROCSW,PROCDTL      TEST PROCESS DETAIL                          
         BNO   *+8                                                              
         BAS   RE,EDT              EDIT AND POST                                
         TM    POSTSW,POSTNXT      TEST MORE POSTINGS                           
         BO    MUL15               YES, GO BACK                                 
*                                                                               
         TM    PFKSW,PFKPROC       PF KEY PROCESSD(UP/DOWN/SWAP) ?              
         BNO   MUL17                                                            
         L     R2,ADETH                                                         
         TM    PFKSW,PFKDELT                                                    
         JO    IMITDEL             ITEM DELETED                                 
         J     IMITEMS             'ITEM(S) XXX DISPLAYED...'                   
*                                                                               
MUL17    BRAS  RE,SAVDET           SAVE THE DETAIL                              
         L     R2,ADOCH                                                         
         J     IMIOKNX             'INPUT COMPLETE...'                          
         EJECT                                                                  
**********************************************************************          
* TEST NUMBER OF DETAIL LINES WITH DATA                              *          
**********************************************************************          
TSTNUM   NTR1  ,                                                                
         XR    R4,R4                                                            
         XR    R6,R6                                                            
         IC    R6,SCRMXD           R6=MAX LINES                                 
         L     R2,ADETH                                                         
*                                                                               
TSTNUM3  BRAS  RE,SETLOW           SET ADCONS FOR LOWER                         
         XR    R0,R0                                                            
         LA    R3,ACLIH                                                         
         LA    R5,NLOWER-1                                                      
*                                                                               
TSTNUM5  ICM   R2,15,0(R3)                                                      
         BZ    TSTNUM6                                                          
         TM    1(R2),X'20'         SKIP PROTECTED                               
         BO    TSTNUM6                                                          
         CLI   5(R2),0             TEST FOR EMPTY FIELD                         
         BE    TSTNUM6                                                          
         LA    R0,1                SET 'THIS LINE HAS DATA'                     
TSTNUM6  LA    R3,4(R3)                                                         
         BCT   R5,TSTNUM5                                                       
*                                                                               
         AR    R4,R0               INCREMENT NUMBER OF LINES WITH DATA          
*                                                                               
         L     R2,ANEXT                                                         
         LTR   R0,R0               TEST LAST WAS 'EMPTY LINE'                   
         BZ    TSTNUM7             YES, REST OF SCREEN MUST BE EMPTY            
         BCT   R6,TSTNUM3                                                       
         STC   R4,NITM             SCREEN IS FULL                               
         J     XIT                                                              
*                                  *HAVE ENCOUNTERED A BLANK LINE*              
TSTNUM7  CLI   NITM,0              TEST ANY PREVIOUS DATA                       
         BE    *+12                NO, CAN'T BE DELETE ERROR                    
         CLM   R4,1,NITM           TEST SAME NUMBER OF ITEMS                    
         BL    TSTNUM13            NO, MUST HAVE DELETED A LINE                 
         STC   R4,NITM                                                          
         BCT   R6,TSTNUM8                                                       
         J     XIT                 END OF SCREEN                                
*                                                                               
TSTNUM8  BRAS  RE,SETLOW           LOOK FOR DATA AFTER BLANK LINE               
         LA    R3,ACLIH                                                         
         LA    R5,NLOWER-1                                                      
*                                                                               
TSTNUM9  ICM   R2,15,0(R3)                                                      
         BZ    TSTNUM11                                                         
         CLI   5(R2),0             MAKE SURE FIELD IS EMPTY                     
         JNE   EMINVIF             'INVALID INPUT FIELD'                        
*                                                                               
TSTNUM11 LA    R3,4(R3)                                                         
         BCT   R5,TSTNUM9                                                       
         L     R2,ANEXT                                                         
         B     TSTNUM7                                                          
*                                                                               
TSTNUM13 OC    NFST,NFST           TEST BLANK SCREEN                            
         BNZ   *+12                                                             
         MVI   NITM,0                                                           
         J     XIT                                                              
*                                                                               
         MVC   NGET,NFST           SET FOR REDISPLAY                            
         L     R2,ACLIH            R2=A(MISSING DATA)                           
         OI    PFKSW,PFKRDSP                                                    
         GOTOR PF                                                               
         J     EMUPFKD             'USE PFKEY TO DELETE..'                      
         EJECT                                                                  
**********************************************************************          
* MANAGE EDIT OF DETAILS                                             *          
**********************************************************************          
EDT      NTR1  ,                                                                
         XR    R6,R6                                                            
         IC    R6,NITM             R6=NUMBER OF DETAIL LINES WITH DATA          
         L     R2,ADETH            R2=A(FIRST ITEM FIELD)                       
*                                                                               
EDT3     LA    RE,DETVALS          CLEAR DETAIL VALUES                          
         LA    RF,DETVALNQ                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ZAP   INAMNT,PZERO        INIT AMOUNT FIELDS                           
         ZAP   CDAMNT,PZERO                                                     
*                                                                               
         BRAS  RE,SETLOW           SET LOWER ADCONS                             
         XR    RF,RF                                                            
         ICM   RF,3,SCRPROC        PROCESSING ROUTINE                           
         A     RF,BASERB                                                        
         GOTOR (RF),DMCB,PROGD     EDIT DETAIL LINE                             
         L     R2,ANEXT                                                         
         BCT   R6,EDT3                                                          
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* CONSTANST AND LITERAL POOL                                         *          
**********************************************************************          
ADTAB    DS    0F                                                               
         DC    AL2(USATAB-T61B0A,AUSATAB-PROGD)                                 
         DC    AL2(CANTAB-T61B0A,ACANTAB-PROGD)                                 
         DC    AL2(UPRTAB-T61B0A,AUPRTAB-PROGD)                                 
         DC    AL2(UPNTAB-T61B0A,AUPNTAB-PROGD)                                 
         DC    AL2(XJOTAB-T61B0A,AXJOTAB-PROGD)                                 
         DC    AL2(ORDTAB-T61B0A,AORDTAB-PROGD)                                 
         DC    AL2(RCDTAB-T61B0A,ARCDTAB-PROGD)                                 
         DC    AL2(PFKTAB-T61B0A,APFKEYS-PROGD)                                 
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE HEADLINE FIELDS                                           *          
**********************************************************************          
VALHED   NMOD1 0,**VALHED                                                       
         L     RC,0(R1)                                                         
         BAS   RE,VALORD           ORDER NUMBER                                 
         BAS   RE,VALDOC           DOCUMENT NUMBER                              
         BAS   RE,VALDAT           DATE                                         
         BAS   RE,VALDUE           DUE DATE                                     
*                                                                               
VALHED2  L     R2,AURGH            VALIDATE URGENT                              
         GOTO1 AFVAL,(R2)                                                       
         MVC   URGENT,FVIFLD                                                    
         BNE   VALHED4             NOTHING IN FIELD                             
         CLI   FVIFLD,C'U'         TEST FOR VALID INPUT                         
         BE    VALHED4             YES                                          
         J     EMINVIF             'INVALID INPUT FIELD'                        
*                                                                               
VALHED4  BAS   RE,VALCASH          CASH ACCOUNT                                 
         LA    RE,VENDVALS         CLEAR SAVED VENDOR VALUES                    
         LA    RF,VENDLNQS                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ZAP   VENDDISC,PZERO     INIT DISCOUNT SAVES                           
         ZAP   XVNDDISC,PZERO                                                   
*                                                                               
         L     R2,AVENH           PRODUCTION VENDOR FIELD                       
         CLI   5(R2),0            IS THERE A PROD VENDOR?                       
         BE    VALHED8            NO                                            
         MVI   TYPE,C'P'          SET PROD VENDOR SWITCH                        
         BAS   RE,VEN             EDIT VENDOR AND CD FIELDS                     
*                                                                               
VALHED8  TM    BCCPYST5,CPYSVEND   COPY THE VENDOR?                             
         BNO   *+8                 NO                                           
         BAS   RE,COPYVEN          COPY PROD VENDOR                             
         L     R2,AXVNH            TEST FOR AN EXPENSE VENDOR                   
         CLI   5(R2),0                                                          
         BE    VALHED9                                                          
         MVI   TYPE,C'E'           NOTE EXPENSE VENDOR                          
         BAS   RE,VEN              NOW VERIFY EXP VENDOR                        
         B     VALHED10                                                         
*                                                                               
VALHED9  TM    BCCPYST2,CPYSVENR   IS VENDOR REQUIRED?                          
         BZ    VALHED10            NO                                           
         CLI   VENDACCT,0          WAS PROD VENDOR ENTERED?                     
         BNE   VALHED10            YES                                          
         CLI   XVNDACCT,0          WAS EXP VENDOR ENTERED?                      
         BNE   VALHED10            YES                                          
         L     R2,AVENH            POINT TO PROD VENDOR FIELD                   
         J     EMPEVIR             'PROD OR EXPENSE VENDOR REQUIRED'            
*                                                                               
VALHED10 L     R2,AXVNH                                                         
         GOTO1 ANARRSCN,DMCB,(2,ANARH),0                                        
         MVC   NARR,BOELEM                                                      
         MVC   NARRLEN,BOHALF1                                                  
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE ORDER NUMBER                                              *          
**********************************************************************          
VALORD   NTR1  ,                                                                
         L     R2,AORDH                                                         
         TM    4(R2),X'20'                                                      
         JO    XIT                                                              
         CLI   5(R2),0             ANY ORDER NUMBER ?                           
         BNE   VALORD2             YES,                                         
         XC    ORDRNUM,ORDRNUM     CLEAR ORDER NUMBER                           
         OC    LASTORD,LASTORD     ANY OLD ORDER ?                              
         BNZ   VALORD21                                                         
         OI    4(R2),X'20'         SET VALIDATED                                
         J     XIT                                                              
*                                                                               
VALORD2  XC    WORK,WORK                                                        
         MVI   ORDSTA,0                                                         
         LA    R3,WORK                                                          
         USING SCANBLKD,R3                                                      
         GOTO1 SCANNER,DMCB,(R2),(3,(R3))                                       
         CLI   4(R1),0                                                          
         JE    EMINVIF            'INVALID INPUT...'                            
         CLI   4(R1),2                                                          
         JH    EMINVIF            'INVALID INPUT...'                            
         CLI   SC1STLEN,6         NOT MORE THAN SIX                             
         JH    EMINVIF                                                          
         CLI   SC2NDLEN,0         CAN'T HAVE RIGHT SIDE                         
         JNE   EMINVIF                                                          
         MVI   ORDRNUM,C'0'                                                     
         MVC   ORDRNUM+1(L'ORDRNUM-1),ORDRNUM                                   
         LA    RF,ORDRNUM+L'ORDRNUM                                             
         XR    R1,R1                                                            
         IC    R1,SC1STLEN        GET INPUT LEGNTH                              
         SR    RF,R1              RIGHT ALIGN                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SC1STFLD                                                 
*                                                                               
         LA    R3,SCBLKLQ(R3)                                                   
         CLI   SC1STLEN,0         TEST SECOND FIELD                             
         BE    VALORD3            NONE                                          
         CLI   SC2NDLEN,0         CAN'T HAVE RIGHT SIDE                         
         JNE   EMINVIF                                                          
         XR    R1,R1                                                            
         IC    R1,SC1STLEN        TEST PARTIAL                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'PART'                                             
         JNE   EMINVIF                                                          
         MVI   ORDSTA,FFNSPRTQ     SET PARTIAL                                  
*                                                                               
VALORD3  OC    LASTORD,LASTORD                                                  
         BZ    VALORD4                                                          
         CLC   LASTORD,ORDRNUM    SAME ORDER ?                                  
         BNE   VALORD21           MUST POST OLD  FIRST                          
         J     XIT                YES, DON'T REPROCESS                          
*                                                                               
VALORD4  LA    R4,IOKEY           BUILD ORDER KEY                               
         USING ORDRECD,R4                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,ORDRNUM                                                  
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'ORDKEY),IOKEYSAV                                         
         JNE   EMORDNF            'ORDER NOT FOUND...'                          
         L     R4,AIO1                                                          
         TM    ORDRSTAT,ORDSDEL+ORDSFMCH+ORDSLDEL                               
         JNZ   EMOFMCH           'ORDER IS FULLY MATCH...'                      
*                                                                               
         GOTOR CLEAR,AORDTAB       CLEAR ORDER FIELDS-TOP                       
         ZAP   INVDNUM,PZERO       NUMBER OF INVOICES AGAINST ORDER             
         ZAP   ORDRAMT,PZERO       AMOUNT OF ORDER                              
         XC    NORDR,NORDR                                                      
         LA    R3,ORDRFST                                                       
         USING ORDEL,R3                                                         
*                                                                               
VALORD5  CLI   0(R3),ORDELQ                                                     
         BE    VALORD7                                                          
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                MISSING ORDEL                                
         XR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VALORD5                                                          
*                                                                               
VALORD7  CLC   ORDACCU(2),=C'SJ'                                                
         BNE   VALORD8             MUST BE EXPENSE ORDER                        
         MVI   TYPE,C'P'                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ORDSUPA),ORDSUPA                                           
         CLC   BCCPYSUP,ORDSUPU                                                 
         BE    *+14                                                             
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(14),ORDSUPU                                                
         L     R2,AVENH            PRODUCTION VENDOR FIELD                      
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISVEN                                                   
         OI    6(R2),X'80'+X'01'                                                
         LR    R5,R3                                                            
         B     VALORD9                                                          
*                                                                               
VALORD8  MVI   TYPE,C'E'                                                        
         MVC   FLD,SPACES                                                       
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(14),ORDSUPU   EXPENSE SUPPLIER                             
         L     R2,AXVNH                                                         
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISXVN                                                   
         OI    6(R2),X'80'+X'01'                                                
*                                                                               
         LR    R5,R3                                                            
*                                                                               
VALORD9  XR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0             TEST EOR                                     
         BE    VALORD13                                                         
         CLI   0(R5),OAMELQ        TEST AMOUNT ELEMENT                          
         BNE   VALORD9                                                          
*                                                                               
         L     R2,ADETH                                                         
         BRAS  RE,ERASE            CLEAR BOTTOM OF SCREEN                       
         BRAS  RE,SETLOW           SET ADCONS FOR LOWER                         
*                                                                               
         USING OAMELD,R5                                                        
         AP    INVDNUM,OAMINUM     NUMBER OF INVOICES                           
         AP    ORDRAMT,OAMAMNT     AMOUNT OF ORDER                              
         CLI   TYPE,C'E'                                                        
         BE    VALORD11                                                         
         L     R2,ACLIH            CLIENT CODE                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(3),ORDACCA                                                   
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISCLI                                                   
         OI    6(R2),X'80'+X'01'                                                
*                                                                               
         L     R2,APROH            PRODUCT CODE                                 
         MVC   FLD,SPACES                                                       
         MVC   FLD(3),ORDACCA+3                                                 
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISPRO                                                   
         OI    6(R2),X'80'+X'01'                                                
*                                                                               
         L     R2,AJOBH            JOB CODE                                     
         MVC   FLD,SPACES                                                       
         MVC   FLD(6),ORDACCA+6                                                 
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISJOB                                                   
         OI    6(R2),X'80'+X'01'                                                
*                                                                               
         L     R2,AWKCH            WORKCODE                                     
         MVC   FLD,SPACES                                                       
         MVC   FLD(3),OAMWORK                                                   
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISWKC                                                   
         OI    6(R2),X'80'+X'01'                                                
*                                                                               
         L     R2,ANONH            NON-COMMISSIONABLE                           
         MVC   FLD,SPACES                                                       
         TM    OAMSTAT,OAMSNOCM                                                 
         BNO   *+8                                                              
         MVI   FLD,C'N'                                                         
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISNON                                                   
         OI    6(R2),X'80'+X'01'                                                
*                                                                               
VALORD11 L     R2,AAMTH            AMOUNT                                       
         MVC   FLD,SPACES                                                       
         ZAP   BCDUB,OAMAMNT       ORDER AMOUNT                                 
         SP    BCDUB,OAMIVAL       LESS: AMOUNT INVOICED                        
         CLC   OAMLAST,TODAYP                                                   
         BNE   *+10                                                             
         SP    BCDUB,OAMTVAL       LESS: AMOUNT INVOICED TODAY                  
         CURED BCDUB,(12,FLD),2,DMCB=BCPARM,MINUS=YES,ALIGN=LEFT                
         STC   R0,5(R2)            SET LENGTH                                   
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'+X'01'                                                
*                                                                               
         CLI   TYPE,C'P'                                                        
         BE    VALORD12                                                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ORDACCA),ORDACCA  EXPENSE ACCOUNT                          
         CLC   ORDACCU(2),=C'SE'                                                
         BE    *+14                                                             
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(14),ORDACCU                                                
         L     R2,AXACH                                                         
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISEXA                                                   
         OI    6(R2),X'80'+X'01'                                                
*                                                                               
VALORD12 L     R2,ADETH                                                         
         GOTOR ADDLIN,DMCB,(R2)    ADD NEW ITEM                                 
         OC    NORDR,NORDR         TEST FIRST FOR ORDER                         
         BNZ   *+10                                                             
         MVC   NORDR,NBUF          SAVE NUMBER OF FIRST                         
         B     VALORD9                                                          
*                                                                               
VALORD13 OI    PFKSW,PFKRDSP       FORCE RE-DISPLAY                             
         MVC   NGET,NORDR          FROM FIRST ORDER RECORD                      
         GOTOR PF                                                               
*                                                                               
         L     R2,AORDH                                                         
         OI    4(R2),X'20'         SET VALIDATED                                
         MVC   LASTORD,ORDRNUM     SAVE LAST ORDER                              
         L     R2,ADOCH                                                         
         J     IMODDEI            'ORDER DETAIL DISPLAYED..'                    
*                                                                               
VALORD21 MVC   FLD,SPACES          MUST REDISPALY OLD                           
         MVC   FLD(L'LASTORD),LASTORD                                           
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'LASTORD                                                  
         OI    6(R2),X'80'+X'01'                                                
         J     EMCCORD              CAN'T CHANGE ORDER NUMBER - POST..          
*                                                                               
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* VALIDATE DOCUMENT NUMBER FIELD                                     *          
**********************************************************************          
VALDOC   NTR1  ,                                                                
         XC    REFNUM,REFNUM                                                    
         XC    LONGINV,LONGINV                                                  
         MVI   FVMINL,1            REQUIRE SOME INPUT                           
         L     R2,ADOCH                                                         
         GOTO1 AFVAL,(R2)                                                       
         JNE   EMMISIF                                                          
*                                                                               
         LA    R3,TEMP                                                          
         USING SCANBLKD,R3                                                      
         GOTO1 SCANNER,DMCB,FVIHDR,(3,(R3)),=C',=,,'                            
         CLI   4(R1),0             TEST FOR INVALID SYNTAX                      
         JE    EMINVIF             'INVALID INPUT FIELD'                        
         CLI   4(R1),2                                                          
         JH    EMINVIF                                                          
         BE    VALDOC3             BRANCH IF TWO COMPONENTS                     
*                                                                               
         CLI   SC1STLEN,L'LONGINV  TEST NUMBER TOO LONG                         
         JH    EMINVIF                                                          
         MVC   REFNUML,SC1STLEN    SAVE REFERENCE                               
         MVC   REFNUM,SC1STFLD                                                  
         MVC   LONGINVL,SC1STLEN   SAVE LONG INVOICE NUMBER                     
         MVC   LONGINV,SC1STFLD                                                 
         CLI   SC1STLEN,L'REFNUM   TEST MORE THAN SIX                           
         BNH   VALDOCX             NO,                                          
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         USE LAST SIX FOR REFERENCE                   
         SHI   RF,L'REFNUM                                                      
         LA    RF,SC1STFLD(RF)                                                  
         MVC   REFNUML,L'REFNUM                                                 
         MVC   REFNUM,0(RF)                                                     
         J     VALDOCX                                                          
*                                                                               
VALDOC3  LA    R0,2                ** TWO COMPONENTS **                         
VALDOC5  CLI   SC1STLEN,L'LONGINV  TEST NUMBER TOO LONG                         
         JH    EMINVIF                                                          
         CLI   REFNUM,C' '         TEST ALREADY HAVE REFERENCE                  
         BH    VALDOC7             YES, ASSUME INVOICE NUMBER                   
         LA    R4,REFNUM           ASSUME FIRST IS REFERENCE                    
         LA    R5,REFNUML                                                       
         CLI   SC1STLEN,L'REFNUM   TEST MAX LENGTH                              
         BNH   VALDOC9                                                          
*                                                                               
VALDOC7  CLI   LONGINV,C' '        TEST ALREADY HAVE LONG INVOICE               
         JH    EMINVIF             YES, IT'S ERROR                              
         LA    R4,LONGINV          MUST BE LONG INVOICE NUMBER                  
         LA    R5,LONGINVL                                                      
*                                                                               
VALDOC9  SR    RF,RF                                                            
         IC    RF,SC1STLEN         SAVE LENGTH                                  
         STC   RF,0(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SC1STFLD    AND NUMBER                                   
*                                                                               
         LA    R3,SCBLKLQ(R3)                                                   
         BCT   R0,VALDOC5                                                       
*                                                                               
VALDOCX  OC    REFNUM,SPACES       BOTH ARE SPACES FILLED                       
         OC    LONGINV,SPACES                                                   
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE TRANSACTION DATE FIELD                                    *          
**********************************************************************          
VALDAT   NTR1  ,                                                                
         L     R2,ADATH            DATE FIELD                                   
         CLI   5(R2),0                                                          
         BNE   VALDDAT2                                                         
         GOTOR VDATCON,DMCB,(5,0),(0,WORK) DEFAULT TO CURRENT DATE              
         B     VALDDAT4                                                         
*                                                                               
VALDDAT2 GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  EDIT DATE INPUT                      
         OC    DMCB(4),DMCB                                                     
         JZ    EMINDAT             'INVALID DATE'                               
*                                                                               
VALDDAT4 GOTO1 DATCON,DMCB,(0,WORK),(1,DOCDATE) PACK IT Y/M/D                   
         CLC   DOCDATE,BCTDATL                                                  
         JL    EMDOPSP             'DATE OUTSIDE PERMITTED SPAN                 
         CLC   DOCDATE,BCTDATH                                                  
         JH    EMDOPSP                                                          
         CLI   5(R2),0              WAS THERE INPUT ?                           
         JNE   XIT                                                              
         OI    6(R2),X'80'          SET TRANS                                   
         GOTO1 DATCON,DMCB,(1,DOCDATE),(8,8(R2))                                
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE DUE DATE FIELD                                            *          
**********************************************************************          
VALDUE   NTR1  ,                                                                
         XC    DODATE,DODATE                                                    
         L     R2,ADUEH            DUE DATE FIELD                               
         CLI   5(R2),0                                                          
         JE    XIT                                                              
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         JZ    EMINDAT             'INVALID DATE'                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,FULL) PACK IT Y/M/D                      
         CLC   FULL(3),DOCDATE                                                  
         JNH   EMDOPSP                                                          
         CLC   FULL(3),BCTDATL                                                  
         JL    EMDOPSP             'DATE OUTSIDE PERMITTED SPAN                 
         CLC   FULL(3),BCTDATH                                                  
         JH    EMDOPSP                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,DODATE)                                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE CASH ACCOUNT FIELD                                         *         
***********************************************************************         
VALCASH  NTR1  ,                                                                
         XC    CASHACCT(51),CASHACCT  VALIDATE CASH ACCT                        
         L     R2,ACSHH                                                         
         CLI   5(R2),0             TEST FOR INPUT                               
         JE    XIT                                                              
*                                                                               
         XR    R3,R3                                                            
         IC    R3,5(R2)           LENGTH OF CASH ACCT                           
         BCTR  R3,0                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SC'    CASH U/L                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)     CASH ACCT CODE                                
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
         MVC   CASHACCT,ACCODE     CASH ACCT #                                  
         MVC   CASHNAME,ACNAME     CASH ACCT NAME                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACNAME),ACNAME                                             
         ICM   R2,15,ACSHNH                                                     
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AVCDH                                                         
         CLI   5(R2),0             IS THERE A PROD CD?                          
         JNE   EMCDALL             YES - FLAG ERROR                             
         L     R2,AXCDH                                                         
         CLI   5(R2),0             IS THERE AN EXP CD?                          
         JNE   EMCDALL             'NO CASH DISCOUNT ALLOWED'                   
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EDIT VENDOR FIELD                                                  *          
**********************************************************************          
VEN      NTR1  ,                                                                
         XR    R3,R3                                                            
         IC    R3,5(R2)           =LEN OF CREDIT ACCOUNT INPUT                  
         BCTR  R3,0               MINUS 1 FOR EX INSTRUCTION                    
         LA    R1,8(R2)                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         CLI   8(R2),C'*'         UNIT LEDGER INPUT OVERRIDE                    
         BE    VEN4               YES                                           
         LA    RF,COMPEL                                                        
         USING CPYELD,RF                                                        
         CLI   TYPE,C'P'          IS IT PROD VENDOR?                            
         BNE   VEN2               NO                                            
         MVC   KEY+1(2),CPYSUPP   ASSUME UNLESS OVERRIDDEN                      
         B     VEN12                                                            
*                                                                               
VEN2     MVC   KEY+1(1),=C'S'                                                   
         MVC   KEY+2(1),CPYSUPX   ASSUME UNLESS OVERRIDDEN                      
         B     VEN12                                                            
         DROP  RF                                                               
*                                                                               
VEN4     CLI   CASHACCT,0         IS THERE A CASH ACCOUNT?                      
         BNE   VEN6               YES - *SC NOT ALLOWED                         
         CLC   9(2,R2),=C'SC'     IS IT CASH ACCT?                              
         BE    VEN10                                                            
*                                                                               
VEN6     LA    RF,ADVCLIST        LIST OF OVERRIDES FOR PROD VEND               
         CLI   TYPE,C'P'          IS IT PRODUCTION VENDOR?                      
         BE    VEN8               YES                                           
         LA    RF,AGYCLIST        LIST OF OVERRIDES FOR EXP VEND                
*                                                                               
VEN8     CLI   0(RF),X'FF'        END OF LIST                                   
         JE    EMINACP            'INVALID ACCOUNT FOR POSTING '                
         CLC   9(2,R2),0(RF)      INPUT TO LIST                                 
         BE    VEN10                                                            
         LA    RF,2(RF)           NEXT VALID ENTRY TO CREDIT                    
         B     VEN8                                                             
*                                                                               
VEN10    MVC   KEY+1(2),9(R2)     2 CHAR U/L FROM INPUT                         
         SHI   R3,3               SUBTRACT *U/L FROM INPUT LEN                  
         LA    R1,11(R2)          POINT TO ACCT INPUT                           
VEN12    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(R1)     CREDIT ACCT TO KEY                            
*                                                                               
         CLI   TYPE,C'P'          IS THIS A PROD VENDOR?                        
         BNE   VEN16              NO                                            
         L     R1,AVCDH           CD FOR PROD VENDOR                            
         B     *+8                                                              
*                                                                               
VEN16    L     R1,AXCDH           CD FOR EXP VENDOR                             
         CLI   5(R1),0            INPUT                                         
         BE    VEN22              NO                                            
         CLI   CASHACCT,0         IS THERE A CASH ACCOUNT?                      
         JNE   EMCDALL            'NO CASH DISCOUNT ALLOWED'                    
*                                                                               
         CLI   8(R1),C'N'         IS THERE CASH DISCOUNT                        
         BE    VEN22              NO GO GET ACCT                                
         CLI   8(R1),C'Y'                                                       
         BE    VEN22                                                            
         LR    R2,R1                                                            
         J     EMINVIF            'INVALID INPUT FIELD'                         
*                                                                               
VEN22    MVC   RKEY,KEY           READ VENDOR FOR CD AND OTHER INFO             
         GOTO1 ARDHI,AIOAREA1                                                   
         L     RE,AIOAREA1                                                      
         CLC   KEYSAVE,0(RE)      WAS REC RETRIEVED                             
         BE    VEN24                                                            
         J     EMINACC            'INVALID ACCOUNT'                             
*                                                                               
VEN24    LA    R0,KEY             MOVE RECORD TO LOCAL STORAGE                  
         LA    R1,2000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,GETVEN          GET VENDOR DATA                               
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
*                                                                               
* PRODUCTION VENDOR FINISH                                                      
*                                                                               
         CLI   TYPE,C'P'          IS THIS A PROD VENDOR?                        
         BNE   VEN36              NO                                            
         L     R2,AVENH                                                         
         CLI   5(R2),0            WAS PROD VENDOR INPUT?                        
         JE    XIT                NO                                            
         MVC   VENDACCT,ACCODE    SAVE PROD VEN KEY                             
         MVC   VENDNAME,ACNAME    SAVE PROD VEN NAME                            
         MVC   PRSTAT,ACBSTAT     SAVE STATUS BYTE                              
*                                                                               
         CLI   CASHACCT,0         IS THERE A CASH ACCT?                         
         BE    VEN34              NO - DON'T WANT 2C OR 27                      
         TM    PRSTAT,ACBSVEND    DO WE NEED 2C FOR PROD VENDOR?                
         BZ    VEN34              NO.                                           
         MVC   KEY(15),VENDACCT   ACCT #                                        
         MVC   KEY+1(2),=C'2C'    U/L                                           
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
*                                                                               
         MVI   PCONSULT,C'Y'      SET 2C SWITCH                                 
         MVC   P2CNUM,ACCODE                                                    
         MVC   P2CNAM,ACNAME                                                    
         MVC   KEY+1(14),=CL14'27999'  BUILD 27 ACCT                            
         GOTOR AGETACC,DMCB,KEY,(R6)                                            
         JNE   EXIT                                                             
         MVC   PROTROL,ACCODE                                                   
         MVC   PROTROLN,ACNAME                                                  
*                                                                               
VEN34    MVC   FLD,SPACES                                                       
         MVC   FLD(20),VENDNAME                                                 
         MVC   FLD+21(10),ADRLIN1                                               
*                                                                               
         ICM   R2,15,AVENNH                                                     
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'        SET TRANSBIT                                  
         J     XIT                                                              
*                                                                               
* EXPENSE VENDOR FINISH                                                         
*                                                                               
VEN36    L     R2,AXVNH           WAS EXP VENDOR INPUT?                         
         CLI   5(R2),0                                                          
         JE    XIT                                                              
         MVC   XVNDACCT,ACCODE    EXP VENDOR ACCT #                             
         MVC   XVNDNAME,ACNAME    EXP VENDOR NAME                               
         MVC   EXSTAT,ACBSTAT     EXP 2C BIT (X'04')                            
         MVI   V29SW,0                                                          
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,XVNDACCT  EXPENSE BVENDOR                               
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         JNE   EMINACC            'INVALID ACCOUNT...'                          
         L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    RE,RE                                                            
         DROP  R4                                                               
*                                                                               
         USING RSTELD,R3          STATUS DSECT                                  
VEN38    CLI   0(R3),0            IS IT END OF RECORD?                          
         BE    VEN44                                                            
         CLI   0(R3),RSTELQ       IS IT X'30'?                                  
         BE    VEN40                                                            
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     VEN38                                                            
*                                                                               
VEN40    TM    RSTSTAT3,RSTSCAIV  DO WE NEED VENDOR AS CONTRA?                  
         BZ    VEN44              NO.                                           
         MVI   V29SW,C'Y'         YES.                                          
         DROP  R3                                                               
*                                                                               
VEN44    MVC   KEY(15),IOKEY      SAVE KEY                                      
         CLI   CASHACCT,0         IS THERE A CASH ACCT?                         
         BE    VEN48              NO - DON'T WANT 2C OR 27                      
         TM    EXSTAT,ACBSVEND    TEST NEED 2C VENDOR                           
         BZ    VEN48              NO.                                           
*                                                                               
         MVC   KEY(15),XVNDACCT                                                 
         MVC   KEY+1(2),=C'2C'                                                  
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
*                                                                               
         MVI   ECONSULT,C'Y'                                                    
         MVC   E2CNUM,ACCODE                                                    
         MVC   E2CNAM,ACNAME                                                    
         MVC   KEY+1(14),=CL14'27999'   BUILD 27                                
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
*                                                                               
         MVC   EXPTROL,ACCODE                                                   
         MVC   EXPTROLN,ACNAME                                                  
*                                                                               
VEN48    MVC   FLD,SPACES                                                       
         MVC   FLD(20),XVNDNAME                                                 
         MVC   FLD+21(10),ADRLIN1                                               
*                                                                               
         ICM   R2,15,AXVNNH                                                     
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* COPY THE PRODUCTION VENDOR TO EXPENSE VENDOR                       *          
**********************************************************************          
COPYVEN  ST    RE,SAVERE                                                        
         L     R1,AVENH                                                         
         GOTO1 AFVAL                                                            
         BNE   COPYVENX            NOTHING TO COPY                              
         MVC   FLD,FVIFLD                                                       
         L     R2,AXVNH                                                         
         TM    FVIHDR+4,X'20'      TEST IF PRODUCTION VENDOR CHANGED            
         BZ    *+12                YES                                          
         CLI   5(R2),0             TEST FOR EMPTY EXPENSE VENDOR                
         BNE   COPYVENX            NO-ASSUME ITS OK NOW                         
         MVC   5(1,R2),FVILEN      SET NEW INPUT LENGTH                         
         NI    4(R2),X'FF'-X'20'                                                
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
COPYVENX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* GET VENDOR RECORD DATA                                             *          
**********************************************************************          
GETVEN   NTR1  ,                                                                
         MVC   ADRLIN1,SPACES      ADDRESS LINE ONE                             
         MVI   ADRLIN1L,0                                                       
         LA    R4,IOAREA           FIND SOME VENDOR DATA                        
*                                                                               
GETVEN2  CLI   0(R4),0                                                          
         BNE   GETVEN6             DIDN'T FIND DISCOUNT ELEMENT                 
         CLI   TYPE,C'P'           IS THIS A PROD VENDOR?                       
         BNE   GETVEN4             NO                                           
         L     R2,AVCDH                                                         
         OI    6(R2),X'80'         PROD VENDOR CD                               
         MVI   8(R2),C' '                                                       
         J     XIT                 SKIP CD FOR PROD VENDOR                      
*                                                                               
GETVEN4  L     R2,AXCDH                                                         
         OI    6(R2),X'80'         EXP VENDOR CD                                
         MVI   8(R2),C' '                                                       
         J     XIT                 SKIP CD FOR EXP VENDOR                       
*                                                                               
GETVEN6  CLI   0(R4),RATEDSCQ      IS THIS A DIS ELEM?                          
         BE    GETVEN10            YES                                          
         CLI   0(R4),ITCELQ        TEST FOR INPUT TAX DEFAULT                   
         BE    GETVEN16                                                         
         CLI   0(R4),ADRELQ        TEST ADDRESS ELEMENT                         
         BE    GETVEN18                                                         
*                                                                               
GETVEN8  XR    R3,R3                                                            
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     GETVEN2                                                          
*                                                                               
GETVEN10 CLI   CASHACCT,0          TEST FOR CASH ACCOUNT                        
         BNE   GETVEN8             YES-NO CASH DISCOUNT ALLOWED                 
         L     RE,AVCDH            SET POINTER TO CD FIELD                      
         CLI   TYPE,C'P'           TEST PROD OR EXP VENDOR                      
         BE    *+8                                                              
         L     RE,AXCDH                                                         
         CLI   8(RE),C'N'          TEST CD SUPPRESSED                           
         BE    GETVEN8             YES                                          
*                                                                               
         MVC   HALF,2(R4)          PLACE CD IN VENDDISC SAVE FLD                
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         CLI   TYPE,C'P'           IS THIS A PROD VENDOR?                       
         BNE   GETVEN12            NO                                           
         ZAP   VENDDISC,DUB        CD VALUE FOR PROD VENDOR                     
         LA    R5,VENDDISC                                                      
         L     R1,AVCDH                                                         
         L     R6,AVCDNH                                                        
         B     GETVEN14            OUTPUT CD                                    
*                                                                               
GETVEN12 ZAP   XVNDDISC,DUB        CD VALUE FOR EXP VENDOR                      
         LA    R5,XVNDDISC                                                      
         L     R1,AXCDH                                                         
         L     R6,AXCDNH                                                        
*                                                                               
GETVEN14 OI    6(R1),X'80'                                                      
         MVI   8(R1),C'Y'                                                       
         CURED (P3,(R5)),(6,8(R6)),2,DMCB=BCPARM                                
         B     GETVEN8                                                          
*                                                                               
         USING ITCELD,R4                                                        
GETVEN16 LA    RE,VENDTAXT         SET POINTER TO DEFAULT TAX                   
         CLI   TYPE,C'P'           TEST FOR PRODUCTION VENDOR                   
         BE    *+8                 YES                                          
         LA    RE,XVNDTAXT                                                      
         CLI   0(RE),0             TEST IF WE ALREADY HAVE DEFAULT              
         BNE   GETVEN8             YES-NEXT ELEMENT                             
         CLC   DOCDATE,ITCEFFD     TEST TRANS DATE >= EFF DATE                  
         BL    GETVEN8                                                          
         MVC   0(1,RE),ITCTYPE     SET DEFAULT TYPE                             
         B     GETVEN8                                                          
*                                                                               
         USING ADRELD,R4                                                        
GETVEN18 MVC   ADRLIN1,ADRADD1     ADDRESS LINE 1                               
         GOTO1 VSQUASH,DMCB,ADRLIN1,L'ADRLIN1                                   
         MVC   ADRLIN1L,7(R1)                                                   
         B     GETVEN8                                                          
         DROP  R4                                                               
*                                                                               
ADVCLIST DS    0H    PRD. U/L'S VALID TO CREDIT                                 
         DC    C'SXSWSYSBSA'       SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
*                                                                               
AGYCLIST DS    0H    EXP. U/L'S VALID TO CREDIT                                 
         DC    C'SVSWSXSYSBSASFSL' SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS A DETAIL LINE                                              *          
**********************************************************************          
PROC     NMOD1 0,**PROC**,R8                                                    
         L     RC,0(R1)                                                         
         L     R2,AWKCH            R2=A(COMMISSIONABLE FIELD)                   
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BE    PROC10              NO-THIS IS AN EXPENSE ITEM                   
         MVI   TYPE,C'P'           NOTE PRODUCTION ITEM                         
*                                                                               
         CLC   8(2,R2),WC99        W/C 99 NOT ALLOWED                           
         JE    EMWC99                                                           
         MVI   COMSW,C'Y'                                                       
         L     R2,ANONH            R2=A(NON-COMMISSIONABLE FIELD)               
         CLI   5(R2),0             TEST FOR COMMISSIONABLE INPUT                
         BE    PROC4                                                            
         MVI   COMSW,C'N'                                                       
         CLI   8(R2),C'N'                                                       
         JNE   EMINVIF             'INVALID INPUT FIELD'                        
*                                                                               
PROC4    L     R2,AVENH                                                         
         CLI   VENDACCT,0          IS THERE AN PROD VENDOR?                     
         BNE   PROC9               YES                                          
         TM    BCCPYST2,CPYSVENR   IS PROD VEND REQUIRED?                       
         JO    EMVMEXP             'VENDOR INPUT MISSING FOR THIS ...'          
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         JE    EMVMEXP             'VENDOR INPUT MISSING FOR THIS ...'          
*                                                                               
PROC9    BAS   RE,PRODET           YES, EDIT LINE FOR ADV. EXP.                 
         B     PROC20                                                           
*                                                                               
PROC10   L     R2,AXVNH                                                         
         CLI   XVNDACCT,0          IS THERE AN EXP VENDOR?                      
         BNE   PROC15              YES                                          
         TM    BCCPYST2,CPYSVENR   IS PROD VEND REQUIRED?                       
         JO    EMVMEXP             'VENDOR INPUT MISSING FOR THIS ...'          
         CLI   CASHACCT,0          IS THERE A CASH ACCT                         
         JE    EMVMEXP             'VENDOR INPUT MISSING FOR THIS ...'          
*                                                                               
PROC15   MVI   TYPE,C'E'                                                        
         BAS   RE,EXPDET           EDIT LINE FOR AGY. EXP.                      
*                                                                               
PROC20   LA    R3,ACLIH            R3=A(FIELD POINTER)                          
         LA    R0,NLOWER-1         R0=LOOP COUNTER                              
*                                                                               
PROC22   ICM   R2,15,0(R3)                                                      
         BZ    PROC24                                                           
         TM    1(R2),X'20'                                                      
         BO    PROC24                                                           
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
PROC24   LA    R3,4(R3)                                                         
         BCT   R0,PROC22                                                        
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PRODUCTION DETAIL ITEM                                             *          
**********************************************************************          
PRODET   NTR1  ,                                                                
         L     R2,ACLIH                                                         
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    PRODET1                                                          
*                                                                               
         GOTOR CLEAR,AXJOTAB       CLEAR X-JOB RELATED FIELDS                   
         L     R1,APROH                                                         
         NI    4(R1),X'FF'-X'20'   TURN OFF PREVIOUS VALID                      
         L     R1,AJOBH                                                         
         NI    4(R1),X'FF'-X'20'                                                
*                                                                               
PRODET1  GOTOR DUP                 TEST FOR DUPLICATION                         
         CLI   5(R2),0                                                          
         JE    EMMISIF             'MISSING INPUT...'                           
         BAS   RE,VALCLI                                                        
         OI    4(R2),X'20'         SET VALID                                    
*                                                                               
         L     R2,APROH                                                         
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    PRODET3                                                          
         GOTOR CLEAR,AXJOTAB                                                    
         L     R1,AJOBH                                                         
         NI    4(R1),X'FF'-X'20'                                                
*                                                                               
PRODET3  GOTOR DUP                                                              
         CLI   5(R2),0                                                          
         JE    EMMISIF             'MISSING INPUT...'                           
         BAS   RE,VALPRO                                                        
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,AJOBH                                                         
         TM    4(R2),X'20'                                                      
         BO    PRODET5                                                          
         GOTOR CLEAR,AXJOTAB                                                    
*                                                                               
PRODET5  GOTOR DUP                                                              
         BAS   RE,VALJOB                                                        
*                                                                               
         OI    4(R2),X'20'                                                      
         GOTO1 AGETJOB,DMCB,JOBACCT  CALL GETOPT                                
         ORG   *-2                                                              
         L     RE,AWKCH                                                         
         LA    RE,8(RE)                                                         
         CLI   0(RE),C'*'                                                       
         BE    *+12                                                             
         ST    RE,4(R1)                                                         
         MVI   0(R1),X'80'         PASSING WORKCODE                             
         BASR  RE,RF                                                            
*                                                                               
         GOTO1 AVALGOB             TEST GOBATCH OPTION                          
         JNE   EXIT                ERROR                                        
*                                                                               
         L     R2,AWKCH            =A(CURRENT WORK CODE INPUT)                  
         GOTOR DUP                                                              
         GOTOR VALWC                                                            
*                                                                               
         L     R4,AGOBLOCK                                                      
         USING GOBLOCKD,R4                                                      
         MVI   PASSCD,C'Y'                                                      
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB?                            
         BO    PRODET7             YES, LEAVE PASSCD AS Y                       
         MVI   PASSCD,C'N'                                                      
         CLI   COCDPASS,C'N'       IS COMP. KEEPING ALL CD'S                    
         BE    *+10                YES                                          
         MVC   PASSCD,GOCLICD                                                   
         DROP  R4                                                               
*                                                                               
PRODET7  L     R2,ACOFH            CREDIT OFFICE                                
         GOTOR DUP                                                              
         BAS   RE,COFF                                                          
*                                                                               
         L     R2,AAMTH                                                         
         GOTOR DUP                                                              
         GOTOR VALAMT                                                           
*                                                                               
         BAS   RE,XJOB             X-JOB FIELDS EDIT                            
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    PRODET12            NO                                           
         GOTO1 AWRKVAL,DMCB,WRKCODE                                             
         JH    EMAEEWC             'AMOUNT EXCEEDS ESTIMAT...'                  
*                                                                               
PRODET12 CP    VENDDISC,PZERO      SAVED DISCOUNT                               
         BE    PRODET14                                                         
         ZAP   DUB,INAMNT          CALCULATE CD                                 
         MP    DUB,VENDDISC                                                     
         SRP   DUB,64-4,5          ROUNDED DIVIDE BY 10,000                     
         ZAP   CDAMNT,DUB                                                       
*                                                                               
PRODET14 ZAP   DUB,INAMNT                                                       
         SP    DUB,CDAMNT                                                       
         CP    VENDDISC,PZERO      IS THERE CASH VENDDISC                       
         BE    PRODET20            NO                                           
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   PRODET20            YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         PASSING CD TO CLIENT                         
         BNE   PRODET20            YES - REDUCE PAYABLE                         
         BAS   RE,VALCD            FIND/VALIDATE CASH DISCOUNT A/C              
*                                                                               
PRODET20 TM    POSTSW,POSTBUF      TEST TIME TO POST                            
         JNO   XIT                 SET GOOD RETURN                              
         GOTOR PPOST,DMCB,PROGD                                                 
         OC    LASTORD,LASTORD     TEST ORDER NUMBER INPUT                      
         BZ    *+8                                                              
         BAS   RE,UPDORD           UPDATE THE ORDER                             
         XR    R0,R0                                                            
         IC    R0,NPOST            COUNT NUMBER POSTED                          
         AHI   R0,1                                                             
         STC   R0,NPOST                                                         
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EXPENSE DETAIL ITEM                                                *          
**********************************************************************          
EXPDET   NTR1  ,                                                                
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         BAS   RE,EDEXP                                                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACNAME),ACNAME                                             
         ICM   R2,15,AXACNH                                                     
         BZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BNE   *+8                 NO                                           
         BAS   RE,GETSTF           GET THE STAFF LEDGER                         
*                                                                               
         L     R2,ADOFH                                                         
         GOTOR DUP                                                              
*                                                                               
         CLC   8(2,R2),=C'++'      SUPPLY THE OFFICE?                           
         BNE   EXPD12              NO                                           
         TM    BCCPYST5,CPYSNCST   YES, ON NEW COST?                            
         BO    EXPD02              YES                                          
*                                                                               
         MVI   COSTSW,C'N'                                                      
         TM    BCCPYST1,CPYSCOST   TEST COST ACCOUNTING                         
         BZ    EXPD04                                                           
         CLI   COSTANAL,C' '                                                    
         BE    EXPD04                                                           
         MVI   COSTSW,C'Y'                                                      
         B     EXPD04                                                           
*                                                                               
EXPD02   L     R1,ACATD                                                         
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC,POSTACC                                                  
         MVC   CATDPT,DEPT                                                      
         GOTO1 VCATCALL                                                         
         MVC   COSTSW,CATPST                                                    
         DROP  R1                                                               
*                                                                               
EXPD04   CLI   COSTSW,C'Y'         TEST FOR ANALYZED BY CLIENT                  
         JNE   EMNAWTT             'NOT ALLOWED WITH THIS TYPE'                 
         L     R2,ACLIH            IF ++ VALID, WE NEED THE CLIENT              
         CLI   5(R2),0                                                          
         JE    EMMISIF             'MISSING INPUT FIELD'                        
         BAS   RE,VALCLI                                                        
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,APROH            AND THE PRODUCT                              
         CLI   5(R2),0                                                          
         JE    EMMISIF             'MISSING INPUT FIELD'                        
         BAS   RE,VALPRO                                                        
         OI    4(R2),X'20'                                                      
*                                                                               
         GOTOR APRFMRGE                                                         
         LA    R4,PROFILE                                                       
         USING PPRELD,R4                                                        
         MVC   PRDOFF,PPRGAOFF     GET CLIENT/PRODUCT OFFICE CODE               
         L     R2,ADOFH            AND MOVE TO SCREEN                           
         MVC   FLD,PRDOFF                                                       
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         MVC   FLD,PRDOFF                                                       
         BRAS  RE,MOVEFLD                                                       
*                                                                               
EXPD12   BAS   RE,FOFF                                                          
         L     R2,ADOFH                                                         
         GOTOR DUP                                                              
         BAS   RE,AOFF                                                          
*                                                                               
         L     R2,ACOFH                                                         
         GOTOR DUP                                                              
         BAS   RE,COFF                                                          
*                                                                               
         L     R2,ADPTH                                                         
         GOTOR DUP                                                              
         BAS   RE,DEP              EDIT DEPARTMENT/VALIDATE ACCOUNTS            
*                                                                               
         TM    BCCPYST5,CPYSNCST   TEST NEW COST ACCOUNTING                     
         BO    EXPD14              YES                                          
         MVI   COSTSW,C'N'                                                      
         TM    BCCPYST1,CPYSCOST   TEST COST ACCOUNTING                         
         BZ    EXPD20                                                           
         CLI   COSTANAL,C' '                                                    
         BE    EXPD20                                                           
         MVI   COSTSW,C'Y'                                                      
         B     EXPD18                                                           
*                                                                               
EXPD14   L     R1,ACATD                                                         
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC,POSTACC                                                  
         MVC   CATOFF,ANAOFF                                                    
         MVC   CATDPT,DEPT                                                      
         GOTO1 VCATCALL                                                         
         MVC   COSTSW,CATPST                                                    
         L     R2,AXACH                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACTKCULA),CATACC3 EXTRACT KEY FOR ERROR                    
         CLI   CATERR,0                                                         
         JNE   EMIANAL             'INVALID ANALYSIS ACCOUNT'                   
*                                                                               
         CLI   CATPST,C'N'         TEST TO POST                                 
         BE    EXPD18                                                           
         MVC   COSTANAL,CATCDE     SAVE CATEGORY CODE                           
         MVC   CR13NUM,CATACC3     AND 13 ACCOUNT                               
         DROP  R1                                                               
*                                                                               
EXPD18   CLI   COSTSW,C'Y'         TEST FOR COST ACCOUNTING                     
         BNE   EXPD20                                                           
         BAS   RE,COST             VALIDATE COST ACCOUNTING ACCOUNTS            
         L     R2,ACLIH            REQUIRE CLIENT INPUT                         
         CLI   5(R2),0             TEST FOR INPUT                               
         JE    EMMISIF             'MISSING INPUT'                              
*                                                                               
         TM    BCCPYST5,CPYSEXPP   TEST IF PRODUCT ALSO REQUIRED                
         BZ    EXPD20                                                           
         L     R2,APROH            YES-MAKE SURE ITS THERE                      
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   EXPD20                                                           
         J     EMMISIF             'MISSING INPUT'                              
*                                                                               
EXPD20   L     R2,APERH                                                         
         GOTOR DUP                                                              
         CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BE    EXPD22                                                           
         CLI   5(R2),0             NO STAFF ANALYSIS--FORCE                     
         JNE   EMANFST             'ACCOUNT NOT FLAGGED FOR STAFF'              
*                                                                               
EXPD22   CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BE    EXPD24              YES                                          
         CLI   COSTSW,C'Y'         TEST COST ACCOUNTING                         
         BE    EXPD24              YES                                          
*                                                                               
         L     R2,ACLIH            THEN NO INPUT TO CLI/PRO                     
         CLI   5(R2),0                                                          
         JNE   EMINVIF             'INVALID INPUT FIELD'                        
         L     R2,APROH                                                         
         CLI   5(R2),0                                                          
         JE    EXPD26                                                           
         J     XIT                                                              
*                                                                               
EXPD24   L     R2,ACLIH                                                         
         GOTOR DUP                                                              
         BAS   RE,VALCLI                                                        
*                                                                               
         L     R2,APROH                                                         
         GOTOR DUP                                                              
         BAS   RE,VALPRO                                                        
*                                                                               
EXPD26   L     R2,AJOBH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF             'INVALID INPUT FIELD'                        
*                                                                               
         CLI   COSTSW,C'Y'         TEST FOR COST ACCOUNTING                     
         BE    EXPD28              YES-MUST GET 1C ACCOUNT                      
         CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BNE   EXPD30              NO-SKIP 1C ACCOUNT FETCH                     
         OC    CLICODE,CLICODE     TEST IF AT LEAST CLIENT INPUT                
         BZ    EXPD30              NO-SKIP 1C ACCOUNT FETCH                     
*                                                                               
EXPD28   GOTOR APRFMRGE                                                         
         XR    R6,R6               NO MORE PROFILES                             
         LA    R4,PROFILE          FIND 1/C ACCOUNT                             
         USING PPRELD,R4                                                        
         MVC   COSTNUM,PPRCOST                                                  
         BAS   RE,SET1C            HANDLE ANY OVERRIDES TO 1C A/C               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),COSTNUM                                                  
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
         MVC   COSTNUM,ACCODE                                                   
         MVC   COSTNAME,ACNAME                                                  
         DROP  R4                                                               
*                                                                               
EXPD30   CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BNE   *+8                                                              
         BAS   RE,STF              YES-HANDLE STAFF AND 29 A/C                  
*                                                                               
         L     R2,AAMTH                                                         
         GOTOR DUP                 EDIT FOR REPEAT                              
         GOTOR VALAMT                                                           
         ZAP   TRANSAMT,INAMNT                                                  
*                                                                               
         CP    XVNDDISC,PZERO      SAVED DISCOUNT                               
         BE    EXPD32                                                           
         ZAP   DUB,INAMNT          CALCULATE CD                                 
         MP    DUB,XVNDDISC                                                     
         SRP   DUB,64-4,5          ROUNDED DIVIDE BY 10,000                     
         ZAP   CDAMNT,DUB                                                       
         BAS   RE,VALECD           VALIDATE EXPENSE CD ACCOUNT                  
*                                                                               
EXPD32   TM    POSTSW,POSTBUF      TEST TIME TO POST                            
         JNO   XIT                 SET GOOD RETURN                              
         GOTOR EPOST,DMCB,PROGD                                                 
         OC    LASTORD,LASTORD     TEST ORDER NUMBER INPUT                      
         BZ    *+8                                                              
         BAS   RE,UPDORD           UPDATE THE ORDER                             
         XR    R0,R0                                                            
         IC    R0,NPOST            COUNT NUMBER PSOTED                          
         AHI   R0,1                                                             
         STC   R0,NPOST                                                         
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE CLIENT FIELD                                              *          
**********************************************************************          
VALCLI   NTR1  ,                                                                
         L     R2,ACLIH                                                         
         GOTOR GETCLI                                                           
         CLI   TYPE,C'E'           TEST EXPENSE TRANSACTION                     
         BE    VALCLI3             YES-SKIP LOCKED/SECURITY TEST                
         TM    ACBSTAT,ACBSLOCK    TEST IF LOCKED                               
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
*                                                                               
         CLC   TWAACCS(2),SPACES TEST ANY OLD SECURITY                          
         BNH   VALCLI3             NO                                           
         CLI   TWAACCS,C'*'        TEST FOR SINGLE OFFICE                       
         BE    VALCLI3             YES                                          
         CLI   TWAACCS,C'$'        TEST FOR OFFICE LIST                         
         BE    VALCLI3                                                          
         CLC   TWAACCS(2),FVIFLD   MATCH ON CLIENT CODE                         
         JE    EMSECLK             'SECURITY LOCKOUT'                           
*                                                                               
VALCLI3  MVC   FLD,SPACES                                                       
         MVC   FLD(L'CLINAME),CLINAME                                           
         ICM   R2,15,ACLINH        CLIENT NAME                                  
         BZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R1,FFTELEM          BUILD FFTEL                                  
         USING FFTELD,R1                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCLPRA                               
         MVI   FFTTYPE,FFTTCLPR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCLPRA                                               
         MVC   FFTCLAC,CLIPRO+3                                                 
         OC    FFTCLPRA,SPACES                                                  
         J     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE PRODUCT FIELD                                             *          
**********************************************************************          
VALPRO   NTR1  ,                                                                
         L     R2,APROH                                                         
         GOTOR GETPRO                                                           
         CLI   TYPE,C'E'           TEST EXPENSE ITEM                            
         BE    VALPRO3                                                          
         TM    ACBSTAT,ACBSLOCK    TEST LOCKED IF PRODUCTION                    
         JO    EMACTLK                                                          
*                                                                               
VALPRO3  MVC   FLD,SPACES                                                       
         MVC   FLD(L'PRONAME),PRONAME                                           
         ICM   R2,15,APRONH        PRODUCT NAME                                 
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R1,FFTELEM          UPDATE FFTEL                                 
         USING FFTELD,R1                                                        
         MVC   FFTPRAC,CLIPRO+6                                                 
         OC    FFTCLPRA,SPACES                                                  
         J     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE JOB FIELD                                                 *          
**********************************************************************          
VALJOB   NTR1  ,                                                                
         L     R2,AJOBH                                                         
         GOTOR GETJOB                                                           
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSCLSE                                                 
         JO    EMACTCL             'ACCOUNT IS CLOSED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'JOBNAME),JOBNAME                                           
         ICM   R2,15,AJOBNH        JOB NAME                                     
         BZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SET OPTION DEFAULTS AND VALIDATE XJOB FIELDS                       *          
**********************************************************************          
XJOB     NTR1  ,                                                                
         TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BO    XJOB1               YES                                          
*                                                                               
         L     R2,AXACH            FOR REGULAR JOB, DO NOT ALLOW INPUT          
         CLI   5(R2),0             IN XJOB FIELDS                               
         JNE   EMINVIF             'INVALID INPUT...'                           
*                                                                               
         L     R2,ADOFH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
*                                                                               
         L     R2,AAOFH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
*                                                                               
         L     R2,ADPTH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
*                                                                               
         L     R2,APERH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
         J     XIT                                                              
*                                                                               
XJOB1    L     R4,AGOXBLK                                                       
         USING GOXBLOCK,R4                                                      
         L     R2,ADOFH                                                         
         CLI   5(R2),0                                                          
         BE    XJOB2                                                            
         CLC   8(2,R2),=C'++'                                                   
         JE    EMNAWTT             'NOT ALLOWED WITH THIS TYPE'                 
*                                                                               
XJOB2    BAS   RE,FOFF              VALIDATE FINANCIAL OFFICE                   
         L     R2,AAOFH                                                         
         CLI   5(R2),0             TEST IF ANALYSIS OFFICE ENTERED              
         BNE   XJOB4               YES                                          
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOAOF),GOAWOAOF PICK UP ANALYSIS OFFICE OPTION          
         OC    GOAWOAOF,GOAWOAOF   TEST FOR ONE                                 
         BNZ   XJOB3               YES                                          
         MVC   FLD(L'FINOFF),FINOFF ELSE USE FINANCIAL OFFICE                   
         OC    FINOFF,FINOFF                                                    
         BNZ   XJOB3                                                            
         MVC   FLD(L'CLIOFFC),CLIOFFC USE CLIENT OFFICE LASTLY                  
*                                                                               
XJOB3    BRAS  RE,MOVEFLD                                                       
         MVC   5(1,R2),BCOFFLEN                                                 
*                                                                               
XJOB4    L     R2,ADPTH                                                         
         CLI   5(R2),0             TEST IF DEPARTMENT INPUT                     
         BNE   XJOB6               YES                                          
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWODEP),GOAWODEP                                         
         MVC   5(1,R2),BCDPTLEN                                                 
         BRAS  RE,MOVEFLD                                                       
*                                                                               
XJOB6    L     R2,APERH                                                         
         CLI   5(R2),0                                                          
         BNE   XJOB8                                                            
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOSTF),GOAWOSTF                                         
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'GOAWOSTF                                                 
*                                                                               
XJOB8    L     R2,AXACH                                                         
         CLI   5(R2),0                                                          
         BNE   XJOB10                                                           
*                                                                               
         OC    GOAWOA,GOAWOA       TEST FOR WRITE-OFF ACCOUNT                   
         BZ    XJOB10              NONE                                         
         MVC   FLD,SPACES                                                       
         CLC   GOAWOA(2),=C'SE'    TEST FOR EXPENSE LEDGER                      
         BE    *+18                YES                                          
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(L'GOAWOA),GOAWOA                                           
         B     *+10                                                             
         MVC   FLD(L'GOAWOA-2),GOAWOA+2 ONLY NEED A/C CODE FOR SE               
         BRAS  RE,MOVEFLD                                                       
*                                                                               
XJOB10   L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         LR    RE,R3                                                            
         LA    RF,EXCELNQ                                                       
         XR    R1,R1               CLEAR EXCEL BLOCK                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   EXCACT,EXCAVAL      VALIDATE CALL                                
         MVC   EXCAEXP,AXACH       PASS A(EXPENSE A/C HEADER)                   
         MVC   EXCAANO,AAOFH                                                    
         MVC   EXCADEP,ADPTH                                                    
         MVC   EXCASTF,APERH                                                    
         ST    R9,EXCAGWS                                                       
         GOTO1 VEXCEL,EXCELD                                                    
         JE    XYES                                                             
         J     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE FINANCIAL OFFICE FIELD                                    *          
**********************************************************************          
FOFF     NTR1  ,                                                                
         L     R2,ADOFH                                                         
         MVC   FVMAXL,BCOFFLEN                                                  
*                                                                               
FOFF1    GOTO1 AFVAL,(R2)                                                       
         BE    FOFF4               SOMETHING IN FIELD                           
         JH    EXIT                INPUT DATA TOO LONG                          
*                                                                               
         CLI   TYPE,C'E'           NO INPUT-TEST FOR EXPENSE                    
         BNE   FOFF3               NO                                           
         TM    BCCPYST1,CPYSOROE   TEST OFFICE REQUIRED                         
         JZ    XIT                 NO                                           
         J     EMMISIF             'MISSING INPUT FIELD'                        
*                                                                               
FOFF3    TM    ACOPSTAT,ACOXJOB    ITS PRODUCTION-TEST FOR XJOB                 
         JZ    XIT                 NO-EXIT                                      
         L     R4,AGOXBLK                                                       
         USING GOXBLOCK,R4                                                      
         OC    GOAWOFOF,GOAWOFOF   TEST FOR OPTION VALUE                        
         JZ    XIT                 NO-EXIT                                      
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOFOF),GOAWOFOF SET OPTION OFFICE IN FIELD              
         BRAS  RE,MOVEFLD                                                       
         MVC   5(1,R2),BCOFFLEN                                                 
         B     FOFF1               GO BACK AND RE-VALIDATE FIELD                
*                                                                               
FOFF4    CLI   TYPE,C'E'           TEST FOR EXPENSE                             
         BE    FOFF6               YES                                          
         TM    ACOPSTAT,ACOXJOB    ITS PRODUCTION-TEST FOR XJOB                 
         BO    FOFF6               YES-ITS OK TO HAVE VALUE IN FIELD            
         MVC   FLD,SPACES          CLEAR THE FIELD OUT                          
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         J     XIT                                                              
*                                                                               
FOFF6    GOTOR GETOFF                                                           
         MVC   FINOFF,ACOFFC       SET FINANCIAL OFFICE                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'OFFNAME),OFFNAME                                           
         ICM   R2,15,ADOFNH                                                     
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE ANALYSIS OFFICE FIELD                                     *          
**********************************************************************          
AOFF     NTR1  ,                                                                
         L     R2,AAOFH                                                         
         MVC   FVMAXL,BCOFFLEN                                                  
*                                                                               
         GOTO1 AFVAL,(R2)                                                       
         BE    AOFF4               SOMETHING IN FIELD                           
         JH    EXIT                INPUT DATA TOO LONG                          
         MVC   ANAOFF,FINOFF       COPY ANAOFF FROM FINANCIAL OFFICE            
         J     XIT                                                              
*                                                                               
AOFF4    GOTOR GETOFF                                                           
         MVC   ANAOFF,ACOFFC       SET ANALYSIS OFFICE                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'OFFNAME),OFFNAME                                           
         ICM   R2,15,AAOFNH                                                     
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE CREDIT OFFICE FIELD                                       *          
**********************************************************************          
COFF     NTR1  ,                                                                
         L     R2,ACOFH                                                         
         MVC   FVMAXL,BCOFFLEN                                                  
         GOTO1 AFVAL,(R2)                                                       
         JH    EXIT                INPUT DATA TOO LONG                          
         JL    XIT                 EXIT FOR NO INPUT                            
*                                                                               
         GOTOR GETOFF                                                           
         MVC   CRDOFF,ACOFFC       SET CREDIT OFFICE                            
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'OFFNAME),OFFNAME                                           
         ICM   R2,15,ACOFNH                                                     
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE EXPENSE ACCOUNT CODE                                      *          
**********************************************************************          
EDEXP    NTR1  ,                                                                
         L     R2,AXACH                                                         
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         CLI   FVIFLD,C'*'         REPEAT FEATURE                               
         BNE   EDEXP10                                                          
         XR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         CH    RE,=H'1'                                                         
         BNH   EDEXP8              MORE THAN 2 INDICATES DEFAULT ACT            
*                                                                               
EDEXP2   LA    RF,DLIST            A(LIST OF VALID U/L'S TO DEBIT)              
EDEXP4   CLI   0(RF),X'FF'         END OF LIST                                  
         JE    EMINACP                                                          
         CLC   FVIFLD+1(2),0(RF)   INPUT TO LIST                                
         BE    EDEXP6                                                           
         LA    RF,2(RF)                                                         
         B     EDEXP4                                                           
*                                                                               
EDEXP6   LR    RF,RE                                                            
         MVC   ACTKUNT(2),FVIFLD+1 DEFAULT U/L TO KEY                           
         SHI   RF,3                DEDUCT FOR *UL                               
         LTR   RF,RF                                                            
         JM    EMINACP                                                          
         LA    R5,FVIFLD+3         A(REST OF DFLT ACCT)                         
         B     EDEXP15                                                          
*                                                                               
EDEXP8   GOTOR DUP                 EDIT FOR REPEAT                              
         GOTO1 AFVAL,(R2)                                                       
         XR    RE,RE                                                            
         IC    RE,FVILEN           LEN FOR DFLT ACT                             
         CLI   8(R2),C'*'          CHK IF ITS DFLT ACT INPUT                    
         BE    EDEXP2              NO                                           
*                                                                               
EDEXP10  MVC   ACTKUNT(2),=C'SE'   U/L DEBIT                                    
         XR    RF,RF                                                            
         IC    RF,FVILEN           INPUT LEN                                    
         LA    R5,FVIFLD                                                        
*                                                                               
EDEXP15  BCTR  RF,0                SUB 1 FOR EXECUTE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),0(R5)    EXP ACC TO KEY                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   POSTACC,ACCODE                                                   
         MVC   POSTACCN,ACNAME                                                  
*                                                                               
         MVI   STFSW,C'N'          INITIALIZE SWITCHES                          
         MVI   DEPSW,C'N'                                                       
         MVI   OFFSW,C'N'                                                       
*                                                                               
         TM    BCCPYST1,CPYSOROE   MANDATORY OFFICE                             
         BZ    *+8                                                              
         MVI   OFFSW,C'Y'                                                       
*                                                                               
         TM    ACBSTAT,ACBSPERS    PERSONAL EXPENSE (STAFF=Y)                   
         BZ    *+8                                                              
         MVI   STFSW,C'Y'                                                       
*                                                                               
         TM    ACBSTAT,ACBSDEPT    DEPT EXPENSE (DEPT=Y)                        
         BZ    *+8                                                              
         MVI   DEPSW,C'Y'                                                       
*                                                                               
         MVC   COSTANAL,SPACES                                                  
         OC    COSTANAL(1),ACCTCOST  (ANALYSIS=)                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCOUNT'),('RSTELQ',AIO1),0                 
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING RSTELD,R6                                                        
         MVC   POSTCNTR,RSTCCTR    COST CENTER                                  
         MVC   POSTCPOS,RSTCCTRR   COST POSITION                                
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         JL    XIT                                                              
         L     R2,AXACH                                                         
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         XR    R6,R6                                                            
         IC    R6,5(R2)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         JM    EMINVIF             'INVALID INPUT FIELD'                        
         J     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* GET STAFF LEDGER VALUES                                            *          
**********************************************************************          
GETSTF   NTR1  ,                                                                
         LA    R4,IOKEY                                                         
         USING LDGRECD,R4                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(2),=C'2P'   GET 2P LEDGER VALUES                         
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,ACALDG                                                     
         USING LDGTABD,R1                                                       
         MVI   LEVEL,3             SET 2P LEDGER LEVEL                          
         XR    R5,R5                                                            
         IC    R5,LDGTLVB          GET DISPLACEMENT TO STAFF CODE               
         CLI   LDGTLVC,L'ACTKACT   TEST FOR 3 LEVEL LEDGER                      
         BE    GETSTF4             YES                                          
*                                                                               
         IC    R5,LDGTLVA                                                       
         MVI   LEVEL,2                                                          
         CLI   LDGTLVB,L'ACTKACT   TEST FOR 2 LEVEL LEDGER                      
         BE    GETSTF4                                                          
*                                                                               
         XR    R5,R5                                                            
         MVI   LEVEL,1                                                          
         CLI   LDGTLVA,L'ACTKACT                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETSTF4  LA    RE,L'ACTKACT                                                     
         SR    RE,R5                                                            
         STC   RE,STAFFL           SAVE L'STAFF CODE                            
         J     XIT                                                              
         DROP  R1,R4                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE DEPARTMENT ANALYSIS ACCOUNTS                              *          
**********************************************************************          
DEP      NTR1  ,                                                                
         L     R2,ADPTH                                                         
         MVC   FVMAXL,BCDPTLEN     SET MAXIMUM LENGTH                           
         MVI   FVMINL,1            SET SOME INPUT REQUIRED                      
         CLI   DEPSW,C'Y'          TEST DEPARTMENT ANALYSIS                     
         BE    DEP2                YES                                          
         CLI   STFSW,C'Y'          TEST STAFF ANALYSIS                          
         BNE   *+12                NO                                           
         CLI   LEVEL,1             TEST 1 LEVEL 2P LEDGER                       
         BH    DEP2                NO                                           
         MVI   FVMINL,0            DEPARTMENT IS NOT REQUIRED                   
*                                                                               
DEP2     MVC   BYTE,FVMINL         SAVE REQUIRED VALUE                          
         GOTO1 AFVAL,(R2)                                                       
         JH    EXIT                                                             
         BE    DEP3                SOMETHING WAS INPUT                          
         CLI   BYTE,1              TEST IF REQUIRED                             
         JE    EXIT                YES-EXIT WITH ERROR                          
         J     XIT                 NO-EXIT ITS OK                               
*                                                                               
DEP3     CLI   BYTE,1              TEST FIELD IS REQUIRED                       
         JNE   EMANFDP             'ACCOUNT NOT FLAGGED FOR DEPART...'          
         MVC   DEPT,FVIFLD                                                      
         CLI   DEPSW,C'N'          TEST FOR DEPARTMENT ANALYSIS                 
         JE    XIT                 NO-EXIT WITHOUT VALIDATING ACCOUNTS          
*                                                                               
         LA    R4,KEY              VALIDATE 2D ACCOUNT                          
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'2D'                                                
         LA    R1,ACTKACT                                                       
*                                                                               
         CLI   OFFSW,C'Y'          OFFICE IN ACCT                               
         BNE   DEP6                                                             
         XR    RF,RF                                                            
         IC    RF,BCOFFLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ANAOFF      OFFICE                                       
         LA    R1,1(RF,R1)                                                      
*                                                                               
DEP6     XR    RF,RF                                                            
         IC    RF,BCDPTLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT        DEPARTMENT                                   
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   DEPNAME,ACNAME                                                   
         MVC   DEPNUM,ACCODE                                                    
         MVC   DEPSTFN,ACNAME                                                   
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'DEPNAME),DEPNAME                                           
         ICM   R2,15,ADPTNH                                                     
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   ACTKEY,SPACES          VALIDATE 28 ACCOUNT                       
         MVC   ACTKCULA,POSTACC       SET EXPENSE ACCOUNT                       
         MVC   ACTKUNT(2),=C'28'                                                
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   CRDSNUM,ACCODE                                                   
         MVC   CRDSNAME,ACNAME                                                  
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE COST ACCOUNTING ACCOUNTS                                  *          
**********************************************************************          
COST     NTR1  ,                   VALIDATE 1P ACCOUNT                          
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1P'   SET UP 1/P ACCOUNT                           
         TM    BCCPYST5,CPYSNCST   TEST NEW COST ACCOUNTING                     
         BZ    *+14                NO                                           
         MVC   ACTKACT,=C'999999999999'                                         
         B     COST4                                                            
*                                                                               
         LA    RF,ACTKACT                                                       
         CLI   OFFSW,C'N'                                                       
         BE    COST2                                                            
         XR    R1,R1                                                            
         IC    R1,BCOFFLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ANAOFF      OFFICE                                       
         LA    RF,1(R1,RF)                                                      
*                                                                               
COST2    LA    R3,=C'9999'         DEFAULT DEPT                                 
         CLI   DEPSW,C'Y'                                                       
         BNE   *+8                                                              
         LA    R3,DEPT             OR REAL DEPT                                 
*                                                                               
         XR    R1,R1                                                            
         IC    R1,BCDPTLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R3)                                                    
         LA    RF,1(R1,RF)                                                      
         MVC   0(1,RF),COSTANAL                                                 
*                                                                               
COST4    GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   CRCNUM,ACCODE                                                    
         MVC   CRCNAME,ACNAME                                                   
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE STAFF ANALYSIS ACCOUNTS                                   *          
**********************************************************************          
STF      NTR1  ,                   EXTRACT STAFF CODE                           
         L     R2,APERH                                                         
         MVC   FVMAXL,STAFFL       SET MAXIMUM LENGTH                           
         MVI   FVMINL,1            REQUIRE SOMETHING                            
         GOTO1 AFVAL,(R2)                                                       
         JNE   EXIT                                                             
         MVC   STAFF,FVIFLD                                                     
*                                                                               
         LA    R4,KEY              VALIDATE 2P ACCOUNT KEY                      
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),=C'2P'                                                
         LA    R1,ACTKACT                                                       
*                                                                               
         CLI   LEVEL,3             ONLY MOVE OFFICE FOR 3 LEVEL ACCTS           
         BL    STF2                                                             
         XR    RF,RF                                                            
         IC    RF,BCOFFLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ANAOFF      MOVE ANALYSIS OFFICE INTO KEY                
         LA    R1,1(RF,R1)         BUMP TO NEXT SPOT IN KEY                     
*                                                                               
STF2     CLI   LEVEL,2                                                          
         BL    STF4                                                             
         XR    RF,RF                                                            
         IC    RF,BCDPTLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT                                                     
         LA    R1,1(RF,R1)                                                      
*                                                                               
STF4     XR    RF,RF                                                            
         IC    RF,STAFFL                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),STAFF       MOVE STAFF INTO KEY                          
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   STAFFNUM,ACCODE                                                  
         MVC   STAFFNAM,ACNAME                                                  
         MVC   DEPSTFN,ACNAME                                                   
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'STAFFNAM),STAFFNAM                                         
         ICM   R2,15,APERNH                                                     
         JZ    *+12                                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   ACTKEY,SPACES       VALIDATE 29 ACCOUNT KEY                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),=C'29'   SET UP 2/9 ACCOUNT                           
         MVC   ACTKACT,=12C'9'     DEFAULT IS 999999 ETC.                       
*                                                                               
         OC    COSTNUM,COSTNUM                                                  
         BZ    *+10                                                             
         MVC   ACTKACT,COSTNUM+3   OR USE COSTING                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   CRPSNUM,ACCODE                                                   
         MVC   CRPSNAME,ACNAME                                                  
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE PRODUCTION CASH DISCOUNT INCOME ACCOUNT                   *          
**********************************************************************          
VALCD    NTR1  ,                                                                
         MVC   KEY,SPACES                                                       
         LA    R4,IOKEY                                                         
         USING PMDRECD,R4                                                       
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ    READ MEDIA RECORD                            
         MVC   PMDKCPY,COMPANY                                                  
         MVC   PMDKMED,JOBACCT+9   1ST BYTE OF JOB IS MEDIA CODE                
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'PMDKEY),IOKEYSAV                                         
         JNE   EMMMR               'MISSING MEDIA RECORD'                       
*                                                                               
         L     R4,AIO1                                                          
         LA    R4,PMDRFST                                                       
         XR    R0,R0                                                            
*                                                                               
         USING PMDELD,R4                                                        
VALCD3   CLI   0(R4),PMDELQ                                                     
         BE    VALCD5                                                           
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALCD3                                                           
*                                                                               
VALCD5   MVC   KEY(L'PMDCSHD),PMDCSHD                                           
         CLC   KEY,SPACES         TEST CASH DISCOUNT BY MEDIA                   
         BH    VALCD10            YES                                           
*                                                                               
         LA    R4,IOKEY                                                         
         USING LDGRECD,R4                                                       
         MVC   LDGKEY,SPACES       READ LEDGER RECORD                           
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT,=C'SJ'      PRODUCTION LEDGER                            
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'LDGKEY),IOKEYSAV                                         
         JNE   EMMSJL              'MISSING SJ LEDGER'                          
*                                                                               
         L     R4,AIO1                                                          
         LA    R4,LDGRFST                                                       
         XR    R0,R0                                                            
*                                                                               
         USING LDGELD,R4                                                        
VALCD6   CLI   0(R4),LDGELQ                                                     
         BE    VALCD7                                                           
         CLI   0(R4),0                                                          
         BNE   *+6                MISSING LEDGER ELEMENT                        
         DC    H'0'                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALCD6                                                           
*                                                                               
VALCD7   LA    R3,KEY                                                           
         USING ACTRECD,R3                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA,LDGCDSC                                                  
         CLC   ACTKULA,SPACES                                                   
         BH    *+10                                                             
         MVC   ACTKULA(4),=C'SIMD' USE DEFAULT                                  
*                                                                               
VALCD10  L     R2,AAMTH            A(CURRENT AMOUNT HEADER)                     
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   SIMDAC,ACCODE                                                    
         MVC   SIMDACN,ACNAME                                                   
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE EXPENSE CASH DISCOUNT INCOME ACCOUNT                      *          
**********************************************************************          
VALECD   NTR1  ,                                                                
         L     R2,AAMTH            DEFAULT COMMISSION ACCT                      
         LA    R4,IOKEY                                                         
         USING LDGRECD,R4                                                       
         MVC   LDGKEY,SPACES         READ LEDGER RECORD                         
         MVC   LDGKCPY,COMPANY                                                  
         MVI   LDGKUNT,C'S'                                                     
         LA    RF,COMPEL                                                        
         USING CPYELD,RF                                                        
         MVC   LDGKLDG,CPYSUPX     EXPENSE VENDOR FROM CO REC                   
         DROP  RF                                                               
*                                                                               
         L     R2,AXVNH                                                         
         CLI   8(R2),C'*'                                                       
         BNE   *+10                                                             
         MVC   LDGKUNT(2),9(R2)    OVERRIDE UNIT/LEDGER                         
*                                                                               
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'LDGKEY),IOKEYSAV                                         
         JNE   EMMEL               'MISSING EXPENSE LEDGER RECORD'              
         L     R4,AIO1                                                          
         LA    R4,LDGRFST                                                       
         XR    R0,R0                                                            
*                                                                               
         USING LDGELD,R4                                                        
VALECD3  CLI   0(R4),LDGELQ                                                     
         BE    VALECD5                                                          
         CLI   0(R4),0                                                          
         BNE   *+6                MISSING LEDGER ELEMENT                        
         DC    H'0'                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALECD3                                                          
*                                                                               
VALECD5  LA    R3,KEY                                                           
         USING ACTRECD,R3                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA,LDGCDSC                                                  
         CLC   ACTKULA,SPACES                                                   
         BH    *+10                                                             
         MVC   ACTKULA(4),=C'SIMD' USE DEFAULT                                  
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   SIMDAC,ACCODE                                                    
         MVC   SIMDACN,ACNAME                                                   
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* 1C ACCOUNT OVERRIDE                                                *          
**********************************************************************          
SET1C    NTR1  ,                                                                
         OC    POSTCNTR,SPACES                                                  
         LA    R3,COSTNUM+7        R3=A(REPLACEMENT POSITION POINTER)           
         CLI   POSTCPOS,0          TEST COST POSITION OVERRIDE                  
         BE    SET1C2              NO                                           
*                                                                               
         XR    R3,R3                                                            
         IC    R3,POSTCPOS         YES                                          
         LA    R3,COSTNUM+2(R3)                                                 
*                                                                               
SET1C2   LA    R1,POSTCNTR         R1=A(OVERRIDE COST CENTER)                   
         LA    RE,L'POSTCNTR                                                    
*                                                                               
SET1C4   CLI   0(R1),C' '          TEST FOR OVERRIDE CHARACTER                  
         BE    *+10                NO                                           
         MVC   0(1,R3),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,SET1C4                                                        
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* UPDATE THE ORDER RECORD                                            *          
**********************************************************************          
UPDORD   NTR1  ,                                                                
         LA    R4,IOKEY           BUILD ORDER KEY                               
         USING ORDRECD,R4                                                       
         XC    ORDKEY,ORDKEY      READ FOR UPDATE                               
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,LASTORD                                                  
         GOTO1 AIO,IOHIUP+IOACCMST+IO1                                          
         CLC   IOKEY(L'ORDKEY),IOKEYSAV                                         
         BE    *+6                                                              
         DC    H'0'                NO ORDER RECORD                              
         LA    R3,ORDRFST                                                       
         USING OAMEL,R3                                                         
*                                                                               
UPDORD3  CLI   0(R3),0                                                          
         JE    XIT                                                              
         CLI   0(R3),OAMELQ                                                     
         BNE   UPDORD5                                                          
         CLC   WRKCODE,SPACES      TEST PRODUCTION                              
         BNH   UPDORD7             NO, OK TO UPDATE                             
         CLC   OAMWORK,WRKCODE     MATCH WORKCODE                               
         BE    UPDORD7             YES, OK TO UPDATE                            
*                                                                               
UPDORD5  XR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     UPDORD3                                                          
*                                                                               
UPDORD7  AP    OAMTVAL,INAMNT      UPDATE AMOUNT                                
         MVC   OAMLAST,TODAYP      SET DATE                                     
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         GOTO1 AIO,IOUNLOCK+IOACCMST+IO1                                        
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
* U/L'S VALID TO DEBIT                                                          
DLIST    DS    0H                                                               
         DC    C'SASBSFSLSC'                                                    
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R8                                                            
         EJECT                                                                  
**********************************************************************          
* EDIT TAX SCREEN DETAIL                                             *          
**********************************************************************          
USET     NMOD1 0,**USET**,R8                                                    
         L     RC,0(R1)                                                         
*                                                                               
         L     R2,ACLIH                                                         
         GOTOR DUP                                                              
         MVI   FVMINL,1                                                         
         GOTOR GETCLI              GET THE CLIENT                               
*                                                                               
         L     R2,APROH                                                         
         GOTOR DUP                                                              
         MVI   FVMINL,1                                                         
         GOTOR GETPRO              PRODUCT                                      
*                                                                               
         L     R2,AJOBH                                                         
         GOTOR DUP                                                              
         MVI   FVMINL,1                                                         
         GOTOR GETJOB              JOB                                          
*                                                                               
         GOTOR AGETJOB,DMCB,JOBACCT  CALL GETOPT                                
         TM    ACOPSTAT,ACOXJOB      TEST X-JOB                                 
         JO    EMNOTXE               'TAX NOT VALID IN EXPENSE JOB..            
         L     R4,AGOBLOCK                                                      
         USING GOBLOCKD,R4                                                      
         MVC   TAXOFF,GOEFFOFC                                                  
         DROP  R4                                                               
*                                                                               
         L     R2,AWKCH                                                         
         GOTOR DUP                                                              
         GOTOR VALWC               GET THE WORKCODE                             
*                                                                               
         L     R2,ABASH                                                         
         GOTOR DUP                                                              
         GOTOR VALAMT              VALIDATE BASIS                               
*                                                                               
         L     R2,ALOCH                                                         
         BRAS  RE,VALLOC           VALIDATE LOCALITY                            
*                                                                               
         L     R2,ATAXH                                                         
         MVC   FLD,SPACES          DISPLAY TAX AMOUNT                           
         CURED TAXAMNT,(12,FLD),2,DMCB=BCPARM,MINUS=YES,ALIGN=LEFT              
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         TM    POSTSW,POSTBUF      TEST TIME TO POST                            
         JNO   XIT                                                              
         GOTOR TPOST,DMCB,PROGD    MAKE TAX POSTINGS                            
         XR    R0,R0                                                            
         IC    R0,NPOST            COUNT NUMBER POSTED                          
         AHI   R0,1                                                             
         STC   R0,NPOST                                                         
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R8                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE AND PROCESS PF KEYS                                       *          
**********************************************************************          
PF       NTR1  BASE=*,LABEL=*                                                   
         TM    PFKSW,PFKRDSP       TEST FORCE RE-DISPLAY                        
         BO    PFNXT                                                            
*                                                                               
         L     R3,APFKEYS                                                       
         USING PFKTD,R3                                                         
PF03     CLC   PFKEY,PFKTNUM       MATCH NUMBER TO TABLE                        
         BNE   PF05                                                             
         XR    R1,R1                                                            
         IC    R1,PFKTVBI          SET VALIDATION BIT                           
         LA    RF,VPFSW            SET VALIDATION BYTE                          
         CLI   PFKTVBY,PFKTVBY0                                                 
         BE    *+8                                                              
         LA    RF,VPFSW1                                                        
         EX    R1,*+8              TEST PFKEY VALID                             
         B     *+8                                                              
         TM    0(RF),0                                                          
         BNO   PF05                                                             
*                                                                               
         XR    RF,RF               GO TO ROUTINE                                
         ICM   RF,3,PFKTROU                                                     
         AR    RF,RB                                                            
         BR    RF                                                               
*                                                                               
PF05     LA    R3,PFKTLNQ(R3)      TRY NEXT ENTRY                               
         CLI   0(R3),X'FF'                                                      
         BNE   PF03                                                             
*                                                                               
PFERR    L     R2,ADETH                                                         
         J     EMMIVPFK            'INVALID PFKEY...'                           
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'SWAP'                                              *          
**********************************************************************          
PFSWAP   CLI   SCRSWAP,0           TEST SWAP SCREEN                             
         JE    XIT                 NONE, IGNORE IT                              
         OC    NBUF,NBUF                                                        
         BZ    PFSWAP3                                                          
         BRAS  RE,SAVDET           SAVE CURRENT DETAIL                          
*                                                                               
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         BO    PFSWAP3                                                          
         XR    R0,R0                                                            
         ICM   R1,15,BCACUR        R1=A(CURSOR)                                 
         ICM   RF,15,ADETH                                                      
         SR    R1,RF               LESS A(FIRST DETAIL)                         
         BM    PFSWAP3             CURSOR BEFORE DETAIL LINE                    
         XR    R3,R3                                                            
         IC    R3,SCRLDL           LENGTH OF DETAIL LINE                        
         DR    R0,R3                                                            
         ICM   R3,3,NFST           R3=# OF FIRST                                
         AR    R3,R1               PLUS LINE NUMBER OF SELECTED LINE            
         CLM   R3,3,NBUF           TEST VS. MAX IN BUFFER                       
         BH    PFSWAP3                                                          
         STCM  R3,3,NFST           SET NUMBER TO DISPLAY                        
*                                                                               
PFSWAP3  GOTOR LOADSCRN,SCRSWAP    LOAD 'SWAP' SCREEN                           
         L     R2,AORDH                                                         
         OC    NBUF,NBUF           TEST FIRST TIME                              
         JZ    IMEREQF             'ENTER REQUIRED FIELDS..'                    
*                                                                               
         TM    SCRSTA,SCRSINQ      TEST SINGLE ITEM SCREEN                      
         BNO   *+8                                                              
         OI    PROCSW,PROCDTL      SET TO PROCESS DETAIL (FOR NAMES)            
         GOTOR RESTOP              RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
         MVC   NGET,NFST           GET FIRST ON SCREEN                          
         B     PFNXT                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'UP'                                                *          
**********************************************************************          
PFUP     OC    NBUF,NBUF           TEST ANYTHING IN BUFFER                      
         BZ    PFERR               NO, ERROR                                    
         BRAS  RE,SAVDET           SAVE ITEMS OF SCREEN                         
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,NFST           NUMBER OF FIRST ITEM                         
         BNZ   *+8                                                              
         ICM   R1,3,NBUF           GET THE MAX                                  
         XR    RF,RF                                                            
         IC    RF,SCRMXD                                                        
         SR    R1,RF               LESS MAX                                     
         BP    *+8                 TEST TOO LOW                                 
         LA    R1,1                USE FIRST                                    
         STCM  R1,3,NGET                                                        
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'DOWN'                                              *          
**********************************************************************          
PFDWN    XR    R3,R3               ** SAVE SCREEN ITEMS **                      
         ICM   R3,1,NITM           NUMBER OF ITEMS ON SCREEN                    
         BZ    PFERR                                                            
         BRAS  RE,SAVDET           SAVE ITEMS OF SCREEN                         
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,NLST           LAST                                         
         BZ    *+8                                                              
         LA    R3,1(R3)            PLUS 1                                       
         STCM  R3,3,NGET                                                        
         CLC   NGET,NBUF           TEST AGAINST TOTAL                           
         BNH   PFNXT                                                            
         XC    NGET,NGET           CLEAR LIST                                   
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'DELETE'                                            *          
**********************************************************************          
PFDEL    BRAS  RE,SAVDET           SAVE CURRENT DETAIL                          
         OC    NBUF,NBUF                                                        
         BZ    PFERR                                                            
*                                                                               
         MVC   NGET,NFST           SET FOR SINGLE                               
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         BO    PFDEL3                                                           
         XR    R0,R0                                                            
         ICM   R1,15,BCACUR        R1=A(CURSOR)                                 
         ICM   RF,15,ADETH                                                      
         SR    R1,RF               LESS A(FIRST DETAIL)                         
         BM    PFERR               CURSOR BEFORE DETAIL LINE                    
         XR    R3,R3                                                            
         IC    R3,SCRLDL           LENGTH OF DETAIL LINE                        
         DR    R0,R3                                                            
         ICM   R3,3,NFST           R3=# OF FIRST                                
         AR    R3,R1               PLUS LINE NUMBER OF SELECTED LINE            
         CLM   R3,3,NBUF           TEST VS. MAX IN BUFFER                       
         BH    PFERR                                                            
         STCM  R3,3,NGET           SET NUMBER TO DELETE                         
*                                                                               
PFDEL3   GOTOR DELLIN              DELETE LINE                                  
         OI    PFKSW,PFKDELT       SET DELETE FLAG                              
         XC    NGET,NGET           CLEAR RECORD POINTERS                        
         XC    NLST,NLST                                                        
         XC    NFST,NFST                                                        
         LA    R3,1                                                             
         STCM  R3,3,NGET           SET TO RETRIEVE FIRST                        
         OC    NBUF,NBUF           TEST BUFFER EMPTY                            
         BNZ   PFNXT               NO, DISPLAY FIRST                            
         XC    NGET,NGET           BUFFER EMPTY, SET FOR 'ENTER REQ..'          
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* SWITCH TO TAX                                                      *          
**********************************************************************          
PFTAX    BRAS  RE,SAVDET           SAVE CURRENT DETAIL                          
         TM    SCRSTA,SCRTAXQ      TEST ALREADY HAVE TAX SCREEN                 
         JO    EMMIVPFK            'INVALID PFKEY...                            
*                                                                               
         CLC   BCACUR,ADETH        TEST CURSOR BEFORE DETAIL                    
         BL    PFTAX3              YES, DISPLAY CLEAN TAX SCREEN                
         CLC   BCACUR,ATOTH        TEST CURSOR BEFORE TOTAL LINE                
         BNL   PFTAX3              NO, DISPLAY CLEAN TAX SCREEN                 
*                                                                               
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         BO    PFTAX5                                                           
         XR    R0,R0                                                            
         ICM   R1,15,BCACUR        R1=A(CURSOR)                                 
         ICM   RF,15,ADETH                                                      
         SR    R1,RF               LESS A(FIRST DETAIL)                         
         BM    PFTAX3                                                           
         XR    R3,R3                                                            
         IC    R3,SCRLDL           LENGTH OF DETAIL LINE                        
         DR    R0,R3                                                            
         ICM   R3,3,NFST           R3=# OF FIRST                                
         AR    R3,R1               PLUS LINE NUMBER OF SELECTED LINE            
         CLM   R3,3,NBUF           TEST VS. MAX IN BUFFER                       
         BH    PFTAX3                                                           
         STCM  R3,3,NFST           SET NUMBER TO DISPLAY                        
         B     PFTAX5                                                           
*                                                                               
PFTAX3   XC    NFST,NFST           CLEAR FIRST TO DISPLAY CLEAN SCREEN          
*                                                                               
PFTAX5   MVC   INVSCRN,SCRNUM      SAVE CURRENT SCREEN                          
         GOTOR LOADSCRN,SCRTAXS    LOAD TAX SCREEN                              
         GOTOR RESTOP              RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
         MVI   NITM,0                                                           
         L     R2,ADETH                                                         
         OC    NFST,NFST                                                        
         BNZ   PFTAX7              DISPLAY REQUESTED 'TRANSFER' INFO            
         CLI   NTAXL,0             ANY PREVIOUS TAX                             
         JE    IMEREQF             NO, 'ENTER REQUIRED FIELDS...'               
         LA    R1,1                                                             
         STCM  R1,3,NGET           DISPLAY PREVIOUS TAX                         
         J     PFNXT                                                            
*                                                                               
PFTAX7   MVC   NGET,NFST                                                        
         GOTOR GETLIN              GET RECORD                                   
         JNE   IMEREQF                                                          
         BRAS  RE,SETLOW           SET ADCONS                                   
         BRAS  RE,DSPREC           DISPLAY RECORD                               
         XC    NFST,NFST                                                        
         J     IMEREQF             'ENTER REQUIRED FIELDS...'                   
         EJECT                                                                  
**********************************************************************          
* SWITCH TO INVOICE                                                  *          
**********************************************************************          
PFINV    BRAS  RE,SAVDET           SAVE CURRENT DETAIL                          
         GOTOR LOADSCRN,INVSCRN    RESTORE INVOICE SCREEN                       
         BRAS  RE,SETLOW                                                        
         GOTOR RESTOP              RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
         LA    R1,1                START AT FIRST                               
         OC    NBUF,NBUF                                                        
         BNZ   *+6                                                              
         XR    R1,R1               UNLESS NOTHING IN BUFFER                     
         STCM  R1,3,NGET                                                        
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* CLEAR SCREEN AND BUFFER                                            *          
**********************************************************************          
PFCLR    GOTOR LOADSCRN,TWASCRN                                                 
         MVI   BUFSW,0             RESET BUFFER CONTROL                         
         MVI   CSSPROG,0           RESET SPROG                                  
         MVI   PROCSW,PROCFST      AND PROCESS CONTROL SWITCH                   
         XC    XCAREA(XCEND-XCAREA),XCAREA                                      
         ZAP   TOTDLRS,PZERO       INITIALIZE TOTAL CASH                        
         L     R2,AORDH                                                         
         NI    4(R2),X'DF'                                                      
         J     IMEREQF             'ENTER REQUIRED FIELDS...'                   
         EJECT                                                                  
**********************************************************************          
* SET UP NEXT SCREEN                                                 *          
**********************************************************************          
PFNXT    GOTOR ERASE               ** CLEAR THE SCREEN **                       
         XC    NLST,NLST                                                        
         XC    NFST,NFST                                                        
         MVI   NITM,0                                                           
         L     R2,ADETH            FIRST DETAIL LINE                            
         OC    NGET,NGET                                                        
         JZ    IMEREQF             'ENTER REQUIRED...'                          
         XR    R3,R3                                                            
         IC    R3,SCRMXD           MAX DETAIL LINE                              
         CLM   R3,3,NBUF           OR MAX IN BUFFER                             
         BL    *+8                                                              
         ICM   R3,3,NBUF                                                        
         XR    R0,R0                                                            
         MVI   RECFSW,RECFNTX      SET TO EXCLUDE TAX                           
         TM    SCRSTA,SCRTAXQ      TEST TAX SCREEN                              
         BNO   *+8                                                              
         MVI   RECFSW,RECFTAX      ONLY WANT TAX ITEMS                          
*                                                                               
PFNXT3   GOTOR GETLIN              ** DISPLAY NEXT SET **                       
         BNE   PFNXT4              SKIP, IF NOT CORRECT SCREEN                  
         BRAS  RE,SETLOW           SET ADCONS                                   
         BRAS  RE,DSPREC           DISPLAY RECORD                               
         OC    NFST,NFST           TEST FIRST                                   
         BNZ   *+10                                                             
         MVC   NFST,NGET           SAVE NUMBER OF FIRST                         
         MVC   NLST,NGET           AND LAST                                     
         AHI   R0,1                COUNT NUMBER ON SCREEN                       
         STC   R0,NITM                                                          
         BCT   R3,*+8                                                           
         B     PFNXT5                                                           
*                                                                               
         L     R2,ANEXT            POINT TO NEXT LINE                           
PFNXT4   XR    R1,R1                                                            
         ICM   R1,3,NGET           INCREMENT 'GET' NUMBER                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,NGET                                                        
         CLC   NGET,NBUF           TEST FIND OF BUFFER                          
         BNH   PFNXT3                                                           
         CLI   NITM,0              TEST ANYTHING FOUND                          
         BNE   PFNXT5                                                           
         XC    NLST,NLST                                                        
         XC    NFST,NFST                                                        
         XC    NGET,NGET                                                        
         L     R2,ADETH            FIRST DETAIL LINE                            
         J     IMEREQF             'ENTER REQUIRED...'                          
*                                                                               
PFNXT5   TM    SCRSTA,SCRSINQ      TEST SINGLE ITEM SCREEN                      
         BNO   *+8                                                              
         OI    PROCSW,PROCDTL      SET TO PROCESS DETAIL (FOR NAMES)            
         OI    PFKSW,PFKPROC       PF KEY REQUEST PROCESSED                     
         J     XIT                'ITEM(S) XXX DISPLAYED...'                    
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'POST'                                              *          
**********************************************************************          
PFPOST   XR    R3,R3                                                            
         ICM   R3,3,NBUF           NUMBER OF ITEMS IN BUFFER                    
         BZ    PFERR                                                            
         TM    POSTSW,POSTBUF+POSTNXT                                           
         BNZ   PFPOST5             NOT FIRST TIME                               
         BRAS  RE,SAVDET           SAVE CURRENT SCREEN DATA                     
         MVI   RECFSW,RECFNTX      EXCLUDE TAX ITEMS                            
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         BO    PFPOST3                                                          
*                                  ** LOAD SINGLE SCREEN **                     
         GOTOR LOADSCRN,SCRSWAP    LOAD NEW SCREEN                              
         GOTOR RESTOP              RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
*                                                                               
PFPOST3  XC    NGET,NGET           FIRST TIME                                   
*                                                                               
PFPOST5  OI    POSTSW,POSTBUF      SET POST SWITCH                              
         NI    POSTSW,X'FF'-(POSTNXT) TURN OFF 'NEXT'                           
         OI    PROCSW,PROCDTL      USING DETAIL SCREEN                          
*                                                                               
         L     R2,ADETH            R2=A(DETAIL LINE)                            
         XR    R0,R0                                                            
         IC    R0,SCRMXD           MAX DETAIL LINES                             
         MVI   NITM,0              NUMBER ON SCREEN                             
         XC    NFST,NFST                                                        
*                                                                               
PFPOST7  XR    R1,R1                                                            
         ICM   R1,3,NGET           INCREMENT LINE NUMBER                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,NGET                                                        
         CLC   NGET,NBUF           TEST LAST DISPLAYED                          
         BH    PFPOST9             YES, ALL DONE                                
         OC    NFST,NFST                                                        
         BNZ   *+8                                                              
         STCM  R1,3,NFST           SAVE FIRST ON SCREEN                         
         GOTOR GETLIN              GET AND DISPLAY NEXT RECORD                  
         BNE   PFPOST7                                                          
         BRAS  RE,SETLOW           SET ADCONS                                   
         BRAS  RE,DSPREC           DISPLAY RECORD                               
         MVC   NLST,NGET                                                        
         XR    R1,R1                                                            
         IC    R1,NITM                                                          
         AHI   R1,1                                                             
         STC   R1,NITM             NUMBER ON SCREEN                             
         L     R2,ANEXT                                                         
         BCT   R0,PFPOST7          TEST END OF SCREEN                           
*                                                                               
PFPOST9  CLI   NITM,0              ANYTHING TO POST                             
         BE    PFPOST10                                                         
         OI    POSTSW,POSTNXT      SET FOR NEXT                                 
         J     XIT                                                              
*                                                                               
PFPOST10 TM    PROCSW,PROCTAX      TEST ANY TAX ITEMS                           
         BNO   PFPOST11                                                         
         TM    POSTSW,POSTTAX      TEST ALREADY PROCESSED TAX                   
         BO    PFPOST11                                                         
         GOTOR LOADSCRN,SCRTAXS    LOAD THE TAX SCREEN                          
         GOTOR RESTOP              RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
         OI    POSTSW,POSTTAX      SET TAX POSTING IN PROCESS                   
         MVI   RECFSW,RECFTAX      ONLY PASS TAX ITEMS                          
         B     PFPOST3                                                          
*                                                                               
PFPOST11 GOTOR LOADSCRN,FRSTSCRN   RESTORE ORIGINAL SCREEN                      
         BRAS  RE,REINIT           RE-INTITIALIZE                               
         J     IMPOSTD             'N ITEMS POSTED $99                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* PRODUCTION POSTINGS                                                *          
**********************************************************************          
PPOST    NMOD1 0,**PPOST*                                                       
         L     RC,0(R1)                                                         
         BRAS  RE,PSTDLD           BUILD DESCRIPTION ELEMENT                    
*                                                                               
*                                  **ELEMENTS FOR DEBIT TO JOB**                
         TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BZ    PPOST03             NO                                           
         USING SPDELD,R8                                                        
         MVI   SPDEL,SPDELQ        ADD ELEMENT WITH REAL CONTRA                 
         MVI   SPDLN,SPDLN1Q+L'ACTKCULA-1                                       
         MVC   SPDACCS(L'ACTKCULA-1),CASHACCT                                   
         OC    VENDACCT,VENDACCT   TEST FOR VENDOR                              
         BZ    *+18                NO, USE CASH                                 
         TM    BCCPYST3,CPYSCA22   TEST TO USE CASH AS CAC(EXPENSE BIT)         
         BO    *+10                YES                                          
         MVC   SPDACCS(L'ACTKCULA-1),VENDACCT+1 NO-USE VENDOR                   
         XR    R0,R0                                                            
         IC    R0,SPDLN                                                         
         AR    R8,R0                                                            
*                                                                               
PPOST03  CLI   PASSCD,C'N'         PASS CD TO CLIENT ?                          
         BE    *+8                                                              
         BRAS  RE,PSTCDL           BUILD/ADD CD ELEMENT                         
*                                                                               
         TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BZ    PPOST05             NO                                           
         USING SCIELD,R8                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITSJXP                                                 
         ZAP   SCIAMNT,INAMNT      CARRY FINANCIAL POSTING AMT                  
         CP    CDAMNT,PZERO        IS THERE A DISCOUNT?                         
         BE    *+18                NO                                           
         CLI   CASHACCT,0          YES, IS THERE A CASH ACCOUNT?                
         BNE   *+10                YES, CD NOT ALLOWED                          
         SP    SCIAMNT,CDAMNT                                                   
         LA    R8,SCILN1Q(R8)                                                   
*                                                                               
PPOST05  BRAS  RE,PSTTRL                                                        
         CLI   CASHACCT,0          IS THERE A CASH ACCOUNT?                     
         BNE   PPOST07             YES, NO PAYABLE POSTING THEN                 
*                                                                               
         LA    RF,PRODLIST                                                      
         CLC   VENDACCT+1(2),0(RF)                                              
         BE    *+20                                                             
         CLI   0(RF),X'FF'                                                      
         BE    PPOST07                                                          
         LA    RF,2(RF)                                                         
         B     *-22                                                             
*                                                                               
         BRAS  RE,PSTPAK           PAYABLE ACCOUNT ELEMENT                      
                                                                                
PPOST07  BRAS  RE,PSTFFL           ADD FFTEL FOR CLIENT/PROD                    
         BRAS  RE,PSTORD           ADD ORDER NUMBER ELEMENT                     
*                                                                               
*                                  ** BUILD 69 DEBIT ELEMENT **                 
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,WRKCODE                                                 
         MVC   DLPSDBAC,JOBACCT    DEBIT JOB - SJ                               
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,VENDACCT   CONTRA = PROD VENDOR                         
         MVC   DLPSCRNM,VENDNAME                                                
*                                                                               
         CLI   VENDACCT,0          IS THERE A VENDOR?                           
         BE    PPOST09             NO - MUST USE CASH                           
         CLI   CASHACCT,0          DO WE HAVE CASH ACCT?                        
         BE    PPOST11             NO.                                          
         TM    BCCPYST2,CPYSCACA   CONTRA A/C ON JOB BT3 IS CASH A/C            
         BZ    PPOST11             NO                                           
*                                                                               
PPOST09  MVC   DLPSCRAC,CASHACCT   CASH ACCT NUMBER.                            
         MVC   DLPSCRNM,CASHNAME   CASH ACCT NAME.                              
*                                                                               
PPOST11  TM    ACOPSTAT,ACOXJOB    TEST XJOB                                    
         BZ    PPOST13                                                          
         L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         MVC   DLPSCRAC,EXCSEAC    SET CAC=EXPENSE JOB                          
         MVC   DLPSCRNM,EXCSENM                                                 
         DROP  R3                                                               
*                                                                               
PPOST13  MVI   DLPSTYPE,0                                                       
         CLI   COMSW,C'Y'                                                       
         BE    *+8                                                              
         OI    DLPSTYPE,DLPSTNCM   SET FOR NO COMMISSION                        
         ZAP   DLPSAMNT,INAMNT     NET                                          
         CP    CDAMNT,PZERO        IS DISCOUNT = 0                              
         BE    PPOST15             YES                                          
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   PPOST15             YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         DO WE PASS CD ALONG?                         
         BE    PPOST15             NO                                           
         SP    DLPSAMNT,CDAMNT     NET - CD                                     
*                                                                               
PPOST15  TM    ACOPSTAT,ACOXJOB    TEST XJOB                                    
         BZ    *+10                                                             
         ZAP   DLPSAMNT,PZERO      MAKE A ZERO FINANCIAL POSTING                
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* BUILD ELEMENTS FOR CREDIT TO VENDOR                                *          
**********************************************************************          
         BRAS  RE,PSTOTH           ADD PRODUCT/JOB ELEMENT                      
         BRAS  RE,PSTCDL           ADD CD ELEMENT                               
         BRAS  RE,PSTTRL           ADD TRANSACTION ELEMENT                      
         BRAS  RE,PSTFFL           ADD FREEFORN TEXT                            
         BRAS  RE,PSTORD           ADD ORDER NUMBER ELEMENT                     
         BRAS  RE,PSTLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,PSTDUE           ADD DUE DATE ELEMENT                         
*                                                                               
*                                  ** BUILD 6A CREDIT ELEMENT **                
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT SUPPLIER                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,CLIACCT    CONTRA = PROD VENDOR                         
         MVC   DLPSDBNM,CLINAME                                                 
         MVC   DLPSCRAC,VENDACCT   CR JOB - SJ                                  
         MVC   DLPSCRNM,VENDNAME                                                
         CLC   VENDACCT+1(2),SXVND                                              
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOBACCT    OR CLI/PRD/JOB FOR EXPENSE VENDORS           
         MVC   DLPSDBNM,JOBNAME                                                 
*                                                                               
         CLI   CASHACCT,0          IS THERE A CASH ACCOUNT?                     
         BE    *+22                NO.                                          
         MVC   DLPSDBAC+6(3),PROACCT+6       IF CASH WE NEED PROD               
         MVC   DLPSCRAC,CASHACCT   CASH ACCT NUMBER.                            
         MVC   DLPSCRNM,CASHNAME   CASH ACCT NAME.                              
*                                                                               
         MVC   DLPSANAL,CLIOFFC                                                 
         OC    CRDOFF,CRDOFF       TEST FOR CREDIT OFFICE                       
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF     YES-USE IT FOR CREDIT POSTING                
         ZAP   DLPSAMNT,INAMNT                                                  
         CLI   CASHACCT,0          IS THERE A CASH ACCOUNT?                     
         BNE   *+10                YES - NO CD ALLOWED                          
         SP    DLPSAMNT,CDAMNT                                                  
*                                                                               
         LR    R6,R8               SAVE A(CREDIT ELEMENT)                       
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* BUILD ELEMENTS FOR CREDIT TO INCOME                                *          
**********************************************************************          
         CP    CDAMNT,PZERO        DID VENDOR HAVE DISCOUNT                     
         BE    PPOST17             NO                                           
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   PPOST17             YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         CD FOR SUPPLIER                              
         BNE   PPOST17                                                          
*                                                                               
         BRAS  RE,PSTTRL           ADD STATUS ELEMENT                           
         BRAS  RE,PSTFFL           ADD FREEFORM TEXT                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,1(R6)            LENGTH OF DLPOST ELEMENT                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R6)       RESTORE CREDIT ELEMENT                       
         MVC   DLPSCRAC,SIMDAC     CREDIT TO INCOME                             
         MVC   DLPSCRNM,SIMDACN                                                 
         MVC   DLPSDBAC,PROACCT    CLI/PROD AS CONTRA                           
         MVC   DLPSDBNM,PRONAME                                                 
         ZAP   DLPSAMNT,CDAMNT                                                  
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
PPOST17  CLI   PCONSULT,C'Y'       SHOULD WE POST TO 2C?                        
         BNE   PPOST19             NO.                                          
*                                                                               
         BRAS  RE,PSTTRL           ADD STATUS ELEMENT                           
         BRAS  RE,PSTFFL           ADD FREEFORM TEXT                            
*                                                                               
         USING DLPOSTD,R8          ** POST 2C **                                
         MVI   DLPSEL,DLPSEDCQ     DEBIT & CREDIT                               
         MVI   DLPSLEN,DLPSLNQ     LENGTH                                       
         MVC   DLPSDBAC,P2CNUM     DB ACCT #                                    
         MVC   DLPSDBNM,P2CNAM     DB ACCT NAME                                 
         MVC   DLPSCRAC,PROTROL    CR ACCT #                                    
         MVC   DLPSCRNM,PROTROLN   CR ACCT NAME                                 
         OI    DLPSTYPE,DLPSTSUB                                                
         MVC   DLPSANAL,CLIOFFC    OFFICE                                       
         OC    CRDOFF,CRDOFF       TEST CREDIT OFFICE OVERRIDE                  
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF                                                  
         ZAP   DLPSAMNT,INAMNT                                                  
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
PPOST19  TM    ACOPSTAT,ACOXJOB    TEST XJOB                                    
         BZ    PPOST31             NO                                           
         BRAS  RE,PSTTRL           ADD STATUS ELEMENT                           
         BRAS  RE,PSTFFL           ADD FREEFORM TEST                            
*                                                                               
         L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         MVI   EXCACT,EXCAPST      POST CALL                                    
         ST    R9,EXCAGWS                                                       
         ST    R8,EXCADAY                                                       
         MVC   EXCFINO,FINOFF                                                   
         OC    FINOFF,FINOFF       TEST FOR FINANCIAL OFFICE                    
         BNZ   *+10                                                             
         MVC   EXCFINO,CLIOFFC                                                  
*                                                                               
         MVC   EXCSECAC,VENDACCT   EXPENSE POSTING CAC                          
         MVC   EXCSECAN,VENDNAME   AND CAC NAME                                 
         CLI   VENDACCT,0          TEST FOR A VENDOR                            
         BE    PPOST21             NO-USE CASH ACCOUNT                          
         CLI   CASHACCT,0          TEST FOR CASH ACCOUNT                        
         BE    PPOST23             NO                                           
         TM    BCCPYST3,CPYSCA22   TEST EXPENSE BIT TO USE CASH AS CAC          
         BZ    PPOST23             NO                                           
*                                                                               
PPOST21  MVC   EXCSECAC,CASHACCT                                                
         MVC   EXCSECAN,CASHNAME                                                
*                                                                               
PPOST23  ZAP   EXCAMNT,INAMNT      SET POSTING AMOUNT                           
         CP    CDAMNT,PZERO        TEST FOR CASH DISCOUNT                       
         BE    PPOST25             NO                                           
         CLI   CASHACCT,0          TEST POSTING TO CASH ACCOUNT                 
         BNE   PPOST25             YES                                          
         SP    EXCAMNT,CDAMNT      DEDUCT CD FROM POSTING                       
         ZAP   EXCDAMNT,CDAMNT                                                  
*                                                                               
PPOST25  GOTO1 VEXCEL,EXCELD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R8,EXCADAY                                                       
*                                                                               
PPOST31  J     PSTEND              FINISH UP THE POSTIND                        
*                                                                               
PRODLIST DC    C'SWSXSYSV',X'FF'                                                
         LTORG                                                                  
*                                                                               
         DROP  R3,R8,RB                                                         
         EJECT                                                                  
**********************************************************************          
* TAX POSTINGS                                                       *          
**********************************************************************          
TPOST    NMOD1 0,**TPOST*                                                       
         L     RC,0(R1)                                                         
         ZAP   INAMNT,PZERO                                                     
         BRAS  RE,PSTDLD           BUILD DESCRIPTION ELEMENT                    
*                                                                               
         LA    R0,MXTXLQ           MAX TAX LOCALITIES                           
         LA    R5,TAXIT                                                         
         USING TXD,R5                                                           
TPOST3   BRAS  RE,PSTORD           ORDER ELEMNT                                 
         BRAS  RE,PSTLIN           LONG INVOICE NUMBER                          
         BRAS  RE,PSTSUT           SALES TAXES ELEMENT                          
         MVC   VENDACCT,TXACC                                                   
         BRAS  RE,PSTPAK           ADD PAYABLE ACCOUNT ELEMENT                  
*                                                                               
*                                  ** BUILD 69 DEBIT ELEMENT **                 
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,JOBACCT                                                 
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,TXACC      CONTRA - PAYABLE ACCOUNT                     
         MVC   DLPSCRNM,TXACCN                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,TXTAX                                                   
         MVC   DLPSANAL,WRKCODE                                                 
         CLC   DLPSDBAC+1(2),=C'SJ'                                             
         BE    *+10                                                             
         MVC   DLPSANAL,TAXOFF                                                  
         LA    R8,DLPSLNQ(R8)                                                   
         AP    INAMNT,TXTAX        ADD TO TOTAL FOR LINE                        
*                                                                               
         BRAS  RE,PSTOTH           ADD PRODUCT/JOB ELEMENT                      
         BRAS  RE,PSTSUT           SALES TAXES ELEMENT                          
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT POSTING                               
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CLIACCT    CONTRA ACCOUNT IS CLIENT                     
         MVC   DLPSDBNM,CLINAME                                                 
*                                                                               
         CLC   TXACC+1(2),SXVND                                                 
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOBACCT    FOR SX IT'S CLI/PRD/JOB                      
         MVC   DLPSDBNM,JOBNAME                                                 
*                                                                               
         MVC   DLPSCRAC,TXACC      CREDIT ACCOUNT                               
         MVC   DLPSCRNM,TXACCN     CREDIT ACCOUNT NAME                          
         MVI   DLPSTYPE,0          MAIN ACCOUNTING ENTRY                        
         ZAP   DLPSAMNT,TXTAX      AMOUNT                                       
         MVC   DLPSANAL,TAXOFF     OFFICE                                       
         LA    R8,DLPSLNQ(R8)                                                   
         LA    R5,TXLNQ(R5)                                                     
         CLI   TXACC,0                                                          
         BE    TPOSTX                                                           
         BCT   R0,TPOST3                                                        
*                                                                               
TPOSTX   J     PSTEND                                                           
         LTORG                                                                  
         DROP  R5,R8,RB                                                         
         EJECT                                                                  
**********************************************************************          
* EXPENSE POSTINGS                                                   *          
**********************************************************************          
EPOST    NMOD1 0,**EPOST*                                                       
         L     RC,0(R1)                                                         
         BRAS  RE,PSTDLD           BUILD DESCRIPTION ELEMENT                    
*                                                                               
         TM    BCCPYST1,CPYSDISC   C/D ON EXPENSES TO INCOME ACCT               
         BNZ   *+8                 YES                                          
         BRAS  RE,PSTCDL           BUILD/ADD CD ELEMENT                         
*                                                                               
         BRAS  RE,PSTANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
         BRAS  RE,PSTORD           ADD ORDER NUMBER ELEMENT                     
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     **DEBIT EXPENSE ACCOUNT **                   
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC(51),POSTACC       DEBIT ACCOUNT - DEF EXP               
         MVC   DLPSCRAC(51),XVNDACCT      CONTRA ACCOUNT - DEF VENDOR           
         CLI   XVNDACCT,0          IS THERE A EXP VENDOR?                       
         BE    EPOST03             NO - MUST USE CASH                           
         CLI   CASHACCT,0          CASH ACCOUNT?                                
         BE    EPOST05             NO                                           
         TM    BCCPYST3,CPYSCA22   C/A ON SE POSTING IS CASH A/C                
         BZ    EPOST05             NO..                                         
*                                                                               
EPOST03  MVC   DLPSCRAC(51),CASHACCT      CASH ACCT # & NAME                    
*                                                                               
EPOST05  MVC   DLPSANAL,FINOFF                                                  
         ZAP   DLPSAMNT,INAMNT                                                  
         CP    CDAMNT,PZERO        IS DIC = 0                                   
         BE    EPOST07             YES                                          
         CLI   CASHACCT,0          DO WE HAVE A CASH ACCT?                      
         BNE   EPOST07             YES - NO CD ALLOWED                          
         TM    BCCPYST1,CPYSDISC   C/D ON EXPENSES TO INCOME ACCT               
         BNZ   EPOST07             YES                                          
         SP    DLPSAMNT,CDAMNT     NET LESS C.D                                 
*                                                                               
EPOST07  ZAP   EXPOST,DLPSAMNT     SAVE EXPENSE POSTING                         
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
         TM    BCCPYST1,CPYSDISC   C/D ON EXPENSES TO INCOME ACCT               
         BNZ   *+8                 YES                                          
         BRAS  RE,PSTCDL           BUILD/ADD CD ELEMENT                         
*                                                                               
         BRAS  RE,PSTANL           ANALYSIS OFFICE                              
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
         BRAS  RE,PSTORD           ADD ORDER NUMBER ELEMENT                     
         BRAS  RE,PSTLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,PSTDUE           ADD DUE DATE ELEMENT                         
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     ** CREDIT VENDOR **                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC(51),POSTACC        CONTRA ACCOUNT - EXP                 
         MVC   DLPSCRAC(51),XVNDACCT       CREDIT ACCOUNT - VENDOR              
         MVC   DLPSANAL,FINOFF                                                  
         OC    CRDOFF,CRDOFF       TEST FOR CREDIT OFFICE                       
         BZ    *+10                NO                                           
         MVC   DLPSANAL,CRDOFF                                                  
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BE    EPOST11             NO.                                          
         MVC   DLPSCRAC,CASHACCT   CASH ACCT #                                  
         MVC   DLPSCRNM,CASHNAME   CASH ACCT NAME                               
*                                                                               
EPOST11  ZAP   DLPSAMNT,INAMNT     NET                                          
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   *+10                YES - NO CD ALLOWED                          
         SP    DLPSAMNT,CDAMNT     NET - CD                                     
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
         CP    CDAMNT,PZERO        DID VENDOR HAVE DISCOUNT                     
         BE    EPOST13             NO                                           
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   EPOST13             YES - NO CD ALLOWED                          
         TM    BCCPYST1,CPYSDISC   C/D ON EXPENSES TO INCOME ACCT               
         BZ    EPOST13             NO                                           
*                                                                               
         BRAS  RE,PSTANL           ANALYSIS OFFICE                              
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
*                                                                               
         MVI   DLPSEL,DLPSECRQ     **CREDIT INCOME ACCOUNT **                   
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,XVNDACCT                                                
         MVC   DLPSDBNM,XVNDNAME                                                
         MVC   DLPSCRAC,SIMDAC                                                  
         MVC   DLPSCRNM,SIMDACN                                                 
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,CDAMNT                                                  
         MVC   DLPSANAL,FINOFF                                                  
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 2P WITH 29 CONTRA POSTING                                          *          
**********************************************************************          
EPOST13  CLI   STFSW,C'Y'                                                       
         BNE   EPOST21                                                          
         BRAS  RE,PSTANL           ANALYSIS OFFICE                              
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     ** DEBIT STAFF **                            
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,STAFFNUM   DEBIT 2P                                     
         MVC   DLPSDBNM,STAFFNAM                                                
         MVC   DLPSCRNM,CRPSNAME   CONTRA 29                                    
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,DLPSTSUB   SUBSIDIARY FROM HERE                         
         MVI   DLPSCRAC,C'*'       CONTRA IS *EXPENSE-CLIENT                    
         MVC   DLPSANAL,FINOFF                                                  
*                                                                               
         L     R2,AXACH            CREDIT SE ACCT                               
         XR    R1,R1                                                            
         IC    R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                FOR EX INST.                                 
*                                                                               
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),POSTACC+3                                          
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         BL    EPOST15                                                          
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   DLPSCRAC+1(14),SPACES                                            
         L     R2,AXACH                                                         
         LA    R6,1(R1)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         BNM   *+6                                                              
         DC    H'0'                SHOULD HAVE CAUGHT THIS EARLIER              
         LA    R3,8(R6,R2)         R3=A(DESIRED PORTION ACCT INPUT)             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),0(R3) = * +DESIRED PORTION ACCT INPUT              
         LR    R1,R4                                                            
*                                                                               
EPOST15  STC   R1,BYTE             LEN OF ACCT INPUT DESIRED                    
         LA    RF,DLPSCRAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),CRPSNUM+3   2/9 ACCT KEY                                 
         MVC   WORK(15),DLPSCRAC   SAVE FOR DEBIT SIDE OF CRD CLI EL            
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 29 WITH 2P CONTRA POSTING                                          *          
**********************************************************************          
         BRAS  RE,PSTANL           ANALYSIS OFFICE                              
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     ** CREDIT CLIENT **                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBNM,STAFFNAM   CONTRA 2P                                    
         MVC   DLPSCRNM,CRPSNAME   CREDIT 29                                    
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,DLPSTSUB   SUBSIDIARY FROM HERE                         
         MVC   DLPSDBAC(15),WORK   SAVED FROM DEBIT ELEMENT                     
         MVC   DLPSCRAC,CRPSNUM                                                 
         XR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         LA    RF,DLPSDBAC+1(R1)                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),STAFFNUM+3  CONTRA IS *EXPENSE-STAFF                     
         MVC   DLPSANAL,FINOFF                                                  
*                                                                               
         CLI   V29SW,C'Y'          SHOULD CONTRA BE VENDOR?                     
         BNE   *+16                NO                                           
         MVC   DLPSDBAC,XVNDACCT   EXP VENDOR                                   
         MVC   DLPSDBNM,XVNDNAME   EXP VENDOR NAME                              
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 2D WITH 28 CONTRA POSTING  DR                                      *          
* 28 WITH 2D CONTRA POSTING  CR                                      *          
**********************************************************************          
EPOST21  CLI   DEPSW,C'Y'                                                       
         BNE   EPOST25                                                          
         BRAS  RE,PSTANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ     ** DEBIT/CREDIT **                           
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DEPNUM     DEBIT 2D   CONTRA = 28                       
         MVC   DLPSDBNM,DEPNAME                                                 
         MVC   DLPSCRAC,CRDSNUM    CREDIT 28  CONTRA = 2D                       
         MVC   DLPSCRNM,CRDSNAME                                                
         ZAP   DLPSAMNT,EXPOST                                                  
         MVC   DLPSANAL,FINOFF                                                  
         OI    DLPSTYPE,DLPSTSUB   IN CASE WE MISSED IT ON ELEMENT 2            
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 2C WITH 27 CONTRA POSTING                                          *          
* 27 WITH 2C CONTRA POSTING                                          *          
**********************************************************************          
EPOST25  CLI   ECONSULT,C'Y'       DO WE NEED EXP 2C?                           
         BNE   EPOST29             NO.                                          
         BRAS  RE,PSTANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ     ** DEBIT/CREDIT **                           
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,E2CNUM     ACCT #                                       
         MVC   DLPSDBNM,E2CNAM     ACCT NAME                                    
         MVC   DLPSCRAC,EXPTROL    27                                           
         MVC   DLPSCRNM,EXPTROLN   27                                           
         ZAP   DLPSAMNT,INAMNT     AMT                                          
         MVC   DLPSANAL,FINOFF                                                  
         OC    CRDOFF,CRDOFF       USE CREDIT OFFICE IF AVAILABLE               
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF                                                  
         OI    DLPSTYPE,DLPSTSUB                                                
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 1C WITH 1P CONTRA POSTING                                          *          
**********************************************************************          
EPOST29  CLI   COSTSW,C'Y'                                                      
         BNE   EPOST33                                                          
         BRAS  RE,PSTANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     ** DEBIT 1C **                               
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CONTRA  1C                                   
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CRCNUM     DEBIT 1P                                     
         MVC   DLPSDBNM,CRCNAME                                                 
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,DLPSTSUB                                                
         MVC   DLPSANAL,FINOFF                                                  
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
         BRAS  RE,PSTANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,PSTFFL           FREEFORM TEXT                                
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     ** CREDIT CLIENT **                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CR 1C                                        
         MVC   DLPSCRNM,COSTNAME                                                
*                                                                               
         MVC   DLPSDBAC(1),COMPANY                                              
         MVC   DLPSDBAC+1(2),=C'13'                                             
         MVC   DLPSDBAC+3(12),SPACES                                            
         MVC   DLPSDBAC+3(L'COSTANAL),COSTANAL                                  
         TM    BCCPYST5,CPYSNCST   NEW COST?                                    
         BZ    *+10                NO                                           
         MVC   DLPSDBAC,CR13NUM    YES, GET CONTRA FROM CATCALL                 
*                                                                               
         MVC   DLPSDBNM,CRCNAME                                                 
         OI    DLPSTYPE,DLPSTSUB                                                
         MVC   DLPSANAL,FINOFF                                                  
         ZAP   DLPSAMNT,EXPOST                                                  
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
EPOST33  J     PSTEND              FINISH POSTING                               
*                                                                               
         LTORG                                                                  
         DROP  R8,RB                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD POSTING DESCRIPTION ELEMENT                                  *          
**********************************************************************          
PSTDLD   LA    R2,IOAREA           CLEAR IO AREA                                
         LA    R3,L'IOAREA                                                      
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
         LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         MVC   DLDSREF,REFNUM                                                   
         MVC   DLDSDATE,DOCDATE                                                 
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         OI    DLDSSTAT,TRNSAUTH   AUTHORIZED INVOICE INDICATION                
         CLI   URGENT,C'U'                                                      
         JNE   *+8                                                              
         OI    DLDSSTAT,TRNSURG    URGENT                                       
*                                                                               
         XR    R1,R1               GET NARRATIVE LENGTH                         
         ICM   R1,3,NARRLEN                                                     
         JZ    *+6                                                              
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   DLDSNARR(0),NARR                                                 
         LA    R1,DLDSNARR-DLDESCD                                              
         AH    R1,NARRLEN                                                       
         STC   R1,DLDSLEN                                                       
         AR    R8,R1                                                            
         BR    RE                                                               
*                                                                               
         DROP  R8                                                               
         EJECT                                                                  
**********************************************************************          
* COMMON ELEMENT ROUTINES                                            *          
**********************************************************************          
         USING OTHELD,R8           BUILD 'OTHERS' ELEMENT                       
PSTOTH   MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(13),SPACES   PRODUCT AND JOB                              
         MVC   OTHNUM(6),PRODCODE                                               
         MVC   OTHNUM+6(6),JOBNUM                                               
         LA    R8,OTHLN1Q(R8)                                                   
         BR    RE                                                               
*                                                                               
PSTCDL   CP    CDAMNT,PZERO        IS THERE DISCOUNT ?                          
         BER   RE                  NO,                                          
         CLI   CASHACCT,0          IS THERE A CASH ACCT ?                       
         BNER  RE                  YES - CD NOT ALLOWED                         
*                                                                               
         USING SCIELD,R8                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCDSC                                                 
         ZAP   SCIAMNT,CDAMNT     CD ELEMENT FOR JOB                            
         LA    R8,SCILN1Q(R8)                                                   
         BR    RE                                                               
         DROP  R8                                                               
*                                  BUILD/ADD FFTEL FOR CLI/PROD                 
PSTFFL   CLI   FFTELEM,0           DO WE HAVE A CLIENT AND PRODUCT?             
         BER   RE                  NO, DONE                                     
         LA    R3,FFTELEM                                                       
         USING FFTELD,R3                                                        
         XR    R1,R1                                                            
         IC    R1,FFTLN                                                         
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   0(0,R8),FFTELEM                                                  
         LA    R8,1(R1,R8)         R8 TO NEXT AREA                              
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
PSTTRL   TM    ACOPSTAT,ACOXJOB                                                 
         BZR   RE                                                               
         USING TRSELD,R8                                                        
         XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ        BUILD TRANSACTION STATUS ELEMENT             
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSXJOB   SET X-JOB BIT ON                             
         LA    R8,TRSLNQ(R8)                                                    
         BR    RE                                                               
         DROP  R8                                                               
*                                                                               
PSTANL   CLC   ANAOFF,C' '         ANALYSIS OFICE                               
         BNHR  RE                                                               
         CLC   ANAOFF,FINOFF                                                    
         BER   RE                                                               
         USING ANOELD,R8                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER                                                  
         MVC   ANOOFFC,ANAOFF                                                   
         LA    R8,ANOLNQ(R8)                                                    
         BR    RE                                                               
         DROP  R8                                                               
*                                                                               
PSTORD   OC    LASTORD,LASTORD     ORDER ELEMENT                                
         BZR   RE                                                               
         USING FFNELD,R8                                                        
         XC    FFNELD(FFNLN2Q),FFNELD                                           
         MVI   FFNEL,FFNELQ                                                     
         MVI   FFNLN,FFNLN2Q                                                    
         MVC   FFNONUM,LASTORD                                                  
         MVC   FFNSTAT,ORDSTA                                                   
         LA    R8,FFNLN2Q(R8)                                                   
         BR    RE                                                               
         DROP  R8                                                               
*                                                                               
PSTLIN   CLI   LONGINVL,0          LONG INVOICE NUMBER                          
         BER   RE                                                               
         USING FFTELD,R8                                                        
         XC    0(FFTDATA-FFTELD+L'LONGINV,R8),0(R8)                             
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTINVN                                                 
         XR    R1,R1                                                            
         IC    R1,LONGINVL                                                      
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   FFTDATA(0),LONGINV                                               
         AHI   R1,FFTDATA-FFTELD+1                                              
         STC   R1,FFTLN                                                         
         LA    R8,0(R1,R8)                                                      
         BR    RE                                                               
         DROP  R8                                                               
*                                                                               
         USING PAKELD,R8                                                        
PSTPAK   MVI   PAKEL,PAKELQ        PAYABLE ACCOUNT ELEMENT                      
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,VENDACCT                                                  
         MVC   PAKOFF,CLIOFFC                                                   
         OC    CRDOFF,CRDOFF                                                    
         JZ    *+10                                                             
         MVC   PAKOFF,CRDOFF                                                    
         MVC   PAKCON,CLIACCT                                                   
         CLC   VENDACCT+1(2),SXVND                                              
         JNE   *+10                                                             
         MVC   PAKCON,JOBACCT                                                   
         MVC   PAKDATE,DOCDATE                                                  
         MVC   PAKREF,REFNUM                                                    
         LA    R8,PAKLNQ(R8)                                                    
         BR    RE                                                               
         DROP  R8                                                               
*                                                                               
         USING TXD,R5                                                           
         USING SUTELD,R8            SALES/USE TAX ELEMENT                       
PSTSUT   XC    SUTEL(SUTLN2Q),SUTEL                                             
         MVI   SUTEL,SUTELQ                                                     
         MVI   SUTLN,SUTLN2Q                                                    
         MVC   SUTEFF,TXEFF                                                     
         MVC   SUTRTE,TXRTE                                                     
         MVC   SUTBAS,INAMNT                                                    
         MVC   SUTLOC(L'TXLOC),TXLOC                                            
         LA    R8,SUTLN2Q(R8)                                                   
         BR    RE                                                               
         DROP  R8                                                               
*                                                                               
         USING DUEELD,R8           DUE DATE ELEMENT                             
PSTDUE   OC    DODATE,DODATE                                                    
         BZR   RE                                                               
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,DODATE                                                   
         LA    R8,DUELNQ(R8)                                                    
         BR    RE                                                               
         DROP  R8                                                               
         EJECT                                                                  
**********************************************************************          
* FINISH POSTING - ADD TO FILE                                       *          
**********************************************************************          
PSTEND   MVI   0(R8),0             MARK END                                     
         LA    R8,1(R8)            END ADDRESS                                  
         LA    R3,IOAREA           START ADDRESS                                
         SR    R8,R3                                                            
         STCM  R8,3,IOAREA         TOTAL LENGTH                                 
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         XR    R1,R1               DOCUMENT #                                   
         IC    R1,REFNUML                                                       
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   BOWORK1(0),REFNUM                                                
         ZAP   BOPL61,INAMNT       AMOUNT                                       
*                                                                               
         GOTO1 AADDITE,BOPARM,IOAREA,BOPL61,BOWORK1                             
         LA    R0,FVFOK            TEST ADD OK                                  
         CLM   R0,3,FVMSGNO                                                     
         JE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
* SAVE SCREEN DETAIL                                                 *          
**********************************************************************          
SAVDET   NTR1  BASE=*,LABEL=*      ** SAVE SCREEN ITEMS **                      
         XR    R3,R3                                                            
         ICM   R3,1,NITM           NUMBER OF ITEMS ON SCREEN                    
         JZ    XIT                                                              
         XR    RF,RF                                                            
         ICM   RF,3,NFST           NUMBER OF FIRST ON SCREEN                    
         JNZ   *+12                                                             
         ICM   RF,3,NBUF           START WITH TOTAL                             
         LA    RF,1(RF)            +1                                           
         STCM  RF,3,NFST                                                        
         STCM  RF,3,NGET                                                        
         L     R2,ADETH                                                         
         MVI   RECFSW,RECFNTX      SET TO EXCLUDE TAX ITEMS                     
         TM    SCRSTA,SCRTAXQ      IF TAX SCREEN                                
         JNO   SAVDET3                                                          
         MVI   RECFSW,RECFTAX      ONLY WANT TAX ITEMS                          
*                                                                               
SAVDET3  BRAS  RE,SETLOW                                                        
         CLC   NGET,NBUF           TEST NEW ITEM                                
         BH    SAVDET5             YES, ADD NEW ITEM                            
         OC    NGET,NGET                                                        
         BZ    SAVDET5                                                          
         OC    NBUF,NBUF           TEST FIRST TIME                              
         BZ    SAVDET5                                                          
         GOTOR GETLIN                                                           
         BNE   SAVDET9                                                          
         GOTOR PUTLIN              PUT DETAIL LINE TO TSAR                      
         B     SAVDET7                                                          
*                                                                               
SAVDET5  GOTOR ADDLIN,DMCB,(R2)    ADD NEW ITEM                                 
         MVC   NLST,NBUF           LAST ON SCREEN IS LAST IN BUFFER             
*                                                                               
SAVDET7  XR    RF,RF                                                            
         IC    RF,SCRLDL           LENGTH OF DETAIL LINE                        
         AR    R2,RF                                                            
         BCT   R3,*+8                                                           
         J     XIT                                                              
*                                                                               
SAVDET9  XR    R1,R1                                                            
         ICM   R1,3,NGET           INCREMENT NEXT LINE NUMBER                   
         BZ    *+8                 FOR 'NEW' DON'T ADD                          
         LA    R1,1(R1)                                                         
         STCM  R1,3,NGET                                                        
         B     SAVDET3                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ADD A SCREEN LINE TO TSAR BUFFER                                   *          
**********************************************************************          
ADDLIN   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACLIH                                                         
         LHI   R0,MXCAP            TEST MAX CAPACITY                            
         CLM   R0,3,NBUF                                                        
         JNH   EMMXCAP             'MAXIMUM CAPACITY IS ....                    
*                                                                               
         XC    RECSAV,RECSAV                                                    
         BRAS  RE,BLDREC           BUILD A TSAR RECORD                          
         XR    R1,R1               INCREMENT RECORD COUNT                       
         ICM   R1,3,NREC                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,NREC                                                        
         MVC   RECNUM,NREC         SET RECORD NUMBER                            
         MVI   RECDAT,RECDIV       SET INVOICE DATA                             
*                                                                               
         ICM   RF,15,ATAXH         TEST TAX RECORD                              
         JZ    ADDLIN3                                                          
         CLI   8(RF),C' '                                                       
         JNH   ADDLIN3                                                          
         MVI   RECDAT,RECDTX       SET TAX DATA                                 
         OI    PROCSW,PROCTAX      SET TAXES TO PROCESS                         
         XR    R1,R1                                                            
         IC    R1,NTAXL            UPDATE NUMBER OF TAX LINES                   
         AHI   R1,1                                                             
         STC   R1,NTAXL                                                         
*                                                                               
ADDLIN3  GOTOR TOTSAR,TSRADDQ      ADD IT                                       
         ICM   RF,15,AAMTH         GET AMOUNT                                   
         CLI   RECDAT,RECDTX                                                    
         JNE   *+8                                                              
         ICM   RF,15,ATAXH         USE TAX - IF TAX RECORD                      
         BRAS  RE,GETAMTL          GET LENGTH OF AMOUNT FIELD                   
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',8(RF)),(R0)                                  
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BAD AMOUNT/SHOULD NOT GET THIS FAR           
         AP    TOTDLRS,4(8,R1)     ADD CURRENT AMOUNT TO TOTAL                  
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* PUT A SCREEN LINE TO TSAR BUFFER                                   *          
**********************************************************************          
PUTLIN   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,BLDAMT           BUILD AMOUNT HEADER/FIELD IN WORK            
         LA    RF,WORK             ** SUBTRACT OLD AMOUNT **                    
         GOTO1 CASHVAL,DMCB,(X'82',8(RF)),(R0)                                  
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BAD AMOUNT/SHOULD NOT GET THIS FAR           
         SP    TOTDLRS,4(8,R1)     SUBTRACT OLD AMOUNT                          
*                                                                               
         CLI   RECDAT,RECDTX       TEST TAX ITEM                                
         JNE   PUTLIN3                                                          
         XR    RF,RF                                                            
         IC    RF,NTAXL            ADJUST NUMBER OF TAX LINES                   
         SHI   RF,1                                                             
         JNM   *+6                                                              
         DC    H'0'                                                             
         STC   RF,NTAXL                                                         
         CLI   NTAXL,0             TEST ANY REMAINING TAX LINES                 
         JNE   *+8                                                              
         NI    PROCSW,X'FF'-PROCTAX  TURNOFF TAX SWITCH                         
*                                                                               
PUTLIN3  MVC   BCHALF,RECNUM       SAVE RECORD NUMBER                           
         BRAS  RE,BLDREC           RE-BUILD A TSAR RECORD                       
         MVC   RECNUM,BCHALF       RESTORE RECORD NUMBER                        
         ICM   RF,15,ATAXH         TEST TAX RECORD                              
         JZ    PUTLIN5                                                          
         CLI   8(RF),C' '                                                       
         JNH   PUTLIN5                                                          
         MVI   RECDAT,RECDTX       SET TAX DATA                                 
         OI    PROCSW,PROCTAX      SET TAXES TO PROCESS                         
         XR    R1,R1                                                            
         IC    R1,NTAXL            UPDATE NUMBER OF TAX LINES                   
         AHI   R1,1                                                             
         STC   R1,NTAXL                                                         
*                                                                               
PUTLIN5  GOTOR TOTSAR,TSRPUTQ      PUT IT BACK                                  
         ICM   RF,15,AAMTH         GET AMOUNT                                   
         CLI   RECDAT,RECDTX                                                    
         JNE   *+8                                                              
         ICM   RF,15,ATAXH         USE TAX - IF TAX RECORD                      
         BRAS  RE,GETAMTL          GET LENGTH OF AMOUNT FIELD                   
         GOTO1 CASHVAL,DMCB,(X'82',8(RF)),(R0)                                  
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BAD AMOUNT/SHOULD NOT GET THIS FAR           
         AP    TOTDLRS,4(8,R1)     ADD NEW AMOUNT                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* DELETE A LINE FROM TSAR BUFFER                                     *          
**********************************************************************          
DELLIN   NTR1  BASE=*,LABEL=*                                                   
         GOTOR TOTSAR,TSRGETQ      RECORD                                       
*                                                                               
         BRAS  RE,BLDAMT           BUILD AMOUNT HEADER/FIELD IN WORK            
         LA    RF,WORK             ** SUBTRACT OLD AMOUNT **                    
         GOTO1 CASHVAL,DMCB,(X'82',8(RF)),(R0)                                  
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BAD AMOUNT/SHOULD NOT GET THIS FAR           
         SP    TOTDLRS,4(8,R1)     SUBTRACT OLD AMOUNT                          
*                                                                               
         GOTOR TOTSAR,TSRDELQ      DELETE                                       
*                                                                               
         CLI   RECDAT,RECDTX       TEST TAX ITEM                                
         JNE   XIT                                                              
         XR    RF,RF                                                            
         IC    RF,NTAXL            ADJUST NUMBER OF TAX LINES                   
         SHI   RF,1                                                             
         JNM   *+6                                                              
         DC    H'0'                                                             
         STC   RF,NTAXL                                                         
         CLI   NTAXL,0             TEST ANY REMAINING TAX LINES                 
         JNE   *+8                                                              
         NI    PROCSW,X'FF'-PROCTAX  TURNOFF TAX SWITCH                         
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* GET A RECORD FROM TSAR BUFFER                                      *          
**********************************************************************          
GETLIN   NTR1  BASE=*,LABEL=*                                                   
         XC    RECSAV,RECSAV                                                    
         GOTOR TOTSAR,TSRGETQ                                                   
         CLI   RECFSW,0            ANY RECORD FILTER ?                          
         JE    XYES                NO FILTER                                    
         TM    RECFSW,RECFTAX      FILTER ON TAX ITEMS ?                        
         JNO   GETLIN3                                                          
         CLI   RECDAT,RECDTX       IS THIS TAX DATA ?                           
         JE    XYES                YES,                                         
         J     XNO                 NO,                                          
*                                                                               
GETLIN3  TM    RECFSW,RECFNTX      EXCLUDE TAX ITEMS ?                          
         JNO   XYES                YES,                                         
         CLI   RECDAT,RECDTX       IS THIS TAX DATA ?                           
         JE    XNO                 YES, EXCLUDE IT                              
         J     XYES                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* INTERFACE TO TSAR                                                  *          
**********************************************************************          
TOTSAR   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,TSARBLK                                                       
         USING TSARD,R4                                                         
         TM    BUFSW,BUFRES        TEST ALREADY RESTORED                        
         BO    TOTSAR5                                                          
*                                                                               
         LR    R0,R1               SAVE ACTION                                  
         MVC   TSACOM,ACOMFACS     A(COMFACS)                                   
         LA    R1,L'RECKEY                                                      
         STC   R1,TSKEYL           SET KEY LENGTRH                              
         LA    R1,RECLNQ                                                        
         STCM  R1,3,TSRECL         SET RECORD LENGTH                            
         LA    R1,RECSAV                                                        
         ST    R1,TSAREC           A(RECORD AREA)                               
         MVI   TSACTN,TSAINI       SET INITIALZE                                
         OI    TSINDS,TSIALLOC     SET TO ALLOCATE                              
         CHI   R0,TSRSAVQ          TEST ACTION SAVE                             
         JE    XIT                 DON'T SAVE - NEVER RESTORED                  
         TM    BUFSW,BUFINI        TEST INITIALIZED                             
         BZ    TOTSAR3             NO, MUST INITIALIZED                         
         MVI   TSACTN,TSARES       SET RESTORE                                  
         MVC   TSPAGL,PAGL         SET LOW PAGE                                 
         MVC   TSPAGN,PAGN         SET NUMBER OF PAGES                          
*                                                                               
TOTSAR3  GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                CAN'T INITIALIZE/RESTORE                     
         MVC   PAGL,TSPAGL                                                      
         MVC   PAGN,TSPAGN                                                      
         OI    BUFSW,BUFINI+BUFRES                                              
         LR    R1,R0                                                            
*                                                                               
TOTSAR5  SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     TOTSADD             ADD                                          
         B     TOTSGET             GET                                          
         B     TOTSPUT             PUT                                          
         B     TOTSSAV             SAVE                                         
         B     TOTSDEL             DELETE                                       
         B     TOTSINT             RE-INITIALIZE                                
*                                                                               
         EJECT                                                                  
TOTSADD  MVI   TSACTN,TSAADD       SET ACTION ADD                               
         B     TOTSALL                                                          
*                                                                               
TOTSGET  MVI   TSACTN,TSAGET       SET ACTION GET                               
         MVC   TSRNUM,NGET         SET RECORD NUMBER                            
         CLC   NGET,TSPRECN        TEST RECORD HIGHER THAN MAX                  
         BH    TOTSERR                                                          
         OC    NGET,NGET                                                        
         BZ    TOTSERR                                                          
         B     TOTSALL                                                          
*                                                                               
TOTSPUT  MVI   TSACTN,TSAPUT       SET ACTION PUT                               
         MVC   TSRNUM,NGET                                                      
         B     TOTSALL                                                          
*                                                                               
TOTSSAV  MVI   TSACTN,TSASAV       SET ACTION SAVE                              
         B     TOTSALL                                                          
*                                                                               
TOTSDEL  MVI   TSACTN,TSADEL       SET ACTION DELETE                            
         MVC   TSRNUM,NGET         SET RECORD NUMBER                            
         B     TOTSALL                                                          
*                                                                               
TOTSALL  GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NBUF,TSPRECN        SAVE NUMBER IN BUFFER                        
         J     XIT                                                              
*                                                                               
TOTSINT  MVI   TSACTN,TSAINI       SET INITIALZE                                
         MVI   TSINDS,TSIALLOC+TSIREUSE                                         
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                CAN'T INITIALIZE                             
         MVC   PAGL,TSPAGL                                                      
         MVC   PAGN,TSPAGN                                                      
         OI    BUFSW,BUFINI+BUFRES                                              
         XC    NBUF,NBUF           CLEAR BUFFER                                 
         J     XIT                                                              
*                                                                               
TOTSERR  BRAS  RE,REINIT           RE-INITIALIZE                                
         J     EMMIVPFK                                                         
         LTORG                                                                  
*                                                                               
         DROP  R4,RB                                                            
         EJECT                                                                  
**********************************************************************          
* CLEAR FIELDS                                                       *          
**********************************************************************          
CLEAR    NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            R3=A(LIST OF FIELDS)                         
         MVC   FLD,SPACES                                                       
*                                                                               
CLEAR3   XR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         LA    RF,PROGD(RF)                                                     
         ICM   R2,15,0(RF)                                                      
         JZ    CLEAR5                                                           
*                                                                               
         TM    4(R2),X'80'         TEST IF FIELD INPUT                          
         JO    *+16                YES                                          
         BRAS  RE,MOVEFLD          NO-CLEAR IT                                  
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CLEAR5   LA    R3,L'XJOTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         JNE   CLEAR3                                                           
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ERASE THE LOWER SCREEN                                             *          
**********************************************************************          
ERASE    NTR1  BASE=*,LABEL=*                                                   
         L     R2,ADETH            R2=A(FIRST DETAIL HEADER)                    
         XR    R6,R6                                                            
         IC    R6,SCRMXD           SET MAX NUMBER OF DETAIL LINES               
*                                                                               
ERASE2   BRAS  RE,SETLOW                                                        
         MVC   FLD,SPACES                                                       
         LA    R3,ACLIH                                                         
         LA    R0,NLOWER-1                                                      
*                                                                               
ERASE4   ICM   R2,15,0(R3)         R2=A(FIELD HEADER)                           
         JZ    ERASE6              NO FIELD ON THIS SCREEN                      
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
ERASE6   LA    R3,4(R3)                                                         
         JCT   R0,ERASE4                                                        
*                                                                               
         L     R2,ANEXT                                                         
         JCT   R6,ERASE2                                                        
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* REPEAT FEATURE ROUTINE-R2=A(CURRENT FLD HDR WITH '*' INPUT)        *          
**********************************************************************          
DUP      CLI   8(R2),C'*'                                                       
         BNER  RE                                                               
DUP1     NTR1  BASE=*,LABEL=*                                                   
         LR    R1,R2               R1=A(SOURCE FIELD)                           
         L     R4,ANEXT            COMPUTE L'DETAIL LINE IN R4                  
         L     RE,ACLIH                                                         
         SR    R4,RE                                                            
         SR    R1,R4               BACK UP TO PREVIOUS LINE                     
         L     R3,ADETH            R3=A(FIRST DETAIL LINE)                      
         CR    R1,R3               TEST IF USER IS ON FIRST LINE                
         JL    EMAITL              'ASTERISK INPUT INVALID ON TOP..'            
*                                                                               
         GOTO1 AFVAL                                                            
         JNE   EMNODUP             'INPUT IS NOT AVAILABLE TO REPEAT'           
*                                                                               
         MVC   5(1,R2),FVILEN      SET NEW LENGTH                               
         MVC   FLD,FVIFLD                                                       
         BRAS  RE,MOVEFLD          MOVE IN NEW DATA                             
         OI    6(R2),X'80'         XMIT IT BACK                                 
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* MOVE DATA TO DISPLAY DATA                                          *          
**********************************************************************          
MOVEFLD  ST    RE,SAVERE                                                        
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   8(0,R2),FLD                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
**********************************************************************          
* RE-INITIALIZE                                                      *          
**********************************************************************          
REINIT   ST    RE,SAVERE                                                        
         MVI   CSSPROG,0           RESET SPROG                                  
         MVI   PROCSW,PROCFST      AND PROCESS CONTROL SWITCH                   
         ZAP   POSTAMT,TOTDLRS                                                  
         XC    XCAREA(XCEND-XCAREA),XCAREA                                      
         ZAP   TOTDLRS,PZERO       CLEAR SAVED STORAGE                          
         GOTOR TOTSAR,TSRINTQ      INITIALIZE                                   
         GOTOR TOTSAR,TSRSAVQ      SAVE BUFFER                                  
         L     R2,AORDH                                                         
         NI    4(R2),X'DF'                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* GET CLIENT RECORD                                                  *          
**********************************************************************          
GETCLI   STM   RE,R6,SVREG                                                      
         MVC   FVMAXL,BCCLILEN     MAXIMUM=CLIENT CODE LENGTH                   
         GOTOR AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,FVIFLD      CLIENT CODE                                  
*                                                                               
         GOTOR AGETACC,DMCB,KEY,CLIPROF                                         
         JNE   EXIT                                                             
*                                                                               
         MVC   CLIACCT,ACCODE      EXTRACT CLIENT KEY                           
         MVC   CLINAME,ACNAME                                                   
         MVC   CLIPRO,ACCODE                                                    
         MVC   CLIPRON,ACNAME                                                   
         MVC   CLICODE,FVIFLD      SAVE CLIENT CODE                             
         J     LMX                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GET PRODUCT RECORD                                                 *          
**********************************************************************          
GETPRO   STM   RE,R6,SVREG                                                      
         XR    RE,RE                                                            
         IC    RE,BCPROLEN         RE=L'CLIENT+L'PRODUCT                        
         XR    RF,RF                                                            
         IC    RF,BCCLILEN         RF=L'CLIENT                                  
         SR    RE,RF               RE=L'PRODUCT                                 
         STC   RE,FVMAXL                                                        
         GOTOR AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,CLIACCT+(ACTKACT-ACTRECD)                                
         XR    R3,R3                                                            
         IC    R3,BCCLILEN                                                      
         LA    R3,ACTKACT(R3)      POINT R3 AT PRODUCT CODE POSITION            
         XR    R1,R1                                                            
         IC    R1,FVXLEN           R1=EXECUTE LENGTH                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R3),FVIFLD      EXTRACT PRODUCT CODE                         
*                                                                               
         GOTOR AGETACC,DMCB,KEY,PRODPROF                                        
         JNE   EXIT                                                             
*                                                                               
         MVC   PROACCT,ACCODE      EXTRACT PRODUCT KEY/NAME                     
         MVC   PRONAME,ACNAME                                                   
         MVC   CLIPRO,ACCODE                                                    
         MVC   CLIPRON,ACNAME                                                   
         MVC   PRODCODE,FVIFLD                                                  
         J     LMX                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GET JOB RECORD                                                     *          
**********************************************************************          
GETJOB   STM   RE,R6,SVREG                                                      
         XR    RE,RE                                                            
         IC    RE,BCJOBLEN         RE=L'CLIENT+L'PRODUCT+L'JOB                  
         XR    RF,RF                                                            
         IC    RF,BCPROLEN         RF=L'CLIENT+L'PRODUCT                        
         SR    RE,RF               RE=L'JOB                                     
         STC   RE,FVMAXL                                                        
         MVI   FVMINL,1            YES-FORCE A MISSING INPUT FIELD              
         GOTOR AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,PROACCT+(ACTKACT-ACTRECD)                                
         XR    R3,R3                                                            
         IC    R3,BCPROLEN                                                      
         LA    R3,ACTKACT(R3)      POINT R3 AT JOB CODE POSITION                
         XR    R1,R1                                                            
         IC    R1,FVXLEN           R1=EXECUTE LENGTH                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R3),FVIFLD      EXTRACT JOB CODE                             
*                                                                               
         GOTOR AGETACC,DMCB,KEY,JOBPROF                                         
         JNE   EXIT                                                             
*                                                                               
         MVC   JOBACCT,ACCODE      EXTRACT JOB KEY/NAME                         
         MVC   JOBNAME,ACNAME                                                   
*                                                                               
         GOTO1 AMRGPRF                                                          
         LA    R6,PSCOMPPR                                                      
         USING PPRELD,R6                                                        
         MVC   CLICOST,PPRCOST     GET COST ACCOUTING POINTER (1C)              
         MVC   CLIOFFC,PPRGAOFF    SAVE PRODUCTION OFFICE                       
         MVC   JOBNUM,FVIFLD       SAVE JOB CODE                                
         J     LMX                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* GET OFFICE SHORTNAME                                               *          
**********************************************************************          
GETOFF   STM   RE,R6,SVREG                                                      
         MVC   OFFNAME,SPACES                                                   
         GOTOR AVALOFFC,BOPARM,(X'80',FVIFLD)                                   
         JNE   EXIT                                                             
         LA    R4,IOKEY                                                         
         USING OFFRECD,R4                                                       
         MVC   OFFKEY,BCSPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,ACOFFC                                                   
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'OFFKEY),IOKEYSAV                                         
         JNE   EMOFCNF            'OFFICE NOT FOUND...'                         
         L     R4,AIO1                                                          
         LA    R4,OFFRFST                                                       
         SR    R1,R1                                                            
*                                                                               
         USING SNMELD,R4                                                        
GETOFF3  CLI   SNMEL,0                                                          
         JE    LMX                                                              
         CLI   SNMEL,SNMELQ                                                     
         JNE   *+14                                                             
         MVC   OFFNAME,SNMNAME                                                  
         J     LMX                                                              
         IC    R1,SNMLN                                                         
         AR    R4,R1                                                            
         J     GETOFF3                                                          
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE LOCATION - BUILD TAX POSTING ENTRIES                      *          
**********************************************************************          
VALLOC   STM   RE,R6,SVREG                                                      
         ZAP   TAXAMNT,PZERO                                                    
         LA    R0,TAXIT            CLEAR TAX ENTRIES                            
         LA    R1,L'TAXIT                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R5,TAXIT                                                         
         USING TXD,R5                                                           
         MVI   FVMINL,2                                                         
         MVI   FVMAXL,8                                                         
         GOTOR AFVAL,(R2)                                                       
         JNE   EXIT                                                             
         SR    R6,R6                                                            
         IC    R6,5(R2)            SAVE LENGTH OF INPUT                         
*                                                                               
VALLOC3  LA    R4,IOKEY                                                         
         USING SUTRECD,R4                                                       
         MVC   SUTKEY,SPACES       READ SALES/USE TAX RECORD                    
         MVI   SUTKTYP,SUTKTYPQ                                                 
         MVI   SUTKSUB,SUTKSUBQ                                                 
         MVC   SUTKCPY,COMPANY                                                  
         LR    R1,R6               REMAINING LOCATION LENGTH                    
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   SUTKLOC(0),8(R2)                                                 
*                                                                               
         GOTOR AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'SUTKEY),IOKEYSAV                                         
         JNE   EMINACC             'INVALID ACCOUNT..                           
         L     R4,AIO1                                                          
         ZAP   TXRTE,PZERO                                                      
         ZAP   TXTAX,PZERO                                                      
         ZAP   TXBAS,PZERO                                                      
         MVC   WORK,SPACES                                                      
         LA    R3,SUTRFST                                                       
         SR    R1,R1                                                            
*                                                                               
         USING NAMELD,R3                                                        
VALLOC5  CLI   NAMEL,NAMELQ                                                     
         JNE   VALLOC7                                                          
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   WORK(0),NAMEREC                                                  
         J     VALLOC9                                                          
*                                                                               
         USING SUTELD,R3                                                        
VALLOC7  CLI   SUTEL,SUTELQ                                                     
         JNE   VALLOC9                                                          
         CLI   SUTLN,SUTLN2Q                                                    
         JL    VALLOC9                                                          
         CLC   SUTEFF,DOCDATE      TEST EFFECTIVE DATE                          
         JH    VALLOC9                                                          
         MVC   TXACC(1),COMPANY                                                 
         MVC   TXACC+1(L'SUTACC),SUTACC                                         
         MVC   TXLOC,SUTKLOC       SAVE LOCALITY CODE                           
         MVC   TXLOCN,WORK         LOCALITY NAME                                
         CLC   SUTEFF,DOCDATE      TEST EFFECTIVE DATE                          
         JH    VALLOC9                                                          
         MVC   TXEFF,SUTEFF                                                     
         ZAP   TXRTE,SUTRTE                                                     
         ZAP   PL13,INAMNT         BASIS AMOUNT                                 
         MP    PL13,TXRTE          X RATE                                       
         SRP   PL13,64-6,5                                                      
         ZAP   TXTAX,PL13          SAVE TAX                                     
         AP    TAXAMNT,TXTAX                                                    
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'TXACC),TXACC VALIDATE POSTING ACCOUNT                      
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
         MVC   TXACCN,ACNAME       ACCOUNT NAME                                 
         LR    RF,R2                                                            
         ICM   R2,15,ALOCNH                                                     
         JZ    VALLOC8                                                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'TXLOCN),TXLOCN                                             
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
VALLOC8  LR    R2,RF                                                            
         J     VALLOC11                                                         
*                                                                               
VALLOC9  IC    R1,SUTLN                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0                                                          
         JNE   VALLOC5                                                          
*                                                                               
         XC    BCWORK,BCWORK                                                    
         LA    R0,L'SUTKLOC                                                     
         STC   R0,BCWORK                                                        
         MVC   BCWORK+1(L'SUTKLOC),SUTKLOC                                      
         J     EMMTXRT             'MISSING TAX RATE FOR...                     
*                                                                               
VALLOC11 LA    R5,TXLNQ(R5)                                                     
         SHI   R6,2                REDUCE LENGTH OF LOCATION                    
         JP    VALLOC3             GET HIGHER LEVEL                             
         J     LMX                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* VALIDATE WORKCODE                                                  *          
**********************************************************************          
VALWC    STM   RE,R6,SVREG                                                      
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,(R2)                                                       
         JNE   EXIT                                                             
         CLC   8(2,R2),WC99                                                     
         JE    EMWC99              'W/C 99 NOT ALLOWED..                        
*                                                                               
         L     R4,AGOBLOCK                                                      
         USING GOBLOCKD,R4                                                      
*                                                                               
VALWC1   LA    RF,GOUWLIST                                                      
         LA    R0,GOUWLSTN                                                      
VALWC2   CLC   FVIFLD(2),0(RF)     CHK FOR MATCH ON NON-BILLABLE WC'S           
         JE    EMINWRK             'INVALID WORKCODE'                           
         LA    RF,2(RF)                                                         
         JCT   R0,VALWC2                                                        
*                                                                               
         GOTOR AGETWC,FVIFLD       READ ANALYSIS RECORD FOR VALID W/C           
         JNE   EXIT                                                             
         MVC   WRKNAME,SPACES                                                   
         MVC   WRKNAME(L'WCODESC),WORK                                          
         MVC   WRKCODE(2),FVIFLD                                                
         J     LMX                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE AMOUNT FIELD                                              *          
**********************************************************************          
VALAMT   STM   RE,R6,SVREG                                                      
         MVI   FVMINL,1            REQUIRE SOME INPUT                           
         GOTO1 AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         XR    R0,R0                                                            
         IC    R0,FVILEN           LENGTH OF AMOUNT INPUT                       
         GOTOR CASHVAL,DMCB,(X'82',FVIFLD),(R0)                                 
         CLI   0(R1),0                                                          
         JNE   EMINAMT             'INVALID AMOUNT'                             
         ZAP   INAMNT,4(8,R1)      SAVE CURRENT AMOUNT                          
*                                                                               
         MVC   FLD,SPACES          REDISPLAY AMOUNT                             
         CURED INAMNT,(12,FLD),2,DMCB=BCPARM,MINUS=YES,ALIGN=LEFT               
         OI    6(R2),X'80'                                                      
         TM    1(R2),X'20'         TEST PROTECTED                               
         JO    *+12                                                             
         OI    6(R2),X'01'                                                      
         STC   R0,5(R2)                                                         
         BRAS  RE,MOVEFLD                                                       
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* GET LENGTH OF AMOUNT FIELD                                         *          
*  RETURN R0=LENGTH                                                  *          
**********************************************************************          
GETAMTL  XR    R0,R0                                                            
         IC    R0,5(RF)            LENGTH OF AMOUNT INPUT                       
         TM    1(RF),X'20'         TEST PROTECTED                               
         BNOR  RE                  NO, LENGTH IS GOOD                           
*                                                                               
         IC    R0,0(RF)                                                         
         SHI   R0,8                                                             
         TM    1(R2),X'02'         TEST EXTENTED HEADER                         
         JNO   *+8                                                              
         SHI   R0,8                                                             
         LA    R1,7(RF)                                                         
         AR    R1,R0                R1 TO END OF DATA                           
         CLI   0(R1),C' '                                                       
         BHR   RE                                                               
         BCTR  R1,0                                                             
         JCT   R0,*-8                                                           
                                                                                
**********************************************************************          
* BUILD AN DUMMY AMOUNT HEADER AND FIELD IN WORK                     *          
*  RETURN R0=LENGTH                                                  *          
**********************************************************************          
BLDAMT   XC    WORK,WORK                                                        
         ICM   RF,15,AAMTH         MUST HAVE AMOUNT                             
         LA    R1,RECAMT                                                        
         CLI   RECDAT,RECDTX       TEST TAX DATA                                
         JNE   *+12                                                             
         ICM   RF,15,ATAXH         USE TAX - IF TAX RECORD                      
         LA    R1,RECTAX                                                        
*                                                                               
         MVC   WORK(8),0(RF)       DUMMY HEADER FOR AMOUNT                      
         MVC   WORK+8(L'RECAMT),0(R1) CHARACTER AMOUNT FIELD                    
         LA    RF,WORK                                                          
         LA    R1,8(RF)                                                         
         LA    R0,L'RECAMT         GET LENGTH OF DATA                           
         AHI   R1,L'RECAMT-1                                                    
         CLI   0(R1),C' '                                                       
         JH    *+10                                                             
         BCTR  R1,0                                                             
         JCT   R0,*-10                                                          
         STC   R0,5(RF)                                                         
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* LOAD NEW SCREEN                                                    *          
*  R1=A(SCREEN NUMBER)                                               *          
**********************************************************************          
LOADSCRN ST    RE,SAVERE                                                        
         XR    R0,R0                                                            
         IC    R0,0(R1)            SCREEN NUMBER                                
         GOTO1 AOVRSCR,BOPARM,((R0),BASOLY2H)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,SETSCR           SET SCREEN PARAMETERS                        
         BRAS  RE,SETUPR           SET NEW UPPER SCREEN FIELD ADDRESSES         
         MVI   NITM,0                                                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
**********************************************************************          
* SET SCREEN DATA                                                    *          
**********************************************************************          
SETSCR   STM   RE,R6,SVREG                                                      
         L     R1,AUSATAB          ASSUME US                                    
         CLI   AGYCTRY,CTRYCAN     UNLESS CANADA                                
         JNE   *+8                                                              
         L     R1,ACANTAB                                                       
*                                                                               
SETSCR3  MVC   SCRD(SCRLNQ),0(R1)  MOVE SCREEN PARAMETERS                       
         CLC   SCRNUM,TWASCRN      TEST CORRECT SCREEN                          
         JE    SETSCR9                                                          
         LA    R1,SCRLNQ(R1)                                                    
         CLI   0(R1),X'FF'                                                      
         JNE   SETSCR3                                                          
         DC    H'0'                BAD SCREEN                                   
*                                                                               
SETSCR9  MVC   CSSPROG,SCRSPRG    SET SPROG                                     
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* SET SCREEN ADCONS                                                  *          
**********************************************************************          
SETUPR   STM   RE,R6,SVREG                                                      
         LA    R1,NUPPER           NUMBER OF UPPER FIELDS                       
         SLL   R1,2                                                             
         LA    RF,UPPER                                                         
         BASR  RE,0                CLEAR UPPER ADDRESSES                        
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         XC    0(0,RF),0(RF)                                                    
         LA    R2,TWAD                                                          
         XR    R1,R1                                                            
         ICM   R1,3,SCRUPR                                                      
         A     R1,BASERB                                                        
         J     SETADDR                                                          
*                                                                               
SETLOW   STM   RE,R6,SVREG         AT ENTRY R2 POINTS TO CLIENT HEADER          
         LA    R1,NLOWER           NUMBER OF LOWER FIELDS                       
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         LA    RF,LOWER                                                         
         BASR  RE,0                CLEAR LOWER ADDRESSES                        
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         XC    0(0,RF),0(RF)                                                    
         XR    R1,R1                                                            
         ICM   R1,3,SCRLOW                                                      
         A     R1,BASERB                                                        
*                                                                               
SETADDR  CLI   0(R1),X'FF'         TEST FOR EOT                                 
         JE    LMX                                                              
         XR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         AR    RE,R2               GET ADDRESS OF SCREEN FIELD                  
         XR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,PROGD(RF)        GET ADDRESS OF ADCON                         
         ST    RE,0(RF)            SAVE ADDRESS OF FIELD HEADER                 
         LA    R1,L'MULTUPR(R1)                                                 
         J     SETADDR                                                          
         EJECT                                                                  
**********************************************************************          
* BUILD A TSAR RECORD FROM SCREEN DATA                               *          
**********************************************************************          
BLDREC   STM   RE,R6,SVREG                                                      
         L     R3,ARCDTAB                                                       
*                                                                               
BLDREC3  XR    RF,RF                                                            
         ICM   RF,3,2(R3)          DISPLACEMENT TO A(FIELD HEADER)              
         LA    RF,PROGD(RF)                                                     
         ICM   R2,15,0(RF)         R2=A(FIELD HEADER)                           
         JZ    BLDREC5                                                          
         XR    RF,RF                                                            
         ICM   RF,3,0(R3)          DISPLACEMENT TO RECORD AREA                  
         LA    RF,PROGD(RF)        RF=A(OUTPUT AREA)                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
*                                                                               
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,RF),8(R2)       MOVE FIELD TO RECORD AREA                    
*                                                                               
BLDREC5  LA    R3,L'RCDTAB(R3)     NEXT RCDTAB ENTRY                            
         CLI   0(R3),X'FF'                                                      
         JNE   BLDREC3                                                          
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* MOVE DATA FROM TSAR RECORD TO SCREEN                               *          
**********************************************************************          
DSPREC   STM   RE,R6,SVREG                                                      
         L     R3,ARCDTAB                                                       
*                                                                               
DSPREC3  XR    RF,RF                                                            
         ICM   RF,3,2(R3)          DISPLACEMENT TO A(FIELD HEADER)              
         LA    RF,PROGD(RF)                                                     
         ICM   R2,15,0(RF)         R2=A(FIELD HEADER)                           
         JZ    DSPREC5                                                          
         XR    RF,RF                                                            
         ICM   RF,3,0(R3)          DISPLACEMENT TO RECORD AREA                  
         LA    RF,PROGD(RF)        RF=A(OUTPUT AREA)                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
*                                                                               
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   8(0,R2),0(RF)       MOVE DATA TO SCREEN                          
*                                                                               
         LA    RE,8(R1,R2)                                                      
         LA    R1,1(R1)            SET INPUT LENGTH                             
         CLI   0(RE),C' '                                                       
         JH    *+10                                                             
         BCTR  RE,0                                                             
         JCT   R1,*-10                                                          
         TM    1(R2),X'20'                                                      
         JO    *+8                                                              
         STC   R1,5(R2)                                                         
*                                                                               
         NI    4(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
*                                                                               
DSPREC5  LA    R3,L'RCDTAB(R3)     NEXT RCDTAB ENTRY                            
         CLI   0(R3),X'FF'                                                      
         JNE   DSPREC3                                                          
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* SAVE UPPER PORTION OF SCREEN                                       *          
**********************************************************************          
SAVTOP   STM   RE,R6,SVREG                                                      
         L     R3,AUPRTAB                                                       
SAVTOP3  XR    R2,R2                                                            
         ICM   R2,3,0(R3)          DISP. TO A(OF FIELD HEADER)                  
         LA    R2,PROGD(R2)                                                     
         ICM   R2,15,0(R2)         R2=A(FIELD HEADER)                           
         XR    RF,RF                                                            
         ICM   RF,3,2(R3)          DISP. TO A(LENGTH & DATA FIELDS)             
         LA    RF,PROGD(RF)        RF=A(SAVED STORAGE AREA)                     
         XR    R1,R1                                                            
         IC    R1,0(R2)            LENGTH + HEADER                              
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   1(0,RF),8(R2)       SAVE SCREEN INPUT                            
         MVC   0(1,RF),5(R2)       SAVE LENGTH                                  
         TM    4(R2),X'20'         TEST VALIDATED BIT                           
         JNO   *+8                                                              
         OI    0(RF),X'80'         SET FLAG FOR REBUILD                         
         LA    R3,L'UPRTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         JNE   SAVTOP3                                                          
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* RESTORE TOP OF SCREEN                                              *          
**********************************************************************          
RESTOP   STM   RE,R6,SVREG                                                      
         L     R3,AUPRTAB                                                       
RESTOP3  XR    R2,R2                                                            
         ICM   R2,3,0(R3)          DISP. TO A(OF FIELD HEADER)                  
         LA    R2,PROGD(R2)                                                     
         ICM   R2,15,0(R2)         R2=A(FIELD HEADER)                           
         XR    RF,RF                                                            
         ICM   RF,3,2(R3)          DISP. TO A(LENGTH & DATA FIELDS)             
         LA    RF,PROGD(RF)        RF=A(SAVED STORAGE AREA)                     
         TM    0(RF),X'80'         TEST VALIDATED                               
         JNO   *+8                                                              
         OI    4(R2),X'20'         SET IN FIELD HEADER                          
         NI    0(RF),X'7F'                                                      
         XR    R1,R1                                                            
         IC    R1,0(R2)            LENGTH + HEADER                              
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   8(0,R2),1(RF)       SET SCREEN INPUT                             
         MVC   5(1,R2),0(RF)       AND LENGTH                                   
         NI    4(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
         LA    R3,L'UPRTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         JNE   RESTOP3                                                          
*                                                                               
LMX      LM    RE,R6,SVREG                                                      
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* INFO AND ERROR ROUTINES                                            *          
**********************************************************************          
*                                  ** INFO MESSAGES **                          
*                                                                               
IMITEMS  XC    BCWORK,BCWORK                                                    
         LA    R3,BCWORK                                                        
         CURED (B2,NFST),(5,1(R3)),0,DMCB=BCPARM,ALIGN=LEFT                     
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         LHI   R0,AI$ITEMD         'ITEM X OF XX DISPLAYED.....'                
         STCM  R0,3,BCHALF                                                      
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         JO    IMITEMS3                                                         
         LHI   R0,AI$ITMDS         'ITEM(S) X THRU XX DISPLAYED.....'           
         STCM  R0,3,BCHALF                                                      
*                                                                               
         CURED (B2,NLST),(5,1(R3)),0,DMCB=BCPARM,ALIGN=LEFT                     
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
IMITEMS3 DS    0H                                                               
         CURED (B2,NBUF),(5,1(R3)),0,DMCB=BCPARM,ALIGN=LEFT                     
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         XC    0(2,R3),0(R3)                                                    
         L     R2,ADETH            CURSOR TO FIRST INPUT                        
         CLI   PFKEY,PFINVQ        PF'D FROM TAX                                
         JNE   *+8                                                              
         L     R2,ADOCH            YES, CURSOR TO DOC # FIELD                   
         STCM  R2,15,BOCURSOR                                                   
         J     IMVX                                                             
*                                                                               
IMPOSTD  LHI   R0,AI$ITMPD         'N ITEMS POSTED $99'                         
         STCM  R0,3,BCHALF                                                      
         XC    BCWORK,BCWORK                                                    
         LA    R3,BCWORK                                                        
         CURED (B1,NPOST),(3,1(R3)),0,DMCB=BCPARM,ALIGN=LEFT                    
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         MVI   1(R3),C'$'                                                       
         CURED POSTAMT,(12,2(R3)),2,DMCB=BCPARM,MINUS=YES,ALIGN=LEFT            
         AHI   R0,2                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         XC    0(2,R3),0(R3)                                                    
         L     R2,AORDH            CURSOR TO ORDER FIELD                        
         STCM  R2,15,BOCURSOR                                                   
         J     IMVX                                                             
*                                                                               
IMVX     MVI   BCBYTE1,GTMINF                                                   
         J     VMX                                                              
*                                                                               
IMEREQF  LHI   R0,AI$EREQF         'ENTER REQUIRED FIELDS..'                    
         J     IMX                                                              
*                                                                               
IMODDEI  LHI   R0,AI$ODDEI         'ORDER DETAILS DISPLAYED...'                 
         J     IMX                                                              
*                                                                               
IMIOKNX  LHI   R0,AI$IOKNX         'INPUT COMPLETE - ENTER NEXT'                
         J     IMX                                                              
*                                                                               
IMITDEL  LHI   R0,AI$ITDEL         'ITEM DELETED...'                            
         J     IMX                                                              
*                                                                               
IMX      MVI   FVOMTYP,GTMINF                                                   
         J     EMX                                                              
         EJECT                                                                  
*                                  ** ERROR MESAGES **                          
EMMXCAP  LHI   R0,AE$MXCAP         'MAXIMUM CAPACITY IS %1 ITEMS'               
         STCM  R0,3,BCHALF                                                      
         XC    BCWORK,BCWORK                                                    
         LA    R3,BCWORK                                                        
         LA    R0,MXCAP                                                         
         CURED (R0),(3,1(R3)),0,DMCB=BCPARM,ALIGN=LEFT                          
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         XC    0(2,R3),0(R3)                                                    
         J     EMVX                                                             
*                                                                               
EMMTXRT  LHI   R0,AE$MTXRT         'MISSING TAX RATE FOR ....                   
         J     EMVX                                                             
*                                                                               
EMWC99   LHI   R0,AE$WC99          'WC 99 NOT VALID'                            
         J     EMX                                                              
*                                                                               
EMMISIF  LHI   R0,AE$MISIF         'MISSING INPUT FIELD'                        
         J     EMX                                                              
*                                                                               
EMMIVPFK LHI   R0,AE$IVPFK         'INVALID PFKEY'                              
         J     EMX                                                              
*                                                                               
EMDEBIT  LHI   R0,AE$DEBIT         'INPUT DEBIT INFORMATION'                    
         J     EMX                                                              
*                                                                               
EMVMEXP  LHI   R0,AE$VMEXP         'VENDOR INPUT MISSING FOR THIS...'           
         J     EMX                                                              
*                                                                               
EMAEEWC  LHI   R0,AE$AEEWC         'AMOUNT EXCEEDS ESTIMATE...'                 
         J     EMX                                                              
*                                                                               
EMNAWTT  LHI   R0,AE$NAWTT         'NOT ALLOWED WITH THIS TYPE'                 
         J     EMX                                                              
*                                                                               
EMIANAL  LHI   R0,AE$IANAL         'INVALID ANALYSIS ACCOUNT'                   
         J     EMX                                                              
*                                                                               
EMANFST  LHI   R0,AE$ANFST         'ACCOUNT NOT FLAGGED FOR STAFF'              
         J     EMX                                                              
*                                                                               
EMINACP  LHI   R0,AE$INACP         'INVALID ACCOUNT FOR POSTING'                
         J     EMX                                                              
*                                                                               
EMACTLK  LHI   R0,AE$ACTLK         'ACCOUNT IS LOCKED'                          
         J     EMX                                                              
*                                                                               
EMSECLK  LHI   R0,AE$SECLK         'SECURITY LOCKOUT'                           
         J     EMX                                                              
*                                                                               
EMACTCL  LHI   R0,AE$ACTCL         'ACCOUNT IS CLOSED'                          
         J     EMX                                                              
*                                                                               
EMINVIF  LHI   R0,AE$INVIF         'INVALID INPUT FIELD'                        
         J     EMX                                                              
*                                                                               
EMANFDP  LHI   R0,AE$ANFDP         'ACCOUNT NOT FLAGGED FOR DEPT'               
         J     EMX                                                              
*                                                                               
EMMMR    LHI   R0,AE$MMR           'MISSING MEDIA RECORD'                       
         J     EMX                                                              
*                                                                               
EMMSJL   LHI   R0,AE$MSJL          'MISSING SJ LEDGER RECORD'                   
         J     EMX                                                              
*                                                                               
EMMEL    LHI   R0,AE$MEL           'MISSING EXPENSE LEDGER RECORD'              
         J     EMX                                                              
*                                                                               
EMINAMT  LHI   R0,AE$INAMT         'INVALID AMOUNT'                             
         J     EMX                                                              
*                                                                               
EMINDAT  LHI   R0,AE$INDAT         'INVALID DATE'                               
         J     EMX                                                              
*                                                                               
EMINWRK  LHI   R0,AE$INWRK         'INVALID WORKCODE'                           
         J     EMX                                                              
*                                                                               
EMAITL   LHI   R0,AE$AITL          'ASTERISK INPUT IVALID ON TOP...'            
         J     EMX                                                              
*                                                                               
EMNODUP  LHI   R0,AE$NODUP         'INPUT IS NOT AVAILABLE TO REPEAT'           
         J     EMX                                                              
*                                                                               
EMPEVIR  LHI   R0,AE$PEVIR         'PROD OR EXPENSE VENDOR REQUIRED'            
         J     EMX                                                              
*                                                                               
EMCDALL  LHI   R0,AE$CDALL         'NO CASH DISC IF CASH ACCOUNT ...'           
         J     EMX                                                              
*                                                                               
EMINACC  LHI   R0,AE$INACC         'INVALID ACCOUNT'                            
         J     EMX                                                              
*                                                                               
EMDOPSP  LHI   R0,AE$DOPSP         'DATE OUTSIDE PERMITTED SPAN'                
         J     EMX                                                              
*                                                                               
EMORDNF  LHI   R0,AE$ORDNF         'ORDER NOT FOUND'                            
         J     EMX                                                              
*                                                                               
EMOFMCH  LHI   R0,AE$OFMCH         'ORDER IS FULLY MATCH...'                    
         J     EMX                                                              
*                                                                               
EMCCORD  LHI   R0,AE$CCORD         'CAN'T CHANGE ORDER NUMBER - POST..          
         J     EMX                                                              
*                                                                               
EMUPFKD  LHI   R0,AE$UPFKD         'USE PFKEY TO DELETE LINE...                 
         J     EMX                                                              
*                                                                               
EMDUPIT  LHI   R0,AE$DUPIT         'DUPLICATE ITEM ON FILE                      
         J     EMX                                                              
*                                                                               
EMNOTXE  LHI   R0,AE$NOTXE         'TAX NOT VALID ON EXPENSE JOB                
         J     EMX                                                              
*                                                                               
EMOFCNF  LHI   R0,AE$OFCNF         'OFFICE DOES NOT EXIST..                     
         J     EMX                                                              
*                                                                               
EMVX     MVI   BCBYTE1,GTMERR                                                   
         J     VMX                                                              
*                                                                               
EMX      STCM  R0,3,FVMSGNO                                                     
         J     EXIT                                                             
*                                                                               
VMX      LA    R1,BCPARM           MESSAGES WITH '&1'                           
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,BCHALF      MESSAGE NUMBER                               
         MVC   GTMSYS,ASSYSO                                                    
         MVC   GTMTYP,BCBYTE1      'I' OR 'E'                                   
         LA    R0,BCWORK           SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         GOTO1 VGETTXT,(R1)        RESOLVE MESSAGE                              
         LA    R0,FVFSET           SET MESSAGE ALREADY SUPPLIED                 
         STCM  R0,3,FVMSGNO                                                     
         J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* EXIT ROUTINES                                                      *          
**********************************************************************          
EXIT     ST    R2,FVADDR                                                        
         MVI   VPFSW,0             SET VALID PF KEY SWITCH                      
         MVI   VPFSW1,0                                                         
         OI    VPFSW1,VPFTAX       SET TO ALLOW TAX PF                          
         LA    R1,VPFMUL           SET FOR MULTIPLE                             
         TM    SCRSTA,SCRSINQ      TEST SINGLE(DETAIL) SCREEN                   
         JO    *+8                 YES, OK                                      
         LA    R1,VPFDTL           SET FOR SINGLE                               
         TM    SCRSTA,SCRTAXQ      TEST TAX SCREEN                              
         JNO   EXIT3               NO                                           
         NI    VPFSW1,X'FF'-VPFTAX YES, DON'T ALLOW TAX                         
         OI    VPFSW1,VPFINV       BUT ALLOW INVOICE                            
         XR    R1,R1               DON'T ALLOW SWAP                             
*                                                                               
EXIT3    STC   R1,VPFSW                                                         
         OC    NBUF,NBUF           TEST ANY IN BUFFER                           
         JZ    EXIT5               NO,                                          
         OI    VPFSW,VPFPOST+VPFCLR ALLOW POST/CLEAR                            
         LA    R3,1                                                             
         CLM   R3,3,NFST           TEST FIRST ON SCREEN                         
         JE    *+8                 YES, DON'T ALLOW 'UP'                        
         OI    VPFSW,VPFUP         ALLOW UP                                     
         CLI   NITM,0              TEST ANY ON SCREEN                           
         JE    *+8                 NO, DON'T ALLOW DELETE                       
         OI    VPFSW,VPFDEL+VPFDWN ALLOW DELETE/DOWN                            
*                                                                               
EXIT5    MVC   BOELEM(L'SPACES),SPACES   BUILD OUTPUT STRING                    
         L     R3,APFKEYS                                                       
         USING PFKTD,R3                                                         
         MVC   BOELEM(2),PFKTNAM                                                
         LA    R3,PFKTLNQ(R3)                                                   
         LA    R2,BOELEM+2                                                      
*                                                                               
EXIT7    XR    R1,R1                                                            
         IC    R1,PFKTVBI          SET VALIDATION BIT                           
         LA    RF,VPFSW            SET VALIDATION BYTE                          
         CLI   PFKTVBY,PFKTVBY0                                                 
         JE    *+8                                                              
         LA    RF,VPFSW1                                                        
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+8                                                              
         TM    0(RF),0             TEST PF KEY VALID                            
         JNO   *+10                                                             
         MVC   0(L'PFKTNAM,R2),PFKTNAM                                          
         CLC   0(L'PFKTNAM,R2),SPACES                                           
         JE    *+8                                                              
         LA    R2,L'PFKTNAM+1(R2)                                               
         LA    R3,PFKTLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         JNE   EXIT7                                                            
         CLI   BOELEM+2,C' '                                                    
         JNH   EXIT9                                                            
         LA    R0,L'SPACES                                                      
         GOTO1 VSQUASH,DMCB,BOELEM,(R0)                                         
         MVC   FLD,BOELEM                                                       
         L     R2,ATOTH                                                         
         BRAS  RE,MOVEFLD          MOVE PFKEYS TO OUTPUT LINE                   
         DROP  R3                                                               
*                                                                               
EXIT9    OC    LASTORD,LASTORD     TEST ANY ORDER                               
         JZ    EXIT11                                                           
         L     R2,AORDAH           AMOUNT OF ORDER                              
         MVC   FLD,SPACES                                                       
         CURED ORDRAMT,(12,FLD),2,DMCB=BCPARM,MINUS=YES,ALIGN=LEFT              
         STC   R0,5(R2)            SET LENGTH                                   
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,AORDNH           NUMBER INVOICED                              
         MVC   FLD,SPACES                                                       
         CURED INVDNUM,(3,FLD),0,DMCB=BCPARM,ALIGN=LEFT                         
         STC   R0,5(R2)            SET LENGTH                                   
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
EXIT11   MVC   CSSPROG,SCRSPRG                                                  
         OC    NBUF,NBUF           TEST ANYTHING IN BUFFER                      
         JZ    EXIT15              NO                                           
*                                                                               
         MVC   BCWORK,SPACES                                                    
         CURED (B2,NBUF),(3,BCWORK),0,DMCB=BCPARM                               
         LHI   R1,ITEMS-T61B0A                                                  
         A     R1,BASERB                                                        
         MVC   BCWORK+5(5),0(R1)                                                
         MVI   BCWORK+11,C'$'                                                   
         CURED TOTDLRS,(12,BCWORK+12),2,DMCB=BCPARM,                   X        
               MINUS=YES,ALIGN=LEFT                                             
         GOTO1 VSQUASH,DMCB,BCWORK,30                                           
         ICM   R1,15,DMCB+4                                                     
         L     R2,ATOTH                                                         
         LA    R2,L'MISTOT+L'MISTOTH(R2)                                        
         SR    R2,R1                                                            
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R2),BCWORK                                                   
*                                                                               
EXIT15   GOTOR TOTSAR,TSRSAVQ      SAVE BUFFER                                  
         LA    RF,TWAD             SAVE SOME DATA IN TWA                        
         AHI   RF,OSSAVE-TWAD                                                   
         MVC   0(STSLNQ,RF),STS                                                 
*                                                                               
         L     RD,BCSVRD           RETURN TO BASE                               
         L     RD,8(RD)                                                         
*                                                                               
XYES     CR    RB,RB                                                            
         J     XIT                                                              
XNO      LTR   RB,RB                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SCREEN CONTROL - SEE SCRD                                          *          
**********************************************************************          
         DS    0F                                                               
USATAB   DS    0X                   ** US TABLES **                             
         DC    AL1(MISCRNQ,MXMULQ,LINLNQ,0)           MULTIPLE                  
         DC    AL1(SISCRNQ,TISCRNQ,1)                                           
         DC    AL1(0,0)                                                         
         DC    AL2(PROC-T61B0A)                                                 
         DC    AL2(MULTUPR-T61B0A,MULTLOW-T61B0A)                               
*                                                                               
         DC    AL1(SISCRNQ,MXSINQ,0,SCRSINQ)          DETAIL                    
         DC    AL1(MISCRNQ,TISCRNQ,1)                                           
         DC    AL1(0,0)                                                         
         DC    AL2(PROC-T61B0A)                                                 
         DC    AL2(SINGUPR-T61B0A,SINGLOW-T61B0A)                               
*                                                                               
         DC    AL1(TISCRNQ,MXUSEQ,USELNQ,SCRTAXQ)      TAX                      
         DC    AL1(SISCRNQ,TISCRNQ,2)                                           
         DC    AL1(0,0)                                                         
         DC    AL2(USET-T61B0A)                                                 
         DC    AL2(USEUPR-T61B0A,USELOW-T61B0A)                                 
         DC    X'FF'                                                            
*                                                                               
*                                   ** CANADIAN TABLES **                       
CANTAB   DC    AL1(CMISCRNQ,MXCMULQ,CLINLNQ,SCRCANQ)  MULTIPLE                  
         DC    AL1(CSISCRNQ,CTISCRNQ,1)                                         
         DC    AL1(0,0)                                                         
         DC    AL2(PROC-T61B0A)                                                 
         DC    AL2(CMULTUPR-T61B0A,CMULTLOW-T61B0A)                             
*                                                                               
         DC    AL1(CSISCRNQ,MXCSINQ,0,SCRSINQ+SCRCANQ) DETAIL                   
         DC    AL1(CMISCRNQ,CTISCRNQ,1)                                         
         DC    AL1(0,0)                                                         
         DC    AL2(PROC-T61B0A)                                                 
         DC    AL2(CSINGUPR-T61B0A,CSINGLOW-T61B0A)                             
*                                                                               
         DC    AL1(CTISCRNQ,MXUSEQ,0,SCRTAXQ)          TAX                      
         DC    AL1(CSISCRNQ,CTISCRNQ,2)                                         
         DC    AL1(0,0)                                                         
         DC    AL2(USET-T61B0A)                                                 
         DC    AL2(USEUPR-T61B0A,USELOW-T61B0A)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* SCREEN TABLES                                                      *          
**********************************************************************          
*  MULTIPLE INPUT - UPPER                                                       
MULTUPR  DS    0F                                                               
         DC    AL2(MISORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(MISORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(MISORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(MISDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(MISDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(MISDUEH-TWAD,ADUEH-PROGD)                                    
         DC    AL2(MISURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(MISCSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(MISCSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(MISVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(MISVENNH-TWAD,AVENNH-PROGD)                                  
         DC    AL2(MISVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(MISVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(MISXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(MISXVNNH-TWAD,AXVNNH-PROGD)                                  
         DC    AL2(MISXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(MISXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(MISOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(MISNARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(MISNAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(MISCLIH-TWAD,ADETH-PROGD)                                    
         DC    AL2(MISTOTH-TWAD,ATOTH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
*  MULTIPLE INPUT - LOWER                                                       
MULTLOW  DS    0F                                                               
         DC    AL2(MISCLIH-MISCLIH,ACLIH-PROGD)                                 
         DC    AL2(MISPROH-MISCLIH,APROH-PROGD)                                 
         DC    AL2(MISJOBH-MISCLIH,AJOBH-PROGD)                                 
         DC    AL2(MISWKCH-MISCLIH,AWKCH-PROGD)                                 
         DC    AL2(MISNONH-MISCLIH,ANONH-PROGD)                                 
         DC    AL2(MISAMTH-MISCLIH,AAMTH-PROGD)                                 
         DC    AL2(MISEXAH-MISCLIH,AXACH-PROGD)                                 
         DC    AL2(MISDOFH-MISCLIH,ADOFH-PROGD)                                 
         DC    AL2(MISCOFH-MISCLIH,ACOFH-PROGD)                                 
         DC    AL2(MISAOFH-MISCLIH,AAOFH-PROGD)                                 
         DC    AL2(MISDPTH-MISCLIH,ADPTH-PROGD)                                 
         DC    AL2(MISPERH-MISCLIH,APERH-PROGD)                                 
         DC    AL2(MISLIN2H-MISCLIH,ANEXT-PROGD)                                
         DC    X'FF'                                                            
                                                                                
*                                                                               
*  SINGLE INPUT - UPPER                                                         
SINGUPR  DS    0F                                                               
         DC    AL2(SISORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(SISORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(SISORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(SISDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(SISDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(SISDUEH-TWAD,ADUEH-PROGD)                                    
         DC    AL2(SISURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(SISCSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(SISCSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(SISVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(SISVENNH-TWAD,AVENNH-PROGD)                                  
         DC    AL2(SISVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(SISVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(SISXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(SISXVNNH-TWAD,AXVNNH-PROGD)                                  
         DC    AL2(SISXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(SISXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(SISOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(SISNARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(SISNAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(SISCLIH-TWAD,ADETH-PROGD)                                    
         DC    AL2(SISTOTH-TWAD,ATOTH-PROGD)                                    
         DC    X'FF'                                                            
                                                                                
*  SINGLE INPUT - LOWER                                                         
SINGLOW  DS    0F                                                               
         DC    AL2(SISCLIH-SISCLIH,ACLIH-PROGD)                                 
         DC    AL2(SISCLNH-SISCLIH,ACLINH-PROGD)                                
         DC    AL2(SISPROH-SISCLIH,APROH-PROGD)                                 
         DC    AL2(SISPRNH-SISCLIH,APRONH-PROGD)                                
         DC    AL2(SISJOBH-SISCLIH,AJOBH-PROGD)                                 
         DC    AL2(SISJONH-SISCLIH,AJOBNH-PROGD)                                
         DC    AL2(SISWKCH-SISCLIH,AWKCH-PROGD)                                 
         DC    AL2(SISWKNH-SISCLIH,AWKCNH-PROGD)                                
         DC    AL2(SISNONH-SISCLIH,ANONH-PROGD)                                 
         DC    AL2(SISAMTH-SISCLIH,AAMTH-PROGD)                                 
         DC    AL2(SISEXAH-SISCLIH,AXACH-PROGD)                                 
         DC    AL2(SISEXNH-SISCLIH,AXACNH-PROGD)                                
         DC    AL2(SISDOFH-SISCLIH,ADOFH-PROGD)                                 
         DC    AL2(SISDONH-SISCLIH,ADOFNH-PROGD)                                
         DC    AL2(SISCOFH-SISCLIH,ACOFH-PROGD)                                 
         DC    AL2(SISCONH-SISCLIH,ACOFNH-PROGD)                                
         DC    AL2(SISAOFH-SISCLIH,AAOFH-PROGD)                                 
         DC    AL2(SISAONH-SISCLIH,AAOFNH-PROGD)                                
         DC    AL2(SISDPTH-SISCLIH,ADPTH-PROGD)                                 
         DC    AL2(SISDPTNH-SISCLIH,ADPTNH-PROGD)                               
         DC    AL2(SISPERH-SISCLIH,APERH-PROGD)                                 
         DC    AL2(SISPERNH-SISCLIH,APERNH-PROGD)                               
         DC    X'FF'                                                            
                                                                                
*  USE TAX - UPPER                                                              
USEUPR   DS    0F                                                               
         DC    AL2(USEORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(USEORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(USEORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(USEDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(USEDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(USEDUEH-TWAD,ADUEH-PROGD)                                    
         DC    AL2(USEURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(USECSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(USECSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(USEVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(USEVENNH-TWAD,AVENNH-PROGD)                                  
         DC    AL2(USEVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(USEVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(USEXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(USEXVNNH-TWAD,AXVNNH-PROGD)                                  
         DC    AL2(USEXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(USEXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(USEOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(USENARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(USENAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(USECLIH-TWAD,ADETH-PROGD)                                    
         DC    AL2(USETOTH-TWAD,ATOTH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
*  USE TAX - LOWER                                                              
USELOW   DS    0F                                                               
         DC    AL2(USECLIH-USECLIH,ACLIH-PROGD)                                 
         DC    AL2(USEPROH-USECLIH,APROH-PROGD)                                 
         DC    AL2(USEJOBH-USECLIH,AJOBH-PROGD)                                 
         DC    AL2(USEWKCH-USECLIH,AWKCH-PROGD)                                 
         DC    AL2(USEBASH-USECLIH,ABASH-PROGD)                                 
         DC    AL2(USELOCH-USECLIH,ALOCH-PROGD)                                 
         DC    AL2(USETAXH-USECLIH,ATAXH-PROGD)                                 
         DC    AL2(USELOCNH-USECLIH,ALOCNH-PROGD)                               
         DC    AL2(USELIN2H-USECLIH,ANEXT-PROGD)                                
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* SCREEN TABLES - CANADIAN                                           *          
**********************************************************************          
*  MULTIPLE INPUT - UPPER                                                       
CMULTUPR DS    0F                                                               
         DC    AL2(CMIORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(CMIORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(CMIORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(CMIDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(CMIDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(CMIDUEH-TWAD,ADUEH-PROGD)                                    
         DC    AL2(CMIURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(CMICSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(CMICSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(CMIVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(CMIVENNH-TWAD,AVENNH-PROGD)                                  
         DC    AL2(CMIVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(CMIVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(CMIXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(CMIXVNNH-TWAD,AXVNNH-PROGD)                                  
         DC    AL2(CMIXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(CMIXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(CMINARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(CMINAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(CMICLIH-TWAD,ADETH-PROGD)                                    
         DC    AL2(CMITOTH-TWAD,ATOTH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
*  MULTIPLE INPUT - LOWER                                                       
CMULTLOW DS    0F                                                               
         DC    AL2(CMICLIH-MISCLIH,ACLIH-PROGD)                                 
         DC    AL2(CMIPROH-MISCLIH,APROH-PROGD)                                 
         DC    AL2(CMIJOBH-MISCLIH,AJOBH-PROGD)                                 
         DC    AL2(CMIWKCH-MISCLIH,AWKCH-PROGD)                                 
         DC    AL2(CMINONH-MISCLIH,ANONH-PROGD)                                 
         DC    AL2(CMIAMTH-MISCLIH,AAMTH-PROGD)                                 
         DC    AL2(CMIEXAH-MISCLIH,AXACH-PROGD)                                 
         DC    AL2(CMIDOFH-MISCLIH,ADOFH-PROGD)                                 
         DC    AL2(CMICOFH-MISCLIH,ACOFH-PROGD)                                 
         DC    AL2(CMIAOFH-MISCLIH,AAOFH-PROGD)                                 
         DC    AL2(CMIDPTH-MISCLIH,ADPTH-PROGD)                                 
         DC    AL2(CMIPERH-MISCLIH,APERH-PROGD)                                 
         DC    AL2(CMILIN2H-MISCLIH,ANEXT-PROGD)                                
         DC    X'FF'                                                            
                                                                                
*                                                                               
*  SINGLE INPUT - UPPER                                                         
CSINGUPR DS    0F                                                               
         DC    AL2(CSIORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(CSIORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(CSIORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(CSIDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(CSIDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(CSIDUEH-TWAD,ADUEH-PROGD)                                    
         DC    AL2(CSIURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(CSICSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(CSICSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(CSIVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(CSIVENNH-TWAD,AVENNH-PROGD)                                  
         DC    AL2(CSIVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(CSIVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(CSIXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(CSIXVNNH-TWAD,AXVNNH-PROGD)                                  
         DC    AL2(CSIXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(CSIXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(CSINARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(CSINAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(CSICLIH-TWAD,ADETH-PROGD)                                    
         DC    AL2(CSITOTH-TWAD,ATOTH-PROGD)                                    
         DC    X'FF'                                                            
                                                                                
*  SINGLE INPUT - LOWER                                                         
CSINGLOW DS    0F                                                               
         DC    AL2(CSICLIH-SISCLIH,ACLIH-PROGD)                                 
         DC    AL2(CSIPROH-SISCLIH,APROH-PROGD)                                 
         DC    AL2(CSIJOBH-SISCLIH,AJOBH-PROGD)                                 
         DC    AL2(CSIWKCH-SISCLIH,AWKCH-PROGD)                                 
         DC    AL2(CSINONH-SISCLIH,ANONH-PROGD)                                 
         DC    AL2(CSIAMTH-SISCLIH,AAMTH-PROGD)                                 
         DC    AL2(CSIEXAH-SISCLIH,AXACH-PROGD)                                 
         DC    AL2(CSIDOFH-SISCLIH,ADOFH-PROGD)                                 
         DC    AL2(CSICOFH-SISCLIH,ACOFH-PROGD)                                 
         DC    AL2(CSIAOFH-SISCLIH,AAOFH-PROGD)                                 
         DC    AL2(CSIDPTH-SISCLIH,ADPTH-PROGD)                                 
         DC    AL2(CSIPERH-SISCLIH,APERH-PROGD)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* SAVED UPPER INPUT FIELDS (2 BYTE DISP TO FIELD, 2 BYTE DISP TO AREA)*         
***********************************************************************         
UPRTAB   DS    0F                                                               
         DC    AL2(AORDH-PROGD,ORDNUML-PROGD)                                   
         DC    AL2(ADOCH-PROGD,DOCNUML-PROGD)                                   
         DC    AL2(ADATH-PROGD,DOCDTEL-PROGD)                                   
         DC    AL2(ADUEH-PROGD,DUEDTEL-PROGD)                                   
         DC    AL2(AURGH-PROGD,URGENTL-PROGD)                                   
         DC    AL2(ACSHH-PROGD,CASHACL-PROGD)                                   
         DC    AL2(AVENH-PROGD,PRDVNDL-PROGD)                                   
         DC    AL2(AVCDH-PROGD,PRDVCDL-PROGD)                                   
         DC    AL2(AXVNH-PROGD,EXPVNDL-PROGD)                                   
         DC    AL2(AXCDH-PROGD,EXPVCDL-PROGD)                                   
         DC    AL2(AOVRH-PROGD,OVRIDEL-PROGD)                                   
         DC    AL2(ANARH-PROGD,NARRA1L-PROGD)                                   
         DC    AL2(ANA2H-PROGD,NARRA2L-PROGD)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* UPPER SCREEN NAME FIELDS TO CLEAR                                   *         
***********************************************************************         
         DS    0F                                                               
UPNTAB   DS    0XL2                                                             
         DC    AL2(AORDAH-PROGD)   ORDER AMOUNT                                 
         DC    AL2(AORDNH-PROGD)   NUMBER OF INVOICES AGAINST ORDER             
         DC    AL2(ACSHNH-PROGD)   CASH ACCOUNT NAME                            
         DC    AL2(AVENNH-PROGD)   VENDOR NAME                                  
         DC    AL2(AVCDNH-PROGD)   VENDOR CD AMOUNT                             
         DC    AL2(AXVNNH-PROGD)   EXPENSE VENDOR NAME                          
         DC    AL2(AXCDNH-PROGD)   EXPENSE VENDOR CD AMOUNT                     
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* XJOB FIELDS TO CLEAR                                                *         
***********************************************************************         
         DS    0F                                                               
XJOTAB   DS    0XL2                                                             
         DC    AL2(ADOFH-PROGD)    FINANCIAL OFFICE                             
         DC    AL2(AAOFH-PROGD)    ANALYSIS OFFICE                              
         DC    AL2(AXACH-PROGD)    EXPENSE ACCOUNT                              
         DC    AL2(ADPTH-PROGD)    DEPARTMENT                                   
         DC    AL2(APERH-PROGD)    STAFF                                        
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* ORDER FIELDS TO CLEAR                                               *         
***********************************************************************         
ORDTAB   DS    0F                                                               
         DC    AL2(AVENH-PROGD)    PRODUCTION VENDOR                            
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* PFKEY TABLE                                                         *         
***********************************************************************         
PFKTAB   DC    AL1(0,0,0),AL2(0),CL11'PF'                                       
         DC    AL1(0,VPFDTL,PFSWAPQ),AL2(PFSWAP-PF),CL11'13=Detail'             
         DC    AL1(0,VPFMUL,PFSWAPQ),AL2(PFSWAP-PF),CL11'13=Multiple'           
         DC    AL1(0,VPFUP,PFUPQ),AL2(PFUP-PF),CL11'14=Up'                      
         DC    AL1(0,VPFDWN,PFDWNQ),AL2(PFDWN-PF),CL11'15=Down'                 
         DC    AL1(0,VPFDEL,PFDELQ),AL2(PFDEL-PF),CL11'16=Delete'               
         DC    AL1(0,VPFPOST,PFPOSTQ),AL2(PFPOST-PF),CL11'17=Post'              
         DC    AL1(0,VPFCLR,PFCLRQ),AL2(PFCLR-PF),CL11'18=Clear'                
         DC    AL1(1,VPFTAX,PFTAXQ),AL2(PFTAX-PF),CL11' '                       
         DC    AL1(1,VPFINV,PFINVQ),AL2(PFINV-PF),CL11' '                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TSAR RECORD FIELD DISPLACEMENT TABLE                                *         
***********************************************************************         
         DS    0F                                                               
RCDTAB   DS    0XL4                                                             
         DC    AL2(RECCLI-PROGD,ACLIH-PROGD)                                    
         DC    AL2(RECPRO-PROGD,APROH-PROGD)                                    
         DC    AL2(RECJOB-PROGD,AJOBH-PROGD)                                    
         DC    AL2(RECWKC-PROGD,AWKCH-PROGD)                                    
         DC    AL2(RECNON-PROGD,ANONH-PROGD)                                    
         DC    AL2(RECAMT-PROGD,AAMTH-PROGD)                                    
         DC    AL2(RECXAC-PROGD,AXACH-PROGD)                                    
         DC    AL2(RECDOF-PROGD,ADOFH-PROGD)                                    
         DC    AL2(RECCOF-PROGD,ACOFH-PROGD)                                    
         DC    AL2(RECAOF-PROGD,AAOFH-PROGD)                                    
         DC    AL2(RECDPT-PROGD,ADPTH-PROGD)                                    
         DC    AL2(RECPER-PROGD,APERH-PROGD)                                    
         DC    AL2(RECBAS-PROGD,ABASH-PROGD)                                    
         DC    AL2(RECLOC-PROGD,ALOCH-PROGD)                                    
         DC    AL2(RECTAX-PROGD,ATAXH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
ITEMS    DC    CL5'Items'                                                       
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
PROGD    DSECT                                                                  
MISCRNQ  EQU   X'F8'               MULTIPLE ITEM SCREEN                         
SISCRNQ  EQU   X'F5'               SINGLE ITEM SCREEN                           
TISCRNQ  EQU   X'B0'               TAX INPUT SCREEN                             
CMISCRNQ EQU   X'FB'               CANADIAN MULTIPLE ITEM SCREEN                
CSISCRNQ EQU   X'F3'               CANADIAN SINGLE ITEM SCREEN                  
CTISCRNQ EQU   X'B1'               CANADIAN TAX INPUT SCREEN                    
*                                                                               
LINLNQ   EQU   (MISLIN2H-MISCLIH)           LENGTH OF DETAIL LINE               
USELNQ   EQU   (USELIN2H-USECLIH)           LENGTH OF USE TAX LINE              
CLINLNQ  EQU   (CMILIN2H-CMICLIH)           LENGTH OF DETAIL LINE               
*                                                                               
MXMULQ   EQU   (MISTABH-MISCLIH)/(LINLNQ)   MAX DETAIL LINES                    
MXUSEQ   EQU   (USETABH-USECLIH)/(USELNQ)   MAX TAX LINES                       
MXCMULQ  EQU   (CMITOTH-CMICLIH)/(CLINLNQ)  MAX DETAIL LINES                    
*                                                                               
MXSINQ   EQU   1                            MAX ON SINGLE DETAIL                
MXCSINQ  EQU   1                            SINGLE DETAILS                      
MXTXLQ   EQU   4                   MAX TAX LOCALITIES                           
*                                  PFKEY EQUATES                                
PFTAXQ   EQU   9                    TAX                                         
PFINVQ   EQU   11                   INVOICE                                     
PFSWAPQ  EQU   13                   DETAIL/MULTIPLE                             
PFUPQ    EQU   14                   UP                                          
PFDWNQ   EQU   15                   DOWN                                        
PFDELQ   EQU   16                   DETAIL                                      
PFPOSTQ  EQU   17                   POST                                        
PFCLRQ   EQU   18                   CLEAR                                       
*                                                                               
MXCAP    EQU   50                  MAXIMUM BUFFER CAPACITY                      
*                                                                               
SCRD     DS    0F                  SCREEN DATA                                  
SCRNUM   DS    XL1                 SCREEN NUMBER                                
SCRMXD   DS    XL1                 MAX NUMBER OF DETAIL LINES                   
SCRLDL   DS    XL1                 LENGTH OF DETAIL LINE                        
SCRSTA   DS    XL1                 STATUS                                       
SCRSINQ  EQU   X'80'                SINGLE INPUT                                
SCRCANQ  EQU   X'40'                CANADIAN SCREEN                             
SCRTAXQ  EQU   X'20'                TAX SCREEN                                  
SCRSWAP  DS    AL1                 SWAP SCREEN CODE                             
SCRTAXS  DS    AL1                 TAX SCREEN CODE                              
SCRSPRG  DS    XL1                 SUB-PROGRAM CODE                             
         DS    XL2                 N/D                                          
SCRPROC  DS    AL2                 PROCESSING ROUTINE                           
SCRUPR   DS    AL2                 DISP. TO TABLE OF UPPER FIELDS               
SCRLOW   DS    AL2                 DISP. TO TABLE OF LOWER FIELDS               
SCRLNQ   EQU   *-SCRD                                                           
*                                                                               
BASERB   DS    F                                                                
RELOA    DS    F                                                                
AEXCELD  DS    A                                                                
ACATD    DS    A                                                                
*                                                                               
AUSATAB  DS    A                   A(US TABLES)                                 
ACANTAB  DS    A                   A(CANADIAN TABLES)                           
AUPRTAB  DS    A                   A(HEADER TABLE)                              
AUPNTAB  DS    A                   A(UPPER NAME FIELDS)                         
AXJOTAB  DS    A                   A(XJOB FIELDS)                               
AORDTAB  DS    A                   A(ORDER FIELDS)                              
ARCDTAB  DS    A                   A(RECORD TABLE)                              
APFKEYS  DS    A                   A(PFKEY TABLE)                               
*                                                                               
UPPER    DS    0A                  ** UPPER FIELDS **                           
AORDH    DS    A                   A(ORDER NUMBER)                              
AORDAH   DS    A                   A(ORDER AMOUNT)                              
AORDNH   DS    A                   A(ORDER NUMBER OF INVOICES)                  
ADOCH    DS    A                   A(DOCUMENT FIELD)                            
ADATH    DS    A                   A(TRANSACTION DATE)                          
ADUEH    DS    A                   A(DUE DATE)                                  
AURGH    DS    A                   A(URGENT)                                    
ACSHH    DS    A                   A(CASH ACCOUNT)                              
ACSHNH   DS    A                   A(CASH ACCOUNT NAME)                         
AVENH    DS    A                   A(PRODUCTION VENDOR)                         
AVENNH   DS    A                   A(PRODUCTION VENDOR NAME)                    
AVCDH    DS    A                   A(VENDOR CASH DISCOUNT)                      
AVCDNH   DS    A                   A(VENDOR CASH DISCOUNT NAME)                 
AXVNH    DS    A                   A(EXPENSE VENDOR)                            
AXVNNH   DS    A                   A(EXPENSE VENDOR NAME)                       
AXCDH    DS    A                   A(EXPENSE VENDOR CD)                         
AXCDNH   DS    A                   A(EXPENSE VENDOR CD NAME)                    
AOVRH    DS    A                   A(OVERRIDE FIELD)                            
ANARH    DS    A                   A(FIRST NARRATIVE)                           
ANA2H    DS    A                   A(SECOND NARRATIVE)                          
ADETH    DS    A                   A(FIRST DETAIL LINE)                         
ATOTH    DS    A                   A(TOTALS)                                    
NUPPER   EQU   (*-UPPER)/4                                                      
*                                                                               
LOWER    DS    0A                  ** LOWER FIELDS **                           
ACLIH    DS    A                   A(CLIENT)                                    
ACLINH   DS    A                   A(CLIENT NAME)                               
APROH    DS    A                   A(PRODUCT)                                   
APRONH   DS    A                   A(PRODUCT NAME)                              
AJOBH    DS    A                   A(JOB)                                       
AJOBNH   DS    A                   A(JOB NAME)                                  
AWKCH    DS    A                   A(COMISSIONABLE W/C)                         
AWKCNH   DS    A                   A(WORKCODE NAME)                             
ANONH    DS    A                   A(NON-COMMISSIONABLE)                        
AAMTH    DS    A                   A(AMOUNT)                                    
         ORG   AAMTH                                                            
ABASH    DS    A                   A(BASIS)                                     
AXACH    DS    A                   A(EXPENSE A/C)                               
AXACNH   DS    A                   A(EXPENSE A/C NAME)                          
ADOFH    DS    A                   A(DEBIT/FINANCIAL OFFICE)                    
ADOFNH   DS    A                   A(DEBIT OFFICE NAME)                         
ACOFH    DS    A                   A(CREDIT OFFICE)                             
ACOFNH   DS    A                   A(CREDIT OFFICE NAME)                        
AAOFH    DS    A                   A(ANALYSIS OFFICE)                           
AAOFNH   DS    A                   A(ANALYSIS OFFICE NAME)                      
ADPTH    DS    A                   A(DEPARTMENT CODE)                           
ADPTNH   DS    A                   A(DEPARTMENT NAME)                           
APERH    DS    A                   A(PERSON/STAFF CODE)                         
APERNH   DS    A                   A(PERSON/STAFF NAME)                         
ATAXH    DS    A                   A(TAX AMOUNT)                                
ALOCH    DS    A                   A(LOCALITY)                                  
ALOCNH   DS    A                   A(LOCALITY NAME)                             
ANEXT    DS    A                                                                
NLOWER   EQU  (*-LOWER)/4                                                       
*                                                                               
SAVERE   DS    F                                                                
SVREG    DS    9F                  SAVE REGISTERS RE,R6                         
*                                                                               
PZERO    DS    PL1                                                              
WC99     DS    CL2                                                              
SXVND    DS    CL2                                                              
BYTE     DS    CL1                                                              
NPOST    DS    XL1                 NUMBER POSTED                                
PL13     DS    PL13                                                             
TAXAMNT  DS    PL6                 TAX AMOUNT                                   
OFFNAME  DS    CL(L'SNMNAME)       SHORT NAME                                   
*                                                                               
POSTSW   DS    XL1                 POSTING CONTROL                              
POSTBUF  EQU   X'80'               POST ITEMS IN BUFFER                         
POSTNXT  EQU   X'40'               RETURN TO POST NEXT                          
POSTTAX  EQU   X'20'               POSTING TAX ITEMS                            
*                                                                               
RECFSW   DS    XL1                 RECORD FILTER SWITCH                         
RECFTAX  EQU   X'80'               ONLY PASS TAX ITEMS                          
RECFNTX  EQU   X'40'               EXCLUDE TAX ITEMS                            
*                                                                               
PFKSW    DS    XL1                 PF KEY CONTROL                               
PFKPROC  EQU   X'80'               PF KEY HAS BEEN PROCESSED                    
PFKDELT  EQU   X'40'               DELETE HAS BEEN PROCESSED                    
PFKRDSP  EQU   X'20'               FORCE A REDISPLAY                            
*                                                                               
CASHACCT DS    CL15                SC CASH ACCT #                               
CASHNAME DS    CL36                SC CASH ACCT NAME                            
*                                                                               
REFNUML  DS    XL1                 LENGTH OF REFERENCE                          
REFNUM   DS    CL6                 REFERENCE                                    
LONGINVL DS    XL1                 LENGTH OF LONG INVOICE NUMBER                
LONGINV  DS    CL20                LONG INVOICE NUMBER                          
DOCDATE  DS    XL3                 INVOICE DATE                                 
DODATE   DS    XL2                 DUE DATE                                     
ADRLIN1  DS    CL(L'ADRADD1)       ADDRESS LINE 1                               
ADRLIN1L DS    XL1                 LENGTH OF ADDRESS LINE 1                     
*                                                                               
VENDVALS DS    0C                  ** SAVED VENDOR VALUES **                    
VENDACCT DS    CL15                PROD VEN KEY                                 
VENDNAME DS    CL36                PROD VEN NAME                                
VENDDISC DS    PL3                 CD VAL FOR PROD VENDS                        
VENDTAXT DS    CL1                 DEFAULT PRODUCTION TAX TYPE                  
*                                                                               
XVNDACCT DS    CL15                EXP VEN KEY                                  
XVNDNAME DS    CL36                EXP VEN NAME                                 
XVNDDISC DS    PL3                 CD VAL FOR EXP VENDS                         
XVNDTAXT DS    CL1                 DEFAULT EXPENSE TAX TYPE                     
*                                                                               
PCONSULT DS    CL1                 1099(2C) VENDOR SWITCH PROD VENDOR.          
ECONSULT DS    CL1                 1099(2C) VENDOR SWITCH EXP VENDOR.           
PRSTAT   DS    CL1                 PROD VENDOR 1099 STATUS                      
EXSTAT   DS    CL1                 EXP VENDOR 1099 STATUS                       
V29SW    DS    CL1                 EXPENSE VENDOR CONTRA.                       
*                                                                               
P2CNUM   DS    CL15                2C NUM  PROD                                 
P2CNAM   DS    CL36                2C NAME PROD                                 
E2CNUM   DS    CL15                2C NUM  EXP                                  
E2CNAM   DS    CL36                2C NAME EXP                                  
PROTROL  DS    CL15                27 NUM  PROD                                 
PROTROLN DS    CL36                27 NAME PROD                                 
EXPTROL  DS    CL15                27 NUM  EXP                                  
EXPTROLN DS    CL36                27 NAME EXP                                  
VENDLNQS EQU   *-VENDVALS          LENGTH OF VENDOR VALUES                      
*                                                                               
NARRLEN  DS    H                   LENGTH OF NARRATIVE                          
NARR     DS    CL120               NARRATIVE FIELD(S)                           
*                                                                               
TAXIT    DS    XL(TXLNQ*MXTXLQ)                                                 
         EJECT                                                                  
* DETAIL VALUES AREA IS CLEARED BEFORE EDITING AND POSTING EACH ITEM            
*                                                                               
DETVALS  DS    0C                  DETAIL VALUES AREA                           
*                                                                               
SIMDAC   DS    CL15                                                             
SIMDACN  DS    CL36                                                             
*                                                                               
CLIACCT  DS    CL15                CLIENT ACCOUNT                               
CLINAME  DS    CL36                CLIENT NAME                                  
PROACCT  DS    CL15                PRODUCT ACCOUNT                              
PRONAME  DS    CL36                PRODUCT NAME                                 
JOBACCT  DS    CL15                JOB ACCOUNT                                  
JOBNAME  DS    CL36                JOB NAME                                     
WRKCODE  DS    CL2                 WORK CODE                                    
WRKNAME  DS    CL35                WORK CODE NAME                               
*                                                                               
CLIOFFC  DS    CL2                 SJ OFFICE CODE                               
CLICOST  DS    CL15                1C ACCOUNT POINTER                           
CLICODE  DS    CL6                 CLIENT CODE                                  
PRODCODE DS    CL6                 PRODUCT CODE                                 
JOBNUM   DS    CL6                 JOB NUMBER                                   
CLIPRO   DS    CL15                CLI,PRO ACCT KEY                             
CLIPRON  DS    CL36                CLI,PRD NAME FOR DISP                        
*                                                                               
FINOFF   DS    CL2                 FINANCIAL OFFICE                             
ANAOFF   DS    CL2                 ANALYSIS OFFICE                              
CRDOFF   DS    CL2                 CREDIT OFFICE                                
PRDOFF   DS    CL2                 PRODUCTION OFFICE                            
         ORG   PRDOFF                                                           
TAXOFF   DS    CL2                 OFFICE FOR TAX POSTINGS                      
*                                                                               
ANOELEM  DS    XL(ANOLNQ)          ANALYSED OFFICE ELEMENT                      
FFTELEM  DS    XL(FFTLN1Q+L'FFTDLEN+L'FFTCLPRA) CLIENT PRODUCT ELEMENT          
*                                                                               
DEPT     DS    CL4                                                              
STAFF    DS    CL12                                                             
STAFFL   DS    XL1                                                              
LEVEL    DS    XL1                                                              
*                                                                               
TYPE     DS    CL1                 E=EXPENSE,P=PRODUCTION                       
OFFSW    DS    CL1                                                              
DEPSW    DS    CL1                                                              
STFSW    DS    CL1                                                              
COSTSW   DS    CL1                                                              
*                                                                               
EXPOST   DS    PL6                 EXPENSE AMOUNT (LESS CD IF OK)               
CDAMNT   DS    PL6                                                              
*                                                                               
INAMNT   DS    PL6                 INVOICE AMOUNT                               
PASSCD   DS    CL1                 PASS CD TO CLIENT                            
COMSW    DS    CL1                 COMMISSIONABLE WORK CODE INDICATOR           
ORDRNUM  DS    CL6                 ORDER NUMBER                                 
POSTAMT  DS    PL6                 AMOUNT POSTED                                
NORDR    DS    XL2                 NUMBER OF FIRST ORDER RECORD                 
*                                                                               
POSTACC  DS    CL15                AGY EXP KEY                                  
POSTACCN DS    CL36                AGY EXP NAME                                 
POSTCNTR DS    CL3                 COST CENTER CODE                             
POSTCPOS DS    XL1                 COST CENTER POSITION IN KEY                  
COSTANAL DS    CL5                                                              
*                                                                               
CRDSNUM  DS    CL15                2/8 ACCT KEY                                 
CRDSNAME DS    CL36                NAME                                         
COSTNUM  DS    CL15                1/C ACCT KEY                                 
COSTNAME DS    CL36                NAME                                         
CRCNUM   DS    CL15                1/P ACCT KEY                                 
CRCNAME  DS    CL36                NAME                                         
CR13NUM  DS    CL15                1/3 ACCT KEY                                 
CRPSNUM  DS    CL15                2/9 ACCT KEY                                 
CRPSNAME DS    CL36                NAME                                         
STAFFNUM DS    CL15                2/P STAFF ACCT KEY                           
STAFFNAM DS    CL36                STAFF NAME                                   
DEPNUM   DS    CL15                DEPT ACCT KEY                                
DEPNAME  DS    CL36                DEPT NAME                                    
DEPSTFN  DS    CL36                OFF,DEPT,STAFF NAME FOR DISP                 
*                                                                               
DETVALNQ EQU   *-DETVALS                                                        
         EJECT                                                                  
*                                  TSAR EQUATES                                 
TSRADDQ  EQU   1                   ADD                                          
TSRGETQ  EQU   2                   GET                                          
TSRPUTQ  EQU   3                   PUT                                          
TSRSAVQ  EQU   4                   SAVE                                         
TSRDELQ  EQU   5                   DELETE                                       
TSRINTQ  EQU   6                   INITIALIZE                                   
*                                                                               
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
*                                                                               
RECSAV   DS    0XL(RECLNQ)         TSAR RECORD                                  
RECKEY   DS    0XL3                KEY                                          
RECDAT   DS    XL1                 DATA TYPE                                    
RECDIV   EQU   0                    INVOICE                                     
RECDTX   EQU   1                    TAX                                         
RECNUM   DS    XL2                 RECORD NUMBER                                
RECCLI   DS    CL(L'MISCLI)                                                     
RECPRO   DS    CL(L'MISPRO)                                                     
RECJOB   DS    CL(L'MISJOB)                                                     
RECWKC   DS    CL(L'MISWKC)                                                     
RECNON   DS    CL(L'MISNON)                                                     
RECAMT   DS    CL(L'MISAMT)                                                     
RECXAC   DS    CL(L'MISEXA)                                                     
RECDOF   DS    CL(L'MISDOF)                                                     
RECCOF   DS    CL(L'MISCOF)                                                     
RECAOF   DS    CL(L'MISAOF)                                                     
RECDPT   DS    CL(L'MISDPT)                                                     
RECPER   DS    CL(L'MISPER)                                                     
RECBAS   DS    CL(L'USEBAS)                                                     
RECLOC   DS    CL(L'USELOC)                                                     
RECTAX   DS    CL(L'USETAX)                                                     
RECLNQ   EQU   *-RECKEY                                                         
         EJECT                                                                  
*                                  **UPPER SCREEN FIELDS LENGTH/DATA**          
ORDNUML  DS    XL1                 LENGTH & ORDER NUMBER                        
ORDNUM   DS    CL(L'MISORD)                                                     
*                                                                               
DOCNUML  DS    XL1                 LENGTH & DOCUMENT NUMBER                     
DOCNUM   DS    CL(L'MISDOC)                                                     
*                                                                               
DOCDTEL  DS    XL1                 LENGTH & DOCUMENT DATE                       
DOCDTE   DS    CL(L'MISDAT)                                                     
*                                                                               
DUEDTEL  DS    XL1                 LENGTH & DUE DATE                            
DUEDTE   DS    CL(L'MISDUE)                                                     
*                                                                               
URGENTL  DS    XL1                 LENGTH & URGENT                              
URGENT   DS    CL(L'MISURG)                                                     
*                                                                               
CASHACL  DS    XL1                 LENGTH & CASH ACCOUNT                        
CASHAC   DS    CL(L'MISCSH)                                                     
*                                                                               
PRDVNDL  DS    XL1                 LENGTH & PRODUCTION VENDOR                   
PRDVND   DS    CL(L'MISVEN)                                                     
*                                                                               
PRDVCDL  DS    XL1                 LENGTH & PROD. VENDOR CD                     
PRDVCD   DS    CL(L'MISVCD)                                                     
*                                                                               
EXPVNDL  DS    XL1                 LENGTH & EXPENSE VENDOR                      
EXPVND   DS    CL(L'MISXVN)                                                     
*                                                                               
EXPVCDL  DS    XL1                 LENGTH & EXPENSE VENDOR CD                   
EXPVCD   DS    CL(L'MISXCD)                                                     
*                                                                               
OVRIDEL  DS    XL1                 OVERRIDE                                     
OVRIDE   DS    CL(L'MISOVR)                                                     
*                                                                               
NARRA1L  DS    XL1                 LENGTH & NARRATIVE 1                         
NARRA1   DS    CL(L'MISNAR)                                                     
*                                                                               
NARRA2L  DS    XL1                 LENGTH & NARRATIVE 2                         
NARRA2   DS    CL(L'MISNAR2)                                                    
         EJECT                                                                  
**********************************************************************          
* FOLLOWING DATA SAVED IN OSSAVE AREA OF TWA                         *          
*  MAX IS 256 BYTES                                                  *          
**********************************************************************          
STS      DS    0X                  **SAVED TWA STORAGE**                        
*                                                                               
PROCSW   DS    XL1                 PROCESS CONTROL                              
PROCFST  EQU   X'80'                FIRST TIME SWITCH                           
PROCDTL  EQU   X'40'                PROCESS DETAIL SCREEN                       
PROCTAX  EQU   X'20'                TAX LINES TO PROCESS                        
*                                                                               
BUFSW    DS    XL1                 BUFFER CONTROL                               
BUFINI   EQU   X'80'                BUFFER INITIALIZED                          
BUFRES   EQU   X'40'                BUFFER RESTORED                             
*                                                                               
FRSTSCRN DS    XL1                 SAVE FIRST SCREEN                            
*                                                                               
PAGL     DS    XL1                 LOW PAGE                                     
PAGN     DS    XL1                 NUMBER OF PAGES                              
*                                                                               
XCAREA   DS    0X                  **FOLLOWING IS CLEARED AFTER POST**          
NBUF     DS    XL2                 NUMBER IN BUFFER                             
NFST     DS    XL2                 NUMBER OF FIRST ON SCREEN                    
NLST     DS    XL2                 NUMBER OF LAST ON SCREEN                     
NGET     DS    XL2                 NUMBER OF THE ITEM TO GET                    
NREC     DS    XL2                 NUMBER OF ADDS                               
NITM     DS    XL1                 NUMBER OF ITEMS ON THE SCREEN                
*                                                                               
VPFSW    DS    XL1                 VALID PF KEYS                                
VPFDTL   EQU   X'80'                DETAIL                                      
VPFMUL   EQU   X'40'                MULTIPLE                                    
VPFUP    EQU   X'20'                UP                                          
VPFDWN   EQU   X'10'                DOWN                                        
VPFDEL   EQU   X'08'                DELETE                                      
VPFPOST  EQU   X'04'                POST                                        
VPFCLR   EQU   X'02'                CLEAR                                       
*                                                                               
VPFSW1   DS    XL1                 VALID PF KEYS -                              
VPFTAX   EQU   X'80'                TAX                                         
VPFINV   EQU   X'40'                INVOICE                                     
*                                                                               
TOTDLRS  DS    PL6                 TOTAL CASH                                   
LASTORD  DS    CL6                 LAST ORDER #                                 
ORDSTA   DS    CL1                 P=PARTIAL                                    
INVDNUM  DS    PL2                 NUMBER OF INVOICES AGAINST ORDER             
ORDRAMT  DS    PL6                 AMOUNT OF ORDER                              
*                                                                               
INVSCRN  DS    XL1                 SAVE INVOICE SCREEN                          
INVNGET  DS    XL2                 SAVED RECORD NUMBER                          
NTAXL    DS    XL1                 NUMBER OF TAX LINES                          
*                                                                               
XCEND    EQU   *                                                                
STSLNQ   EQU   *-STS                                                            
**** END SAVED TWA STUFF  *******************************************           
         EJECT                                                                  
**********************************************************************          
*  OTHER LOCAL STORAGE AREAS                                         *          
**********************************************************************          
         DS    0D                                                               
KEY      DS    CL49                                                             
IOAREA   DS    XL2000                                                           
*                                                                               
         DS    0D                                                               
EXCBLK   DS    XL(EXCELNQ)                                                      
*                                                                               
         DS    0D                                                               
CATBLK   DS    XL(CATLNQ)                                                       
*                                                                               
PROGDX   DS    0C                                                               
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER TAX ENTRY                                           *          
**********************************************************************          
TXD      DSECT                                                                  
TXACC    DS    CL15                CREDIT ACCOUNT                               
TXACCN   DS    CL36                CREDIT ACCOUNT NAME                          
TXLOC    DS    CL8                 LOCALITY                                     
TXLOCN   DS    CL36                NAME                                         
TXEFF    DS    XL3                 EFFECTIVE DATE                               
TXRTE    DS    PL4                 PERCENT                                      
TXBAS    DS    PL6                 BASIS                                        
TXTAX    DS    PL6                 TAX                                          
TXLNQ    EQU   *-TXD                                                            
                                                                                
                                                                                
**********************************************************************          
* DSECT TO COVER PFKEY TABLE                                         *          
**********************************************************************          
PFKTD    DSECT                                                                  
PFKTVBY  DS    XL1                 VALIDATION BYTE                              
PFKTVBY0 EQU   0                    DEFAULT (VPFSW)                             
PFKTVBY1 EQU   1                    SECOND (VPFSW1)                             
PFKTVBI  DS    XL1                 VALIDATION BIT                               
PFKTNUM  DS    XL1                 PFKEY NUMBER                                 
PFKTROU  DS    AL2                 ROUTINE                                      
PFKTNAM  DS    CL11                NAME                                         
PFKTLNQ  EQU   *-PFKTD                                                          
         EJECT                                                                  
         PRINT ON                                                               
* ACBATDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBATDSECT                                                     
         PRINT ON                                                               
         EJECT                                                                  
* ACBATF8D  - DSECT TO COVER MULTIPLE INPUT SCREEN                              
         PRINT OFF                                                              
       ++INCLUDE ACBATF8D                                                       
         PRINT ON                                                               
                                                                                
* ACBATF5D  - DSECT TO COVER SINGLE INPUT SCREEN                                
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATF5D                                                       
         PRINT ON                                                               
                                                                                
* ACBATB0D  - DSECT TO COVER TAX INPUT SCREEN                                   
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATB0D                                                       
         PRINT ON                                                               
                                                                                
* ACBATFBD  - DSECT TO COVER MULTIPLE INPUT SCREEN - CANADIAN                   
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATFBD                                                       
         PRINT ON                                                               
                                                                                
* ACBATF3D  - DSECT TO COVER SINGLE INPUT SCREEN - CANADIAN                     
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATF3D                                                       
         PRINT ON                                                               
                                                                                
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
* ACEXCELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACEXCELD                                                       
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDDSCANBLKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACBAT0AS  10/03/03'                                      
         END                                                                    
