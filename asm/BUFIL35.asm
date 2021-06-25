*          DATA SET BUFIL35    AT LEVEL 135 AS OF 05/01/02                      
*PHASE T50235A,*                                                                
*INCLUDE BUEDRULE                                                               
T50235   TITLE 'BUFIL35 - BUDGET CNTL LFM - OUTLINE CREATE FROM JOBS'           
T50235   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI35**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
*                                                                               
         L     RE,=V(EDRULES)                                                   
         AR    RE,R2                                                            
         ST    RE,VEDRULES                                                      
         GOTO1 VSETADD                                                          
*                                                                               
         MVI   IOOPT,C'Y'          DO OUR OWN IOS                               
         XC    CURRSYST,CURRSYST   ALWAYS STARTS OUT IN CONNECTED SYS           
         OI    CONSERVH+1,X'01'    SO WE DON'T GET "NO DATA" MESSAGE            
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    VREC                COPY OUTLINE RECORDS                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELDS ON SCREEN                                                 
***********************************************************************         
VKEY     MVC   ANODBLK2,ATIA       BORROW TIA FOR SECOND NODIO BLOCK            
         MVI   BITFLAG1,0                                                       
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
VKCLT00  LA    R2,OJTCLTH                                                       
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
VKCLT10  GOTO1 VVALCLT,PARAS,(R2),0                                             
         OI    4(R2),X'20'         FIELD VALIDATED NOW                          
         MVC   CLT,CLTCODE         SAVE CLIENT CODE                             
         MVC   OJTCLN,CLTNAM       SHOW THE CLIENT NAME                         
         OI    OJTCLNH+6,X'80'                                                  
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
VKPRD00  LA    R2,OJTPRDH                                                       
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
VKPRD10  GOTO1 VVALPRD,PARAS,(R2),0                                             
         OI    4(R2),X'20'         FIELD VALIDATED NOW                          
         MVC   PRD,PRDCODE         SAVE PRODUCT CODE                            
         MVC   OJTPRN,PRDNAM       SHOW THE PRODUCT NAME                        
         OI    OJTPRNH+6,X'80'                                                  
*                                                                               
* VALIDATE PLAN                                                                 
*                                                                               
VKPLN00  LA    R2,OJTPLAH                                                       
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
VKPLN10  GOTO1 VVALPLAN,PARAS,(R2),0                                            
         OI    4(R2),X'20'         FIELD VALIDATED NOW                          
         MVC   PLAN,PLANCODE       SAVE PLAN CODE                               
         MVC   OJTPLN,PLANNAM      SHOW THE PLAN NAME                           
         OI    OJTPLNH+6,X'80'                                                  
         MVC   SVPLNKEY,NODKEY     SAVE PLAN NODAL KEY                          
         MVC   SVPARKEY,NODKEY                                                  
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         LA    R3,NDLVTABL(R3)     R3=A(FIRST OUTLINE LEVEL)                    
         MVC   NEXTNODE,NDLVNOD    EXTRACT NODE ESTABLISHED BY PLAN             
         MVI   NEXTLEV,1           INIT NEXT OUTLINE LEVEL TO 1                 
         DROP  R3                                                               
*                                                                               
         MVI   ERROR,LIMERR                                                     
         SR    R1,R1                                                            
         ICM   R1,3,PLANCNT        TEST FOR BLOWING OUTLINE LIMIT               
         LA    R1,1(R1)                                                         
         CH    R1,=Y(OUTLIMIT)                                                  
         BH    TRAPERR                                                          
         STCM  R1,3,PLANCNT                                                     
*                                                                               
* EDIT AND VALIDATE ATTACH TO POINT                                             
*                                                                               
VKATT00  LA    R2,OJTATTH                                                       
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
         XC    OJTATTN,OJTATTN                                                  
         OI    OJTATTNH+6,X'80'                                                 
*                                                                               
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         CLI   FLDH+5,0                                                         
         BE    VKATTX                                                           
*                                                                               
         MVC   ATTACH,FLD          EXTRACT ATTACH TO CODE                       
         GOTO1 VFINDOUT,PARAS,ATTACH,NDIOA                                      
         BNE   TRAPERR             INVALID PARENT                               
         GOTO1 VTRACE              GET NODAL KEY                                
         MVC   ATTKEY,NODKEY       SAVE NODAL KEY                               
         MVC   SVPARKEY,NODKEY                                                  
*                                                                               
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
*                                                                               
         GOTO1 VGETVAL                                                          
         MVI   ERROR,LEVERR                                                     
         CLI   OUTLEV,MAXOUTS      TEST OVERFLOWED LEVEL LIMIT                  
         BE    TRAPERR             YES                                          
*                                                                               
         LA    R3,NDLVTABL(R3)     NEXT LEVEL TABLE ENTRY                       
         MVC   NEXTNODE,NDLVNOD    EXTRACT NODE ESTABLISHED BY PARENT           
         ZIC   R1,OUTLEV                                                        
         LA    R1,1(R1)            INCREMENT OUTLINE LEVEL                      
         STC   R1,NEXTLEV          SET NEXT OUTLINE LEVEL                       
*                                                                               
         MVC   OJTATTN,OUTNAME     EXTRACT PARENT OUTLINE'S NAME                
VKATTX   OI    4(R2),X'20'         FIELD VALIDATED NOW                          
         DROP  R3                                                               
*                                                                               
* EDIT ADD BEFORE POINT                                                         
*                                                                               
VKBEF00  LA    R2,OJTBEFH                                                       
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         CLI   FLDH+5,0                                                         
         BE    VKBEFX                                                           
         MVC   BEFORE,FLD          EXTRACT BEFORE OUTLINE CODE                  
*                                                                               
         GOTO1 VFINDOUT,PARAS,BEFORE,AIO                                        
         BNE   TRAPERR                                                          
         MVI   ERROR,NOTCHILD                                                   
         L     R4,AIO              MAKE THE RECORD ADDRESSABLE                  
         USING BURECD,R4                                                        
         CLC   BUKNODE,NEXTNODE    TEST THAT RECORD IS CHILD                    
         BNE   TRAPERR                                                          
VKBEFX   OI    4(R2),X'20'         FIELD VALIDATED NOW                          
         DROP  R4                                                               
*                                                                               
* VALIDATE PRODUCTION CLIENT CODE                                               
*                                                                               
VKPCL00  DS    0H                                                               
         BAS   RE,GETCMPNY         GET THE COMPANY AND LEDGER                   
*                                                                               
         LA    R2,OJTPCLTH                                                      
         MVI   ERROR,MISSING                                                    
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
         CLI   5(R2),0             NEED PRODUCTION CLIENT                       
         BE    TRAPERR                                                          
*                                                                               
         MVC   PRODCLT,8(R2)       COPY THE PRODUCTION CLIENT ENTERED           
         OC    PRODCLT,SPACES                                                   
         BAS   RE,VPRODCLT         VALIDATE THE PRODUCTION CLIENT               
         BNE   INVLFLD                                                          
*                                                                               
         MVC   OJTPCLN,ACCLTNAM                                                 
         OI    OJTPCLNH+6,X'80'                                                 
*                                                                               
VKPCLX   OI    4(R2),X'20'         FIELD VALIDATED NOW                          
*                                                                               
* VALIDATE PRODUCTION PRODUCT CODE                                              
*                                                                               
VKPPR00  XC    PRODPRD,PRODPRD     ASSUME NO PRODUCT FILTER YET                 
*                                                                               
         LA    R2,OJTPPRDH                                                      
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
         XC    OJTPPRN,OJTPPRN                                                  
         OI    OJTPPRNH+6,X'80'                                                 
*                                                                               
         CLI   5(R2),0             ANYTHING FOR PRODUCT FILTER?                 
         BE    VKPPRX              NONE                                         
*                                                                               
         MVC   PRODPRD,8(R2)       COPY THE PRODUCTION PRODUCT ENTERED          
         OC    PRODPRD,SPACES                                                   
         MVC   QPRD,PRODPRD                                                     
         BAS   RE,VPRODPRD         VALIDATE THE PRODUCTION PRODUCT              
         BNE   INVLFLD                                                          
         MVC   OJTPPRN,ACPRDNAM                                                 
*                                                                               
VKPPRX   OI    4(R2),X'20'         FIELD VALIDATED NOW                          
*                                                                               
* VALIDATE DATE ADDED FILTER                                                    
*                                  ASSUME NO DATE ADDED FILTER YET              
VKPDT00  XC    PRODSTDT(L'PRODSTDT+L'PRODNDDT),PRODSTDT                         
         LA    R2,OJTPDTEH                                                      
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
         CLI   5(R2),0             ANYTHING FOR DATE ADDED FILTER?              
         BE    VKPDTX              NONE                                         
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(0,PERVALST)                           
         TM    DMCB+4,X'03'        ANY ERRORS FROM PERVAL?                      
         BNZ   INVLFLD                                                          
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   PRODSTDT,PVALPSTA   COPY START AND END DATE                      
         MVC   PRODNDDT,PVALPEND                                                
         DROP  R1                                                               
*                                                                               
VKPDTX   OI    4(R2),X'20'         FIELD VALIDATED NOW                          
*                                                                               
* VALIDATE PRODUCTION MEDIA                                                     
*                                                                               
VKPMD00  MVI   PRODMED,0           ASSUME NO MEDIA FILTER YET                   
         LA    R2,OJTPMEDH                                                      
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
         CLI   5(R2),0             ANYTHING FOR PROD MEDIA FILTER?              
         BE    VKPMDX              NONE                                         
*                                                                               
         MVC   PRODMED,8(R2)       COPY MEDIA FILTER                            
*                                                                               
VKPMDX   OI    4(R2),X'20'         FIELD VALIDATED NOW                          
*                                                                               
* VALIDATE FILTERS FIELD                                                        
*                                                                               
VKFIL00  LA    R2,OJTPFILH                                                      
         BAS   RE,TSTKYCHG         FIELD VALIDATED BEFORE?                      
*                                                                               
         XC    QFILTER,QFILTER     CLEAR ENTERED FILTER                         
         CLI   5(R2),0             ANYTHING FOR FILTERS?                        
         BE    VKFILX              NONE                                         
         GOTO1 VALFIL,DMCB,QFILTER,L'QFILTER                                    
*                                                                               
VKFILX   OI    4(R2),X'20'         FIELD VALIDATED NOW                          
*                                                                               
VKX      B     XIT                                                              
*                                                                               
TSTKYCHG TM    4(R2),X'20'         DID THIS KEY FIELD CHANGE?                   
         BNZR  RE                                                               
         OI    BITFLAG1,B1KEYCHG   YES                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SCREEN                                                           
***********************************************************************         
VREC     DS    0H                                                               
         TM    BITFLAG1,B1KEYCHG   DID ANY KEY FIELDS CHANGE?                   
         BNZ   VR100               YES, IGNORE ANY SELECT FIELDS                
*                                                                               
         LA    R2,OJTSEL1H         R2 = A(FIRST SELECT FIELD)                   
         LA    R3,SVDJBKYS         R3 = A(FIRST KEY IN THE LIST)                
*                                                                               
VR10     LA    R4,OJTSELLH         R4 = A(LAST SELECT FIELD)                    
*                                                                               
         CR    R2,R4               PASSED THE LIST SCREEN ALREADY?              
         BH    VR100               YES, DISPLAY THE JOBS                        
*                                                                               
         OC    0(L'SVDJBKYS,R3),0(R3)   ANY KEY IN THIS LINE?                   
         BZ    VR100               NO MORE IN LIST                              
*                                                                               
         TM    4(R2),X'20'         DID THIS SELECT FIELD CHANGE?                
         BNZ   VR50                NO, CHECK THE NEXT LINE IN LIST              
*                                                                               
         CLI   5(R2),0                                                          
         BE    VR50                                                             
*                                                                               
         CLI   8(R2),C'*'          SKIP '*'                                     
         BE    VR50                                                             
         CLI   8(R2),C'S'          ONLY 'S' IS ACCEPTED                         
         BNE   INVLFLD                                                          
*                                                                               
         LA    R6,3(R3)            GET CLT CODE FROM PRODUCTION KEY             
         ZIC   R1,LPRODCLT                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JOBCLTCD(0),0(R6)                                                
*                                                                               
         ZIC   R0,LPRODCLT         GET PRD CODE FROM PRODUCTION KEY             
         AR    R6,R0                                                            
         IC    R1,LPRODPRD                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JOBPRDCD(0),0(R6)                                                
*                                                                               
         IC    R0,LPRODPRD         GET JOB CODE FROM PRODUCTION KEY             
         AR    R6,R0                                                            
         MVC   JOBJOBCD,0(R6)                                                   
*                                                                               
         LA    R1,JOBJOBCD+L'JOBJOBCD-1    POINT TO LAST CHAR IN CODE           
VR20     CLI   0(R1),C' '                                                       
         BNE   VR30                                                             
         MVI   0(R1),0                                                          
         BCTR  R1,0                                                             
         B     VR20                                                             
*                                                                               
VR30     XC    OUTCODE,OUTCODE                                                  
         MVC   OUTCODE(L'JOBJOBCD),JOBJOBCD                                     
         BAS   RE,CKOUTCOD         OUTLINE CODE EXISTS ALREADY?                 
         BNE   DUPLCODE            YES                                          
*                                                                               
         BAS   RE,XFERJOB          TRANSFER THE JOB                             
*                                                                               
         MVC   SVDJOBKY,SVDJBKYS   A JOB HAS BEEN TRANSFERRED                   
*                                                                               
VR50     XC    8(L'LINSEL,R2),8(R2)  CLEAR, VALIDATE & TRANSMIT THE             
         OI    4(R2),X'20'               SELECT FIELD                           
         OI    6(R2),X'80'                                                      
*                                                                               
         AH    R2,=Y(LINNEXT-LINDSECT)   POINT TO NEXT LINE                     
         LA    R3,L'SVDJBKYS(R3)         POINT TO NEXT KEY IN TABLE             
         B     VR10                      AND CHECK IF ANY CHANGES               
*                                                                               
VR100    BAS   RE,LRECS            LIST THE JOB RECORDS                         
*                                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE JOB NAME ALREADY EXISTS AS AN               
* OUTLINE CODE                                                                  
*                                                                               
* ON ENTRY:    (R2)                A(SELECTED LIST LINE)                        
*              OUTCODE,JOBJOBCD    JOB CODE TO BE USED                          
***********************************************************************         
CKOUTCOD NTR1                                                                   
         CLC   =C'MPL',CURRSYST    IN THE MPL SYSTEM ALREADY?                   
         BE    COCD10                                                           
         L     R1,ACOMFACS         NO, SWITCH TO THE MPL SYSTEM                 
         L     RF,CSWITCH-COMFACSD(R1)                                          
         GOTO1 (RF),DMCB,=C'MPL',0                                              
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURRSYST,=C'MPL'                                                 
*                                                                               
COCD10   XC    KEY,KEY             SET UP PASSIVE KEY TO FIND ANY DUPS          
         LA    R4,KEY                                                           
         USING BUCKEY,R4                                                        
         MVI   BUCSYS,C'B'                                                      
         MVC   BUCAGY,AGENCY                                                    
         MVI   BUCRTYP,BUCRTYPQ                                                 
         MVC   BUCCLT,OJTCLT                                                    
         MVC   BUCPRD,OJTPRD                                                    
         MVC   BUCPLAN,OJTPLA                                                   
         MVC   BUCCODE,OUTCODE                                                  
         DROP  R4                                                               
*                                                                               
         BAS   RE,MPLHIGH                                                       
*                                                                               
         CLC   KEYSAVE(L'BUCKEY),KEY  COULDN'T FIND PASSIVE?                    
         BNE   COCDYES             YES, GOOD THEN WE CAN ADD                    
*                                                                               
         TM    DMCB+8,X'02'        RECORD DELETED?                              
         BNZ   COCDYES             YES                                          
*                                                                               
COCDNO   B     NEQXIT              NO, FOUND A DUPLICATE                        
*                                                                               
COCDYES  B     EQXIT                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE TRANSFERS A JOB TO THE BUDGET SYSTEM                             
*                                                                               
* ON ENTRY:    JOBCLTCD            CLIENT CODE TO BE USED                       
*              JOBPRDCD            PRODUCT CODE TO BE USED                      
*              OUTCODE,JOBJOBCD    JOB CODE TO BE USED                          
*              (R2)                A(SELECTED LIST LINE)                        
***********************************************************************         
XFERJOB  NTR1                                                                   
         USING LINDSECT,R2                                                      
         MVC   NODKEY,SVPLNKEY     SET UP NODIO KEY                             
         GOTO1 CONCAT,OUTCODE                                                   
         MVC   SVNKEY,NODKEY                                                    
*                                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
*                                                                               
         LR    R0,R4                                                            
         L     R1,SIZEIO                                                        
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR THE IO AREA FOR ADD                    
*                                                                               
         MVC   BUKEY,NDKEY         INITIALIZE KEY W MASTER                      
         MVC   BURLEN,DATADISP                                                  
         OI    BURCTYP,BUKCOUT     OUTLINE RECORD INDICATOR                     
*                                                                               
         XC    ELEM(BUOUTLNQ),ELEM                                              
         LA    R6,ELEM             INITIALIZE OUTLINE DESCRITPION ELEM          
         USING BUOUTD,R6                                                        
         MVI   BUOUTEL,BUOUTELQ                                                 
         MVI   BUOUTLEN,BUOUTLNQ                                                
         MVC   BUOUTNAM(L'JOBJOBCD),JOBJOBCD                                    
*                                                                               
         MVC   SAVOUTEL,ELEM       SAVE OUTLINE ELEMENT                         
*                                                                               
         MVI   ELCODE,BURULELQ     DELETE RULES ELEMENTS                        
         GOTO1 REMELEM                                                          
         MVI   ELCODE,BUINELQ      DELETE SCREEN INPUT ELEMENTS                 
         GOTO1 REMELEM                                                          
*                                                                               
         MVC   FAKEFLD(17),=CL17'SYS=PROD/ALL,CLI='                             
         LA    R3,FAKEFLD+17                                                    
         ZIC   R1,LPRODCLT                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),JOBCLTCD                                                 
*                                                                               
         LA    R3,0(R1,R3)         R3 = LAST CHAR OF CLIENT                     
         CLI   0(R3),C' '          REMOVE TRAILING BLANKS FROM CLIENT           
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   0(5,R3),=CL5',PRD='                                              
         LA    R3,5(R3)                                                         
         ZIC   R1,LPRODPRD                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),JOBPRDCD                                                 
*                                                                               
         LA    R3,0(R1,R3)         R3 = LAST CHAR OF PRODUCT                    
         CLI   0(R3),C' '          REMOVE TRAILING BLANKS FROM PRODUCT          
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   0(5,R3),=CL5',JOB='                                              
         LA    R3,5(R3)                                                         
         LA    R1,L'JOBJOBCD                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),JOBJOBCD                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         LA    R1,FAKEFLD                                                       
         SR    R3,R1                                                            
         STC   R3,FAKEFLDH+5                                                    
         MVI   FAKEFLDH,L'FAKEFLDH+L'FAKEFLD                                    
*                                                                               
         GOTO1 VEDRULES,DMCB,GEND,FAKEFLDH,FAKEFLDH                             
*                                                                               
         LA    R6,ELEM             ADD SCREEN INPUT ELEMENT                     
         USING BUIND,R6                                                         
         XC    ELEM,ELEM                                                        
         MVI   BUINEL,BUINELQ                                                   
         MVI   BUINSEQ,1           FIRST LINE                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BUINPUT(0),FAKEFLD                                               
         LA    R3,BUINPUT-BUIND+1(R3)                                           
         STC   R3,BUINLEN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R6,AIO2             ADD THE RULE ELEMENTS                        
         LA    R6,1000(R6)                                                      
         USING BURULD,R6                                                        
XJOB10   CLI   0(R6),0                                                          
         BE    XJOB20                                                           
         XC    ELEM,ELEM                                                        
         ZIC   R1,BURULEN                                                       
         LR    R0,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         GOTO1 ADDELEM                                                          
         AR    R6,R0                                                            
         B     XJOB10                                                           
*                                                                               
XJOB20   LA    R6,ELEM             ADD OUTLINE DESCRIPTION ELEMENT              
         USING BUOUTD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVC   ELEM(L'SAVOUTEL),SAVOUTEL                                        
         MVI   BUOUTBEF,0                                                       
         MVI   BUOUTAFT,0                                                       
         XC    BUOUTIND,BUOUTIND                                                
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM           ADD PASSIVE POINTER ELEMENT                  
         USING BUPTRD,R6                                                        
         MVI   BUPTREL,BUPTRELQ                                                 
         MVI   BUPTRLEN,BUPTRLNQ                                                
         LA    R3,BUPOINT                                                       
         USING BUCRECD,R3                                                       
         MVC   BUCSYS,BUKSYS                                                    
         MVC   BUCAGY,BUKAGY                                                    
         MVI   BUCRTYP,BUCRTYPQ                                                 
         MVC   BUCCLT,CLTCODE                                                   
         MVC   BUCPRD,PRDCODE                                                   
         MVC   BUCPLAN,PLANCODE                                                 
         MVC   BUCCODE,OUTCODE                                                  
         MVC   PASSVPTR,0(R3)                                                   
         GOTO1 ADDELEM                                                          
         DROP  R3,R6                                                            
*                                                                               
         GOTO1 VADDACTV            ADD THE ACTIVITY ELEMENT                     
*                                                                               
         XC    NDHOOK,NDHOOK       CHECK FOR DUPLICATE KEYS                     
         MVI   NDDELRSW,YES        RETURN DELETED RECORDS                       
         MVC   NDIOA,AIO2          GET RECORD INTO IO2                          
         MVC   NDIOA2,AIO3                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         MVI   NDDELRSW,0          CLEAR PASS DELETE SWITCH                     
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    XJOB23              YES-PROCEED WITH ADD                         
         CLI   NDERR,0                                                          
         BE    XJOB22              RECORD FOUND-DUPLICATE                       
         GOTO1 VNODERR                                                          
*                                                                               
XJOB22   L     R4,NDIOA                                                         
         MVI   ERROR,DELEXIST                                                   
         XC    KEY,KEY             READ THE DIRECTORY POINTER                   
         MVC   KEY(L'BUKEY),BUKEY                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 READ                                                             
         TM    KEY+(BUKCSTA-BUKEY),X'80'                                        
         BO    *+8                 RECORD IS DELETED                            
         MVI   ERROR,RECEXIST                                                   
         B     TRAPERR                                                          
*                                                                               
XJOB23   OC    BEFORE,BEFORE       TEST FOR ADD BEFORE POINT                    
         BZ    XJOB24              NO-ADD TO END OF LIST                        
         MVC   NODKEY,SVNKEY       INITIALIZE NODIO KEY                         
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2         GET RECORD INTO IO2                          
         LA    R0,L'BEFORE                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'B',=C'ADD'),NODKEY,((R0),BEFORE),X        
               0,0                                                              
         CLI   NDERR,0                                                          
         BE    XJOB25                                                           
         GOTO1 VNODERR                                                          
*                                                                               
XJOB24   MVC   NODKEY,SVNKEY                                                    
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'A',=C'ADD'),NODKEY,0,0                    
         CLI   NDERR,0                                                          
         BE    XJOB25                                                           
         GOTO1 VNODERR                                                          
*                                                                               
* ADD PASSIVE POINTER                                                           
*                                                                               
XJOB25   GOTO1 PASSKEY,PARAS,PASSVPTR                                           
         GOTO1 NEWPTR,(R1),PASSVPTR                                             
*                                                                               
XJOB27   L     R4,NDIOA                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  EXTRACT KEY FROM RECORD                      
         MVC   TWAKEYSV,KEY                                                     
*                                                                               
* UPDATE PLAN RECORD FOR OUTLINE ADD, DELETE, OR RESTORE                        
*                                                                               
XJOB30   GOTO1 VSETKEY             BUILD PLAN KEY                               
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT HERE ON NODIO ERROR               
         L     R6,NDIOA                                                         
         MVI   ELCODE,BUPLNELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPLND,R6                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,BUPLNCNT       R1=OUTLINE COUNT FOR PLAN                    
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    *+12                                                             
         LA    R1,1(R1)            INCREMENT COUNT                              
         B     *+6                                                              
*                                                                               
         BCTR  R1,0                DECREMENT COUNT FOR DELETE                   
*                                                                               
         STCM  R1,3,BUPLNCNT                                                    
         CLI   ACTNUM,ACTADD       TEST FOR ACTION ADD                          
         BNE   XJOB32              SKIP SETTING LOWEST LEVEL                    
         CLC   BUPLNLOW,NEXTLEV    TEST IF NEW LOW LEVEL FOR PLAN               
         BH    *+10                                                             
         MVC   BUPLNLOW,NEXTLEV                                                 
*                                                                               
XJOB32   GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',NODKEY,0                             
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XJOBX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CONCATENATES THE OUTLINE CODE TO ITS PARENT'S NODAL KEY.         
*                                                                               
* ON ENTRY:    (R1)                OUTLINE CODE                                 
*                                                                               
* ON EXIT:     NODKEY & NODKEYSV   FULL NODAL KEY                               
*                                                                               
* NOTE: R0 GETS CLOBBERRED                                                      
***********************************************************************         
CONCAT   ST    RE,SAVERE                                                        
         MVC   NODKEY,SVPARKEY                                                  
         LA    R0,L'NODKEY                                                      
         LA    RE,NODKEY+L'NODKEY-1                                             
         CLI   0(RE),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    CONCAT10                                                         
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                                                             
*                                                                               
CONCAT10 LA    RE,1(RE)            POSITION FOR DELIMITER                       
         MVC   0(1,RE),NDDELIM                                                  
         MVC   1(L'OUTCODE,RE),0(R1)                                            
         OC    NODKEY,SPACES                                                    
         MVC   NODKEYSV,NODKEY                                                  
*                                                                               
CONCATX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO BUILD A PASSIVE POINTER KEY                                    
* AT ENTRY P1=A(OUTPUT)                                                         
***********************************************************************         
PASSKEY  NTR1                                                                   
         L     R2,0(R1)                                                         
         USING BUCRECD,R2                                                       
         L     R6,NDIOA                                                         
         USING BURECD,R6                                                        
         MVI   ELCODE,BUPTRELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUCKEY,BUPOINT-BUPTREL(R6) EXTRACT PASSIVE KEY                   
         L     R6,NDIOA                                                         
         MVC   BUCCTL,BURCTL       GET CONTROL VALUES FROM RECORD               
         LA    R3,NDLVTAB-NODBLKD(R5) R3=A(START OF LEVEL TABLE)                
         ZIC   R1,NDLEV                                                         
         MH    R1,=Y(NDLVTABL)     DEVELOP INDEX INTO LEVEL TABLE               
         LA    R3,0(R1,R3)         R3 POINTS TO LEVEL TABLE FOR OUTLINE         
         USING NDLVTABD,R3                                                      
         MVC   BUCDA,NDLVDA        EXTRACT DISK ADDRESS FOR LEVEL TABLE         
         CLC   BUCCODE,BUKCODE     TEST FOR MATCH ON OUTLINE CODES              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO HANDLE I/O ON NEW POINTER                                      
* AT ENTRY, P1=A(PASSIVE POINTER)                                               
***********************************************************************         
NEWPTR   NTR1                                                                   
         L     R2,0(R1)            R2=A(POINTER)                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'PASSVPTR),0(R2)                                            
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUKEY),KEYSAVE                                             
         BE    NEWPTR2                                                          
         MVC   KEY(L'PASSVPTR),0(R2)                                            
         GOTO1 ADD                                                              
         B     NEWPTRX                                                          
*                                                                               
NEWPTR2  MVC   KEY(L'PASSVPTR),0(R2)                                            
         GOTO1 WRITE                                                            
*                                                                               
NEWPTRX  NI    DMINBTS,X'FF'-X'08' TURN OFF PASS DELETES                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE LISTS THE JOB RECORDS                                            
***********************************************************************         
LRECS    NTR1                                                                   
         OI    OJTSEL1H+6,X'40'    POSITION CURSOR TO THIS FIELD                
         LA    R2,OJTSELLH         CLEAR LIST PORTION OF THE SCREEN             
         USING LINDSECT,R2                                                      
         TWAXC OJTSEL1H,LINJBNH,PROT=Y                                          
*                                                                               
         LA    R0,SVDJBKYS         CLEAR THE JOB KEYS TABLE                     
         LA    R1,NUMLINES*L'KEY                                                
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,OJTSEL1H         R2 = A(FIRST LIST LINE ON SCREEN)            
         LA    R4,SVDJBKYS         R4 = A(FIRST JOB KEY IN TABLE)               
*                                                                               
         TM    BITFLAG1,B1KEYCHG   START WITH A FRESH KEY?                      
         BNZ   LR10                                                             
*                                                                               
         OC    SVDJOBKY,SVDJOBKY   ANY KEY TO CONTINUE LIST FROM?               
         BZ    LR10                NO, START FROM BEGINNING                     
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'SVDJOBKY),SVDJOBKY   YES, USE LAST JOB KEY                 
         B     LRRDHI                                                           
*                                                                               
LR10     XC    PRVJOBCD,PRVJOBCD   CLEAR OUT PREVIOUS JOB CODE                  
         MVC   KEY,SPACES          SETUP JOB KEY FOR THE LIST                   
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODLDGR                                                
         ZIC   R1,LPRODCLT                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),PRODCLT                                                 
*                                                                               
         OC    PRODPRD,PRODPRD     ANY PRODUCT TO FILTER ON?                    
         BZ    LRRDHI                                                           
         ZIC   R1,LPRODCLT                                                      
         LA    RE,KEY+3(R1)                                                     
         ZIC   R1,LPRODPRD                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),PRODPRD     YES                                          
*                                                                               
LRRDHI   BAS   RE,ACCHIGH                                                       
         B     *+8                                                              
*                                                                               
LRRDSQ   BAS   RE,ACCSEQ                                                        
*                                                                               
         CLC   KEY(3),COMPANY      STILL IN SAME COMPANY AND LEDGER?            
         BNE   LRNOMORE            NO, DONE                                     
*                                                                               
         ZIC   R1,LPRODCLT                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PRODCLT(0),KEY+3    CLIENT STILL THE SAME?                       
         BNE   LRNOMORE            NO, DONE                                     
*                                                                               
         ZIC   R1,LPRODCLT         R3 = A(PRODUCT IN KEY)                       
         LA    R3,KEY+3(R1)                                                     
         ZIC   R1,LPRODPRD                                                      
         BCTR  R1,0                                                             
*                                                                               
         OC    PRODPRD,PRODPRD                                                  
         BNZ   LR20                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),SPACES      ANY PRODUCT?                                 
         BE    LRRDSQ              NONE, READ NEXT RECORD                       
*                                                                               
         EX    R1,*+8              SAME PRODUCT AS BEFORE?                      
         B     *+10                                                             
         CLC   PRVPRDCD(0),0(R3)                                                
         BE    LR30                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRVPRDCD(0),0(R3)                                                
         XC    PRVJOBCD,PRVJOBCD   NO, CLEAR OUT PREVIOUS JOB CODE              
         B     LR30                                                             
*                                                                               
LR20     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),PRODPRD     SAME PRODUCT?                                
         BNE   LRRDSQ              NO, READ NEXT RECORD                         
*                                                                               
LR30     ZIC   R1,LPRODPRD         R3 = A(JOB CODE IN KEY)                      
         AR    R3,R1                                                            
*                                                                               
         CLI   PRODMED,0           ANY MEDIA FILTER?                            
         BE    *+14                                                             
         CLC   PRODMED,0(R3)       YES, SEE IF SAME                             
         BNE   LRRDSQ              NOT THE SAME, CHECK THE NEXT JOB REC         
*                                                                               
         CLC   0(L'PRVJOBCD,R3),SPACES                                          
         BE    LRRDSQ                                                           
         CLC   PRVJOBCD,0(R3)                                                   
         BE    LRRDSQ                                                           
         MVC   PRVJOBCD,0(R3)                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AJOBELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LRFILTER                                                         
         USING AJOBELD,R6                                                       
         OC    PRODSTDT(L'PRODSTDT+L'PRODNDDT),PRODSTDT                         
         BZ    LRFILTER                                                         
         CLC   AJOBADATE,PRODSTDT                                               
         BL    LRRDSQ                                                           
         CLC   AJOBADATE,PRODNDDT                                               
         BH    LRRDSQ                                                           
         DROP  R6                                                               
*                                                                               
LRFILTER OC    QFILTER,QFILTER     ANY FILTERS TO CHECK AGAINST?                
         BZ    LRDISPLN            NO, DISPLAY THE JOB                          
*                                                                               
         OC    PRODPRD,PRODPRD     DO WE HAVE OUR PRODUCT FILTERS?              
         BNZ   LRFLTR20            YES                                          
*                                                                               
         MVC   SAVEDKEY,KEY        CHECK THE FILTERS                            
         MVC   QPRD,PRVPRDCD                                                    
         BAS   RE,VPRODPRD                                                      
         BNE   LRFLTRNO                                                         
         MVC   KEY,SAVEDKEY                                                     
         BAS   RE,ACCHIGH                                                       
*                                                                               
LRFLTR20 L     R6,AIO                                                           
         MVI   ELCODE,ARSTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LRRDSQ                                                           
         USING ARSTELD,R6                                                       
         MVC   JOBFILT1,ARSTFILT1   COPY THE JOB FILTERS                        
         MVC   JOBFILT2,ARSTFILT2                                               
         MVC   JOBFILT3,ARSTFILT3                                               
         MVC   JOBFILT4,ARSTFILT4                                               
         MVC   JOBFILT5,ARSTFILT5                                               
         DROP  R6                                                               
*                                                                               
         MVC   EFFFILTR,CLTFILTR   CALCULATE THE EFFECTIVE FILTER               
*                                                                               
         CLI   PRDFILT1,X'41'      SEE WHICH FILTER BYTE GETS REPLACED          
         BL    *+10                    BY A PRODUCT FILTER BYTE                 
         MVC   EFFFILT1,PRDFILT1                                                
         CLI   PRDFILT2,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFFILT2,PRDFILT2                                                
         CLI   PRDFILT3,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFFILT3,PRDFILT3                                                
         CLI   PRDFILT4,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFFILT4,PRDFILT4                                                
         CLI   PRDFILT5,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFFILT5,PRDFILT5                                                
*                                                                               
         CLI   JOBFILT1,X'41'      SEE WHICH FILTER BYTE GETS REPLACED          
         BL    *+10                    BY A JOB FILTER BYTE                     
         MVC   EFFFILT1,JOBFILT1                                                
         CLI   JOBFILT2,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFFILT2,JOBFILT2                                                
         CLI   JOBFILT3,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFFILT3,JOBFILT3                                                
         CLI   JOBFILT4,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFFILT4,JOBFILT4                                                
         CLI   JOBFILT5,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFFILT5,JOBFILT5                                                
*                                                                               
LRFLTR30 CLI   QFILTER1,C' '       CHECK AGAINST ENTERED FILTER                 
         BE    LRFLTR40                                                         
*                                                                               
         TM    QFILTER1,X'40'      NEGATIVE FILTER?                             
         BZ    LRFLTR35            YES                                          
         CLC   QFILTER1,EFFFILT1   NO, CHECK EFFECTIVE FILTER                   
         BNE   LRRDSQ                                                           
         B     LRFLTR40                                                         
*                                                                               
LRFLTR35 MVC   BYTE,QFILTER1       CHECK NEGATIVE FILTER                        
         OI    BYTE,X'40'                                                       
         CLC   BYTE,EFFFILT1                                                    
         BE    LRRDSQ              SKIP ANYTHING THAT MATCHES FILTER            
*                                                                               
LRFLTR40 CLI   QFILTER2,C' '                                                    
         BE    LRFLTR50                                                         
*                                                                               
         TM    QFILTER2,X'40'      NEGATIVE FILTER?                             
         BZ    LRFLTR45            YES                                          
         CLC   QFILTER2,EFFFILT2   NO, CHECK EFFECTIVE FILTER                   
         BNE   LRRDSQ                                                           
         B     LRFLTR50                                                         
*                                                                               
LRFLTR45 MVC   BYTE,QFILTER2       CHECK NEGATIVE FILTER                        
         OI    BYTE,X'40'                                                       
         CLC   BYTE,EFFFILT2                                                    
         BE    LRRDSQ              SKIP ANYTHING THAT MATCHES FILTER            
*                                                                               
LRFLTR50 CLI   QFILTER3,C' '                                                    
         BE    LRFLTR60                                                         
*                                                                               
         TM    QFILTER3,X'40'      NEGATIVE FILTER?                             
         BZ    LRFLTR55            YES                                          
         CLC   QFILTER3,EFFFILT3   NO, CHECK EFFECTIVE FILTER                   
         BNE   LRRDSQ                                                           
         B     LRFLTR60                                                         
*                                                                               
LRFLTR55 MVC   BYTE,QFILTER3       CHECK NEGATIVE FILTER                        
         OI    BYTE,X'40'                                                       
         CLC   BYTE,EFFFILT3                                                    
         BE    LRRDSQ              SKIP ANYTHING THAT MATCHES FILTER            
*                                                                               
LRFLTR60 CLI   QFILTER4,C' '                                                    
         BE    LRFLTR70                                                         
*                                                                               
         TM    QFILTER4,X'40'      NEGATIVE FILTER?                             
         BZ    LRFLTR65            YES                                          
         CLC   QFILTER4,EFFFILT4   NO, CHECK EFFECTIVE FILTER                   
         BNE   LRRDSQ                                                           
         B     LRFLTR70                                                         
*                                                                               
LRFLTR65 MVC   BYTE,QFILTER4       CHECK NEGATIVE FILTER                        
         OI    BYTE,X'40'                                                       
         CLC   BYTE,EFFFILT4                                                    
         BE    LRRDSQ              SKIP ANYTHING THAT MATCHES FILTER            
*                                                                               
LRFLTR70 CLI   QFILTER5,C' '                                                    
         BE    LRDISPLN                                                         
*                                                                               
         TM    QFILTER5,X'40'      NEGATIVE FILTER?                             
         BZ    LRFLTR75            YES                                          
         CLC   QFILTER5,EFFFILT5   NO, CHECK EFFECTIVE FILTER                   
         BNE   LRRDSQ                                                           
         B     LRDISPLN                                                         
*                                                                               
LRFLTR75 MVC   BYTE,QFILTER5       CHECK NEGATIVE FILTER                        
         OI    BYTE,X'40'                                                       
         CLC   BYTE,EFFFILT5                                                    
         BE    LRRDSQ              SKIP ANYTHING THAT MATCHES FILTER            
         B     LRDISPLN            ALL FILTERS PASSED, DISPLAY LINE             
*                                                                               
LRFLTRNO MVC   KEY,SAVEDKEY                                                     
         BAS   RE,ACCHIGH                                                       
         B     LRRDSQ                                                           
*                                                                               
LRDISPLN L     R6,AIO                                                           
         USING AACCKEY,R6                                                       
         MVC   SVDJOBKY(L'AACCKEY),AACCKEY                                      
         LA    R1,OJTLAST                                                       
         CR    R2,R1                                                            
         BNL   LR50                                                             
*                                                                               
         OI    LINSELH+4,X'20'     VALIDATE THIS LINE                           
         MVC   0(L'SVDJBKYS,R4),AACCKEY  SAVE THIS JOB KEY IN TABLE             
*                                                                               
         MVC   LINCLT,AACCKEY+3    DISPLAY INFORMATION ON THE LINE              
*                                                                               
         ZIC   R1,LPRODCLT         DISPLAY PRODUCT                              
         LA    R3,AACCKEY+3(R1)                                                 
         ZIC   R1,LPRODPRD                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINPRD(0),0(R3)                                                  
*                                                                               
         ZIC   R1,LPRODPRD         DISPLAY JOB CODE                             
         AR    R3,R1                                                            
         MVC   LINJCD,0(R3)                                                     
*                                                                               
         MVI   ELCODE,ANAMELQ      DISPLAY THE JOB NAME IF ANY                  
         BAS   RE,GETEL                                                         
         BNE   LRNXTLIN                                                         
         USING ANAMELD,R6                                                       
         ZIC   R1,ANAMLN                                                        
         SH    R1,=Y(ANAMLN1Q)                                                  
         CH    R1,=Y(L'LINJBN)                                                  
         BL    *+14                                                             
         MVC   LINJBN,ANAMEREC                                                  
         B     LRNXTLIN                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINJBN(0),ANAMEREC                                               
         DROP  R6                                                               
*                                                                               
LRNXTLIN LA    R2,LINNEXT                                                       
         LA    R4,L'SVDJBKYS(R4)                                                
         B     LRRDSQ                                                           
*                                                                               
LRNOMORE XC    SVDJOBKY,SVDJOBKY                                                
*                                                                               
LR50     LA    R2,OJTSEL1H         R2 = A(FIRST SELECT FIELD)                   
         LA    R3,SVDJBKYS         R3 = A(FIRST KEY IN THE LIST)                
*                                                                               
LR60     LA    R4,OJTSELLH         R4 = A(LAST SELECT FIELD)                    
*                                                                               
         CR    R2,R4               PASSED THE LIST SCREEN ALREADY?              
         BH    LRX                 YES, DISPLAY THE JOBS                        
*                                                                               
         OC    0(L'SVDJBKYS,R3),0(R3)   ANY KEY IN THIS LINE?                   
         BZ    LRX                 NO MORE IN LIST                              
*                                                                               
         LA    R6,3(R3)            GET CLT CODE FROM PRODUCTION KEY             
         ZIC   R1,LPRODCLT                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JOBCLTCD(0),0(R6)                                                
*                                                                               
         ZIC   R0,LPRODCLT         GET PRD CODE FROM PRODUCTION KEY             
         AR    R6,R0                                                            
         IC    R1,LPRODPRD                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JOBPRDCD(0),0(R6)                                                
*                                                                               
         IC    R0,LPRODPRD         GET JOB CODE FROM PRODUCTION KEY             
         AR    R6,R0                                                            
         MVC   JOBJOBCD,0(R6)                                                   
*                                                                               
         LA    R1,JOBJOBCD+L'JOBJOBCD-1    POINT TO LAST CHAR IN CODE           
LR70     CLI   0(R1),C' '                                                       
         BNE   LR80                                                             
         MVI   0(R1),0                                                          
         BCTR  R1,0                                                             
         B     LR70                                                             
*                                                                               
LR80     XC    OUTCODE,OUTCODE                                                  
         MVC   OUTCODE(L'JOBJOBCD),JOBJOBCD                                     
*                                                                               
         BAS   RE,CKOUTCOD         OUTLINE CODE EXISTS ALREADY?                 
         BE    LR90                NO                                           
         MVI   8(R2),C'*'          MARK THIS LINE                               
         OI    6(R2),X'A0'         PROTECT AND TRANSMIT THIS FIELD              
*                                                                               
LR90     AH    R2,=Y(LINNEXT-LINDSECT)   POINT TO THE NEXT LINE                 
         LA    R3,L'SVDJBKYS(R3)                                                
         B     LR60                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE COMPANY CODE AND THE PRODUCT LEDGER                     
***********************************************************************         
*                                                                               
GETCMPNY NTR1                                                                   
         XC    KEY,KEY             LOOKUP COMPANY CODE IN ID REC                
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         OC    CT5KALPH,SPACES                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
GCPYDIE  DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING CT5REC,R6                                                        
         CLC   CT5KEY,KEY                                                       
         BNE   GCPYDIE                                                          
*                                                                               
         MVC   DATADISP,=Y(CT5DATA-CT5REC)                                      
         MVI   ELCODE,CTSYSELQ                                                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GCPY10   BAS   RE,NEXTEL                                                        
         BNE   GCPYDIE                                                          
*                                                                               
         USING CTSYSD,R6                                                        
         CLI   CTSYSNUM,X'06'      ACC SYSTEM?                                  
         BNE   GCPY10                                                           
         MVC   COMPANY,CTSYSAGB                                                 
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SPACES          GET COMPANY RECORD                           
         MVC   KEY(L'COMPANY),COMPANY                                           
         GOTO1 ACCHIGH                                                          
*                                                                               
         CLC   KEY(L'AACCKEY),KEYSAVE   SAME COMPANY?                           
         BNE   GCPYDIE             NO, BAD COMPANY                              
*                                                                               
         L     R6,AIO              GET THE COMPANY ELEMENT                      
         MVI   ELCODE,ACPYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GCPYDIE                                                          
*                                                                               
         USING ACPYELD,R6                                                       
         MVC   PRODLDGR,ACPYPROD                                                
         DROP  R6                                                               
*                                                                               
         MVC   KEY+1(L'PRODLDGR),PRODLDGR   VALIDATE THE PROD LEDGER            
         GOTO1 ACCHIGH                                                          
*                                                                               
         CLC   KEY(L'AACCKEY),KEYSAVE                                           
         BNE   GCPYDIE                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AACLELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GCPYDIE                                                          
*                                                                               
         USING AACLELD,R6                                                       
         MVC   LPRODCLT,AACLVLEN   SAVE CLIENT & PRODUCT LENGTHS                
         ZIC   R1,AACLVLEN                                                      
         ZIC   R0,AACLVLEN+L'AACLVALS                                           
         SR    R0,R1                                                            
         STC   R0,LPRODPRD                                                      
         DROP  R6                                                               
*                                                                               
GCPYX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE PRODUCTION CLIENT FIELD                            
*                                                                               
* ON ENTRY:    PRODCLT             PRODUCTION CLIENT TO BE VALIDATED            
*              LPRODCLT            PRODUCTION CLIENT LENGTH                     
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = VALID CLIENT CODE                       
*                                  NE = INVALID CLIENT CODE                     
***********************************************************************         
*                                                                               
VPRODCLT NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(L'PRODLDGR),PRODLDGR                                       
*                                                                               
         ZIC   R1,LPRODCLT         VALIDATE THE CLIENT                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),PRODCLT                                                 
*                                                                               
         BAS   RE,ACCHIGH                                                       
*                                                                               
         CLC   KEY(L'AACCKEY),KEYSAVE                                           
         BNE   VPCLTNO                                                          
*                                                                               
         L     R6,AIO                                                           
         XC    ACCLTNAM,ACCLTNAM                                                
         MVI   ELCODE,ANAMELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   VPCLT10                                                          
         USING ANAMELD,R6                                                       
         ZIC   R1,ANAMLN                                                        
         SH    R1,=Y(ANAMLN1Q)                                                  
         BZ    VPCLT10                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACCLTNAM(0),ANAMEREC                                             
         DROP  R6                                                               
*                                                                               
VPCLT10  L     R6,AIO                                                           
         MVI   ELCODE,ARSTELQ                                                   
         BAS   RE,GETEL                                                         
         USING ARSTELD,R6                                                       
         MVC   CLTFILT1,ARSTFILT1   COPY THE CLIENT FILTERS                     
         MVC   CLTFILT2,ARSTFILT2                                               
         MVC   CLTFILT3,ARSTFILT3                                               
         MVC   CLTFILT4,ARSTFILT4                                               
         MVC   CLTFILT5,ARSTFILT5                                               
         DROP  R6                                                               
*                                                                               
VPCLTYES B     EQXIT                                                            
*                                                                               
VPCLTNO  B     NEQXIT                                                           
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE PRODUCTION PRODUCT FIELD                           
*                                                                               
* ON ENTRY:    QPRD                PRODUCTION PRODUCT TO BE VALIDATED           
*              LPRODPRD            PRODUCTION PRODUCT LENGTH                    
*                                                                               
* ON EXIT:     CONDITION CODE      EQ = VALID PRODUCT CODE                      
*                                  NE = INVALID PRODUCT CODE                    
***********************************************************************         
*                                                                               
VPRODPRD NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(L'PRODLDGR),PRODLDGR                                       
*                                                                               
         ZIC   R1,LPRODCLT         SET UP CLIENT IN KEY                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),PRODCLT                                                 
*                                                                               
         LA    R2,KEY+3            VALIDATE THE PRODUCT                         
         ZIC   R1,LPRODCLT                                                      
         AR    R2,R1                                                            
         ZIC   R1,LPRODPRD         VALIDATE THE CLIENT                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),QPRD                                                     
*                                                                               
         BAS   RE,ACCHIGH                                                       
*                                                                               
         CLC   KEY(L'AACCKEY),KEYSAVE                                           
         BNE   VPPRDNO                                                          
*                                                                               
         L     R6,AIO                                                           
         XC    ACPRDNAM,ACPRDNAM                                                
         MVI   ELCODE,ANAMELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   VPPRD10                                                          
         USING ANAMELD,R6                                                       
         ZIC   R1,ANAMLN                                                        
         SH    R1,=Y(ANAMLN1Q)                                                  
         BZ    VPPRD10                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACPRDNAM(0),ANAMEREC                                             
         DROP  R6                                                               
*                                                                               
VPPRD10  L     R6,AIO                                                           
         MVI   ELCODE,ARSTELQ                                                   
         BAS   RE,GETEL                                                         
         USING ARSTELD,R6                                                       
         MVC   PRDFILT1,ARSTFILT1   COPY THE PRODUCT FILTERS                    
         MVC   PRDFILT2,ARSTFILT2                                               
         MVC   PRDFILT3,ARSTFILT3                                               
         MVC   PRDFILT4,ARSTFILT4                                               
         MVC   PRDFILT5,ARSTFILT5                                               
         DROP  R6                                                               
*                                                                               
VPPRDYES B     EQXIT                                                            
*                                                                               
VPPRDNO  B     NEQXIT                                                           
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE FILTER FIELD                                       
*                                                                               
* ON ENTRY:    (R2)                A(FILTER FIELD HEADER)                       
*              P1                  A(OUTPUT)                                    
*              P2                  N'OUTPUT POSITIONS                           
***********************************************************************         
VALFIL   NTR1  ,                                                                
         L     R3,0(R1)            R3=A(OUTPUT)                                 
         L     R5,4(R1)            R5=N'OUTPUT POSITIONS                        
         LA    R4,8(R2)            R4=A(INPUT)                                  
         ZIC   R6,5(R2)                                                         
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VALFIL2                                                          
         MVC   0(0,R3),SPACES      PRE-CLEAR OUTPUT TO SPACES                   
*                                                                               
VALFIL2  CLI   0(R4),C'*'          TEST FOR NEGATIVE FILTER                     
         BE    VALFIL4             YES                                          
         CLI   0(R4),C' '          TEST ANY FILTER IN POSITION                  
         BE    VALFIL6             YES                                          
         CLI   0(R4),C'.'          TEST FOR NO FILTER VALUE                     
         BE    VALFIL3                                                          
         LA    R0,1                                                             
         GOTO1 TSTAN,(R4)                                                       
*                                                                               
VALFIL3  MVC   0(1,R3),0(R4)       SET VALUE                                    
         B     VALFIL6                                                          
*                                                                               
VALFIL4  LA    R4,1(R4)            NEXT INPUT CHARACTER                         
         SH    R6,=H'1'                                                         
         BZ    INVLFLD                                                          
         LA    R0,1                                                             
         GOTO1 TSTAN,(R4)                                                       
         MVC   0(1,R3),0(R4)                                                    
         NI    0(R3),X'FF'-X'40'   TURN OFF UPPER CASE BIT                      
*                                                                               
VALFIL6  LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         SH    R6,=H'1'                                                         
         BZ    VALFILX             NO MORE INPUT TO EDIT                        
         BCT   R5,VALFIL2                                                       
         B     INVLFLD             EXCEEDED FILTER LIMIT                        
*                                                                               
VALFILX  B      XIT                                                             
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO CHECK FOR ALPHANUMERIC FIELD                                   
*                                                                               
* AT ENTRY, R0=N'BYTES TO CHECK, R1=A(STRING TO CHECK)                          
***********************************************************************         
TSTAN    ST    RE,SAVERE                                                        
*                                                                               
TSTAN1   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    INVLFLD                                                          
         B     TSTAN2                                                           
*                                                                               
         CLI   0(R1),C'A'                                                       
         BL    INVLFLD                                                          
         CLI   0(R1),C'I'                                                       
         BNH   TSTAN2              CHARACTER IS BETWEEN A-I                     
         CLI   0(R1),C'J'                                                       
         BL    INVLFLD                                                          
         CLI   0(R1),C'R'                                                       
         BNH   TSTAN2                                                           
         CLI   0(R1),C'S'                                                       
         BL    INVLFLD                                                          
         CLI   0(R1),C'Z'                                                       
         BH    INVLFLD                                                          
*                                                                               
TSTAN2   LA    R1,1(R1)            NEXT CHARACTER IN FIELD                      
         BCT   R0,TSTAN1                                                        
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DATAMGR ACC ROUTINES                                                          
***********************************************************************         
*                                                                               
ACCHIGH  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     ACCDRCT                                                          
*                                                                               
ACCSEQ   NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
*                                                                               
ACCDRCT  DS    0H                                                               
         MVC   DATADISP,=Y(AACCRFST-AACCRECD)                                   
*                                                                               
         CLC   =C'ACC',CURRSYST    IN THE ACC SYSTEM ALREADY?                   
         BE    ACCD10                                                           
         L     R1,ACOMFACS         NO, SWITCH TO THE ACC SYSTEM                 
         L     RF,CSWITCH-COMFACSD(R1)                                          
         GOTO1 (RF),DMCB,=C'ACC',0                                              
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURRSYST,=C'ACC'                                                 
*                                                                               
ACCD10   MVC   DMFILE,=CL8'ACCDIR'                                              
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,AIO                    
*                                                                               
         L     R6,AIO                                                           
         USING AACCKEY,R6                                                       
         MVC   KEY(L'AACCKEY),AACCKEY                                           
         MVC   KEYSTATS,AACCKSTA                                                
         MVC   KEYDSKAD,AACCKDA                                                 
         DROP  R6                                                               
*                                                                               
         TM    DMCB+8,X'02'        RECORD DELETED?                              
         BNZ   ACCD20                                                           
         CLI   DMCB+8,0            NO, SOME OTHER ERROR?                        
         BE    ACCD20                                                           
         DC    H'0'                    YES, THEN DIE                            
*                                                                               
ACCD20   MVC   DMFILE,=CL8'ACCMST'                                              
         MVC   COMMAND,=CL8'GETREC'                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEYDSKAD,AIO,     X        
               DMWORK                                                           
*                                                                               
         TM    DMCB+8,X'02'        RECORD DELETED?                              
         BNZ   ACCFX                                                            
         CLI   DMCB+8,0            NO, SOME OTHER ERROR?                        
         BE    ACCFX                                                            
         DC    H'0'                    YES, THEN DIE                            
*                                                                               
ACCFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATAMGR MPL ROUTINES                                                          
***********************************************************************         
*                                                                               
MPLHIGH  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     MPLDRCT                                                          
*                                                                               
MPLDRCT  DS    0H                                                               
         MVC   DATADISP,=Y(BUFRSTEL-BUKEY)                                      
*                                                                               
         CLC   =C'MPL',CURRSYST    IN THE MPL SYSTEM ALREADY?                   
         BE    MPLD10                                                           
         L     R1,ACOMFACS         NO, SWITCH TO THE MPL SYSTEM                 
         L     RF,CSWITCH-COMFACSD(R1)                                          
         GOTO1 (RF),DMCB,=C'MPL',0                                              
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURRSYST,=C'MPL'                                                 
*                                                                               
MPLD10   MVC   DMFILE,=CL8'BUDDIR'                                              
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,AIO                    
*                                                                               
         L     R6,AIO              SAVE THE KEY WE GOT                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUCKEY),0(R6)                                              
*                                                                               
         TM    DMCB+8,X'02'        RECORD DELETED?                              
         BNZ   MPLFX                                                            
         CLI   DMCB+8,0            NO, SOME OTHER ERROR?                        
         BE    MPLFX                                                            
         DC    H'0'                    YES, THEN DIE                            
*                                                                               
MPLFX    B     XIT                                                              
         EJECT                                                                  
* ERROR ROUTINES                                                                
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
DUPLCODE MVI   ERROR,DUPCDERR      DUPLICATE OUTLINE CODE                       
         B     TRAPERR                                                          
*                                                                               
* COMMON ROUTINES                                                               
*                                                                               
EQXIT    SR    RC,RC               RETURN TO CALLER WITH CC=EQ                  
NEQXIT   LTR   RC,RC               RETURN TO CALLER WITH CC=NEQ                 
XIT      XIT1                                                                   
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
RELO     DS    A                                                                
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
COPYMSG  DC    C'COPY COMPLETED'                                                
NEXTMSG  DC    C'COPY CONTINUING-TYPE IN NEW CODES FOR OUTLINES BELOW'          
FIRSTMSG DC    C'OUTLINES DISPLAYED - TYPE IN NEW CODES'                        
UNTILMSG DC    C'** XX OUTLINES READ - DID NOT FIND COPY UNTIL OUTLINE X        
               **'                                                              
CHGMSG   DC    C'**FROM OUTLINES HAVE BEEN CHANGED - COPY STOPPED**'            
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER OUTLINE CREATE FROM JOB SCREEN                                 
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILB5D                                                       
         EJECT                                                                  
* DSECT TO COVER THE LIST OF PRODUCTION JOBS                                    
*                                                                               
LINDSECT DSECT                                                                  
LINSELH  DS    CL(L'OJTSEL1H)                                                   
LINSEL   DS    CL(L'OJTSEL1)                                                    
LINCLTH  DS    CL(L'OJTCLT1H)                                                   
LINCLT   DS    CL(L'OJTCLT1)                                                    
LINPRDH  DS    CL(L'OJTPRD1H)                                                   
LINPRD   DS    CL(L'OJTPRD1)                                                    
LINJCDH  DS    CL(L'OJTJCD1H)                                                   
LINJCD   DS    CL(L'OJTJCD1)                                                    
LINJBNH  DS    CL(L'OJTJBN1H)                                                   
LINJBN   DS    CL(L'OJTJBN1)                                                    
LINNEXT  DS    0C                                                               
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
VEDRULES DS    V                                                                
*                                                                               
CTLVALS  DS    0C                                                               
CLT      DS    CL3                                                              
PRD      DS    CL3                                                              
PLAN     DS    CL3                                                              
ATTACH   DS    CL(L'BUKCODE)                                                    
BEFORE   DS    CL(L'BUKCODE)                                                    
CTLVALN1 EQU   *-CTLVALS           CONTROL LENGTH EXCLUDING ASSIGN              
CTLVALLN EQU   *-CTLVALS           CONTROL FIELDS LENGTH                        
*                                                                               
CURRSYST DS    CL3                 CURRENT SYSTEM                               
COMPANY  DS    XL1                 COMPANY CODE                                 
PRODLDGR DS    CL2                 PRODUCT LEDGER                               
LPRODCLT DS    XL1                 LENGTH OF PRODUCTION CLIENT CODE             
LPRODPRD DS    XL1                                      PRODUCT CODE            
PRODCLT  DS    CL6                 PRODUCTION CLIENT CODE                       
PRODPRD  DS    CL6                            PRODUCT CODE                      
PRODMED  DS    CL1                            MEDIA                             
PRODSTDT DS    PL3                 DATE ADDED START DATE                        
PRODNDDT DS    PL3                            END DATE                          
*                                                                               
QPRD     DS    CL6                 VPRODPRD'S PRODUCT CODE                      
*                                                                               
ACCLTNAM DS    CL36                PRODUCTION CLIENT NAME                       
ACPRDNAM DS    CL36                PRODUCTION PRODUCT NAME                      
*                                                                               
CLTFILTR DS    0CL5                PRODUCTION CLIENT FILTER                     
CLTFILT1 DS    C                                                                
CLTFILT2 DS    C                                                                
CLTFILT3 DS    C                                                                
CLTFILT4 DS    C                                                                
CLTFILT5 DS    C                                                                
*                                                                               
PRDFILTR DS    0CL5                PRODUCTION PRODUCT FILTER                    
PRDFILT1 DS    C                                                                
PRDFILT2 DS    C                                                                
PRDFILT3 DS    C                                                                
PRDFILT4 DS    C                                                                
PRDFILT5 DS    C                                                                
*                                                                               
JOBFILTR DS    0CL5                PRODUCTION JOB FILTER                        
JOBFILT1 DS    C                                                                
JOBFILT2 DS    C                                                                
JOBFILT3 DS    C                                                                
JOBFILT4 DS    C                                                                
JOBFILT5 DS    C                                                                
*                                                                               
EFFFILTR DS    0CL5                EFFECTIVE FILTERS                            
EFFFILT1 DS    C                                                                
EFFFILT2 DS    C                                                                
EFFFILT3 DS    C                                                                
EFFFILT4 DS    C                                                                
EFFFILT5 DS    C                                                                
*                                                                               
QFILTER  DS    0CL5                ENTERED FILTERS                              
QFILTER1 DS    C                                                                
QFILTER2 DS    C                                                                
QFILTER3 DS    C                                                                
QFILTER4 DS    C                                                                
QFILTER5 DS    C                                                                
*                                                                               
PRVPRDCD DS    CL6                 PREVIOUS PRODUCT CODE                        
PRVJOBCD DS    CL6                          JOB CODE                            
*                                                                               
JOBCLTCD DS    CL3                 JOB'S CLIENT CODE                            
JOBPRDCD DS    CL3                 JOB'S PRODUCT CODE                           
JOBJOBCD DS    CL6                 JOB'S JOB CODE                               
*                                                                               
SAVOUTEL DS    CL(BUOUTLNQ)        OUTLINE ELEMENT SAVE AREA                    
*                                                                               
SAVEDKEY DS    XL(L'KEY)                                                        
KEYSTATS DS    XL(L'AACCKSTA)       THE KEY'S STATUS BYTES                      
KEYDSKAD DS    XL(L'AACCKDA)                  DISK ADDRESS                      
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER                            
FAKEFLD  DS    CL80                           DATA                              
*                                                                               
BITFLAG1 DS    X                   FIRST SET OF BIT FLAGS                       
B1KEYCHG EQU   X'80'               A KEY FIELD HAS BEEN CHANGED                 
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE BLOCK                         
*                                                                               
LEVADJ   DS    H                   LEVEL ADJUSTMENT ATTACH/FROM LEVEL           
FRLEV    DS    X                   FROM OUTLINE'S LEVEL                         
*                                                                               
NODKEY2  DS    CL(L'NODKEY)        SECOND NODAL KEY                             
PASSVPTR DS    CL(BUFRSTEL-BUKEY)  PASSIVE POINTER                              
OUTNUM   DS    F                   START NUMBER FOR OUTLINE CODE                
NOUTS    DS    H                   N'OUTLINES ON PLAN                           
LOWLEV   DS    X                   LOWEST LEVEL                                 
NRECS    DS    X                                                                
RECLIM   DS    X                                                                
UNTILSW  DS    C                   Y=UNTIL RECORD FOUND                         
RULSW    DS    C                   Y/N-RULE ELEMENTS ON RECORD                  
*                                                                               
ANODBLK2 DS    A                   A(SECOND NODIO BLOCK)                        
ATHISENT DS    A                   A(THIS OUTLINE TABLE ENTRY)                  
*                                                                               
NEWCODE  DS    CL(L'BUKCODE)                                                    
NEWNAME  DS    CL(L'BUOUTNAM)                                                   
AFTER    DS    CL(L'BUKCODE)                                                    
ATTKEY   DS    CL(L'NODKEY)                                                     
NEXTNODE DS    CL(L'BUKNODE)                                                    
NEXTLEV  DS    X                                                                
*                                                                               
OUTTAB   DS    (NUMLINES)CL(L'BUKCODE)                                          
*                                                                               
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 2                                                                
         ORG   TWA1USER                                                         
SVCTLVAL DS    0CL(CTLVALLN)                                                    
SVCLT    DS    CL3                                                              
SVPRD    DS    CL3                                                              
SVPLAN   DS    CL3                                                              
SVATTACH DS    CL(L'BUKCODE)                                                    
SVBEFORE DS    CL(L'BUKCODE)                                                    
SVFROM   DS    CL(L'BUKCODE)                                                    
SVUNTIL  DS    CL(L'BUKCODE)                                                    
SVASSIGN DS    C                                                                
SVRULES  DS    C                                                                
*                                                                               
SVPLNKEY DS    CL(L'NODKEY)        PLAN KEY                                     
SVPARKEY DS    CL(L'NODKEY)        PARENT KEY                                   
SVDJOBKY DS    CL(L'AACCKEY)       LAST JOB KEY USED                            
SVDJBKYS DS    (NUMLINES)CL(L'AACCKEY)   JOB KEYS LISTED ON SCREEN              
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
NUMLINES EQU   (OJTLAST-OJTSEL1H)/(LINNEXT-LINDSECT)                            
MAXMAN   EQU   50                  MAXIMUM SIZE FOR MANUAL COPY                 
         SPACE 2                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
* ACGENFILE    ** USES A PREFIX OF 'A'  ,  ACCKEY   -->   AACCKEY               
*                                                                               
         PRINT OFF                                                              
*PREFIX=A                                                                       
       ++INCLUDE ACGENFILE                                                      
*PREFIX=                                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135BUFIL35   05/01/02'                                      
         END                                                                    
