*          DATA SET ACCTA05    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T61E05A                                                                  
*INCLUDE RIGHT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCTA05 -- CTA PURCHASE ORDERS                       *         
*                                                                     *         
*  COMMENTS:     MAINTAINS CTA PURCHASE ORDER RECORDS                 *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61E00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCTAF5 (HEADER)                             *         
*                        ACCTAF6 (DETAIL)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED CTA PURCHASE ORDER RECS                      *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- SYSSPARE                                       *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T61E05 - CTA PURCHASE ORDERS'                                   
T61E05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1E05**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61EFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         APPLICATION SAVED STORAGE                    
         USING STORED,R5                                                        
*                                                                               
         ST    R3,RELO                                                          
         ST    RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(SETUP),DMCB,RR=RELO                                           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY - SELECT                         
         BE    DK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    RDEL                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT PO REPORT                              
         BNE   XIT                                                              
         GOTO1 =A(PR),DMCB,RR=RELO                                              
         B     DR                  DISPLAY RECORD WHEN DONE                     
         EJECT                                                                  
***********************************************************************         
*        VALKEY - VALIDATE ORDER NUMBER                               *         
***********************************************************************         
*                                                                               
VK       XC    ORDNUM,ORDNUM                                                    
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK05                                                             
         BAS   RE,VKLIST           VALKEY FOR LIST                              
         B     VKX                                                              
*                                                                               
VK05     LA    R2,OHDORDNH                                                      
         ZICM  R1,OHDORDNH+5,1     CHECK FOR AUTO NUM                           
         BZ    ERRMISS                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OHDORDN(0),=CL6'AUTO'                                            
         BNE   VK20                                                             
         CLI   RECNUM,RTDET        NOT VALID FOR DETAIL REC                     
         BE    ERRINV                                                           
         CLI   ACTEQU,ACTADD       ONLY GOOD FOR ADD                            
         BNE   ERRINV                                                           
         OI    POSTAT,POSAUTO                                                   
         GOTO1 GETPO#              GET NEXT NUMBER FORM CONTROL REC             
         BNE   ERRINV                                                           
         MVC   ORDNUM,NXTPONUM                                                  
         MVC   OHDORDN,ORDNUM      RE-DISPLAY                                   
         OI    OHDORDNH+6,X'80'                                                 
         B     VK50                                                             
*                                                                               
VK20     LA    R3,OHDORDN                                                       
         ZIC   R1,OHDORDNH+5                                                    
         SH    R1,=H'1'                                                         
         TM    OHDORDNH+4,X'08'    FIELD IS VALID NUMERIC                       
         BO    VK25                                                             
         LTR   R1,R1                                                            
         BZ    ERINVORF            JUST A LETTER                                
         MVC   WORK(5),=5X'F0'     TEST REST FOR NUMERIC                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),OHDORDN+1                                                
         CLC   WORK(5),=5X'F0'                                                  
         BNE   ERINVORF            INVALID ORDER NUMBER FORMAT                  
         LA    R3,OHDORDN+1                                                     
*                                                                               
VK25     EX    R1,*+8              PACK NUMERIC PART                            
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'0'            ONLY 0 IS INVALID                            
         BNE   VK28                                                             
         TM    OHDORDNH+4,X'08'    FIELD IS VALID NUMERIC                       
         BO    ERINVORF                                                         
VK28     EDIT  (R1),(6,ORDNUM),FILL=0                                           
         TM    OHDORDNH+4,X'08'    FIELD IS VALID NUMERIC                       
         BO    *+10                                                             
         MVC   ORDNUM(1),OHDORDN   FIRST CHAR CAN BE ANYTHING                   
         MVC   OHDORDN,ORDNUM      RE-DISPLAY                                   
         OI    OHDORDNH+6,X'80'                                                 
         CLI   RECNUM,RTDET        RESET AUTO NUM IF NOT USED                   
         BE    *+8                                                              
         NI    POSTAT,X'FF'-POSAUTO                                             
         EJECT                                                                  
***********************************************************************         
*        VALKEY - SEE IF ORDER REC EXISTS                             *         
***********************************************************************         
*                                                                               
         USING ORDRECD,R6                                                       
VK50     LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,CMPY                                                     
         MVC   ORDKORD,ORDNUM                                                   
         MVC   SAVEKEY,BIGKEY                                                   
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ORDKEY),KEYSAVE                                         
         BNE   VK70                                                             
         TM    ORDKSTA,ORDSCON     CONTRACT ORDER                               
         BNO   ERNOTCON            RECORD NOT A CONTRACT ORDER                  
         CLI   ACTEQU,ACTADD       IF ADDING                                    
         BE    ERRECXS             RECORD EXISTS                                
*                                                                               
         TM    ORDKSTA,X'80'+ORDSLDEL   X'20' RECORD IS DELETED                 
         BZ    VK52                                                             
         CLI   ACTEQU,ACTREST      ONLY VALID FOR RESTORE                       
         BNE   ERRINV              RECORD MARKED FOR DELETION                   
         B     VK100                                                            
*                                                                               
VK52     CLI   ACTEQU,ACTREST      PLAIN REC NOT VALID FOR RESTORE              
         BE    ERRINV              RECORD NOT DELETED                           
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         CLI   ACTEQU,ACTDEL       DELETE?                                      
         BNE   VK100                                                            
         TM    ORDKSTA,ORDSFMCH    X'40' CLOSED - FULLY MATCHED                 
         BO    ERRCONCL            THEN CAN'T DELETE                            
         BAS   RE,OKTODEL                                                       
         B     VK100                                                            
*                                                                               
VK70     CLI   ACTEQU,ACTADD       RECORD NOT FOUND IF NOT ADD                  
         BNE   ERRECNF             RNF                                          
         L     RE,AIO3             SET UP FOR ADD IN AIO3                       
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R6,AIO3                                                          
         MVC   0(L'ORDKEY,R6),SAVEKEY                                           
         OI    ORDRSTAT,ORDSCON    ORDER IS A CONTRACT ORDER                    
*                                                                               
VK100    NI    POSTAT,X'FF'-POSKEYCH                                            
         CLC   SVORDNUM,ORDNUM     SAME ORDER NUMBER                            
         BE    *+12                                                             
         NI    POSTAT,X'FF'-POSHEAD     HEADER NOT VALIDATED                    
         OI    POSTAT,POSKEYCH          KEY HAS CHANGED                         
         MVC   SVORDNUM,ORDNUM                                                  
VKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST VALKEY - VALIDATE SYSTEM/MEDIA AND CONTRACTOR                     
***********************************************************************         
*                                                                               
VKLIST   NTR1                                                                   
         LA    R2,ORLMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    ERRPLS                                                           
         CLI   5(R2),2             MUST BE TWO CHARACTERS                       
         BNE   ERRMEDIA                                                         
         CLI   8(R2),C'S'          JUST SPOT FOR NOW                            
         BNE   ERRMEDIA                                                         
         MVC   QSYS,8(R2)          1ST CHAR IS SYSTEM                           
         MVC   QMED,9(R2)          2ND CHAR IS MEDIA                            
         GOTO1 SPTSYS              SWITCH TO SPOT SYSTEM                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VALMED              VALIDATE SPOT MEDIA                          
         BNE   ERRMEDIA                                                         
*                                                                               
         LA    R2,ORLCNTRH         VALIDATE SPOT CONTRACTOR                     
         CLI   5(R2),0                                                          
         BE    ERRPLS              REQ'D                                        
         MVC   CONTRCTR,ORLCNTR    FOR VALIDATION                               
         OC    CONTRCTR,SPACES                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 VALCNTR                                                          
         BNE   ERINVCTR            INVALID CONTRACTOR                           
         MVC   AIO,AIO1                                                         
         MVC   ORLCNME,CNTRNAME    DISPLAY NAME                                 
         OI    ORLCNMEH+6,X'80'                                                 
         MVC   SVCTRCTR,CONTRCTR   SAVE CONTRACTOR                              
         EJECT                                                                  
***********************************************************************         
*        LIST VALKEY - VALIDATE CONTRACT AND CATEGORY FILTERS                   
***********************************************************************         
*                                                                               
         XC    PCON#,PCON#         PACKED CONTRACT NUMBER                       
         MVC   CON#,SPACES                                                      
         LA    R2,ORLCONTH         VALIDATE SPOT CONTRACT NUMBER                
         CLI   5(R2),0             NOT REQ'D                                    
         BE    VKL20                                                            
         CLC   =C'ALL',8(R2)       ALL CONTRACTS                                
         BE    VKL20                                                            
         MVC   AIO,AIO2            READ SPOT CONTRACT RECORD INTO AIO2          
         GOTO1 VALCON#                                                          
         MVC   AIO,AIO1                                                         
         BNE   ERINVCON            INVALID CONTRACT                             
         CLC   SVCTRCTR,CONTRCTR   MAKE SURE CONTRACT/CONTRACTOR MATCH          
         BNE   ERINVCC             CONTRACT NOT VALID FOR CONTRACTOR            
*                                                                               
VKL20    DS    0H                                                               
         XC    CATFILT,CATFILT     CATEGORY FILTER ON LIST                      
         LA    R2,ORLCATGH         CATEGORY                                     
         CLI   5(R2),0                                                          
         BE    VKL50                                                            
         GOTO1 VALCATG             VALIDATE CATEGORY                            
         BNE   ERINVCTG            INVALID CATEGORY                             
         MVC   CATFILT,CATEGORY                                                 
         EJECT                                                                  
***********************************************************************         
*        LIST VALKEY - BUILD KEY                                                
***********************************************************************         
*                                                                               
VKL50    GOTO1 ACCSYS              SWITCH BACK TO ACC                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ORDRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,CMPY                                                     
         MVC   ORDKORD,ORDNUM                                                   
         MVC   SAVEKEY,BIGKEY                                                   
*        MVC   AIO,AIO3                                                         
VKLX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        RECORD DELETE                                                *         
***********************************************************************         
*                                                                               
RDEL     BAS   RE,OKTODEL                                                       
         B     XIT                                                              
***********************************************************************         
*        OK TO DELETE RECORD = NO X'94' ORDER ITEMS                             
***********************************************************************         
*                                                                               
OKTODEL  NTR1                                                                   
         LA    R2,CONACTH                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,CPOELQ       X'94'                                        
         BAS   RE,GETEL                                                         
         BE    ERRDELOR            CANNOT DELETE ORDER - DELETE ITEMS           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY KEY                                                  *         
***********************************************************************         
*                                                                               
         USING ORDRECD,R6                                                       
DK       LA    R6,BIGKEY                                                        
         MVC   OHDORDN,ORDKORD     ORDER NUMBER                                 
         OI    OHDORDNH+6,X'80'                                                 
         MVC   SAVEKEY,BIGKEY                                                   
         L     RE,AIO3             MOVE REC TO AIO3                             
         L     RF,SIZEIO                                                        
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                              *         
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
         CLI   ACTEQU,ACTDIS       DISPLAY ONLY                                 
         BE    DR                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VR02                                                             
         TM    TRANSTAT,RCHANG     RECORD CHANGE                                
         BO    DR                                                               
         TM    SCRSTAT,SCRCHG      SCREEN CHANGE                                
         BO    DR                                                               
         TM    POSTAT,POSKEYCH     KEY CHANGE                                   
         BO    DR                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         MVC   AIO,AIO3            UPDATE REC IN AIO3                           
         GOTO1 GETREC                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD - DETAIL SCREEN                              *         
***********************************************************************         
*                                                                               
VR02     CLI   RECNUM,RTDET        DETAIL RECORD                                
         BNE   VRHEADER                                                         
         CLI   ACTEQU,ACTADD                                                    
         BNE   VR05                                                             
         TM    POSTAT,POSHEAD      HAS HEADER BEEN VALIDATED                    
         BNO   ERNOHD              MISSING ORDER REC INFO                       
         L     RE,AIO3             MOVE HALF BUILT RECORD TO AIO3               
         L     RF,SIZEIO                                                        
         LA    R0,NEWOREC                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,CONTAB           CLEAR TABLE OF CHANGES                       
         LH    RF,=Y(L'CONTAB)                                                  
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
VR05     BAS   RE,VRDETAIL         VALIDATE DETAIL SCREEN                       
         B     VRX                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD - HEADER SCREEN                              *         
***********************************************************************         
*                                                                               
VRHEADER DS    0H                                                               
         BAS   RE,CHKINV           CHECK IF ORDER HAS BEEN INVOICED             
         TM    POSTAT,POSINV       INVOICED                                     
         BO    ERNOCHA             CANNOT CHANGE - ITEM INVOICED                
*                                                                               
         LA    R2,OHDODTEH         ORDER DATE                                   
         GOTO1 PERVAL,DMCB,(OHDODTEH+5,OHDODTE),BLOCK                           
         CLI   4(R1),1                                                          
         BE    ERINVDTE                                                         
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         MVC   PODATE,PVALPSTA                                                  
         MVC   OHDODTE,PVALCPER                                                 
         OI    OHDODTEH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         LA    R2,OHDMEDH          MEDIA                                        
         CLI   OHDMEDH+5,0                                                      
         BE    ERRMISS                                                          
         CLI   OHDMEDH+5,2                                                      
         BNE   ERRMEDIA                                                         
         MVC   S1ACCNT,SPACES                                                   
         MVC   S1ACCNT(L'QSYSMED),OHDMED                                        
         GOTO1 GTLEVNM,DMCB,C'S1',S1ACCNT,0                                     
         BNE   ERRNOS1                                                          
         MVC   S2ACCNT,S1ACCNT                                                  
         GOTO1 GTLEVNM,DMCB,C'S2',S2ACCNT,0                                     
         BNE   ERRNOS2                                                          
         MVC   QSYSMED,S1ACCNT                                                  
*                                                                               
         LA    R2,OHDCNTRH         CONTRACTOR                                   
         CLI   OHDCNTRH+5,0                                                     
         BE    ERRMISS             REQD                                         
         ZIC   R1,OHDCNTRH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   S1ACCNT+L'QSYSMED(0),OHDCNTR                                     
         MVC   AIO,AIO2                                                         
         GOTO1 GTLEVNM,DMCB,C'S1',S1ACCNT,OHDCNMEH                              
         BNE   ERINVCTR            INVALID CONTRACTOR                           
         MVC   CONTRCTR,S1ACCNT+L'QSYSMED                                       
*                                                                               
         BAS   RE,VALCONT          VALIDATE CONTRACT NUMBER                     
*                                                                               
VR10     LA    R2,OHDAUTHH         AUTHORIZED BY                                
         CLI   OHDAUTHH+5,0                                                     
         BE    ERRMISS             REQ'D                                        
         MVC   AUTHBY,SPACES                                                    
         ZIC   R1,OHDAUTHH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   AUTHBY(0),OHDAUTH                                                
*                                                                               
         LA    R2,OHDDUEH          DUE DATE                                     
         GOTO1 PERVAL,DMCB,(OHDDUEH+5,OHDDUE),BLOCK                             
         CLI   4(R1),1                                                          
         BE    ERINVDTE                                                         
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         MVC   DATEDUE,PVALPSTA                                                 
         MVC   OHDDUE,PVALCPER                                                  
         OI    OHDDUEH+6,X'80'                                                  
         DROP  R3                                                               
*                                                                               
         LA    R2,OHDSUPPH         SUPPLIER                                     
         CLI   OHDSUPPH+5,0                                                     
         BE    ERRMISS                                                          
         MVC   SUPUL,SUPPUL        DEFAULT U/L                                  
         TM    OHDSUPPH+4,X'20'    VALIDATED                                    
         BO    VR20                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 VALSUPP                                                          
         BNE   ERINVSUP            INVALID SUPPLIER                             
         MVC   SUPUL,SUPPOUL                                                    
         MVC   SUPACCNT,SUPPACT                                                 
VR20     MVC   OHDSUPN,SUPPNAME                                                 
         OI    OHDSUPNH+6,X'80'                                                 
         GOTO1 =A(VALSUPAD),DMCB,RR=RELO      SUPPLR ADDRESS                    
*                                                                               
         LA    R2,OHDTAXH          TAXABLE                                      
         MVI   TAXABLE,C'N'        NO=DEFAULT                                   
         CLI   OHDTAXH+5,0                                                      
         BE    VR30                                                             
         CLI   OHDTAX,C'N'                                                      
         BE    VR30                                                             
         CLI   OHDTAX,C'Y'                                                      
         BNE   ERRINV                                                           
         MVI   TAXABLE,C'Y'                                                     
VR30     MVC   OHDTAX,TAXABLE                                                   
         OI    OHDTAXH+6,X'80'                                                  
*                                                                               
         BAS   RE,VALCTGRY         VALIDATE CATEGORY                            
         CLC   CON#,SPACES         SKIP NET CHECK IF NO CONTRACT                
         BNH   *+8                                                              
         BAS   RE,VALNET           VALIDATE NET PAY EXISTS FOR CATEGORY         
         BAS   RE,VALSHIP          VALIDATE SHIP TO ADDRESS                     
         OI    POSTAT,POSHEAD      HEADER HAS BEEN VALIDATED                    
         CLI   ACTEQU,ACTADD                                                    
         BE    VRX                                                              
*                                                                               
VR40     BAS   RE,ELEM67           UPDATE X'67' ELEM                            
         BAS   RE,HELEM68          HEADER SCREEN UPDATE X'68' ELEM              
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         BAS   RE,CONAMTO          SAVE OLD AMOUNTS                             
         MVC   AIO,AIO3                                                         
         GOTO1 PUTREC                                                           
         BAS   RE,UPDCON           UPDATE CONTRACT ACCOUNT                      
*                                                                               
VRX      L     RE,AIO3             SAVE HALF BUILT RECORD                       
         L     RF,SIZEIO                                                        
         LA    R0,NEWOREC                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   IOOPT,C'Y'          ALREADY DID OWN IO                           
         B     DR                  DISPLAY REC CHANGES                          
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CONTRACT NUMBER                                     *         
***********************************************************************         
*                                                                               
VALCONT  NTR1                                                                   
         LA    R2,OHDCONTH         CONTRACT NUMBER                              
         CLI   RECNUM,RTDET        DETAIL RECORD                                
         BNE   *+8                                                              
         LA    R2,ODTCONTH                                                      
         XC    CON#,CON#                                                        
         CLI   5(R2),0             NOT REQUIRED ON ADD                          
         BNE   VALC10                                                           
         CLI   ACTEQU,ACTADD                                                    
         BE    VALCX                                                            
         B     ERRMISS                                                          
*                                                                               
VALC10   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   S2ACCNT+L'QSYSMED(0),8(R2)                                       
         MVC   AIO,AIO1            SAVE CONTRACT RECORD IN AIO1                 
         GOTO1 GTLEVNM,DMCB,C'S2',S2ACCNT,0                                     
         BNE   ERINVCON            INVALID CONTRACT                             
         MVC   CON#,S2ACCNT+L'QSYSMED                                           
         USING ACTRECD,R6                                                       
         L     R6,AIO                                                           
         TM    ACTRSTAT,ACTSCLOS                                                
         BO    ERRCONCL            ACCOUNT CLOSED                               
         TM    ACTRSTAT,ACTSLOCK                                                
         BO    ERRCONLK            ACCOUNT LOCKED                               
         USING CNTELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92' CONTRACT ELEM                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CNTCONTR,CONTRCTR                                                
         BNE   ERINVCC             CONTRACT NOT VALID FOR CONTRACTOR            
         CLI   RECNUM,RTDET        DETAIL RECORD                                
         BNE   *+8                                                              
         BAS   RE,VALNET           VALIDATE NET PAY EXISTS FOR CATEGORY         
VALCX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CATEGORY                                            *         
***********************************************************************         
*                                                                               
VALCTGRY NTR1                                                                   
         USING CTGTABD,R3                                                       
         LA    R3,CTGTABLE         CATEGORY TABLE                               
         MVC   CTGWKCDE,SPACES                                                  
*                                                                               
VC10     ZICM  R2,CTGFIELD,2       DISP TO FIELD HEADER                         
         AR    R2,RA                                                            
         CLI   5(R2),0                                                          
         BNE   VC40                                                             
VC20     LA    R3,CTGLENQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   VC10                                                             
         CLC   CTGWKCDE,SPACES                                                  
         BNE   VCX                                                              
         LA    R2,OHDMDSEH                                                      
         B     ERRMISS             MISSING CATEGORY SELECTION                   
*                                                                               
VC40     CLC   CTGWKCDE,SPACES                                                  
         BNE   ERINVCTG                                                         
         MVC   CTGWKCDE,CTGCODE    MATCHING WORK CODE                           
         ST    R2,FULL                                                          
         B     VC20                                                             
*                                                                               
VCX      L     R2,FULL                                                          
         XIT1  REGS=(R2)                                                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CATEGORY NET PAYABLE                                          
***********************************************************************         
*                                                                               
         USING CTGELD,R6                                                        
VALNET   NTR1                                                                   
         L     R6,AIO1             CONTRACT REC IN AIO1                         
         MVI   ELCODE,CTGELQ       X'93' CATEGORY(WC) ELEM                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VN40     BAS   RE,NEXTEL                                                        
         BNE   ERRNONET            NO NET PAY FOR CATEGORY                      
         CLC   CTGCTGY,CTGWKCDE                                                 
         BNE   VN40                                                             
         CP    CTGNPAY,=P'0'                                                    
         BNH   ERRNONET                                                         
VNX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SHIP TO ADDRESS AND ATTENTION NAME                  *         
***********************************************************************         
*                                                                               
         USING ADRELD,R3                                                        
VALSHIP  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         MVI   ELCODE,OADELQ       X'8C' OTHER ADDRESS ELEM                     
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         MVI   ADREL,OADELQ        X'8C'                                        
         MVI   ADRNUM,1                                                         
         MVI   ADRLN,ADRLN1Q                                                    
         LA    R2,OHDSHP1H                                                      
         CLI   OHDSHP1H+5,0                                                     
         BE    ERRMISS                                                          
         MVC   ADRADD1,OHDSHP1                                                  
         OC    ADRADD1,SPACES                                                   
*                                                                               
         CLI   OHDSHP2H+5,0                                                     
         BE    VSHP50                                                           
         MVC   ADRADD2,OHDSHP2                                                  
         OC    ADRADD2,SPACES                                                   
         MVI   ADRLN,ADRLN2Q                                                    
         MVI   ADRNUM,2                                                         
*                                                                               
         CLI   OHDSHP3H+5,0                                                     
         BE    VSHP50                                                           
         MVC   ADRADD3,OHDSHP3                                                  
         OC    ADRADD3,SPACES                                                   
         MVI   ADRLN,ADRLN3Q                                                    
         MVI   ADRNUM,3                                                         
VSHP50   GOTO1 ADDELEM                                                          
*                                                                               
         USING FFTELD,R3                                                        
         MVI   ELCODE,FFTELQ       X'DB' FREEFORM = SHIP TO ATTN                
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         MVI   FFTEL,FFTELQ        X'DB'                                        
         MVI   FFTTYPE,FFTTSATN    SHIP TO ATTN                                 
         ZICM  R1,OHDSATNH+5,1                                                  
         BZ    VSHPX                                                            
         STC   R1,FFTDLEN          LEN OF DATA                                  
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   FFTDATA,OHDSATN                                                  
         LA    R1,FFTLN1Q+2(R1)    +2 BECAUSE LN1Q DOESN'T INC. DLEN            
         STC   R1,FFTLN            ELEM LEN                                     
         GOTO1 ADDELEM                                                          
VSHPX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DETAIL SCREEN                                       *         
***********************************************************************         
*                                                                               
VRDETAIL NTR1                                                                   
         NI    POSTAT,X'FF'-POSCHECK  ASSUME NOT CHECK ORDER                    
         ZAP   TOTCOST,=P'0'                                                    
         ZAP   TOTRET,=P'0'                                                     
*                                                                               
         BAS   RE,VALCONT                                                       
*                                                                               
         LA    R2,ODTLIN1H                                                      
         TM    ODTLIN1H+1,X'20'    PROTECTED                                    
         BO    *+12                                                             
         CLI   ODTLIN1H+5,0                                                     
         BE    ERRMISS                                                          
*                                                                               
         MVC   AIO,AIO3            BUILD NEW REC IN AIO3                        
         CLI   ACTEQU,ACTADD                                                    
         BE    VRD08                                                            
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO1            OLD RECORD IN AIO1                           
         GOTO1 GETREC                                                           
         MVC   AIO,AIO3            BUILD NEW REC IN AIO3                        
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
VRD08    LA    R3,ODTLIN1H                                                      
         MVI   LINENUM,1                                                        
         MVI   ELCODE,CPOELQ       X'94' - REMOVE EXISTING                      
         GOTO1 REMELEM                                                          
         USING CPOELD,R6                                                        
*                                                                               
VRD10    L     R6,AIO1                                                          
         MVI   ELCODE,CPOELQ       X'94' - GET OLD ELEM                         
         XC    ELEM,ELEM                                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VRDNXT   BAS   RE,NEXTEL                                                        
         BNE   VRD12                                                            
         CLC   LINENUM,CPOLINE                                                  
         BNE   VRDNXT                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'1'                                                         
         BM    VRD12                                                            
         EX    R1,*+8                                                           
         B     *+4                                                              
         MVC   ELEM(0),0(R6)                                                    
         DROP  R6                                                               
*                                                                               
VRD12    BAS   RE,VALLINE          VALIDATE LINE                                
         BNE   VRD20               NO DATA                                      
         BAS   RE,ELEM94           ADD X'94' ELEMS                              
VRD20    LA    R3,DETLENQ(R3)                                                   
         LA    R1,ODTLIN8H                                                      
         CR    R3,R1                                                            
         BNH   VRD10                                                            
         EDIT  (P6,TOTCOST),(11,ODTCTOT),2,MINUS=YES                            
         OI    ODTCTOTH+6,X'80'                                                 
         EDIT  (P6,TOTRET),(11,ODTRTOT),2,MINUS=YES                             
         OI    ODTRTOTH+6,X'80'                                                 
*                                                                               
         CLI   ACTEQU,ACTADD       WAS CONTRACT SPECIFIED                       
         BNE   VRD30                                                            
         CLC   CON#,SPACES                                                      
         BH    VRD30                                                            
         GOTO1 =A(FINDCON),DMCB,RR=RELO   FIND CONTRACT TO COVER $              
         BE    VRD30                                                            
         LA    R2,ODTCONTH         NO CON# FOUND - ENTER TO OVERDLVR            
         B     ERRCONNF                                                         
*                                                                               
VRD30    MVC   AIO,AIO3            BUILD NEW REC IN AIO3                        
         BAS   RE,ELEM67           X'67' ELEM                                   
         BAS   RE,ELEM68           X'68' ELEM                                   
         BAS   RE,ELEM3E           X'3E' NARR AND FOOTLINE ELEM                 
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   VRDWRITE                                                         
         GOTO1 ADDREC                                                           
         TM    POSTAT,POSAUTO                                                   
         BNO   VRDX                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 UPDPO#              UPDATE ORDER CONTROL REC WITH NEW #          
         MVC   AIO,AIO3                                                         
         NI    POSTAT,X'FF'-POSAUTO                                             
         B     VRDX                                                             
VRDWRITE XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         BAS   RE,CONAMTO          SAVE OLD AMOUNTS                             
         MVC   AIO,AIO3                                                         
         GOTO1 PUTREC                                                           
VRDX     BAS   RE,UPDCON           UPDATE CONTRACT ACCOUNT                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DETAIL LINE                                         *         
***********************************************************************         
*                                                                               
         USING DETLINED,R3                                                      
         USING CPOELD,R6                                                        
VALLINE  NTR1                                                                   
         LA    R2,DETQTYH          QUANTITY                                     
         TM    1(R2),X'20'         QTY PROTECTED                                
         BNO   VL01                SKIP LINE                                    
         GOTO1 ADDELEM             ADD OLD ELEM BACK                            
         ZIC   R1,LINENUM          AND BUMP NUMBER                              
         LA    R1,1(R1)                                                         
         STC   R1,LINENUM                                                       
         LA    R6,ELEM                                                          
         ZAP   QTY,CPOQTY                                                       
         B     VL06                                                             
*                                                                               
VL01     LA    R2,DETQTYH          QUANTITY                                     
         ZICM  R4,DETQTYH+5,1                                                   
         BNZ   VL02                                                             
         LA    R6,ELEM                                                          
         CLI   CPOEL,CPOELQ        CAN'T DELETE IF INVOICED                     
         BNE   VLXNO                                                            
         CP    CPOQTYIN,=P'0'                                                   
         BNE   ERNOCHA                                                          
         OC    CPOQTYTD,CPOQTYTD                                                
         BZ    VLXNO                                                            
         CP    CPOQTYTD,=P'0'      OPEN BATCHES                                 
         BE    VLXNO                                                            
         B     ERNOCHA             CANNOT CHANGE - ITEM INVOICED                
*                                                                               
VL02     CLI   DETQTY,C'*'         CHECK ORDER = NO QTY                         
         BNE   VL05                                                             
         BAS   RE,CHKINV           CHECK IF ORDER HAS BEEN INVOICED             
         TM    POSTAT,POSINV       INVOICED                                     
         BO    ERNOCHA             CANNOT CHANGE - ITEM INVOICED                
         OI    POSTAT,POSCHECK                                                  
         ZAP   QTY,=P'1'                                                        
         B     VL10                                                             
*                                                                               
VL05     GOTO1 CASHVAL,DMCB,(C'0',DETQTY),(R4)                                  
         CLI   DMCB,X'FF'                                                       
         BE    ERINVAMT                                                         
         ZAP   QTY,DMCB+4(8)                                                    
*                                                                               
         LA    R6,ELEM                                                          
         CLI   CPOEL,CPOELQ        CHECK QTY NOT < INVOICED                     
         BNE   VL08                                                             
         ZAP   WORK(3),CPOQTYIN    QTY INVOICED                                 
         OC    CPOQTYTD,CPOQTYTD                                                
         BZ    *+10                                                             
         AP    WORK(3),CPOQTYTD    ADD OPEN BATCHES                             
         CP    QTY,WORK(3)                                                      
         BL    ERRINQTY            QTY LESS THAN INVOICED                       
         CP    WORK(3),=P'0'       ANYTHING INVOICED                            
         BE    VL08                                                             
VL06     ZAP   UNITPRI,CPOUPRI     THEN CAN'T CHANGE AMOUNTS                    
         ZAP   UNITRET,CPOURET                                                  
         ZAP   URATIO,CPORATIO                                                  
         B     VL60                                                             
VL08     CP    QTY,=P'0'                                                        
         BE    VLXNO               NOTHING TO ADD                               
         DROP  R6                                                               
*                                                                               
VL10     LA    R2,DETDESCH         STOCK NO AND DESC                            
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         XC    BLOCK(150),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(25,DETDESCH),(1,BLOCK),C',=,/'                     
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         USING SCANBLKD,R4                                                      
         LA    R4,BLOCK                                                         
         CLI   SC1STLEN,L'CPOSTOCK                                              
         BH    ERRLSTCK            STOCK NUMBER TOO LONG                        
         MVC   STOCK#,SC1STFLD                                                  
         MVC   DESCLEN,SC2NDLEN                                                 
         MVC   DESC,SC2NDFLD                                                    
         DROP  R4                                                               
*                                                                               
         LA    R2,DETUPRH          UNIT PRICE                                   
         ZICM  R4,DETUPRH+5,1                                                   
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',DETUPR),(R4)                                 
         CLI   DMCB,X'FF'                                                       
         BE    ERINVAMT                                                         
         CP    DMCB+4(8),=P'0'                                                  
         BNH   ERINVAMT                                                         
         ZAP   UNITPRI,DMCB+4(8)                                                
*                                                                               
         LA    R2,DETRPRH          UNIT RETAIL                                  
         ZICM  R4,DETRPRH+5,1                                                   
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',DETRPR),(R4)                                 
         CLI   DMCB,X'FF'                                                       
         BE    ERINVAMT                                                         
         CP    DMCB+4(8),=P'0'                                                  
         BNH   ERINVAMT                                                         
         ZAP   UNITRET,DMCB+4(8)                                                
*                                                                               
         LA    R2,DETRATH          RATIO FOR RETAIL EXTENSION                   
         ZICM  R4,DETRATH+5,1                                                   
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',DETRAT),(R4)                                 
         CLI   DMCB,X'FF'                                                       
         BE    ERINVAMT                                                         
         CP    DMCB+4(8),=P'0'                                                  
         BNH   ERINVAMT                                                         
         ZAP   URATIO,DMCB+4(8)                                                 
*                                                                               
VL60     ZAP   WORK(10),UNITPRI    COST EXT= UNIT PRICE*QTY                     
         MP    WORK(10),QTY                                                     
         ZAP   COSTEXT,WORK(10)                                                 
         EDIT  (P6,COSTEXT),(11,DETCOS),2,MINUS=YES                             
         OI    DETCOSH+6,X'80'                                                  
*                                                                               
         ZAP   WORK(10),UNITRET    RETAIL EXT= RET PRICE*QTY*RATIO              
         MP    WORK(10),URATIO                                                  
         MP    WORK(10),QTY                                                     
         SRP   WORK(10),64-2,5                                                  
         ZAP   RETEXT,WORK(10)                                                  
         EDIT  (P6,RETEXT),(11,DETRET),2,MINUS=YES                              
         OI    DETRETH+6,X'80'                                                  
*                                                                               
         AP    TOTCOST,COSTEXT                                                  
         AP    TOTRET,RETEXT                                                    
         TM    DETQTYH+1,X'20'     IF QTY PROTECTED                             
         BO    VLXNO               ELEM ALREADY ADDED                           
*                                                                               
VLXYES   B     XYES                                                             
VLXNO    B     XNO                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'94' CONTRACT PURCHASE ORDER ELEM                    *         
***********************************************************************         
*                                                                               
         USING DETLINED,R3                                                      
         USING CPOELD,R6                                                        
ELEM94   NTR1                                                                   
         LA    R6,ELEM                                                          
         CLI   CPOEL,CPOELQ        CHANGING AN ELEM                             
         BNE   EL94B                                                            
         CP    CPOQTYIN,=P'0'      IF INVOICED CAN ONLY CHANGE QTY              
         BNE   EL94A                                                            
         OC    CPOQTYTD,CPOQTYTD                                                
         BZ    EL94B                                                            
         CP    CPOQTYTD,=P'0'      OPEN BATCHES                                 
         BE    EL94B                                                            
EL94A    ZAP   CPOQTY,QTY                                                       
         B     EL94C                                                            
*                                                                               
EL94B    XC    ELEM,ELEM                                                        
         MVI   CPOEL,CPOELQ        X'94'                                        
         MVI   CPOLN,CPOLN1Q                                                    
         MVC   CPOLINE,LINENUM                                                  
         ZAP   CPOQTY,QTY                                                       
         ZAP   CPOUPRI,UNITPRI                                                  
         ZAP   CPOURET,UNITRET                                                  
         ZAP   CPORATIO,URATIO                                                  
         MVC   CPOSTOCK,STOCK#                                                  
         ZAP   CPOQTYIN,=P'0'                                                   
         ZAP   CPOAMTIN,=P'0'                                                   
         ZAP   CPOQTYTD,=P'0'                                                   
         ZAP   CPOAMTTD,=P'0'                                                   
         ZICM  R1,DESCLEN          DESCRIPTION LEN                              
         BZ    EL94C                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   CPODESC(0),DESC                                                  
         LA    R1,CPOLN1Q+1(R1)                                                 
         STC   R1,CPOLN                                                         
EL94C    GOTO1 ADDELEM                                                          
         ZIC   R1,LINENUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LINENUM                                                       
*                                                                               
EL94X    B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'67' PRODUCTION ORDER ELEM                           *         
***********************************************************************         
*                                                                               
         USING ORDELD,R6                                                        
ELEM67   NTR1                                                                   
         CLI   ACTEQU,ACTADD                                                    
         BE    EL67A                                                            
         CLI   RECNUM,RTDET        DETAIL SCREEN                                
         BE    EL67X               DOESN'T UPDATE THIS ELEM                     
*                                                                               
EL67A    NI    POSTAT,X'FF'-POSADDEL                                            
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,ORDELQ       X'67'                                        
         BAS   RE,GETEL                                                         
         BE    EL67C                                                            
*                                                                               
         OI    POSTAT,POSADDEL                                                  
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ORDEL,ORDELQ        X'67'                                        
         MVI   ORDLN,ORDLN2Q                                                    
EL67C    MVC   ORDACCC,CMPY                                                     
         MVC   ORDACCU(2),=C'S1'                                                
         MVC   ORDACCA,S1ACCNT                                                  
         MVC   ORDSUPC,CMPY                                                     
         MVC   ORDSUPU(L'SUPPUL),SUPUL                                          
         MVC   ORDSUPA,SUPACCNT                                                 
         MVC   ORDDATE,PODATE                                                   
         MVC   ORDAUTH,AUTHBY                                                   
         MVC   ORDDDTE,DATEDUE                                                  
         MVC   ORDATTN,POAATTN                                                  
         MVC   ORDTAX,TAXABLE                                                   
         ZIC   R1,ORDAMNO                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ORDAMNO                                                       
         GOTO1 DATCON,DMCB,(5,ORDAMDT),(1,ORDAMDT)                              
         TM    POSTAT,POSADDEL                                                  
         BNO   EL67X                                                            
         GOTO1 ADDELEM                                                          
EL67X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK IF ORDER HAS BEEN INVOICED                             *         
***********************************************************************         
*                                                                               
         USING OAMELD,R6                                                        
CHKINV   NTR1                                                                   
         NI    POSTAT,X'FF'-POSINV                                              
         L     R6,AIO3                                                          
         MVI   ELCODE,OAMELQ       X'68' ORDER AMOUNT ELEM                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CI10NX   BAS   RE,NEXTEL                                                        
         BNE   CIX                                                              
         CP    OAMIVAL,=P'0'                                                    
         BE    *+8                                                              
         OI    POSTAT,POSINV       THERE IS AN INVOICED AMOUNT                  
         B     CI10NX                                                           
CIX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SAVE OLD AMOUNTS IN TABLE - FOR UPDATES TO CONTRACTS                   
***********************************************************************         
*                                                                               
         USING OAMELD,R6                                                        
         USING CONTABD,R3                                                       
CONAMTO  NTR1                                                                   
         LA    RE,CONTAB           CLEAR TABLE OF CHANGES                       
         LH    RF,=Y(L'CONTAB)                                                  
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R3,CONTAB           CONTRACT TABLE                               
         L     R6,AIO                                                           
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CONACCNT,OAMCONA    CONTRACT ACCOUNT                             
         MVC   CONOWC,OAMWORK      WORK CODE                                    
         ZAP   CONOAMT,=P'0'                                                    
         MVC   CONNWC,SPACES                                                    
         ZAP   CONNAMT,=P'0'                                                    
*                                                                               
         USING CPOELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CPOELQ       X'94'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CAO50NX  BAS   RE,NEXTEL                                                        
         BNE   CAOX                                                             
         ZAP   WORK(10),CPOURET    RETAIL EXT= RET PRICE*QTY*RATIO              
         MP    WORK(10),CPORATIO                                                
         MP    WORK(10),CPOQTY                                                  
         SRP   WORK(10),64-2,5                                                  
         AP    CONOAMT,WORK(10)                                                 
         B     CAO50NX                                                          
CAOX     B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        ADD NEW AMOUNTS TO TABLE - FOR UPDATES TO CONTRACTS                    
***********************************************************************         
*                                                                               
         USING OAMELD,R6                                                        
         USING CONTABD,R3                                                       
CONAMTN  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,CONTAB           CONTRACT TABLE                               
CAN20    CLI   0(R3),X'00'                                                      
         BE    CAN40               ADD NEW ENTRY                                
         CLC   CONACCNT,OAMCONA    CONTRACT ACCOUNT                             
         BE    CAN30                                                            
         LA    R3,CONLENQ(R3)                                                   
         LA    R1,CONTABX                                                       
         CR    R3,R1                                                            
         BL    CAN20                                                            
         DC    H'0'                                                             
*                                                                               
CAN30    MVC   CONNWC,OAMWORK      NEW WORK CODE                                
         B     CAN50                                                            
CAN40    MVC   CONACCNT,OAMCONA    CONTRACT ACCOUNT                             
         MVC   CONOWC,SPACES                                                    
         ZAP   CONOAMT,=P'0'                                                    
         MVC   CONNWC,OAMWORK      NEW WORK CODE                                
*                                                                               
         USING CPOELD,R6                                                        
CAN50    L     R6,AIO                                                           
         ZAP   CONNAMT,=P'0'       VALUE TO CONTRACT                            
         MVI   ELCODE,CPOELQ       X'94'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CAN50NX  BAS   RE,NEXTEL                                                        
         BNE   CANX                                                             
         ZAP   WORK(10),CPOURET    RETAIL EXT= RET PRICE*QTY*RATIO              
         MP    WORK(10),CPORATIO                                                
         MP    WORK(10),CPOQTY                                                  
         SRP   WORK(10),64-2,5                                                  
         AP    CONNAMT,WORK(10)                                                 
         B     CAN50NX                                                          
*                                                                               
CANX     B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'68' ORDER AMOUNT ELEM                               *         
***********************************************************************         
*                                                                               
         USING OAMELD,R6                                                        
ELEM68   NTR1                                                                   
         NI    POSTAT,X'FF'-POSADDEL                                            
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         BE    EL68A                                                            
*                                                                               
         OI    POSTAT,POSADDEL                                                  
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   OAMEL,OAMELQ        X'68'                                        
         MVI   OAMLN,OAMLN2Q                                                    
         B     EL68B               IF NEW ADD EVERYTHING                        
EL68A    CLI   RECNUM,RTDET        DETAIL SCREEN                                
         BE    EL68C               THEN SKIP HEADER SCREEN STUFF                
EL68B    MVC   OAMWORK,CTGWKCDE                                                 
         MVC   OAMCONU(2),=C'S2'                                                
*        MVC   OAMCONA,S2ACCNT                                                  
         CLI   RECNUM,RTDET        DETAIL RECORD                                
         BNE   EL68X                                                            
EL68C    MVC   OAMCONA,S2ACCNT                                                  
         ZAP   OAMAMNT,TOTCOST                                                  
*        ZAP   WORK(12),TOTRET     RATIO= RETAIL/COST                           
*        MP    WORK(12),=P'100000'                                              
*        DP    WORK(12),TOTCOST                                                 
*        ZAP   OAMRATIO,WORK(6)                                                 
         NI    OAMTYPE,X'FF'-OAMTCHK                                            
         TM    POSTAT,POSCHECK     CHECK ORDER                                  
         BNO   *+8                                                              
         OI    OAMTYPE,OAMTCHK                                                  
         TM    POSTAT,POSADDEL                                                  
         BNO   EL68X                                                            
         ZAP   OAMINUM,=P'0'                                                    
         ZAP   OAMIVAL,=P'0'                                                    
         ZAP   OAMTVAL,=P'0'                                                    
         GOTO1 ADDELEM                                                          
EL68X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        HEADER SCREEN UPDATE X'68' ORDER AMOUNT ELEM                           
***********************************************************************         
*                                                                               
         USING OAMELD,R6                                                        
HELEM68  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
HEL68A   BAS   RE,NEXTEL                                                        
         BNE   HEL68X                                                           
         MVC   OAMWORK,CTGWKCDE    WORK CODE ON ORDER SCREEN                    
         MVC   OAMCONU(2),=C'S2' CONTRACT                                       
         MVC   OAMCONA,S2ACCNT                                                  
         B     HEL68A                                                           
HEL68X   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'3E' NARRATIVE AND FOOTLINE                          *         
***********************************************************************         
*                                                                               
         USING SCMELD,R6                                                        
ELEM3E   NTR1                                                                   
         MVI   ELCODE,SCMELQ       X'3E'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         USING NARTABD,R3                                                       
         LA    R3,NARTABLE                                                      
EL3E10   ZICM  R2,NARFIELD,2       DISP TO FIELD HEADER                         
         AR    R2,RA                                                            
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         BM    EL3ENX                                                           
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SCMEL,SCMELQ        X'3E'                                        
         MVC   SCMSEQ,NARLINE      LINE NUMBER                                  
         MVC   SCMTYPE,NARTYPE     TYPE                                         
         EX    R1,*+4                                                           
         MVC   SCMNARR(0),8(R2)                                                 
         LA    R1,SCMLN1Q+1(R1)                                                 
         STC   R1,SCMLN                                                         
         GOTO1 ADDELEM                                                          
EL3ENX   LA    R3,NARLENQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   EL3E10                                                           
*                                                                               
EL3EX    B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        AFTER ADD/CHANGE-UPDATE CONTRACT ACCOUNTS                    *         
***********************************************************************         
*                                                                               
         USING CONTABD,R3                                                       
UPDCON   NTR1                                                                   
         BAS   RE,CONAMTN          ADD NEW AMOUNTS TO CONTRACT TABLE            
         MVC   AIO,AIO1                                                         
*                                                                               
         USING CONTABD,R3                                                       
         LA    R3,CONTAB                                                        
UP10     CLI   0(R3),X'00'                                                      
         BE    UPX                                                              
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'S2'                                                
         MVC   ACTKACT,CONACCNT    CONTRACT ACCOUNT                             
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING CNTELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92' CONTRACT ELEM                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SP    CNTOPNPO,CONOAMT    MINUS OLD AMOUNT                             
         AP    CNTOPNPO,CONNAMT    PLUS NEW AMOUNT                              
*                                                                               
         USING CTGELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93' CATEGORY(WC) ELEM                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
UP40     BAS   RE,NEXTEL                                                        
         BNE   UP100                                                            
         CLC   CONOWC,SPACES       ANYTHING TO SUBTRACT                         
         BNH   UP50                                                             
         CLC   CTGCTGY,CONOWC                                                   
         BNE   UP50                                                             
         SP    CTGOPNPO,CONOAMT    MINUS OLD AMOUNT                             
*                                                                               
UP50     CLC   CONNWC,SPACES       ANYTHING TO ADD                              
         BNH   UP40                                                             
         CLC   CTGCTGY,CONNWC                                                   
         BNE   UP40                                                             
         AP    CTGOPNPO,CONNAMT    PLUS NEW AMOUNT                              
         B     UP40                                                             
*                                                                               
UP100    GOTO1 PUTREC                                                           
         LA    R3,CONLENQ(R3)                                                   
         B     UP10                                                             
*                                                                               
UPX      B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RECORD                                               *         
***********************************************************************         
*                                                                               
DR       DS    0H                                                               
         MVC   AIO,AIO3                                                         
         BAS   RE,CHKINV           HAS ORDER BEEN INVOICED                      
         CLI   RECNUM,RTDET        DETAIL RECORD                                
         BE    DRDETAIL            DISPLAY DETAIL SCREEN                        
*                                                                               
         USING ORDELD,R6                                                        
DRHEADER L     R6,AIO                                                           
         MVI   ELCODE,ORDELQ       X'67'                                        
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         GOTO1 DATCON,DMCB,(1,ORDDATE),(11,OHDODTE)    ORDER DATE               
         OI    OHDODTEH+6,X'80'                                                 
         MVC   PODATE,ORDDATE                                                   
*                                                                               
         MVC   OHDMED(L'QSYSMED),ORDACCA               SYSTEM/MEDIA             
         OI    OHDMEDH+6,X'80'                                                  
*                                                                               
         MVC   OHDCNTR,ORDACCA+L'QSYSMED               CONTRACTOR               
         OI    OHDCNTRH+6,X'80'                                                 
         MVC   S1ACCNT,ORDACCA                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 GTLEVNM,DMCB,C'S1',S1ACCNT,OHDCNMEH     CONTRACTOR NAME          
*                                                                               
         MVC   OHDAUTH,ORDAUTH                         AUTHORIZED BY            
         OI    OHDAUTHH+6,X'80'                                                 
         MVC   AUTHBY,ORDAUTH                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(1,ORDDDTE),(11,OHDDUE)     DUE DATE                 
         OI    OHDDUEH+6,X'80'                                                  
         MVC   DATEDUE,ORDDDTE                                                  
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   SUPACCNT,ORDSUPA                                                 
         MVC   DMCB(L'SUPPUL),ORDSUPU                                           
         CLC   SUPPUL,ORDSUPU      SAME U/L?                                    
         BE    DR10                                                             
         MVC   SUPUL,ORDSUPU                                                    
         MVC   OHDSUPP+1(L'OHDSUPP-1),ORDSUPU                                   
         MVI   OHDSUPP,C'*'                                                     
         B     *+16                                                             
DR10     MVC   OHDSUPP,SPACES                                                   
         MVC   OHDSUPP(12),ORDSUPA               SUPPLIER                       
         OI    OHDSUPPH+6,X'80'                                                 
         GOTO1 GTLEVNM,DMCB,,ORDSUPA,OHDSUPNH    SUPPLIER NAME                  
         BAS   RE,DISSUPAD                             SUPPLIER ADDRESS         
         MVC   OHDATTN,ORDATTN                                                  
         OI    OHDATTNH+6,X'80'                                                 
         MVC   POAATTN,ORDATTN                                                  
*                                                                               
         MVC   OHDTAX,ORDTAX                           TAXABLE                  
         OI    OHDTAXH+6,X'80'                                                  
         MVC   TAXABLE,ORDTAX                                                   
*                                                                               
         USING OAMELD,R6                                                        
         L     R6,AIO3                                                          
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         MVC   OHDCONT,OAMCONA+L'QSYSMED                                        
         OI    OHDCONTH+6,X'80'                                                 
         MVC   S2ACCNT,OAMCONA                                                  
*                                                                               
         BAS   RE,DISCTGRY                             CATEGORY                 
         BAS   RE,DISSHIP                              SHIPPING ADDRESS         
         B     DRX                                                              
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DETAIL SCREEN                                        *         
***********************************************************************         
*                                                                               
         USING DETLINED,R3                                                      
         USING OAMELD,R6                                                        
DRDETAIL DS    0H                                                               
         MVC   AIO,AIO3            REC IN AIO3                                  
         BAS   RE,CLRDET           CLEAR SCREEN                                 
         ZAP   TOTCOST,=P'0'                                                    
         ZAP   TOTRET,=P'0'                                                     
*                                                                               
         NI    ODTTT1AH+1,X'FF'-X'0C'                                           
         NI    ODTTT1BH+1,X'FF'-X'0C'                                           
         NI    ODTTT1CH+1,X'FF'-X'0C'                                           
         NI    ODTTT1EH+1,X'FF'-X'0C'                                           
         OI    ODTTT1AH+6,X'80'                                                 
         OI    ODTTT1BH+6,X'80'                                                 
         OI    ODTTT1CH+6,X'80'                                                 
         OI    ODTTT1EH+6,X'80'                                                 
*                                                                               
         LA    R2,ODTTOTSH                                                      
         LA    R3,ODTTTL2H                                                      
         MVI   BYTE,X'FF'-X'0C'                                                 
DRD010   NC    DETUPRH+1(1),BYTE                                                
         NC    DETRPRH+1(1),BYTE                                                
         NC    DETRATH+1(1),BYTE                                                
         NC    DETRETH+1(1),BYTE                                                
         OI    DETUPRH+6,X'80'                                                  
         OI    DETRPRH+6,X'80'                                                  
         OI    DETRATH+6,X'80'                                                  
         OI    DETRETH+6,X'80'                                                  
         MVI   BYTE,X'FF'-X'2C'                                                 
         AH    R3,=Y(ODTLIN2H-ODTLIN1H)                                         
         CR    R3,R2                                                            
         BL    DRD010                                                           
         NI    ODTRTOTH+1,X'FF'-X'0C'                                           
         OI    ODTRTOTH+6,X'80'                                                 
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         BNE   DRD050                                                           
         CLC   OAMWORK,=C'CA'      CASH / BILL PAY?                             
         BNE   DRD050              NO, DON'T HIDE TITLES                        
*                                                                               
         OI    ODTTT1AH+1,X'0C'    HIDE                                         
         OI    ODTTT1BH+1,X'0C'                                                 
         OI    ODTTT1CH+1,X'0C'                                                 
         OI    ODTTT1EH+1,X'0C'                                                 
*                                                                               
         LA    R2,ODTTOTSH                                                      
         LA    R3,ODTTTL2H                                                      
DRD020   OI    DETUPRH+1,X'2C'     PROTECT AND HIDE                             
         OI    DETRPRH+1,X'2C'                                                  
         OI    DETRATH+1,X'2C'                                                  
         OI    DETRETH+1,X'2C'                                                  
         AH    R3,=Y(ODTLIN2H-ODTLIN1H)                                         
         CR    R3,R2                                                            
         BL    DRD020                                                           
         OI    ODTRTOTH+1,X'0C'                                                 
         OI    ODTRTOTH+6,X'80'                                                 
*                                                                               
         USING ORDELD,R6                                                        
DRD050   L     R6,AIO3                                                          
         MVI   ELCODE,ORDELQ       X'67'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CONTRCTR,ORDACCA+L'QSYSMED    SAVE CONTRACTOR                    
         MVC   S1ACCNT,ORDACCA                                                  
*                                                                               
         USING OAMELD,R6                                                        
         NI    POSTAT,X'FF'-POSCHECK                                            
         L     R6,AIO3                                                          
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CTGWKCDE,OAMWORK                SAVE CATEGORY                    
         MVC   ODTCONT,OAMCONA+L'QSYSMED       DISPLAY CONTRACT                 
         OI    ODTCONTH+6,X'80'                                                 
         MVC   S2ACCNT,OAMCONA                                                  
         TM    OAMTYPE,OAMTCHK     CHECK ORDER?                                 
         BNO   *+8                                                              
         OI    POSTAT,POSCHECK                                                  
         DROP  R6                                                               
*                                                                               
         LA    R3,ODTLIN1H                                                      
         MVI   LINENUM,1                                                        
DRD100   BAS   RE,DISLINE          DISPLAY LINE                                 
         ZIC   R1,LINENUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LINENUM                                                       
         LA    R3,DETLENQ(R3)                                                   
         LA    R1,ODTLIN8H                                                      
         CR    R3,R1                                                            
         BNH   DRD100                                                           
         EDIT  (P6,TOTCOST),(11,ODTCTOT),2,MINUS=YES                            
         OI    ODTCTOTH+6,X'80'                                                 
         EDIT  (P6,TOTRET),(11,ODTRTOT),2,MINUS=YES                             
         OI    ODTRTOTH+6,X'80'                                                 
*                                                                               
         BAS   RE,DISNARR                                                       
*                                                                               
DRX      NI    POSTAT,X'FF'-POSKEYCH                                            
         CLI   RECNUM,RTDET        DETAIL RECORD                                
         BE    XIT                                                              
         CLI   ACTEQU,ACTADD       ONLY GOOD FOR ADD                            
         BNE   XIT                                                              
         MVI   PFKEY,6             PF TO DETAIL SCREEN                          
         GOTO1 =A(SETUP),DMCB,RR=RELO                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DETAIL LINE                                                    
***********************************************************************         
*                                                                               
         USING CPOELD,R6                                                        
DISLINE  NTR1                                                                   
         L     R6,AIO3                                                          
         MVI   ELCODE,CPOELQ       X'94'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DLNXT    BAS   RE,NEXTEL                                                        
         BNE   DLX                                                              
         CLC   LINENUM,CPOLINE                                                  
         BNE   DLNXT                                                            
*                                                                               
         TM    POSTAT,POSCHECK     CHECK ORDER                                  
         BNO   DL20                                                             
         MVI   DETQTY,C'*'                                                      
         CP    CPOQTYIN,=P'0'                                                   
         BNE   DL18                                                             
         OC    CPOQTYTD,CPOQTYTD                                                
         BZ    DL30                                                             
         CP    CPOQTYTD,=P'0'                                                   
         BE    DL30                                                             
DL18     OI    DETQTYH+1,X'20'     PROTECT                                      
         B     DL30                                                             
DL20     EDIT  (P3,CPOQTY),(3,DETQTY),ALIGN=LEFT                                
         ZAP   WORK(3),CPOQTYIN    QTY INVOICED                                 
         OC    CPOQTYTD,CPOQTYTD                                                
         BZ    *+10                                                             
         AP    WORK(3),CPOQTYTD    ADD OPEN BATCHES                             
         CP    WORK(3),CPOQTY                                                   
         BL    *+8                                                              
         OI    DETQTYH+1,X'20'     PROTECT                                      
*                                                                               
DL30     MVC   BLOCK(100),SPACES                                                
         MVC   BLOCK(L'CPOSTOCK),CPOSTOCK                                       
         MVI   BLOCK+L'CPOSTOCK+5,C'/'                                          
         ZIC   R1,CPOLN                                                         
         LA    R0,CPOLN1Q+1                                                     
         SR    R1,R0                                                            
         BM    *+14                                                             
         EX    R1,*+4                                                           
         MVC   BLOCK+L'CPOSTOCK+7(0),CPODESC                                    
         GOTO1 SQUASHER,DMCB,BLOCK,40                                           
         MVC   DETDESC,BLOCK                                                    
         EDIT  (P6,CPOUPRI),(9,DETUPR),2,MINUS=YES                              
         EDIT  (P6,CPOURET),(9,DETRPR),2,MINUS=YES                              
         EDIT  (P4,CPORATIO),(4,DETRAT),2                                       
         ZAP   WORK(3),CPOQTYIN    QTY INVOICED                                 
         OC    CPOQTYTD,CPOQTYTD                                                
         BZ    *+10                                                             
         AP    WORK(3),CPOQTYTD    ADD OPEN BATCHES                             
         CP    WORK(3),=P'0'                                                    
         BE    DL50                                                             
DL40     OI    DETDESCH+1,X'20'    PROTECT                                      
         OI    DETUPRH+1,X'20'                                                  
         OI    DETRPRH+1,X'20'                                                  
         OI    DETRATH+1,X'20'                                                  
*                                                                               
DL50     ZAP   WORK(10),CPOUPRI    COST EXT= UNIT PRICE*QTY                     
         MP    WORK(10),CPOQTY                                                  
         ZAP   COSTEXT,WORK(10)                                                 
         EDIT  (P6,COSTEXT),(11,DETCOS),2,MINUS=YES                             
         OI    DETCOSH+1,X'20'     PROTECT                                      
*                                                                               
         ZAP   WORK(10),CPOURET    RETAIL EXT= RET PRICE*QTY*RATIO              
         MP    WORK(10),CPORATIO                                                
         MP    WORK(10),CPOQTY                                                  
         SRP   WORK(10),64-2,5                                                  
         ZAP   RETEXT,WORK(10)                                                  
         EDIT  (P6,RETEXT),(11,DETRET),2,MINUS=YES                              
         OI    DETRETH+1,X'20'     PROTECT                                      
*                                                                               
         AP    TOTCOST,COSTEXT                                                  
         AP    TOTRET,RETEXT                                                    
DLX      B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY NARRATIVE AND FOOTLINE                               *         
***********************************************************************         
*                                                                               
         USING NARTABD,R3                                                       
         USING SCMELD,R6                                                        
DISNARR  NTR1                                                                   
         LA    R3,NARTABLE         CLEAR FIRST                                  
DNCL10   ZICM  R2,NARFIELD,2       DISP TO FIELD HEADER                         
         AR    R2,RA                                                            
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         BM    DNCL20                                                           
         EX    R1,*+4                                                           
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),X'00'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
DNCL20   LA    R3,NARLENQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   DNCL10                                                           
*                                                                               
         LA    R3,NARTABLE                                                      
DN10     ZICM  R2,NARFIELD,2       DISP TO FIELD HEADER                         
         AR    R2,RA                                                            
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,SCMELQ       X'3E'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DN20     BAS   RE,NEXTEL                                                        
         BNE   DN30                                                             
         CLC   SCMSEQ,NARLINE      LINE NUMBER                                  
         BNE   DN20                                                             
         ZIC   R1,SCMLN                                                         
         LA    R0,SCMLN1Q+1                                                     
         SR    R1,R0                                                            
         BM    DN30                                                             
         EX    R1,*+4                                                           
         MVC   8(0,R2),SCMNARR                                                  
*                                                                               
DN30     LA    R3,NARLENQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   DN10                                                             
*                                                                               
DNX      B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY SUPPLIER ADDRESS                                     *         
***********************************************************************         
*                                                                               
         USING ADRELD,R6                                                        
DISSUPAD NTR1                                                                   
         L     R6,AIO3                                                          
         MVI   ELCODE,ADRELQ       X'22'                                        
         BAS   RE,GETEL                                                         
         BNE   DSX                                                              
         OI    OHDSUPPH+4,X'20'                                                 
*                                                                               
         MVC   OHDPOA1,ADRADD1                                                  
         NI    OHDPOA1H+4,X'FF'-X'80'                                           
         OI    OHDPOA1H+6,X'80'                                                 
*                                                                               
         MVC   OHDPOA2,SPACES                                                   
         OI    OHDPOA2H+6,X'80'                                                 
         CLI   ADRNUM,2                                                         
         BL    DS10                                                             
         MVC   OHDPOA2,ADRADD2                                                  
         NI    OHDPOA2H+4,X'FF'-X'80'                                           
*                                                                               
DS10     MVC   OHDPOA3,SPACES                                                   
         OI    OHDPOA3H+6,X'80'                                                 
         CLI   ADRNUM,3                                                         
         BL    DSX                                                              
         MVC   OHDPOA3,ADRADD3                                                  
         NI    OHDPOA3H+4,X'FF'-X'80'                                           
*                                                                               
DSX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY CATEGORY                                             *         
***********************************************************************         
*                                                                               
         USING OAMELD,R6                                                        
         USING CTGTABD,R3                                                       
DISCTGRY NTR1                                                                   
         MVC   CTGWKCDE,SPACES                                                  
         L     R6,AIO3                                                          
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   CTGWKCDE,OAMWORK                                                 
         LA    R3,CTGTABLE         CATEGORY TABLE                               
DC10     ZICM  R2,CTGFIELD,2       DISP TO FIELD HEADER                         
         AR    R2,RA                                                            
         MVI   8(R2),X'40'                                                      
         OI    6(R2),X'80'                                                      
         CLC   CTGWKCDE,CTGCODE                                                 
         BNE   DC20                                                             
         MVI   8(R2),C'X'                                                       
DC20     LA    R3,CTGLENQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   DC10                                                             
DCX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY SHIP TO ADDRESS                                      *         
***********************************************************************         
*                                                                               
         USING ADRELD,R6                                                        
DISSHIP  NTR1                                                                   
         MVC   OHDSHP1,SPACES                                                   
         OI    OHDSHP1H+6,X'80'                                                 
         MVC   OHDSHP2,SPACES                                                   
         OI    OHDSHP2H+6,X'80'                                                 
         MVC   OHDSHP3,SPACES                                                   
         OI    OHDSHP3H+6,X'80'                                                 
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,OADELQ       X'8C' OTHER ADDRESS ELEM                     
         BAS   RE,GETEL                                                         
         BNE   DSHP100                                                          
         MVC   OHDSHP1,ADRADD1                                                  
         CLI   ADRNUM,2                                                         
         BL    DSHP100                                                          
         MVC   OHDSHP2,ADRADD2                                                  
         CLI   ADRNUM,3                                                         
         BL    DSHP100                                                          
         MVC   OHDSHP3,ADRADD3                                                  
*                                                                               
         USING FFTELD,R6                                                        
DSHP100  MVC   OHDSATN,SPACES      ATTENTION NAME                               
         OI    OHDSATNH+6,X'80'                                                 
         L     R6,AIO3                                                          
         MVI   ELCODE,FFTELQ       X'DB' FREEFORM = SHIP TO ATTN                
         BAS   RE,GETEL                                                         
         BNE   DSHPX                                                            
         ZIC   R1,FFTDLEN                                                       
         SH    R1,=H'1'                                                         
         BM    DSHPX                                                            
         EX    R1,*+4                                                           
         MVC   OHDSATN(0),FFTDATA                                               
*                                                                               
DSHPX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CLEAR DETAIL SCREEN                                                    
***********************************************************************         
*                                                                               
CLRDET   NTR1                                                                   
         LA    R2,ODTLIN1H         START FIELD                                  
         LA    R3,ODTTOTSH         END FIELD                                    
*                                                                               
CLR10    DS    0H                                                               
*        TM    1(R2),X'20'         PROTECTED                                    
*        BO    CLR20                                                            
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+4                                                           
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),X'00'                                                      
         NI    1(R2),X'FF'-X'20'   UN-PROTECT                                   
         OI    6(R2),X'80'         TRANSMIT                                     
CLR20    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLR10               NO                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LISTRECS                                                     *         
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
         GOTO1 =A(LRECS),DMCB,RR=RELO                                           
         B     XIT                                                              
***********************************************************************         
*        GET STANDARD COMMENT RECORD                                            
***********************************************************************         
*                                                                               
         USING SCMRECD,R6                                                       
STDCOMM  NTR1                                                                   
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVI   SCMKTYP,SCMKTYPQ    X'0C'                                        
         MVC   SCMKCPY,CMPY                                                     
         MVC   SCMKCODE,COMMCODE                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'SCMKEY),BIGKEY                                         
         BNE   SCX                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING SCMELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,SCMELQ       X'3E'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SC10     BAS   RE,NEXTEL                                                        
         BNE   SCX                                                              
*                                                                               
         ZIC   R1,SCMLN                                                         
         LA    R0,SCMLN1Q+1                                                     
         SR    R1,R0                                                            
         BM    SC10                                                             
         EX    R1,*+4                                                           
         MVC   P(0),SCMNARR                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     SC10                                                             
*                                                                               
SCX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ERRORS AND RANDOM STUFF                                      *         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
ERRECNF  MVC   GERROR,=AL2(ACERECNF)   RECORD NOT FOUND                         
         B     ACCERRX                                                          
ERRECXS  MVC   GERROR,=AL2(ACERECEX)   RECORD ALREADY EXISTS                    
         B     ACCERRX                                                          
ERINVAMT MVC   GERROR,=AL2(ACEAMNT)    INVALID AMOUNT                           
         B     ACCERRX                                                          
ERRLSTCK MVC   GERROR,=AL2(ACELSTCK)   STOCK NUMBER TOO LONG                    
         B     ACCERRX                                                          
ERNOCHA  MVC   GERROR,=AL2(ACENOCHA)   CANNOT CHANGE - ITEM INVOICED            
         B     ACCERRX                                                          
ERINVSUP MVC   GERROR,=AL2(ACEINVSP)   INVALID SUPPLIER                         
         B     ACCERRX                                                          
ERNOHD   MVC   GERROR,=AL2(ACEMISSO)   MISSING ORDER REC INFO                   
         B     ACCERRX                                                          
ERNOTCON MVC   GERROR,=AL2(ACENTCON)   ORDER IS NOT A CONTRACT ORDER            
         B     ACCERRX                                                          
ERINVORF MVC   GERROR,=AL2(ACEINVOR)   INVALID ORDER NUMBER FORMAT              
         B     ACCERRX                                                          
ERINVDTE MVC   GERROR,=AL2(ACEIVDTE)   INVALID DATE                             
         B     ACCERRX                                                          
ERRMEDIA MVC   GERROR,=AL2(ACEINVMD)   INVALID MEDIA                            
         B     ACCERRX                                                          
ERRNOS1  MVC   GERROR,=AL2(ACENOS1)    S1 ACCOUNT DOESN'T EXIST                 
         B     ACCERRX                                                          
ERRNOS2  MVC   GERROR,=AL2(ACENOS2)    S2 ACCOUNT DOESN'T EXIST                 
         B     ACCERRX                                                          
ERINVCON MVC   GERROR,=AL2(ACEINVCN)   INVALID CONTRACT                         
         B     ACCERRX                                                          
ERINVCC  MVC   GERROR,=AL2(ACECTCON)   INVALID CONTRACT FOR CONTRACTOR          
         B     ACCERRX                                                          
ERINVCTR MVC   GERROR,=AL2(ACEINVCT)   INVALID CONTRACTOR                       
         B     ACCERRX                                                          
ERINVCTG MVC   GERROR,=AL2(ACEINVCG)   INVALID CATEGORY                         
         B     ACCERRX                                                          
ERRNONET MVC   GERROR,=AL2(ACENONET)   NO NET PAY FOR CATEGORY                  
         B     ACCERRX                                                          
ERRCONNF MVC   GERROR,=AL2(ACECONNF)   NO CONTRACT FOUND - OVERDELIVER?         
         B     ACCERRX                                                          
ERRCONLK MVC   GERROR,=AL2(ACECONLK)   CONTRACT LOCKED                          
         B     ACCERRX                                                          
ERRCONCL MVC   GERROR,=AL2(ACECONCL)   CONTRACT CLOSED                          
         B     ACCERRX                                                          
ERRINQTY MVC   GERROR,=AL2(ACEINQTY)   QTY LESS THAN INVOICED                   
         B     ACCERRX                                                          
ERRDELOR MVC   GERROR,=AL2(ACEDELOR)   CANNOT DELETE ORDER - DEL ITEMS          
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        HEADSPECS AND HEADHOOK                                                 
***********************************************************************         
*                                                                               
HDSPECS  DS    0H                                                               
*        SSPEC H1,1,AGYNAME                                                     
*        SSPEC H2,1,AGYADD                                                      
         DC    X'00'                                                            
*                                                                               
HDHK     NTR1                                                                   
         MVC   H1+60(6),=C'ORDER#'                                              
         MVC   H1+68(L'ORDNUM),ORDNUM                                           
         MVC   H2+60(4),=C'DATE'                                                
         MVC   H2+68(L'CPODATE),CPODATE                                         
         MVC   H1(2),=C'TO'                                                     
         MVC   H1+12(L'SUPNAME),SUPNAME                                         
         MVC   H2+12(L'POADR1),POADR1                                           
         MVC   H3+12(L'POADR2),POADR2                                           
         MVC   H4+12(L'POADR3),POADR3                                           
         MVC   H5(4),=C'ATTN'                                                   
         MVC   H5+12(L'POAATTN),POAATTN                                         
         MVC   H4+60(6),=C'DUE BY'                                              
         MVC   H4+68(L'CDUEDATE),CDUEDATE                                       
         MVC   H6(80),DOTLINE                                                   
         MVC   H7(7),=C'SHIP TO'                                                
         MVC   H7+12(L'SHPADR1),SHPADR1                                         
         MVC   H8+12(L'SHPADR2),SHPADR2                                         
         MVC   H9+12(L'SHPADR3),SHPADR3                                         
         MVC   H10(4),=C'ATTN'                                                  
         MVC   H10+12(L'SHPATTN),SHPATTN                                        
         MVC   H7+60(16),=C'* SHIP PREPAID *'                                   
         MVC   H8+60(15),=C'* NON TAXABLE *'                                    
         CLI   TAXABLE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   H8+60(15),=C'* TAXABLE *    '                                    
         MVC   H11(80),DOTLINE                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLES                                                                 
***********************************************************************         
*                                                                               
CTGTABLE DC    C'MD',AL2(OHDMDSEH-T61EFFD)                                      
         DC    C'AX',AL2(OHDAMEXH-T61EFFD)                                      
         DC    C'CA',AL2(OHDCASHH-T61EFFD)                                      
         DC    C'HL',AL2(OHDHOTLH-T61EFFD)                                      
         DC    C'SC',AL2(OHDSCRPH-T61EFFD)                                      
         DC    C'OR',AL2(OHDOTHRH-T61EFFD)                                      
         DC    X'FF'                                                            
*                                                                               
NARTABLE DC    X'00',AL1(SCMTSTND),AL2(ODTNAR1H-T61EFFD)                        
         DC    X'01',AL1(SCMTSTND),AL2(ODTNAR2H-T61EFFD)                        
         DC    X'02',AL1(SCMTSTND),AL2(ODTNAR3H-T61EFFD)                        
         DC    X'03',AL1(SCMTPOFT),AL2(ODTFOOTH-T61EFFD)                        
         DC    X'FF'                                                            
*                                                                               
DOTLINE  DC    80C'-'                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SUPPLIER ADDRESS                                    *         
***********************************************************************         
*                                                                               
         USING ADRELD,R3                                                        
VALSUPAD NMOD1 0,**VSAD**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   ELCODE,ADRELQ       X'22'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R3,SUPPADR          ADDRESS ELEM                                 
         TM    OHDPOA1H+4,X'80'    INPUT THIS TIME                              
         BO    VS100                                                            
         TM    OHDPOA2H+4,X'80'                                                 
         BO    VS100                                                            
         TM    OHDPOA3H+4,X'80'                                                 
         BO    VS100                                                            
         CLC   SUPPADR,SPACES      COMES IN FIRST TIME W/ SPACES                
         BE    VS100                                                            
         MVI   ADREL,ADRELQ        X'22' - MAKE SURE NOT X'8C' OVERRIDE         
         ZIC   R1,ADRLN                                                         
         SH    R1,=H'1'                                                         
         BM    VS100                                                            
         EX    R1,*+4                                                           
         MVC   ELEM(0),SUPPADR                                                  
         GOTO1 ADDELEM                                                          
         B     VS200                                                            
*                                                                               
VS100    XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         MVI   ADREL,ADRELQ        X'22'                                        
         MVI   ADRNUM,1                                                         
         MVI   ADRLN,ADRLN1Q                                                    
         LA    R2,OHDPOA1H                                                      
         CLI   OHDPOA1H+5,0                                                     
         BE    ERRMISS                                                          
         MVC   ADRADD1,OHDPOA1                                                  
         OC    ADRADD1,SPACES                                                   
*                                                                               
         CLI   OHDPOA2H+5,0                                                     
         BE    VS120                                                            
         MVC   ADRADD2,OHDPOA2                                                  
         OC    ADRADD2,SPACES                                                   
         MVI   ADRLN,ADRLN2Q                                                    
         MVI   ADRNUM,2                                                         
*                                                                               
         CLI   OHDPOA3H+5,0                                                     
         BE    VS120                                                            
         MVC   ADRADD3,OHDPOA3                                                  
         OC    ADRADD3,SPACES                                                   
         MVI   ADRLN,ADRLN3Q                                                    
         MVI   ADRNUM,3                                                         
VS120    MVC   SUPPADR,ELEM                                                     
         GOTO1 ADDELEM                                                          
*                                                                               
VS200    BAS   RE,DISSUPAD                                                      
*                                                                               
         LA    R2,OHDATTNH                                                      
         MVC   POAATTN,SPACES      ATTENTION NAME                               
         ZICM  R1,OHDATTNH+5,1                                                  
         BZ    VSX                                                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   POAATTN,OHDATTN                                                  
*                                                                               
VSX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        FIND CONTRACT TO COVER AMOUNT OF ORDER                                 
***********************************************************************         
*                                                                               
FINDCON  NMOD1 0,**FC****                                                       
         L     RC,SAVERC                                                        
*                                                                               
         BAS   RE,BLDCONT          TABLE OF CONTRACTS FOR CONTRACTOR            
         OC    CONTLIST,CONTLIST   ANY CONTRACTS                                
         BZ    FCXNO                                                            
         LA    R3,CONTLIST                                                      
         MVC   AIO,AIO2                                                         
         B     FC10HI                                                           
FC10SEQ  LA    R3,L'CON#(R3)       NEXT CONTRACT                                
         CLI   0(R3),X'FF'         ANY MORE                                     
         BE    FCXNO                                                            
*                                                                               
         USING ACTRECD,R6                                                       
FC10HI   LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'S'                                                     
         MVI   ACTKLDG,C'2'                                                     
         MVC   ACTKACT(L'QSYSMED),QSYSMED                                       
         MVC   ACTKACT+L'QSYSMED(L'CON#),0(R3)                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    ACTKSTAT,ACTSLOCK+ACTSCLOS                                       
         BNZ   FC10SEQ                                                          
         GOTO1 GETREC                                                           
*                                                                               
         USING CTGELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93' CONTRACT CATEGORY ELEM                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FC20NX   BAS   RE,NEXTEL                                                        
         BNE   FC10SEQ             CHECK NEXT CONTRACT                          
         CLC   CTGWKCDE,CTGCTGY    MATCHING WORK CODE                           
         BNE   FC20NX                                                           
*                                                                               
         ZAP   WORK(10),CTGNPAY    NET PAY                                      
         SP    WORK(10),CTGOPNPO                                                
         SP    WORK(10),CTGINVPO   TOTAL GOODS DELIVERED                        
         CLI   CTGLN,CTGLNQ        ANY BAL B/F                                  
         BNH   *+10                                                             
         SP    WORK(10),CTGBINV                                                 
         CP    TOTRET,WORK(10)     ENOUGH LEFT TO COVER ORDER                   
         BH    FC10SEQ                                                          
         MVC   CON#,0(R3)                                                       
         MVC   S2ACCNT+L'QSYSMED(L'CON#),CON#                                   
*                                                                               
FCXYES   B     XYES                                                             
FCXNO    B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD CONTRACT LIST GIVEN CONTRACTOR                                   
***********************************************************************         
*                                                                               
BLDCONT  NTR1                                                                   
         XC    CONTLIST,CONTLIST                                                
         LA    R3,CONTLIST                                                      
*                                                                               
         USING CHDRECD,R6          LOOK FOR CONTRA ACCOUNT HEADER RECS          
BC10     LA    R6,BIGKEY           TO GET ALL CONTRACTS FOR CONTRACTOR          
         MVC   BIGKEY,SPACES                                                    
         MVC   CHDKCPY,CMPY                                                     
         MVI   CHDKUNT,C'S'                                                     
         MVI   CHDKLDG,C'1'                                                     
         MVC   CHDKACT(L'QSYSMED),QSYSMED               SYSTEM/MEDIA            
         MVC   CHDKACT+L'QSYSMED(L'CONTRCTR),CONTRCTR   CONRACTOR               
         MVC   CHDKCCPY,CMPY                                                    
         MVI   CHDKCUNT,C'S'                                                    
         MVI   CHDKCLDG,C'2'                                                    
         MVC   CHDKCACT(L'QSYSMED),QSYSMED                                      
         MVC   CHDKCACT+L'QSYSMED(L'CON#),CON#                                  
         XC    CHDKNULL,CHDKNULL                                                
         MVC   SAVEKEY,BIGKEY                                                   
BCHIGH   GOTO1 HIGH                                                             
         B     BC20                                                             
BCSEQ    GOTO1 SEQ                                                              
BC20     CLC   BIGKEY(L'CHDKCULA),SAVEKEY         SAME ACCOUNT                  
         BNE   BCX                                                              
         CLC   CHDKWRK,SPACES                     OFFICE IS SPACES              
         BNE   BCSEQ                                                            
         CLC   =C'S2',CHDKCUNT                    CONTRA S2                     
         BNE   BCSEQ                                                            
         CLC   CHDKCACT(L'QSYSMED),QSYSMED        CONTRA SYSTEM/MEDIA           
         BNE   BCSEQ                                                            
         OC    CHDKNULL,CHDKNULL                  CONTRA ACT HEADER?            
         BNZ   BCSEQ                                                            
         MVC   0(L'CON#,R3),CHDKCACT+L'QSYSMED                                  
         LA    R3,L'CON#(R3)                                                    
         LA    R1,CONTLEND                                                      
         MVI   CHDKNULL,X'FF'      SKIP TRANSACTIONS                            
         CR    R3,R1                                                            
         BL    BCHIGH                                                           
         DC    H'0'                CONTRACT TABLE FULL                          
*                                                                               
BCX      MVI   0(R3),X'FF'         END OF TABLE                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        LISTRECS                                                     *         
***********************************************************************         
*                                                                               
LRECS    NMOD1 0,*LRECS**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING LSTLINED,R2                                                      
         MVC   AIO,AIO3                                                         
         LA    R2,LISTAR                                                        
         OI    GENSTAT2,DISTHSPG   REDISPLAY SAME PAGE AFTER SEL                
         MVI   NLISTS,13                                                        
*                                                                               
         USING ORDRECD,R6                                                       
         OC    BIGKEY,BIGKEY                                                    
         BNZ   LRHI                                                             
         MVC   BIGKEY(L'ORDKEY),SAVEKEY                                         
LRHI     GOTO1 HIGH                                                             
         B     LR10                                                             
LRSEQ    GOTO1 SEQ                                                              
LR10     CLC   BIGKEY(L'ORDKTYP+L'ORDKCPY),KEYSAVE                              
         BNE   LRX                                                              
         LA    R6,BIGKEY                                                        
         TM    ORDKSTAT,ORDSCON    CONTRACT ORDER                               
         BNO   LRSEQ                                                            
         MVC   SAVEKEY,BIGKEY                                                   
         GOTO1 GETREC                                                           
         EJECT                                                                  
***********************************************************************         
*        FILTER CHECK                                                           
***********************************************************************         
*                                                                               
         L     R6,AIO                                                           
         MVC   ORDNUM,ORDKORD                                                   
         MVC   LSTORD#,ORDKORD                                                  
*                                                                               
         USING ORDELD,R6                                                        
         MVI   ELCODE,ORDELQ       X'67'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   ORDACCA(L'QSYSMED),QSYSMED       SYSTEM AND MEDIA                
         BNE   LRSEQ                                                            
         CLC   ORDACCA+L'QSYSMED(L'CONTRCTR),CONTRCTR   CONTRACTOR              
         BNE   LRSEQ                                                            
         MVC   LSTSUPP,ORDSUPA                                                  
         GOTO1 DATCON,DMCB,(1,ORDDATE),(11,LSTDATE)                             
*                                                                               
         USING OAMELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,OAMELQ       X'68'                                        
         BAS   RE,GETEL                                                         
         BNE   LRSEQ                                                            
         CLC   CON#,SPACES                                                      
         BNH   LR42                                                             
         CLC   OAMCONA+L'QSYSMED(L'CON#),CON#      CONTRACT                     
         BNE   LRSEQ                                                            
LR42     OC    CATFILT,CATFILT                                                  
         BZ    LR50                                                             
         CLC   CATFILT,OAMWORK     CATEGORY                                     
         BNE   LRSEQ                                                            
*                                                                               
LR50     DS    0H                                                               
         MVC   LSTCON#,OAMCONA+L'QSYSMED                                        
         MVC   LSTCTG,OAMWORK      CATEGORY                                     
         EDIT  (P6,OAMAMNT),(11,LSTAMNT),2,MINUS=YES                            
*                                                                               
         USING ORDRECD,R3                                                       
         L     R3,AIO                                                           
         MVC   LSTSTAT,=C'OPEN'                                                 
         CP    OAMINUM,=P'0'       ANY INVOICES = PARTIAL                       
         BNH   *+10                                                             
         MVC   LSTSTAT,=C'PRTL'                                                 
         TM    ORDRSTAT,ORDSFMCH   FULLY MATCHED = CLOSED                       
         BNO   *+10                                                             
         MVC   LSTSTAT,=C'CLSD'                                                 
*                                                                               
         GOTO1 LISTMON                                                          
         BE    LRSEQ                                                            
*                                                                               
LRX      B     XIT                                                              
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT PURCHASE ORDER - HEADLINES                                       
***********************************************************************         
*                                                                               
PR       NMOD1 0,**PR****                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R1,HDSPECS           HEAD SPECS AND HEAD HOOK                    
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    BIGKEY,BIGKEY       GET ORDER RECORD                             
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'ACTKEY),BIGKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING ORDRECD,R6                                                       
         L     R6,AIO                                                           
         MVC   ORDNUM,ORDKORD      ORDER NUMBER                                 
*                                                                               
         USING ORDELD,R6           ORDER ELEM                                   
         MVI   ELCODE,ORDELQ       X'67'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(1,ORDDATE),(11,CPODATE)                             
         GOTO1 DATCON,DMCB,(1,ORDDDTE),(11,CDUEDATE)                            
         MVC   AUTHBY,ORDAUTH                                                   
         MVC   POAATTN,ORDATTN                                                  
         MVC   TAXABLE,ORDTAX                                                   
         MVC   SUPACCNT,ORDSUPA                                                 
         MVC   AIO,AIO2                                                         
         MVC   DMCB(L'SUPPUL),ORDSUPU                                           
         GOTO1 GTLEVNM,DMCB,,SUPACCNT,0                                         
         MVC   AIO,AIO1                                                         
         MVC   SUPNAME,WORK                                                     
*                                                                               
         USING ADRELD,R6           PO ADDRESS                                   
         MVC   POADR1,SPACES                                                    
         MVC   POADR2,SPACES                                                    
         MVC   POADR3,SPACES                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,ADRELQ       X'22'                                        
         BAS   RE,GETEL                                                         
         BNE   PR200                                                            
         MVC   POADR1,ADRADD1                                                   
         CLI   ADRNUM,2                                                         
         BL    PR200                                                            
         MVC   POADR2,ADRADD2                                                   
         CLI   ADRNUM,3                                                         
         BL    PR200                                                            
         MVC   POADR3,ADRADD3                                                   
*                                                                               
PR200    MVC   SHPADR1,SPACES      SHIP TO ADDRESS                              
         MVC   SHPADR2,SPACES                                                   
         MVC   SHPADR3,SPACES                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,OADELQ       X'8C' OTHER ADDRESS ELEM                     
         BAS   RE,GETEL                                                         
         BNE   PR300                                                            
         MVC   SHPADR1,ADRADD1                                                  
         CLI   ADRNUM,2                                                         
         BL    PR300                                                            
         MVC   SHPADR2,ADRADD2                                                  
         CLI   ADRNUM,3                                                         
         BL    PR300                                                            
         MVC   SHPADR3,ADRADD3                                                  
*                                                                               
         USING FFTELD,R6                                                        
PR300    MVC   SHPATTN,SPACES      ATTENTION NAME                               
         L     R6,AIO                                                           
         MVI   ELCODE,FFTELQ       X'DB' FREEFORM = SHIP TO ATTN                
         BAS   RE,GETEL                                                         
         BNE   PR400                                                            
         ZIC   R1,FFTDLEN                                                       
         SH    R1,=H'1'                                                         
         BM    PR400                                                            
         EX    R1,*+4                                                           
         MVC   SHPATTN(0),FFTDATA                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT PURCHASE ORDER - DETAIL LINES                                    
***********************************************************************         
*                                                                               
         USING CPOELD,R6                                                        
PR400    L     R6,AIO                                                           
         MVI   ELCODE,CPOELQ       X'94'                                        
         BAS   RE,GETEL                       MAKE SURE THERE'S INFO            
         BNE   PR500                                                            
*                                                                               
         ZAP   TOTCOST,=P'0'                                                    
         MVC   PRQTY(3),=C'QTY'               PRINT COLUMN HEADINGS             
         MVC   PRQTYD(4),=C'DLVD'                                               
         MVC   PRSTOCK(6),=C'STOCK#'                                            
         MVC   PRDESC(11),=C'DESCRIPTION'                                       
*        MVC   PRRPR+2(8),=C'UNIT RET'                                          
         MVC   PRUPR+1(9),=C'UNIT COST'                                         
         MVC   PRTOT(10),=C'TOTAL COST'                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(80),DOTLINE                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR420                                                            
*                                                                               
PR410    BAS   RE,NEXTEL                                                        
         BNE   PR500                                                            
*                                                                               
PR420    EDIT  (P3,CPOQTY),(5,PRQTY),ALIGN=LEFT        QUANTITY                 
         EDIT  (P3,CPOQTYIN),(5,PRQTYD),ALIGN=LEFT     QUANTITY DLVD            
*                                                                               
         MVC   PRSTOCK,CPOSTOCK                        STOCK NO                 
         ZIC   R1,CPOLN                                                         
         LA    R0,CPOLN1Q+1                                                     
         SR    R1,R0                                                            
         BM    PR440                                                            
         CH    R1,=AL2(L'PRDESC-1)                                              
         BNH   *+8                                                              
         LA    R1,L'PRDESC-1                                                    
         EX    R1,*+4                                                           
         MVC   PRDESC(0),CPODESC                       DESCRIPTION              
*                                                                               
*R440    EDIT  (P6,CPOURET),(11,PRRPR),2,MINUS=YES     UNIT RETAIL              
PR440    EDIT  (P6,CPOUPRI),(11,PRUPR),2,MINUS=YES     UNIT PRICE               
*                                                                               
         ZAP   WORK(10),CPOUPRI                        UNIT PRICE*QTY           
         MP    WORK(10),CPOQTY                                                  
         ZAP   COSTEXT,WORK(10)                                                 
         EDIT  (P6,COSTEXT),(11,PRTOT),2,MINUS=YES                              
         AP    TOTCOST,COSTEXT                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR410                                                            
*                                                                               
PR500    MVC   PRTOT,DOTLINE       TOTAL COST                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (P6,TOTCOST),(11,PRTOT),2,MINUS=YES                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT PURCHASE ORDER - NARRATIVE AND FOOTLINES                         
***********************************************************************         
*                                                                               
         USING SCMELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,SCMELQ       X'3E'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PR510    BAS   RE,NEXTEL                                                        
         BNE   PR600                                                            
         CLC   =C'NARR=',SCMNARR                                                
         BNE   PR520                                                            
         MVC   COMMCODE,SPACES                                                  
         ZIC   R1,SCMLN                                                         
         LA    R0,SCMLN1Q+6        HEADER+5+1                                   
         SR    R1,R0                                                            
         BM    PR510                                                            
         EX    R1,*+4                                                           
         MVC   COMMCODE(0),SCMNARR+5                                            
         GOTO1 ARIGHT,DMCB,COMMCODE,L'COMMCODE                                  
         BAS   RE,STDCOMM          GET STANDARD COMMENT REC                     
         B     PR510                                                            
*                                                                               
PR520    ZIC   R1,SCMLN                                                         
         LA    R0,SCMLN1Q+1                                                     
         SR    R1,R0                                                            
         BM    PR510                                                            
         EX    R1,*+4                                                           
         MVC   P(0),SCMNARR                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR510                                                            
*                                                                               
PR600    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NMOD1 0,*SETUP**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVC   PFORDACT,SPACES                                                  
         MVC   PFDETACT,SPACES                                                  
         CLI   ACTEQU,ACTSEL       SELECT NOT VALID FOR PFING                   
         BNE   *+16                                                             
         MVC   PFORDACT,=C'DISPLAY'                                             
         MVC   PFDETACT,=C'DISPLAY'                                             
*                                                                               
         CLI   PFKEY,5             PFING BETWEEN DET AND ORD                    
         BE    SET10               NEED TO DISPLAY                              
         CLI   PFKEY,6                                                          
         BNE   SET20                                                            
SET10    CLI   ACTEQU,ACTCHA       WITH ACTION CHANGE                           
         BNE   *+8                                                              
         OI    TRANSTAT,RCHANG     SET CHANGE                                   
*                                                                               
SET20    L     R1,=V(RIGHT)                                                     
         A     R1,RELO                                                          
         ST    R1,ARIGHT                                                        
*                                                                               
         LA    R2,PFTABLE          PF KEYS                                      
         LA    R3,OHDPFKYH         ORDER SCREEN                                 
         CLI   ACTEQU,ACTLIST      LIST                                         
         BNE   *+16                                                             
         LA    R2,LPFTABLE         LIST PF KEYS                                 
         LA    R3,ORLPFKYH         LIST SCREEN                                  
         B     SET50                                                            
         CLI   RECNUM,RTDET        DETAIL SCREEN                                
         BNE   SET50                                                            
         LA    R2,DPFTABLE                                                      
         LA    R3,ODTPFKYH                                                      
SET50    GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
SUX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        ORDER HEADER PFKEY TABLE DEFINITIONS                                   
***********************************************************************         
PFTABLE  DS    0C                                                               
         DC    AL1(PF02X-*,02,PFTCPROG,(PF02X-PF02)/KEYLNQ,0)                   
         DC    CL3' ',CL8'CTAGS   ',CL8'DISPLAY '                               
PF02     DC    AL1(KEYTYTWA,L'OHDMED-1),AL2(OHDMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'OHDCONT-1),AL2(OHDCONT-T61EFFD)                   
PF02X    EQU   *                                                                
*                                                                               
         DC    AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'LIST    '                               
PF03     DC    AL1(KEYTYTWA,L'OHDMED-1),AL2(OHDMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'OHDCNTR-1),AL2(OHDCNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'OHDCONT-1),AL2(OHDCONT-T61EFFD)                   
PF03X    EQU   *                                                                
*                                                                               
         DC    AL1(PF04X-*,04,PFTCPROG,(PF04X-PF04)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'DISPLAY '                               
PF04     DC    AL1(KEYTYTWA,L'OHDMED-1),AL2(OHDMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'OHDCNTR-1),AL2(OHDCNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'OHDCONT-1),AL2(OHDCONT-T61EFFD)                   
PF04X    EQU   *                                                                
*                                                                               
         DC    AL1(PF06X-*,06,PFTCPROG,(PF06X-PF06)/KEYLNQ,0)                   
         DC    CL3' ',CL8'DETAIL  '                                             
PFDETACT DC    CL8'       '                                                     
PF06     DC    AL1(KEYTYTWA,L'OHDORDN-1),AL2(OHDORDN-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'OHDCONT-1),AL2(OHDCONT-T61EFFD)                   
PF06X    EQU   *                                                                
*                                                                               
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        LIST PFKEY TABLE DEFINITIONS                                           
***********************************************************************         
LPFTABLE DS    0C                                                               
         DC    AL1(LPF02X-*,02,PFTCPROG,(LPF02X-LPF02)/KEYLNQ,0)                
         DC    CL3' ',CL8'CTAGS   ',CL8'DISPLAY '                               
LPF02    DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T61EFFD)                     
         DC    AL1(KEYTYCUR,L'LSTCON#-1),AL2(LSTCON#-LSTLINED)                  
LPF02X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF03X-*,03,PFTCPROG,(LPF03X-LPF03)/KEYLNQ,0)                
         DC    CL3' ',CL8'FIN     ',CL8'LIST    '                               
LPF03    DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'ORLCNTR-1),AL2(ORLCNTR-T61EFFD)                   
         DC    AL1(KEYTYCUR,L'LSTCON#-1),AL2(LSTCON#-LSTLINED)                  
LPF03X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF04X-*,04,PFTCPROG,(LPF04X-LPF04)/KEYLNQ,0)                
         DC    CL3' ',CL8'FIN     ',CL8'DISPLAY '                               
LPF04    DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'ORLCNTR-1),AL2(ORLCNTR-T61EFFD)                   
         DC    AL1(KEYTYCUR,L'LSTCON#-1),AL2(LSTCON#-LSTLINED)                  
LPF04X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        ORDER DETAILS PFKEY TABLE DEFINITIONS                                  
***********************************************************************         
DPFTABLE DS    0C                                                               
         DC    AL1(DPF02X-*,02,PFTCPROG,(DPF02X-DPF02)/KEYLNQ,0)                
         DC    CL3' ',CL8'CTAGS   ',CL8'DISPLAY '                               
DPF02    DC    AL1(KEYTYTWA,L'QSYSMED-1),AL2(S2ACCNT-T61EFFD)                   
         DC    AL1(KEYTYTWA,5-1),AL2(S2ACCNT+2-T61EFFD)                         
DPF02X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF03X-*,03,PFTCPROG,(DPF03X-DPF03)/KEYLNQ,0)                
         DC    CL3' ',CL8'FIN     ',CL8'LIST    '                               
DPF03    DC    AL1(KEYTYTWA,L'QSYSMED-1),AL2(S2ACCNT-T61EFFD)                   
         DC    AL1(KEYTYTWA,6-1),AL2(S1ACCNT+2-T61EFFD)                         
         DC    AL1(KEYTYTWA,5-1),AL2(S2ACCNT+2-T61EFFD)                         
DPF03X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF04X-*,04,PFTCPROG,(DPF04X-DPF04)/KEYLNQ,0)                
         DC    CL3' ',CL8'FIN     ',CL8'DISPLAY '                               
DPF04    DC    AL1(KEYTYTWA,L'QSYSMED-1),AL2(S2ACCNT-T61EFFD)                   
         DC    AL1(KEYTYTWA,6-1),AL2(S1ACCNT+2-T61EFFD)                         
         DC    AL1(KEYTYTWA,5-1),AL2(S2ACCNT+2-T61EFFD)                         
DPF04X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF05X-*,05,PFTCPROG,(DPF05X-DPF05)/KEYLNQ,0)                
         DC    CL3' ',CL8'ORDER   '                                             
PFORDACT DC    CL8'       '                                                     
DPF05    DC    AL1(KEYTYTWA,L'ODTORDN-1),AL2(ODTORDN-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'ODTCONT-1),AL2(ODTCONT-T61EFFD)                   
DPF05X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
DPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        SCREEENS                                                     *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACCTAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCTAF5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCTAF8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCTAF6D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                          *         
***********************************************************************         
*                                                                               
STARTWRK DS    0F                                                               
RELO     DS    A                                                                
SAVERC   DS    F                   SAVED RC                                     
STADDR   DS    F                   START ADDRESS                                
ARIGHT   DS    F                   A(RIGHT)                                     
WORK2    DS    4F                                                               
*                                                                               
ORDNUM   DS    CL6                 ORDER NUMBER                                 
SVORDNUM DS    CL6                 SAVED ORDER NUMBER                           
PODATE   DS    PL3                 ORDER DATE                                   
CPODATE  DS    CL8                 ORDER DATE                                   
DATEDUE  DS    PL3                 DUE DATE                                     
CDUEDATE DS    CL8                 DUE DATE                                     
AUTHBY   DS    CL15                AUTHORIZED BY                                
POAATTN  DS    CL20                PO ADDRESS ATTENTION                         
TAXABLE  DS    CL1                 TAX                                          
CTGWKCDE DS    CL2                 CATEGORY WORK CODE                           
CATFILT  DS    CL2                 CATEGORY WORK CODE FILTER (LIST)             
SHPATTN  DS    CL20                SHIPPING ADDRESS ATTENTION                   
STOCK#   DS    CL8                 STOCK NUMBER                                 
DESCLEN  DS    XL1                 DESCRIPTION LEN                              
DESC     DS    CL25                DESCRIPTION                                  
COMMCODE DS    CL6                 STANDARD COMMENT CODE                        
SVCTRCTR DS    CL(L'CONTRCTR)                                                   
*                                                                               
LINENUM  DS    XL1                 LINE NUMBER                                  
QTY      DS    PL4                 QUANTITY                                     
UNITPRI  DS    PL6                 UNIT PRICE                                   
UNITRET  DS    PL6                 UNIT RETAIL                                  
URATIO   DS    PL4                 RATIO FOR RETAIL EXTENSION                   
COSTEXT  DS    PL6                 COST EXTENSION                               
RETEXT   DS    PL6                 RETAIL EXTENSION                             
TOTCOST  DS    PL6                 TOTAL COST EXT                               
TOTRET   DS    PL6                 TOTAL RETAIL EXT                             
*                                                                               
POSTAT   DS    XL1                                                              
POSAUTO  EQU   X'80'               AUTO GENERATE ORDER NUMBER                   
POSADDEL EQU   X'40'               NEED TO ADD ELEM                             
POSHEAD  EQU   X'20'               HEADER HAS BEEN ADDED                        
POSINV   EQU   X'10'               ORDER HAS BEN INVOICED                       
POSKEYCH EQU   X'08'               KEY HAS CHANGED                              
POSCHECK EQU   X'04'               CHECK ORDER                                  
*                                                                               
SUPUL    DS    CL2                 SUPPLIER U/L                                 
SUPACCNT DS    CL12                SUPPLIER ACCOUNT                             
SUPNAME  DS    CL36                SUPPLIER NAME                                
S1ACCNT  DS    CL12                                                             
S2ACCNT  DS    CL12                                                             
SAVEKEY  DS    CL(L'ACTKEY)                                                     
*                                                                               
POADR1   DS    CL(L'ADRADD1)                                                    
POADR2   DS    CL(L'ADRADD1)                                                    
POADR3   DS    CL(L'ADRADD1)                                                    
SHPADR1  DS    CL(L'ADRADD1)                                                    
SHPADR2  DS    CL(L'ADRADD1)                                                    
SHPADR3  DS    CL(L'ADRADD1)                                                    
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE                                                          
***********************************************************************         
*                                                                               
STORED   DSECT                                                                  
CONTLIST DS    CL(CONTLSTQ)                                                     
CONTLEND DS    0C                                                               
CONTLSTQ EQU   30*(L'CON#)                                                      
*                                                                               
CONTAB   DS    CL(4*CONLENQ)                                                    
CONTABX  DS    0C                                                               
NEWOREC  DS    XL2000                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
*                                                                               
CTGTABD  DSECT                     CATEGORY TABLE DSECT                         
CTGCODE  DS    CL2                 WORK CODE                                    
CTGFIELD DS    XL2                 DISP TO FIELD HEADER                         
CTGLENQ  EQU   *-CTGTABD                                                        
*                                                                               
NARTABD  DSECT                     NARRATIVE AND FOOTLINES                      
NARLINE  DS    XL1                 LINE NUMBER                                  
NARTYPE  DS    XL1                 TYPE OF COMMENT                              
NARFIELD DS    XL2                 DISP TO FIELD HEADER                         
NARLENQ  EQU   *-NARTABD                                                        
*                                                                               
CONTABD  DSECT                     CONTRACT ACCNT AND AMOUNTS TABLE             
CONACCNT DS    CL12                CONTRACT ACCOUNT                             
CONOWC   DS    CL2                 OLD WORK CODE                                
CONOAMT  DS    PL6                 OLD AMOUNT FOR THIS CONTRACT                 
CONNWC   DS    CL2                 NEW WORK CODE                                
CONNAMT  DS    PL6                 NEW AMOUNT FOR THIS CONTRACT                 
CONLENQ  EQU   *-CONTABD                                                        
*                                                                               
DETLINED DSECT                     DETAIL LINE DSECT                            
DETQTYH  DS    XL8                                                              
DETQTY   DS    CL3                 QUANTITY                                     
DETDESCH DS    XL8                                                              
DETDESC  DS    CL25                DESCRIPTION                                  
DETUPRH  DS    XL8                                                              
DETUPR   DS    CL9                 UNIT PRICE                                   
DETRPRH  DS    XL8                                                              
DETRPR   DS    CL9                 RETAIL PRICE                                 
DETRATH  DS    XL8                                                              
DETRAT   DS    CL4                 RATIO                                        
DETCOSH  DS    XL8                                                              
DETCOS   DS    CL11                COST                                         
DETRETH  DS    XL8                                                              
DETRET   DS    CL11                RETAIL EXTENSION                             
DETLENQ  EQU   *-DETLINED                                                       
*                                                                               
*                                                                               
LSTLINED DSECT                     LIST LINE DSECT                              
LSTORD#  DS    CL6                 ORDER NUMBER                                 
         DS    CL2                                                              
LSTDATE  DS    CL8                 ORDER DATE                                   
         DS    CL2                                                              
LSTSUPP  DS    CL12                SUPPLIER                                     
         DS    CL1                                                              
LSTCON#  DS    CL5                 CONTRACTS                                    
         DS    CL3                                                              
LSTCTG   DS    CL2                 CATEGORY                                     
         DS    CL1                                                              
LSTAMNT  DS    CL12                AMOUNT                                       
         DS    CL1                                                              
LSTSTAT  DS    CL4                 STATUS                                       
LSTLENQ  EQU   *-LSTLINED                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                     *         
***********************************************************************         
*                                                                               
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        DDPERVALD                                                              
*        DDSCANBLKD                                                             
*        ACCTAWORKD                                                             
*        ACCTADSECT                                                             
*        ACGENFILE                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACCTAWORKD                                                     
       ++INCLUDE ACCTADSECT                                                     
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT DSECTS                                                 *         
***********************************************************************         
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PRQTY    DS    CL5                                                              
         DS    CL1                                                              
PRQTYD   DS    CL5                                                              
         DS    CL1                                                              
PRSTOCK  DS    CL8                                                              
         DS    CL1                                                              
PRDESC   DS    CL20                                                             
         DS    CL1                                                              
PRRPR    DS    CL11                UNIT RETAIL                                  
         DS    CL2                                                              
PRUPR    DS    CL11                UNIT PRICE                                   
         DS    CL2                                                              
PRTOT    DS    CL11                TOTAL PRICE                                  
         ORG                                                                    
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACCTA05   05/01/02'                                      
         END                                                                    
