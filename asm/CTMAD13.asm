*          DATA SET CTMAD13    AT LEVEL 035 AS OF 11/09/99                      
*PHASE TA0C13B,*                                                                
*                                                                               
***********************************************************************         
* SYMPTOM:        WORKER FILE CONTAINS ONLY THE LAST TRANSACTION'S DATA         
*                                                                               
* PROBABLE CAUSE: A CHANGE WAS MADE TO THE OVERLAY SAVED STORAGE THAT           
*                 AFFECTED THE SIZE OF THE SAVED STORAGE.                       
*                                                                               
* SOLUTION:       CHANGE THE 'MAGIC NUMBER' IN 'PRCSSALL' SO THAT THE           
*                 THE BEGINNING OF THE WORKER SAVE AREA IS WHERE IS             
*                 IT SHOULD BE.  DUMP 'DMWRKR' TO FIND THIS MAGIC               
*                 NUMBER WHEN IT COMPARES "SKLABEL,MYLABEL".                    
***********************************************************************         
*                                                                               
TA0C13   TITLE 'CTMAD13 - $MAD UPLOAD BUDGETS TO BUILD WORKER FILE'             
TA0C13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENFREE,TA0C13,RA                                                
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
***********************************************************************         
* INITIALIZE OVERLAY WIDE REGISTERS                                             
***********************************************************************         
         LR    RF,RC               RF = A(OVERLAY'S SPARE MEMEORY)              
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
*                                                                               
         ST    RF,AWRKRIOA         SAVE A(WORKER'S IO AREA)                     
         EJECT                                                                  
***********************************************************************         
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
***********************************************************************         
*                                                                               
INIT     NTR1                                                                   
*                                  INITIALIZE SYSTEM                            
         GOTO1 SETSYS,DMCB,=C'ACC',0,=CL8'ACCOUNT'                              
         BNE   EXIT                                                             
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET THE SYSTEM INPUT NUMBER                  
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   SYSINNUM,FASIN                                                   
         DROP  R1                                                               
*                                                                               
         LA    R1,WRKRBUFF         SAVE A(WORKER BUFFER)                        
         ST    R1,AWRKRBUF                                                      
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE START MODE.                                        
***********************************************************************         
*                                                                               
PROCSTRT NTR1                                                                   
         MVI   MISCFLG1,0          INITIALIZE ALL FLAGS                         
         XC    NUMBDGTS,NUMBDGTS   RESET NUMBER OF BUDGETS PROCESSED            
         XC    ULSTABLE(ULSTBLLN),ULSTABLE                                      
*                                                                               
         BAS   RE,PRCSSALL         PROCESS ALL OBJECTS IN INPUT FRAME           
*                                                                               
PSX      B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE PROCESSES THE CONTINUATION MODE.                                 
***********************************************************************         
*                                                                               
PROCMID  NTR1                                                                   
         BAS   RE,PRCSSALL         PROCESS ALL OBJECTS IN INPUT FRAME           
*                                                                               
PMX      B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE PROCESSES THE END MODE.  IT DOES NOTHING CURRENTLY.              
***********************************************************************         
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE OBJECTS FROM THE INPUT FRAME AND PROCESSES ALL          
* OF THEM                                                                       
***********************************************************************         
PRCSSALL NTR1                                                                   
*                                                                               
         TM    MISCFLG1,X'10'                                                   
         BZ    PALLLOOP                                                         
*                                                                               
         LH    R0,=Y(LENWKBUF)     RESTORE WORKER BUFFER                        
         GOTO1 TMPOPEN,DMCB,=C'GET',(R0)                                        
         BNE   EXIT                                                             
         GOTO1 GETTMP,DMCB,AWRKRBUF,(R0)                                        
         BNE   EXIT                                                             
         GOTO1 TMPCLOSE                                                         
         BNE   EXIT                                                             
*                                                                               
         L     R1,AWRKRBUF         R1 = A(BUFFER SAVE AREA)                     
         AH    R1,=H'3660'   <---------MAGIC NUMBER                             
         USING SKBUFFD,R1                                                       
         MVC   SKLABEL,=C'*WKSAVE*'                                             
         MVC   SKLABEL+3(L'SYSINNUM),SYSINNUM                                   
         DROP  R1                                                               
*                                                                               
PALLLOOP GOTO1 GETITEM             GET AN OBJECT FROM THE INPUT FRAME           
         BNE   EXIT                                                             
*                                                                               
         CLI   EIFFLAG,C'Y'        IF END OF FRAME                              
         BNE   PALL10                                                           
*                                                                               
         GOTO1 PUTITEM,DMCB,ITEOD,0,BLOCK                                       
         BNE   EXIT                                                             
*                                                                               
         TM    MISCFLG1,X'20'      THEN IF PREV OBJ WAS END-OF-DATA             
         BNZ   PALLDONE                 THEN WE'RE DONE                         
*                                                                               
         TM    MISCFLG1,X'10'                                                   
         BZ    PALL05                                                           
*                                                                               
         GOTO1 TMPOPEN,DMCB,=C'PUT'   SAVE WORKER BUFFER                        
         BNE   EXIT                                                             
         LH    R0,=Y(LENWKBUF)                                                  
         GOTO1 PUTTMP,DMCB,AWRKRBUF,(R0)                                        
         BNE   EXIT                                                             
         GOTO1 TMPCLOSE                                                         
         BNE   EXIT                                                             
*                                                                               
PALL05   B     PALLX               ELSE WE NEED MORE OBJECTS                    
         EJECT                                                                  
*****                                                                           
* BUDGET HEADER OBJECT                                                          
*****                                                                           
*                                                                               
PALL10   CLC   TYPENUM,=A(ITBDGHDR)  BUDGET HEADER OBJECT?                      
         BNE   PALL20                                                           
*                                                                               
         TM    MISCFLG1,X'20'      ERROR IF PREV OBJ WAS END-OF-DATA            
         BZ    *+12                                                             
         BAS   RE,ERBADOBJ                                                      
         B     PALLLOOP                                                         
*                                                                               
         TM    MISCFLG1,X'80'      GOT ANOTHER BUDGET HEADER?                   
         BZ    *+12                                                             
         BAS   RE,ERBADOBJ         YES, ONE PER SEND PLEASE                     
         B     PALLLOOP                                                         
*                                                                               
         CLC   DATALEN,=A(HDRLEN)  LENGTH OF HEADER OBJECT VALID?               
         BH    *+14                                                             
         OC    DATALEN,DATALEN     NO LENGTH?                                   
         BNZ   PALL15                                                           
         BAS   RE,ERBADHDR         NO                                           
         B     PALLLOOP                                                         
*                                                                               
PALL15   XC    HDROBJCT(HDRLEN),HDROBJCT   SAVE THE BUDGET HEADER IN            
         L     R4,ADATA                        LOCAL STORAGE                    
         L     R1,DATALEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HDROBJCT(0),0(R4)                                                
*                                                                               
         BAS   RE,VALHEADR         VALIDATE THE BUDGET HEADER                   
         BNE   PALLLOOP                                                         
*                                                                               
         BAS   RE,SETOFFAL         SET UP OFFAL                                 
*                                                                               
         OI    MISCFLG1,X'80'      DON'T WANT ANY MORE BUDGET HEADERS           
*                                                                               
         B     PALLLOOP                                                         
         EJECT                                                                  
*****                                                                           
* BUDGET ELEMENT OBJECT                                                         
*****                                                                           
*                                                                               
PALL20   CLC   TYPENUM,=A(ITBDGOBJ)    BUDGET ELEMENT OBJECT?                   
         BNE   PALL30                                                           
*                                                                               
         TM    MISCFLG1,X'20'      ERROR IF PREV OBJ WAS END-OF-DATA            
         BZ    *+12                                                             
         BAS   RE,ERBADOBJ                                                      
         B     PALLLOOP                                                         
*                                                                               
         TM    MISCFLG1,X'80'      BUDGET HEADER REQUIRED?                      
         BNZ   *+12                                                             
         BAS   RE,ERHDRREQ         YES                                          
         B     PALLLOOP                                                         
*                                                                               
         CLC   DATALEN,=A(BDGLEN)  LENGTH OF BUDGET OBJECT VALID?               
         BH    *+14                                                             
         OC    DATALEN,DATALEN     NO LENGTH?                                   
         BNZ   PALL25                                                           
         BAS   RE,ERBADBDG         NO                                           
         B     PALLLOOP                                                         
*                                                                               
PALL25   XC    BDGOBJCT(BDGLEN),BDGOBJCT   SAVE THE BUDGET ELEMENT IN           
         L     R4,ADATA                        LOCAL STORAGE                    
         L     R1,DATALEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BDGOBJCT(0),0(R4)                                                
*                                                                               
         BAS   RE,VALBUDGT         VALIDATE THE BUDGET ELEMENT                  
         BNE   PALLLOOP                                                         
*                                                                               
         OI    MISCFLG1,X'40'      GOT @ LEAST 1 BUDGET ELEM                    
*                                                                               
         BAS   RE,RECTOWKR         PUT RECORD OUT TO WORKER FILE                
*                                                                               
         ZICM  R1,NUMBDGTS,2       INCREASE NUMBER OF BUDGETS UPLOADED          
         AH    R1,=H'1'                                                         
         STCM  R1,3,NUMBDGTS                                                    
*                                                                               
         BAS   RE,ACCEPTED         BUDGET OBJECT HAS BEEN ACCEPTED              
*                                                                               
         B     PALLLOOP                                                         
         EJECT                                                                  
*****                                                                           
* END-OF-DATA OBJECT                                                            
*****                                                                           
*                                                                               
PALL30   CLC   TYPENUM,=A(ITEOD)   END-OF-DATA OBJECT?                          
         BE    *+12                                                             
         BAS   RE,ERBADOBJ         NO, NO OTHER VALID OBJECTS                   
         B     PALLLOOP                                                         
*                                                                               
         TM    MISCFLG1,X'20'      ERROR IF PREV OBJ WAS END-OF-DATA            
         BZ    *+12                                                             
         BAS   RE,ERBADOBJ                                                      
         B     PALLLOOP                                                         
*                                                                               
         OI    MISCFLG1,X'20'      LAST OBJECT WAS END-OF-DATA                  
         B     PALLLOOP                                                         
*                                                                               
*****                                                                           
* ALL DONE, NO MORE OBJECTS TO PROCESS                                          
*****                                                                           
*                                                                               
PALLDONE MVI   MDLAST,C'Y'         NO MORE DATA, LAST TRANSACTION               
         NI    MISCFLG1,X'FF'-X'E0'                                             
*                                                                               
         BAS   RE,WRKRCLSE         DONE WITH WORKER FILE                        
         BNE   EXIT                                                             
*                                                                               
*****                                                                           
* SET UP A REQUEST FOR THIS WORKER FILE                                         
*****                                                                           
*                                                                               
         OC    NUMBDGTS,NUMBDGTS   ANY BUDGETS UPLOADED?                        
         BZ    PALLX               NONE, DON'T GENERATE REQUESTS                
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
*        MVI   ELEMENT+10,12                                                    
*                                  SEE QRECORD IN ACREPWORKC                    
         MVC   ELEMENT+26(80),SPACES                                            
         MVC   ELEMENT+26(2),=C'B5'      QPROG                                  
         MVC   ELEMENT+26+9(1),HEDRCPYC  QCOMPANY                               
*                                                                               
         CLI   HEDRCPYC,0          DID WE SOMEHOW LOSE COMPANY CODE?            
         BNE   *+6                                                              
         DC    H'0'                YES, DIE TO SEE HOW IT HAPPENED              
*                                                                               
         MVC   ELEMENT+26+59(2),=C'WL'   QOPT1 & QOPT2 FOR WORKER FILE          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCREQS',ELEMENT,ELEMENT               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PALLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE HEADER OBJECT.                                     
***********************************************************************         
*                                                                               
VALHEADR NTR1                                                                   
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
*                                                                               
         MVC   FAKEFLD,HDROBJCT                                                 
         L     R1,DATALEN                                                       
         STC   R1,FAKEFLDH+5                                                    
         LA    R2,FAKEFLD                                                       
*                                                                               
         CLI   FAKEFLDH+5,0                                                     
         BNE   *+12                                                             
         BAS   RE,ERBADHDR                                                      
         B     VHEADNO                                                          
*                                                                               
VHEAD10  CLI   0(R2),C' '          MAKE THE SPACE BETWEEN AGENCY AND            
         BNE   *+8                     YEAR A COMMA                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BCT   R1,VHEAD10                                                       
*                                                                               
         GOTO1 SCANNER,DMCB,(0,FAKEFLDH),(2,BLOCK)                              
         CLI   DMCB+4,0            SHOULD HAVE DATA                             
         BNE   *+12                                                             
         BAS   RE,ERBADHDR                                                      
         B     VHEADNO                                                          
*                                                                               
         LA    R3,BLOCK            R3 = A(AGENCY FLD IN HEADER OBJECT)          
         MVC   HDRAGCY,12(R3)                                                   
*                                                                               
         XC    KEY,KEY             LOOKUP COMPANY CODE IN ID REC                
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,HDRAGCY                                                   
         OC    CTIKID,SPACES                                                    
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING CTIREC,R6                                                        
         CLC   CTIKEY,KEY                                                       
         BE    *+12                                                             
         BAS   RE,ERBADAGY                                                      
         B     VHEADNO                                                          
*                                                                               
VHEAD20  MVC   DATADISP,=Y(CTIDATA-CTIREC)                                      
         MVI   ELCODE,CTSYSELQ                                                  
         BAS   RE,GETEL                                                         
VHEAD22  BE    *+12                                                             
VHEAD25  BAS   RE,ERBADCOM                                                      
         B     VHEADNO                                                          
*                                                                               
         USING CTSYSD,R6                                                        
         CLI   CTSYSNUM,X'06'      ACC SYSTEM?                                  
         BNE   VHEAD30                                                          
         MVI   HEDRTYPE,BUDKTYPQ   BUDGET RECORD TYPE                           
         MVC   HEDRCPYC,CTSYSAGB                                                
         MVC   HEDRCCPY,CTSYSAGB                                                
         B     VHEAD40                                                          
         DROP  R6                                                               
*                                                                               
VHEAD30  BAS   RE,NEXTEL                                                        
         B     VHEAD22                                                          
*                                                                               
VHEAD40  LA    R3,32(R3)           R3 = A(YEAR FIELD IN HEADER OBJECT)          
         CLI   0(R3),0                                                          
         BNE   *+12                                                             
         BAS   RE,ERBADYR          ERROR IF NONE                                
         B     VHEADNO                                                          
*                                                                               
         TM    2(R3),X'80'         VALID NUMERIC?                               
         BNE   *+12                                                             
         BAS   RE,ERBADYR          ERROR IF NOT                                 
         B     VHEADNO                                                          
*                                                                               
         MVC   HDRYEAR,12(R3)                                                   
         MVC   TMPCDATE,=C'010101'    Y2K COMPLIANT                             
         MVC   TMPCDATE(2),HDRYEAR                                              
         GOTO1 DATCON,DMCB,(0,TMPCDATE),(1,TMPPDATE)                            
         MVC   BDGTYEAR,TMPPDATE                                                
*&&DO                                                                           
         PACK  DUB,HDRYEAR                                                      
         ZICM  R1,DUB+6,2          PACK YEAR                                    
         SRL   R1,4                MAKE YEAR PWOS                               
         STC   R1,BDGTYEAR         SAVE PWOS YEAR                               
*&&                                                                             
         MVC   KEY,SPACES          GET COMPANY RECORD                           
         MVC   KEY(L'HEDRCPYC),HEDRCPYC                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'BUDKEY),KEYSAVE   SAME COMPANY?                            
         BNE   VHEAD25                 NO, BAD COMPANY                          
*                                                                               
         L     R6,AIO              GET THE COMPANY ELEMENT                      
         MVC   DATADISP,=Y(ACCORFST)                                            
         MVI   ELCODE,CPYELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   VHEAD25                                                          
*                                                                               
         USING CPYELD,R6                                                        
         MVC   COMPSTA1,CPYSTAT1                                                
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         DROP  R6                                                               
*                                                                               
VHEADYES B     YES                                                              
*                                                                               
VHEADNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS UP OFFAL AND ITS DATA BLOCK                                 
***********************************************************************         
SETOFFAL NTR1                                                                   
         GOTO1 CALLOV,DMCB,0,X'D9000A62',0                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VOFFAL,0(R1)                                                     
*                                                                               
         LA    R1,OFFBLCK                                                       
         USING OFFALD,R1                                                        
         L     R2,ATWA                                                          
         USING TWAD,R2                                                          
*                                                                               
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,HEDRCPYC                                                 
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTA1                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         OI    OFFAINDS,OFFAIOFF   CHECK OFFICE SECURITY                        
         DROP  R2                                                               
*                                                                               
         MVI   OFFAACT,OFFAINI     INITIALIZE FOR FIRST TRANSACTION             
         CLI   OVERMODE,C'S'           IF MODE IS START                         
         BE    *+14                                                             
         MVI   OFFAACT,OFFARES     OTHERWISE, RESTORE BUFFER VALUES             
         MVC   OFFASAV(OFFASAVL),SAVEOFFA                                       
*                                                                               
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEOFFA,OFFASAV                                                 
         DROP  R1                                                               
*                                                                               
SOFLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE BUDGET ELEMENT OBJECT.                             
***********************************************************************         
*                                                                               
VALBUDGT NTR1                                                                   
         MVC   HEDRUNTC,BDGUNIT                                                 
         MVC   HEDRLDGC,BDGLEDGR                                                
         MVC   HEDRACCT,BDGACCNT                                                
         MVC   HEDRWORK,=2C' '                                                  
*                                                                               
         MVC   HEDRCUNT,BDGCNUNT                                                
         MVC   HEDRCLDG,BDGCNLDG                                                
         MVC   HEDRCACC,BDGCNACC                                                
*                                                                               
         BAS   RE,GETBTYPE         GET BUDGET TYPE                              
         BNE   VBDGTNO                                                          
*                                                                               
         MVC   HEDRBDGT,PRVBTNUM   SAVE THE BUDGET TYPE NUMBER                  
         MVI   HEDRBCKT,0              AND BUCKET TYPE                          
         CLI   BDGBUCKT,C' '                                                    
         BE    *+10                                                             
         MVC   HEDRBCKT,BDGBUCKT                                                
*                                                                               
* ACCOUNT                                                                       
*                                                                               
         MVC   QUNIT,HEDRUNTC                                                   
         MVC   QLEDGER,HEDRLDGC                                                 
         MVC   QACCOUNT,HEDRACCT                                                
         MVC   BLEVEL,PRVACLV                                                   
*                                                                               
         CLI   HEDRBCKT,0          IF BUCKET TYPE USED, DON'T CHECK             
         BNE   *+12                    LEVELS YET                               
*                                                                               
         BAS   RE,VALACCNT         VALIDATE THE ACCOUNT                         
         BNE   VBDGTNO                                                          
*                                                                               
         BAS   RE,GETUNLDG         GET UNIT/LEDGER RECORD                       
         BNE   VBDGTNO                                                          
*                                                                               
* CONTRA-ACCOUNT                                                                
*                                                                               
         LA    R1,PRVVALS                                                       
         USING PRVVCUNT,R1                                                      
*                                                                               
         OC    PRVVCUNT(L'PRVVALS),PRVVCUNT   ALL CONTRA-ACCOUNTS?              
         BNZ   VBDGT10                                                          
         CLC   HEDRCUNT(14),SPACES    YES, MAKE SURE NONE SPECFIED              
         BE    VBDGT15                                                          
         BAS   RE,ERNOCACC         NO CONTRA-ACCOUNTS ALLOWED                   
         B     VBDGTNO                                                          
*                                                                               
VBDGT10  CLI   PRVVCUNT,X'FF'                                                   
         BNE   *+12                                                             
         BAS   RE,ERBDICON                                                      
         B     VBDGTNO                                                          
*                                                                               
         CLC   PRVVCUNT(2),HEDRCUNT                                             
         BE    *+12                                                             
         LA    R1,L'PRVVALS(R1)                                                 
         B     VBDGT10                                                          
*                                                                               
         MVC   QUNIT,HEDRCUNT                                                   
         MVC   QLEDGER,HEDRCLDG                                                 
         MVC   QACCOUNT,HEDRCACC                                                
         MVC   BLEVEL,PRVVCACL                                                  
         DROP  R1                                                               
*                                                                               
         CLI   HEDRBCKT,0          IF BUCKET TYPE USED, DON'T CHECK             
         BNE   *+12                    LEVELS YET                               
*                                                                               
         BAS   RE,GETUNLDG         GET UNIT/LEDGER RECORD                       
         BNE   VBDGTNO                                                          
*                                                                               
* MONTHLY BUDGETS                                                               
*                                                                               
VBDGT15  LA    R0,12               12 MONTHLY BUDGETS                           
         LA    R1,BDGAMT01         R1 = A(JANUARY BUDGET)                       
*                                                                               
VBDGT20  LA    R2,12               12 CHARACTERS PER BUDGET                     
         CLI   0(R1),C'-'          NEGATIVE BUDGET?                             
         BNE   VBDGT30                                                          
         MVI   0(R1),C'0'          GET RID OF NEGATIVE SIGN                     
         OI    MISCFLG1,X'08'      NEGATIVE BUDGET USED                         
         B     VBDGT40                                                          
*                                                                               
VBDGT30  CLI   0(R1),C'0'          NUMERIC?                                     
         BNL   *+12                                                             
         BAS   RE,ERBADAMT                                                      
         B     VBDGTNO                                                          
*                                                                               
         CLI   0(R1),C'9'                                                       
         BNH   *+12                                                             
         BAS   RE,ERBADAMT                                                      
         B     VBDGTNO                                                          
*                                                                               
         CH    R2,=H'1'                                                         
         BNE   VBDGT40                                                          
         TM    MISCFLG1,X'08'      NEGATIVE BUDGET?                             
         BZ    VBDGT40                                                          
         NI    0(R1),X'DF'         YES, CHANGE IT                               
*                                                                               
VBDGT40  LA    R1,1(R1)                                                         
         BCT   R2,VBDGT30          CONTINUE CHECKING THE MONTH'S BUDGET         
*                                                                               
         NI    MISCFLG1,X'FF'-X'08'    RESET NEGATIVE BUDGET                    
         BCT   R0,VBDGT20          CONTINUE CHECKING ALL THE MONTHS             
*                                                                               
VBDGTYES B     YES                                                              
*                                                                               
VBDGTNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE BUDGET TYPE CODE                                   
*                                                                               
* ON ENTRY:    BDGTTYPE            BUDGET TYPE CODE                             
*              PREVBTYP            PREVIOUS BUDGET TYPE CODE                    
*              PRVAUNT             PREVIOUS ACCOUNT UNIT                        
*              PRVALDG                              LEDGER                      
*              HEDRCPYC            COMPANY CODE                                 
*              HEDRUNTC            CURRENT ACCOUNT UNIT                         
*              HEDRLDGC                            LEDGER                       
*                                                                               
* ON EXIT:     PREVBTYP            BUDGET TYPE CODE                             
*              PRVBTNUM                        NUMBER                           
*              PRVBSTAT                        STATUS (X'10' = 1000'S)          
*              PRVAUNT             ACCOUNT UNIT                                 
*              PRVALDG                     LEDGER                               
*              PRVACLV                     LEVEL                                
*              PRVVALS             CONTRA UNIT/LEDGER/LEVEL SETS                
***********************************************************************         
*                                                                               
GETBTYPE NTR1                                                                   
         CLC   PREVBTYP(L'BDGTTYPE),BDGTTYPE     GOT THIS ONE ALREADY?          
         BNE   GBTYP02                                                          
         CLC   PREVBCKT,BDGBUCKT                 AND SAME BUCKET TYPE?          
         BNE   GBTYP02                                                          
         CLC   PRVAUNT(2),HEDRUNTC               AND SAME UNIT/LEDGER?          
         BE    GBTYPYES                                                         
*                                                                               
GBTYP02  XC    KEY,KEY             LOOK FOR BUDGET TYPE RECORD                  
         LA    R4,KEY                                                           
         USING BUDKEY,R4                                                        
         MVI   BUDKTYP,BUDKTYPQ                                                 
         MVC   BUDKCPY,HEDRCPYC                                                 
         XC    BUDKCOD,BUDKCOD                                                  
         MVC   BUDKCOD(L'BDGTTYPE),BDGTTYPE                                     
         OC    BUDKCOD,SPACES                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(BUDKNO2-BUDKEY),KEYSAVE                                      
         BE    *+12                                                             
         BAS   RE,ERBADBGC                                                      
         B     GBTYPNO                                                          
*&&DO                                                                           
         CLI   BDGBUCKT,C' '       ANY BUCKET TYPE?                             
         BE    GBTYP05                                                          
         MVC   BUDKBTYP,BDGBUCKT   YES                                          
*                                                                               
         GOTO1 HIGH                BUDGET TYPE HAS THIS BUCKET TYPE?            
*                                                                               
         CLC   KEY(BUDKEND1-BUDKEY),KEYSAVE                                     
         BE    *+12                                                             
         BAS   RE,ERBADBKT         NO                                           
         B     GBTYPNO                                                          
*&&                                                                             
GBTYP05  MVC   PREVBTYP,BUDKCOD                                                 
         MVC   PRVBTNUM,BUDKNO2                                                 
         MVC   PREVBCKT,BUDKBTYP                                                
         MVI   PRVBSTAT,0                                                       
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         USING BUDRECD,R6                                                       
*                                                                               
         TM    ACCOSTAT(R6),X'10'      UNITS=1000?                              
         BZ    *+8                                                              
         OI    PRVBSTAT,X'10'          YES                                      
*                                                                               
         MVC   DATADISP,=Y(ACCORFST)                                            
         MVI   ELCODE,BIVELQ                                                    
*                                                                               
         BAS   RE,GETEL                                                         
GBTYP20  BE    *+12                                                             
         BAS   RE,ERBDIACC         BUDGET TYPE INCOMPATIBLE WITH ACCT           
         B     GBTYPNO                                                          
*                                                                               
         USING BIVELD,R6                                                        
         CLC   BIVAUNT(2),HEDRUNTC    FOR THIS UNIT LEDGER?                     
         BE    *+12                                                             
         BAS   RE,NEXTEL              NO, CHECK NEXT ELEMENT                    
         B     GBTYP20                                                          
*                                                                               
         MVC   PRVAUNT(3),BIVAUNT   SAVE ACCOUNT UNIT/LEDGER/LEVEL              
         XC    PRVVALS(PRVVALSL),PRVVALS                                        
         ZIC   R1,BIVLN                                                         
         SH    R1,=H'6'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRVVALS(0),BIVVALS                                               
         AH    R1,=H'1'                                                         
         LA    R2,PRVVALS                                                       
         AR    R2,R1                                                            
         MVI   0(R2),X'FF'         END OF THE CONTRA SET                        
         DROP  R6                                                               
*                                                                               
GBTYPYES B     YES                                                              
*                                                                               
GBTYPNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE ACCOUNT TO SEE IF IT EXISTS                        
*                                                                               
* ON ENTRY:    QUNIT               UNIT                                         
*              QLEDGER             LEDGER                                       
*              QACCOUNT            ACCOUNT                                      
***********************************************************************         
*                                                                               
VALACCNT NTR1                                                                   
         MVC   KEY,SPACES          CHECK TO SEE IF ACCOUNT EXISTS               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKCPY,HEDRCPYC                                                 
         MVC   ACTKULA,QUNIT                                                    
*                                                                               
         MVC   AIO,AIO2            SAVE ACCOUNT RECORD IN AIO2                  
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1            EVERYTHING ELSE USES AIO1                    
*                                                                               
         CLC   KEY(ACTKEND),KEYSAVE                                             
         BE    *+12                                                             
         BAS   RE,ERBADACC         IT DOESN'T EXIST                             
         B     VACCTNO                                                          
*                                                                               
VACCTYES B     YES                                                              
*                                                                               
VACCTNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE ACCOUNT AGAINST THE LEVEL STATED IN THE            
* BUDGET TYPE FOR THE GIVEN UNIT/LEDGER                                         
*                                                                               
* ON ENTRY:    QUNIT               UNIT                                         
*              QLEDGER             LEDGER                                       
*              QACCOUNT            ACCOUNT                                      
*              BLEVEL              LEVEL                                        
***********************************************************************         
*                                                                               
GETUNLDG NTR1                                                                   
         LA    R3,1                FIRST ENTRY IN TABLE                         
         LA    R2,ULSTABLE                                                      
         USING ULSUNIT,R2                                                       
*                                                                               
GULDG10  CLI   ULSUNIT,0           ANY UNIT/LEDGERS LEFT?                       
         BE    GULDG20             NO                                           
*                                                                               
         CLC   QUNIT(2),ULSUNIT    SAME UNIT/LEDGER?                            
         BNE   *+14                                                             
         MVC   ULOFFPOS,ULSOFPOS   YES, HAVE THE INFO FOR U/L ALREADY           
         B     GULDG80                                                          
*                                                                               
         CH    R3,=H'10'           CAN HAVE ONLY UPTO 10 ENTRIES                
         BE    GULDG20             OVERWRITE 10TH ENTRY WITH NEW ONE            
*                                                                               
         LA    R3,1(R3)            CHECK NEXT ENTRY                             
         LA    R2,L'ULSTABLE(R2)                                                
         B     GULDG10                                                          
*                                                                               
GULDG20  XC    ULSUNIT(L'ULSTABLE),ULSUNIT                                      
*                                                                               
         MVC   KEY,SPACES          LOOK FOR LEDGER RECORD                       
         LA    R4,KEY                                                           
         USING LDGKEY,R4                                                        
         MVC   LDGKCPY,HEDRCPYC                                                 
         MVC   LDGKUNT,QUNIT                                                    
         MVC   LDGKLDG,QLEDGER                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(LDGKLDG+L'LDGKLDG-LDGKEY),KEYSAVE                            
         BE    *+12                                                             
         BAS   RE,ERBADUNL                                                      
         B     GULDGNO                                                          
         DROP  R4                                                               
*                                                                               
         L     R6,AIO              GET LEDGER ELEMENT                           
         MVC   DATADISP,=Y(ACCORFST)                                            
         MVI   ELCODE,LDGELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
GULDG30  BAS   RE,ERINVUNL         INVALID UNIT/LEDGER RECORD                   
         B     GULDGNO                                                          
*                                                                               
         USING LDGELD,R6                                                        
         MVC   ULOFFPOS,LDGOPOS    COPY OFFICE POSITION                         
*                                                                               
         CLI   LDGLN,X'20'         OLD ELEMENT?                                 
         BL    GULDG35             YES                                          
*                                                                               
         CLI   LDGBPOS,0           ANY BUDGET OVERRIDE?                         
         BE    GULDG35             NO, USE DEFAULT OFFICE POSITION              
*                                                                               
         MVC   ULOFFPOS,LDGBPOS    YES, USE THIS INSTEAD                        
*                                                                               
GULDG35  CLI   ULOFFPOS,LDGONKHI   WEIRD OFFICE POSITION?                       
         BNH   *+8                                                              
         MVI   ULOFFPOS,0          YES, DON'T CHECK OFFICE                      
*                                                                               
         MVC   ULSOFPOS,ULOFFPOS   SAVE OFFICE POSITION IN TABLE                
         DROP  R6                                                               
*                                                                               
GULDG40  L     R6,AIO                                                           
         MVI   ELCODE,ACLELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   GULDG30                                                          
*                                                                               
         USING ACLELD,R6                                                        
         MVC   ULSUNIT(2),QUNIT                                                 
         ZIC   R4,ACLLN                                                         
         SH    R4,=Y(ACLLN1Q)                                                   
         BNP   GULDG80             NO LEVELS FOR THIS UNIT/LEDGER               
*                                                                               
         LA    R1,ULSLVLS          R1 = A(1ST LEVEL LENGTH IN OUR TBL)          
         LA    R3,ACLVALS          R3 = A(1ST LEVEL LENGTH)                     
         DROP  R6                                                               
*                                                                               
GULDG70  MVC   0(1,R1),0(R3)       COPY LENGTH FOR THE LEVEL                    
*                                                                               
         LA    R1,1(R1)            GET THE NEXT LEVEL LENGTH                    
         LA    R3,L'ACLVALS(R3)                                                 
         SH    R4,=Y(L'ACLVALS)                                                 
         BP    GULDG70                                                          
*                                                                               
GULDG80  LA    R3,QACCOUNT         R3=A(LEVEL STARTS IN ACCOUNT)                
         LA    R4,1                CURRENT LEVEL NUMBER                         
         SR    R5,R5               LENGTH OF PREVIOUS LEVEL                     
*                                                                               
GULDG90  LR    R1,R4               R1=A(LENGTH OF ACCT FOR CURR LVL)            
         BCTR  R1,0                                                             
         LA    R1,ULSLVLS(R1)                                                   
*                                                                               
         CLI   0(R1),0             CURRENT LEVEL IS NOT DEFINED?                
         BNE   GULDG100                                                         
*                                                                               
         LA    R1,L'QACCOUNT       YES, CHECK REST OF ACCOUNT TO MAKE           
         SR    R1,R5                   SURE IT IS UP TO LAST LEVEL ONLY         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),SPACES                                                   
         BE    GULDG120                                                         
         BAS   RE,ERLVLNDF         LEVEL NOT DEFINED FOR UNIT/LEDGER            
         B     GULDGNO                                                          
*                                                                               
GULDG100 ZIC   RE,0(R1)            IS ACCOUNT AT THIS LEVEL?                    
         SR    RE,R5                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),SPACES                                                   
         BE    GULDG110                                                         
*                                                                               
         CLM   R4,1,BLEVEL         YES, IS IT SUPPOSE TO BE?                    
         BNH   GULDGNXT                 YES                                     
GULDG105 BAS   RE,ERACWLVL              NO, ACCT IS IN THE WRONG LEVEL          
         B     GULDGNO                                                          
         DROP  R2                                                               
*                                                                               
GULDG110 CLM   R4,1,BLEVEL         NO, IS IT SUPPOSE TO BE?                     
         BNH   GULDG105                YES, ACCT IS IN THE WRONG LEVEL          
*                                                                               
GULDGNXT ZIC   R5,0(R1)            SAVE LENGTH OF THIS LEVEL FOR PREV           
         CH    R5,=Y(L'QACCOUNT)   CHECKED WHOLE ACCOUNT FIELD ALREADY?         
         BNL   GULDG120            YES, NOTHING LEFT TO CHECK                   
*                                                                               
         LR    R3,R5                                                            
         LA    R3,QACCOUNT(R3)     R3=A(NEXT LEVEL STARTS IN ACCOUNT)           
*                                                                               
         LA    R4,1(R4)            LOOP UNTIL WE CHECKED ALL 4 LEVELS           
         CH    R4,=H'4'                                                         
         BNH   GULDG90                                                          
*                                                                               
GULDG120 CLI   ULOFFPOS,0          NO OFFICE TO CHECK AGAINST?                  
         BE    GULDGYES            NONE                                         
*                                                                               
         MVC   BYTE,ULOFFPOS       COPY THE OFFICE CODE FROM THE                
         NI    BYTE,X'0F'              ACCOUNT CODE                             
         ZIC   R3,BYTE                                                          
         BCTR  R3,0                                                             
         LA    R3,QACCOUNT(R3)                                                  
         MVC   QOFFICE,SPACES                                                   
         MVC   QOFFICE(1),0(R3)                                                 
         TM    ULOFFPOS,LDGOKEY2                                                
         BZ    *+10                                                             
         MVC   QOFFICE+1(1),1(R3)                                               
*                                                                               
         LA    R1,OFFBLCK          CHECK TO SEE IF THIS ACCOUNT OKAY            
         USING OFFALD,R1               FOR OFFICE                               
         MVC   OFFAREC,AIO2                                                     
         MVC   OFFAOPOS,ULOFFPOS                                                
         MVC   OFFAOFFC,QOFFICE                                                 
         MVI   OFFAACT,OFFATST                                                  
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFAL                                                           
         BE    *+12                                                             
         BAS   RE,EROFFICE         SECURITY LOCK OUT                            
         B     GULDGNO                                                          
*                                                                               
GULDGYES B     YES                                                              
*                                                                               
GULDGNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS THE BUDGET RECORD TO THE WORKER FILE                        
***********************************************************************         
*                                                                               
RECTOWKR NTR1                                                                   
         TM    MISCFLG1,X'10'      HAS WORKER FILE BEEN SET ALREADY?            
         BNZ   *+12                                                             
         OI    MISCFLG1,X'10'                                                   
         BAS   RE,SETUPWKR         SET UP WORKER FILE                           
*                                                                               
         MVC   AIO,AADDIO          HEADER ELEMENT IS REALLY THE KEY             
         L     R6,AIO                                                           
         USING BUDRECD,R6                                                       
         LR    RE,R6                                                            
         LA    RF,2048                                                          
         XCEFL                                                                  
         MVC   0(BUDKEND2,R6),HEDRELEM                                          
*                                                                               
         LA    R0,1                FIRST MONTH OF THE BUDGET YEAR               
         LA    R3,BDGAMT01                                                      
*                                                                               
RECWKRLP XC    BDGTELEM,BDGTELEM   MAKE A BUDGET ELEMENT                        
*                                                                               
         LA    R2,ELEMENT          ADD THE ELEMENT TO THE RECORD                
         USING BAMELD,R2                                                        
         XC    BAMEL(BAMLNQ),BAMEL                                              
         MVI   BAMEL,BAMELQ                                                     
         MVI   BAMLN,BAMLNQ                                                     
*                                                                               
         MVC   BAMMNTH(L'BDGTYEAR),BDGTYEAR    STORE YYMM AS PWOS               
         LR    R1,R0                                                            
         CH    R1,=H'10'                                                        
         BL    *+8                                                              
         AH    R1,=H'6'                                                         
         STC   R1,BAMMNTH+1                                                     
*                                                                               
         PACK  DUB,0(12,R3)                                                     
         ZAP   BAMBUDG,DUB                                                      
*                                                                               
         TM    PRVBSTAT,X'10'      UNITS=1000?                                  
         BZ    *+10                                                             
         SRP   BAMBUDG,3,0         YES, MULTIPLY BY 1000                        
         DROP  R2                                                               
*                                                                               
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
         LA    R3,L'BDGAMT01(R3)                                                
         AH    R0,=H'1'                                                         
         CH    R0,=H'12'                                                        
         BNH   RECWKRLP            LOOP BACK TO GET ALL BUDGET ELEMS            
*                                                                               
RECWKRDN L     R0,AWRKRIOA         COPY BUDGET RECORD OVER TO WRKR IO           
         AH    R0,=H'4'                                                         
         LA    R1,2047                                                          
         LA    RE,BUDRECD                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,AWRKRIOA                                                      
         XC    0(4,RE),0(RE)           SET UP STANDARD IBM CONVENTIONS          
         ZICM  R1,BUDRLEN,2                FOR WORKER FILE                      
         LA    R1,4(R1)                                                         
         STCM  R1,3,0(RE)                                                       
*                                                                               
         BAS   RE,WRKRADD                                                       
         BNE   EXIT                                                             
*                                                                               
RECWKRX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS UP THE WORKER FILE                                          
***********************************************************************         
*                                                                               
SETUPWKR NTR1                                                                   
         LA    R2,WRKRINDX                                                      
         USING UKKEY,R2                                                         
         MVC   UKUSRID,SIGNON2     USER ID NUMBER                               
         MVC   UKSYSPRG,=C'ABU'    ACC BUDGET UPDATE                            
         MVI   UKSUBPRG,0                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         PACK  FULL(2),WORK+4(2)                                                
         LH    R1,FULL                                                          
         SRL   R1,4                                                             
         STC   R1,UKDAY            DAY NUMBER                                   
*                                                                               
         MVI   UKCLASS,C'B'        BUDGET CLASS                                 
         OI    UKFLAG,X'01'        DUPLICATES ALLOWED (NEW SEQ NUMBER)          
         DROP  R2                                                               
*                                                                               
         BAS   RE,WRKRBINI         INITIALIZE WORKER BUFFER                     
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,WRKROPEN                                                      
         BNE   EXIT                                                             
*                                                                               
SETWKRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WORKER ROUTINES                                                               
***********************************************************************         
*                                                                               
WRKRADD  MVC   WRKRCMD,WKADDR                                                   
         B     WRKRGO                                                           
*                                                                               
WRKRBINI MVC   WRKRCMD,WKBINI      INITIALIZE WORKER BUFFER                     
         B     WRKRGO                                                           
*                                                                               
WRKRBRST MVC   WRKRCMD,WKBRST      RESTORE WORKER BUFFER                        
         B     WRKRGO                                                           
*                                                                               
WRKRBSAV MVC   WRKRCMD,WKBSAV      SAVE WORKER BUFFER                           
         B     WRKRGO                                                           
*                                                                               
WRKRCLSE MVC   WRKRCMD,WKCLOS                                                   
         B     WRKRGO                                                           
*                                                                               
WRKROPEN MVC   WRKRCMD,WKOPEN                                                   
         B     WRKRGO                                                           
*                                                                               
WRKRGO   NTR1  ,                                                                
         GOTO1 DATAMGR,DMCB,WRKRCMD,WRKRFILE,WRKRINDX,AWRKRIOA,AWRKRBUF         
         CLI   8(R1),0                                                          
         BE    WRKRYES                                                          
*                                                                               
         CLI   8(R1),X'80'         END OF FILE?                                 
         BNE   *+12                                                             
         BAS   RE,ERWKREOF         YES                                          
         B     WRKRNO                                                           
*                                                                               
         CLI   8(R1),X'10'         RECORD NOT FOUND?                            
         BNE   *+12                                                             
         BAS   RE,ERWKNTFD         YES                                          
         B     WRKRNO                                                           
*                                                                               
         B     EXIT                EXIT IF BAD ERROR                            
*                                                                               
WRKRYES  B     YES                                                              
*                                                                               
WRKRNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* RETURNS AN ACCEPTED OBJECT FOR EACH BUDGET OBJECT THAT HAS BEEN               
* VALIDATED AND ACCEPTED                                                        
***********************************************************************         
*                                                                               
ACCEPTED NTR1                                                                   
         LA    R0,BLOCK               COPY THE KEY TO BLOCK SO WE CAN           
         L     RE,ADATA                   MAKE UP THE ACCEPTED OBJECT           
         LA    R1,BDGAMT01-BDGOBJCT                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,BDGAMT01-BDGOBJCT                                             
*                                                                               
         GOTO1 PUTITEM,DMCB,ITBDGACC,(R0),BLOCK                                 
         BNE   EXIT                                                             
*                                                                               
ACCPTDX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HANDLE ERROR MESSAGES                                                         
***********************************************************************         
*                                                                               
ERBADOBJ MVC   APPLERR,=Y(E13BDOBJ)   BAD OBJECT                                
         B     RETEROBJ                                                         
*                                                                               
ERHDRREQ MVC   APPLERR,=Y(E13HDRRQ)   BUDGET HEADER REQUIRED                    
         B     RETEROBJ                                                         
*                                                                               
ERBADHDR MVC   APPLERR,=Y(E13BDHDR)   BAD HEADER OBJECT                         
         B     RETEROBJ                                                         
*                                                                               
ERBADBDG MVC   APPLERR,=Y(E13BDBDG)   BAD BUDGET OBJECT                         
         B     RETEROBJ                                                         
*                                                                               
ERBADAGY MVC   APPLERR,=Y(E13BDAGY)   AGENCY ID DOES NOT EXIST                  
         B     RETEROBJ                                                         
*                                                                               
ERBADYR  MVC   APPLERR,=Y(E13BADYR)   BAD YEAR                                  
         B     RETEROBJ                                                         
*                                                                               
ERBADCOM MVC   APPLERR,=Y(E13CPYCD)   AGY DOESN'T HAVE A COMPANY CODE           
         B     RETEROBJ                                                         
*                                                                               
ERBADBGC MVC   APPLERR,=Y(E13BDGCD)   BAD BUDGET CODE                           
         B     RETEROBJ                                                         
*                                                                               
ERBADAMT MVC   APPLERR,=Y(E13BDAMT)   BAD AMOUNT                                
         B     RETEROBJ                                                         
*                                                                               
ERWKREOF MVC   APPLERR,=Y(E13WEOF)    END OF WORKER FILE                        
         B     RETEROBJ                                                         
*                                                                               
ERWKNTFD MVC   APPLERR,=Y(E13WNOR)    WORKER RECORD NOT FOUND                   
         B     RETEROBJ                                                         
*                                                                               
ERBDIACC MVC   APPLERR,=Y(E13BIACC)   BUDGET TYPE INCOMPATIBLE W/ ACCT          
         B     RETEROBJ                                                         
*                                                                               
ERBDICON MVC   APPLERR,=Y(E13BICON)   BUDGET TYPE INCOMPATIBLE W/ ACCT          
         B     RETEROBJ                                                         
*                                                                               
ERBADUNL MVC   APPLERR,=Y(E13BDUNL)   UNIT/LEDGER DOES NOT EXIST                
         B     RETEROBJ                                                         
*                                                                               
ERINVUNL MVC   APPLERR,=Y(E13INVUL)   INVALID UNIT/LEDGER                       
         B     RETEROBJ                                                         
*                                                                               
ERLVLNDF MVC   APPLERR,=Y(E13LVLND)   LEVEL NOT DEFINED IN UNIT/LEDGER          
         B     RETEROBJ                                                         
*                                                                               
ERACWLVL MVC   APPLERR,=Y(E13AWLVL)   ACCT IS IN THE WRONG LEVEL                
         B     RETEROBJ                                                         
*                                                                               
ERNOCACC MVC   APPLERR,=Y(E13NOCAC)   NO CONTRA-ACCOUNTS ALLOWED                
         B     RETEROBJ                                                         
*                                                                               
EROFFICE MVC   APPLERR,=Y(E13OFICE)   OFFICE CODE SECURITY LOCK OUT             
         B     RETEROBJ                                                         
*                                                                               
ERBADACC MVC   APPLERR,=Y(E13BDACC)   BAD ACCOUNT CODE                          
         B     RETEROBJ                                                         
*                                                                               
RETEROBJ NTR1  ,                                                                
         GOTO1 HEXOUT,DMCB,APPLERR,BLOCK,2   SET UP RETURN ERROR OBJECT         
*                                                                               
*****                                                                           
* FOLLOWING ERRORS NEED ONLY THE ERROR NUMBER FOR THE ERROR OBJECT              
*****                                                                           
*                                                                               
         CLC   APPLERR,=Y(E13HDRRQ)   BUDGET HEADER REQUIRED?                   
         BE    RETERNUM                                                         
         CLC   APPLERR,=Y(E13WEOF)    END OF WORKER FILE?                       
         BE    RETERNUM                                                         
         CLC   APPLERR,=Y(E13WNOR)    WORKER RECORD NOT FOUND?                  
         BE    RETERNUM                                                         
*                                                                               
*****                                                                           
* FOLLOWING ERRORS RETURN THE WHOLE OBJECT SENT AS PART OF THE ERROR            
* OBJECT                                                                        
*****                                                                           
*                                                                               
         CLC   APPLERR,=Y(E13BDOBJ)   BAD OBJECT?                               
         BE    RETERWHL                                                         
         CLC   APPLERR,=Y(E13BDHDR)   BAD HEADER OBJECT?                        
         BE    RETERWHL                                                         
         CLC   APPLERR,=Y(E13BDBDG)   BAD BUDGET OBJECT?                        
         BE    RETERWHL                                                         
         CLC   APPLERR,=Y(E13BDAGY)   AGENCY ID DOES NOT EXIST?                 
         BE    RETERWHL                                                         
         CLC   APPLERR,=Y(E13BADYR)   BAD YEAR?                                 
         BE    RETERWHL                                                         
         CLC   APPLERR,=Y(E13CPYCD)   AGY DOESN'T HAVE A COMPANY CODE?          
         BE    RETERWHL                                                         
*                                                                               
*****                                                                           
* FOLLOWING ERRORS RETURN THE KEY OF THE BUDGET OBJECT AS PART OF THE           
* ERROR OBJECT                                                                  
*****                                                                           
*                                                                               
         CLC   APPLERR,=Y(E13BDGCD)   BAD BUDGET CODE?                          
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13BDAMT)   BAD AMOUNT?                               
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13BIACC)   BUDGET TYPE INCOMPATIBLE W/ ACCT?         
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13BICON)   BDGT TYPE INCOMPATIBLE W/ CONTRA?         
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13BDUNL)   UNIT/LEDGER DOES NOT EXIST?               
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13INVUL)   INVALID UNIT/LEDGER?                      
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13LVLND)   LEVEL NOT DEFINED IN UNIT/LEDGER?         
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13AWLVL)   ACCOUNT IS IN THE WRONG LEVEL?            
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13NOCAC)   NO CONTRA-ACCOUNTS ALLOWED?               
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13OFICE)   OFFICE CODE SECURITY LOCK OUT?            
         BE    RETERKEY                                                         
         CLC   APPLERR,=Y(E13BDACC)   BAD ACCOUNT CODE?                         
         BE    RETERKEY                                                         
*                                                                               
*****                                                                           
* MORE ERRORS THAT HAVE SPECIAL THINGS TO SEND OUT IN THE ERROR OBJECT          
*****                                                                           
*                                                                               
         DS    0H                                                               
*                                                                               
RETERNUM LA    R0,4                   NO OBJECT TO PUT OUT                      
         B     RETERPUT                                                         
*                                                                               
RETERWHL MVI   BLOCK+4,C','           COPY THE OBJECT TO BLOCK SO WE            
         LA    R0,BLOCK+5                 CAN MAKE UP THE ERROR OBJECT          
         L     RE,ADATA                                                         
         L     R1,DATALEN                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R0,DATALEN                                                       
         AH    R0,=H'5'                                                         
         B     RETERPUT                                                         
*                                                                               
RETERKEY MVI   BLOCK+4,C','           COPY THE KEY TO BLOCK SO WE CAN           
         LA    R0,BLOCK+5                 MAKE UP THE ERROR OBJECT              
         L     RE,ADATA                                                         
         LA    R1,BDGAMT01-BDGOBJCT                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,BDGAMT01-BDGOBJCT+5                                           
         B     RETERPUT                                                         
*                                                                               
RETERPUT GOTO1 PUTITEM,DMCB,ITBDGERR,(R0),BLOCK                                 
         BNE   EXIT                                                             
*                                                                               
RETERX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BASIC ADDRESSES FOR THE ROUTINES                                              
***********************************************************************         
*                                                                               
EXIT     L     RD,SAVEDRD          LEAVE THIS WHOLE OVERLAY                     
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 3                                                                
*                                                                               
* LITERALS NEEDED FOR WORKER FILES                                              
*                                                                               
WRKRFILE DC    CL8'WKFILE'                                                      
*                                                                               
WKADDR   DC    CL8'ADD'                                                         
WKBINI   DC    CL8'BUFFER'                                                      
WKBRST   DC    CL8'BURSTR'                                                      
WKBSAV   DC    CL8'BUSAVE'                                                      
WKCLOS   DC    CL8'CLOSE'                                                       
WKDELT   DC    CL8'DELETE'                                                      
WKHOLD   DC    CL8'HOLD'                                                        
WKINDX   DC    CL8'INDEX'                                                       
WKKEEP   DC    CL8'KEEP'                                                        
WKOPEN   DC    CL8'OPEN'                                                        
WKPRGE   DC    CL8'PURGE'                                                       
WKRAND   DC    CL8'RANDOM'                                                      
WKREAD   DC    CL8'READ'                                                        
WKRSTR   DC    CL8'RESTORE'                                                     
WKSEQU   DC    CL8'SEQ'                                                         
WKUNKP   DC    CL8'UNKEEP'                                                      
WKWRIT   DC    CL8'WRITE'                                                       
*                                                                               
SPACES   DC    CL80' '                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*CTMADWORKD                                                                     
*CTMADEQUS                                                                      
*CTMADDSECT                                                                     
*CTGENFILE                                                                      
*ACGENFILE                                                                      
*ACOFFALD                                                                       
*DMWRKRD                                                                        
*DMWRKRK                                                                        
*DMWRKRS                                                                        
*DMWRKRW                                                                        
*FAFACTS                                                                        
*FATWA                                                                          
         PRINT OFF                                                              
       ++INCLUDE CTMADWORKD                                                     
       ++INCLUDE CTMADEQUS                                                      
       ++INCLUDE CTMADDSECT                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACOFFALD                                                       
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DMWRKRS                                                        
       ++INCLUDE DMWRKRW                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
AWRKRBUF DS    A                   ADDRESS OF WORKER'S BUFFER AREA              
AWRKRIOA DS    A                   ADDRESS OF I/O AREA USED BY WORKER           
VOFFAL   DS    V                   OFFAL                                        
SYSINNUM DS    F                   SYSTEM INPUT NUMBER                          
*                                                                               
*  (ITBDGHDR)       HEADER OBJECT                                               
*                                                                               
HDROBJCT DS    0C                  STRUCTURE OF HEADER OBJECT                   
HDRTEXT  DS    CL80                TEXT IN HEADER OBJECT                        
*                                      1ST PARM IS AGENCY SIGN-ON               
*                                      2ND PARM IS YEAR LAST 2 DIGITS           
HDRLEN   EQU   *-HDROBJCT                                                       
*                                                                               
HDRAGCY  DS    CL10                AGENCY SIGN-ON                               
HDRYEAR  DS    CL2                 YEAR LAST 2 DIGITS                           
BDGTYEAR DS    XL1                 YEAR LAST 2 DIGITS PWOS                      
TMPCDATE DS    CL6                 TEMPORARY DATE (CHAR)                        
TMPPDATE DS    XL3                 TEMPORARY DATE (PACKED)                      
*                                                                               
*  (ITBDGOBJ)       BUDGET OBJECT                                               
*                                                                               
BDGOBJCT DS    0C                  STRUCTURE OF BUDGET OBJECT                   
BDGUNIT  DS    CL1                     UNIT                                     
BDGLEDGR DS    CL1                     LEDGER                                   
BDGACCNT DS    CL12                    ACCOUNT                                  
BDGCNUNT DS    CL1                     CONTRA UNIT                              
BDGCNLDG DS    CL1                            LEDGER                            
BDGCNACC DS    CL12                           ACCOUNT                           
BDGBUCKT DS    CL1                     BUCKET TYPE                              
BDGTTYPE DS    CL6                     BUDGET TYPE                              
BDGAMT01 DS    CL12                    JANUARY   ACCOUNT BALANCE                
BDGAMT02 DS    CL12                    FEBRUARY  ACCOUNT BALANCE                
BDGAMT03 DS    CL12                    MARCH     ACCOUNT BALANCE                
BDGAMT04 DS    CL12                    APRIL     ACCOUNT BALANCE                
BDGAMT05 DS    CL12                    MAY       ACCOUNT BALANCE                
BDGAMT06 DS    CL12                    JUNE      ACCOUNT BALANCE                
BDGAMT07 DS    CL12                    JULY      ACCOUNT BALANCE                
BDGAMT08 DS    CL12                    AUGUST    ACCOUNT BALANCE                
BDGAMT09 DS    CL12                    SEPTEMBER ACCOUNT BALANCE                
BDGAMT10 DS    CL12                    OCTOBER   ACCOUNT BALANCE                
BDGAMT11 DS    CL12                    NOVEMBER  ACCOUNT BALANCE                
BDGAMT12 DS    CL12                    DECEMBER  ACCOUNT BALANCE                
BDGLEN   EQU   *-BDGOBJCT                                                       
*                                                                               
*                   HEADER ELEMENT                                              
*                                                                               
HEDRELEM DS    0CL35               STRUCTURE OF HEADER ELEMENT                  
HEDRTYPE DS    XL1                 RECORD TYPE   (X'1B')                        
HEDRCPYC DS    XL1                 COMPANY CODE                                 
HEDRUNTC DS    CL1                 UNIT CODE                                    
HEDRLDGC DS    CL1                 LEDGER CODE                                  
HEDRACCT DS    CL12                ACCOUNT CODE                                 
HEDRWORK DS    CL2                 WORK CODE OR SPACES (SPACES FOR NOW)         
HEDRCCPY DS    XL1                 CONTRA - COMPANY CODE                        
HEDRCUNT DS    CL1                        - UNIT CODE                           
HEDRCLDG DS    CL1                        - LEDGER CODE                         
HEDRCACC DS    CL12                       - ACCOUNT CODE                        
HEDRBDGT DS    XL2                 BUDGET NUMBER                                
HEDRBCKT DS    CL1                 BUCKET TYPE                                  
*                                                                               
*                   BUDGET ELEMENT                                              
*                                                                               
BDGTELEM DS    0PL10               STRUCTURE OF BUDGET ELEMENT                  
BDGTMNTH DS    PL2                 MONTH (YYMM - PWOS)                          
BDGTAMNT DS    PL8                 BUDGET AMOUNT (PENNIES)                      
*                                                                               
*                   WORKER VARIABLES                                            
*                                                                               
WRKRCMD  DS    CL8                                                              
WRKRINDX DS    CL(L'UKINDEX)                                                    
WKRSVBUF DS    XL20                WORKER'S SAVED DATA                          
*                                                                               
*                   MISCELLANEOUS                                               
*                                                                               
DATADISP DS    H                   DATA DISPLACEMENT IN RECORD                  
ELCODE   DS    XL1                 NEEDED FOR GETEL AND NEXTEL                  
*                                                                               
NUMBDGTS DS    XL2                 NUMBER OF BUDGETS PROCESSED                  
*                                                                               
COMPSTA1 DS    XL1                 COMPANY STATUS BYTE 1                        
COMPSTA2 DS    XL1                 COMPANY STATUS BYTE 2                        
COMPSTA3 DS    XL1                 COMPANY STATUS BYTE 3                        
COMPSTA4 DS    XL1                 COMPANY STATUS BYTE 4                        
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAG #1                        
*                                  X'80' - DON'T NEED BUDGET HEADER             
*                                  X'40' - GOT AT LEAST ONE BUDGET ELEM         
*                                  X'20' - LAST OBJ WAS END-OF-DATA OBJ         
*                                  X'10' - WORKER FILE HAS BEEN SET             
*                                  X'08' - NEGATIVE BUDGET FOR THIS MTH         
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER                            
FAKEFLD  DS    CL80                FAKE FIELD                                   
*                                                                               
QUNIT    DS    CL1                 UNIT                                         
QLEDGER  DS    CL1                 LEDGER                                       
QACCOUNT DS    CL12                ACCOUNT                                      
BLEVEL   DS    XL1                 LEVEL FOR ACCOUNT IN BUDGET TYPE             
*                                                                               
ULOFFPOS DS    XL1                 U/L OFFICE POSITION                          
QOFFICE  DS    CL2                 OFFICE FROM ACCOUNT                          
SAVEOFFA DS    XL(OFFASAVL)        DATA SAVED FOR OFFAL BTWN TRANSACTNS         
*                                                                               
PREVBTYP DS    CL10                PREVIOUS BUDGET TYPE CODE                    
PRVBTNUM DS    XL2                                      NUMBER                  
PREVBCKT DS    CL1                 PREVIOUS BUCKET TYPE                         
PRVBSTAT DS    XL1                 X'10' -  UNITS=1000                          
*                                                                               
PRVAUNT  DS    CL1                 PREVIOUS ACCOUNT UNIT                        
PRVALDG  DS    CL1                                  LEDGER                      
PRVACLV  DS    XL1                                  LEVEL                       
*                                                                               
PRVVCUNT DS    CL1                 PREVIOUS CONTRA ACCOUNT UNIT                 
PRVVCLDG DS    CL1                                         LEDGER               
PRVVCACL DS    XL1                                         LEVEL                
         ORG   PRVVCUNT                                                         
PRVVALS  DS    9XL3                                                             
PRVVALSL EQU   *-PRVVALS                                                        
*                                                                               
ULSUNIT  DS    CL1                 UNIT                                         
ULSLDGR  DS    CL1                 LEDGER                                       
ULSOFPOS DS    XL1                 OFFICE POSITION                              
ULSLVLS  DS    XL4                 MAX LENGTHS OF ACCOUNT AT LEVELS             
         ORG   ULSUNIT                                                          
ULSTABLE DS    10XL7                                                            
ULSTBLLN EQU   *-ULSTABLE                                                       
*                                                                               
OFFBLCK  DS    XL(OFFALLEN)        OFFAL BLOCK                                  
*                                                                               
         DS    0F                                                               
WRKRBUFF DS    XL(LENWKBUF)                                                     
*                                                                               
*                   ERROR EQUATES                                               
*                                                                               
E13BDOBJ EQU   0001                BAD OBJECT                                   
E13HDRRQ EQU   0002                BUDGET HEADER REQUIRED                       
E13BDHDR EQU   0003                BAD HEADER OBJECT                            
E13BDBDG EQU   0004                BAD BUDGET OBJECT                            
E13BDAGY EQU   0005                AGENCY ID DOES NOT EXIST                     
E13BADYR EQU   0006                BAD YEAR                                     
E13CPYCD EQU   0007                AGENCY DOESN'T HAVE A COMPANY CODE           
E13BDGCD EQU   0008                BAD BUDGET CODE                              
E13BDAMT EQU   0009                BAD AMOUNT                                   
E13WEOF  EQU   0010                END OF WORKER FILE                           
E13WNOR  EQU   0011                WORKER RECORD NOT FOUND                      
E13BIACC EQU   0012                BUDGET TYPE INCOMPATIBLE WITH ACCT           
E13BICON EQU   0013                BUDGET TYPE INCOMPATIBLE WITH CONTRA         
E13BDUNL EQU   0014                UNIT/LEDGER RECORD DOES NOT EXIST            
E13INVUL EQU   0015                INVALID UNIT/LEDGER RECORD                   
E13LVLND EQU   0016                LEVEL NOT DEFINED IN UNIT/LEDGER             
E13AWLVL EQU   0017                ACCOUNT IS IN THE WRONG LEVEL                
E13NOCAC EQU   0018                NO CONTRA-ACCOUNTS ALLOWED                   
E13OFICE EQU   0019                OFFICE CODE SECURITY LOCK OUT                
E13BDACC EQU   0020                BAD ACCOUNT CODE                             
*                                                                               
*                   EQUATES                                                     
*                                                                               
LENWKRIO EQU   2052                SPACE NEEDED FOR WORKER IO AREA              
LENWKBUF EQU   4*1024              SPACE NEEDED FOR WORKER BUFFER               
LENFREE  EQU   LENWKRIO            SPARE MEMORY NEEDED FOR OVERLAY              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035CTMAD13   11/09/99'                                      
         END                                                                    
