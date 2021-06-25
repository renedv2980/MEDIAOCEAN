*          DATA SET ACCAP16    AT LEVEL 029 AS OF 02/19/13                      
*PHASE T61D16C                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP16 -- OVERHEAD MAINTENANCE/LIST                 *         
*                                                                     *         
*  COMMENTS:     MAINTAINS OVERHEAD 1R RECORDS                        *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPE7 (MAINTENANCE)                        *         
*                        ACCAPE6 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED 1R OVERHEAD RECORDS                          *         
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
* FKON   7 INCREASE MAXRATE / FIX BUG IN VALMONS ROUTINE              *         
* PCAS   8 INCREASE MAXRATE TO 99,000,000.00                          *         
* DCUR  21 INCREASE MAX % RATE TO 1000.00000%                         *         
* RMOR 22MAY02 027 BAL/BALR -> BAS/BASR                               *         
* TKLU 19JUL02 028 FIX GERMAN MSABASIS VALUE SETTING                  *         
* DCUR  29 SET 'X'10' BIT AND FILTERS IN STATUS OF KEY AND RECORD     *         
*          WRITE BACK DIRECTORY KEY AFTER PUTREC MANUALLY SINCE GENCON*         
*          DOES NOT DO THE WRITE.                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         TITLE 'T61D16 - OVERHEAD RECORD MAINT/LIST'                            
T61D16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D16**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         APPLICATION STORAGE AREA                     
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP            ANY INITIALIZING                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        REDISPLAY AFTER CHANGES                      
         BE    DR                                                               
         CLI   MODE,XRECPUT        REDISPLAY AFTER CHANGES                      
         BE    XP                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
*                                                                               
VK       MVC   OFFICE,SPACES                                                    
         MVC   DEPT,SPACES                                                      
         MVC   SUBDPT,SPACES                                                    
         MVC   LOWLEVEL,SPACES                                                  
         MVC   ACCNT,SPACES                                                     
         MVI   INPBITS,0                                                        
         TM    TRANSTAT,RACHANG    REC OR ACT CHANGED?                          
         BZ    VK01                                                             
         MVI   SVOPSTAT,0                                                       
         MVI   OPTSTAT,0                                                        
VK01     GOTO1 GETLDG,DMCB,C'1R'   GET LENGTHS                                  
         MVC   LEVS1R,ABCDLEN                                                   
         CLI   ACTEQU,ACTLIST      DON'T VALIDATE NOW                           
         BE    VK05                                                             
         BAS   RE,VALOPTS                                                       
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OFFICE                                              *         
***********************************************************************         
*                                                                               
VK05     LA    R2,OVMOFFH                                                       
         CLI   OVMOFFH+5,0                                                      
         BNE   VK10                                                             
         CLI   ACTEQU,ACTLIST      NOT REQ'D FOR LIST                           
         BNE   ERRMISS                                                          
         B     VK20                                                             
*                                                                               
VK10     LA    R3,ACCNT                                                         
         CLC   OVMOFFH+5(1),LEVA1R    RIGHT LENGTH                              
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,OVMOFFH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),OVMOFF                                                   
         EX    R1,*+4                                                           
         MVC   OFFICE,OVMOFF                                                    
         LA    R3,1(R1,R3)                                                      
         OI    INPBITS,INPOFF                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DEPARTMENT                                          *         
***********************************************************************         
*                                                                               
VK20     LA    R2,OVMDEPTH                                                      
         CLI   OVMDEPTH+5,0        ANY DEPT?                                    
         BNE   VK25                                                             
         CLI   ACTEQU,ACTLIST      NOT REQ'D FOR LIST                           
         BNE   ERRMISS                                                          
         B     VK30                                                             
*                                                                               
VK25     CLC   OVMDEPTH+5(1),LEVB1R   RIGHT LENGTH                              
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         TM    INPBITS,INPOFF                                                   
         BNO   EMISHIGH            MISSING HIGHER LEVELS                        
         BAS   RE,VALACCNT         VALIDATE HIGHER LEVELS EXIST                 
         BNE   EMISHIGH                                                         
*                                                                               
         ZIC   R1,OVMDEPTH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),OVMDEPT                                                  
         EX    R1,*+4                                                           
         MVC   DEPT,OVMDEPT                                                     
         LA    R3,1(R1,R3)                                                      
         OI    INPBITS,INPDEPT                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SUB DEPARTMENT                                      *         
***********************************************************************         
*                                                                               
VK30     LA    R2,OVMSDPTH                                                      
         CLI   OVMSDPTH+5,0                                                     
         BNE   VK35                                                             
         CLI   ACTEQU,ACTLIST      NOT REQ'D FOR LIST                           
         BNE   ERRMISS                                                          
         B     VK40                                                             
*                                                                               
VK35     CLC   OVMSDPTH+5(1),LEVC1R   RIGHT LENGTH                              
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         TM    INPBITS,INPOFF+INPDEPT                                           
         BNO   EMISHIGH            MISSING HIGHER LEVELS                        
         BAS   RE,VALACCNT         VALIDATE HIGHER LEVELS EXIST                 
         BNE   EMISHIGH                                                         
*                                                                               
         ZIC   R1,OVMSDPTH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),OVMSDPT                                                  
         EX    R1,*+4                                                           
         MVC   SUBDPT,OVMSDPT                                                   
         LA    R3,1(R1,R3)                                                      
         OI    INPBITS,INPSDPT                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LOW LEVEL                                           *         
***********************************************************************         
*                                                                               
VK40     LA    R2,OVMPERH                                                       
         CLI   OVMPERH+5,0                                                      
         BNE   VK45                                                             
         CLI   ACTEQU,ACTLIST      NOT REQ'D FOR LIST                           
         BNE   ERRMISS                                                          
         B     VK50                                                             
*                                                                               
VK45     CLC   OVMPERH+5(1),LEVD1R    RIGHT LENGTH                              
         BH    ETOOLONG                                                         
         TM    INPBITS,INPOFF+INPDEPT+INPSDPT                                   
         BNO   EMISHIGH            MISSING HIGHER LEVELS                        
         BAS   RE,VALACCNT         VALIDATE HIGHER LEVELS EXIST                 
         BNE   EMISHIGH                                                         
*                                                                               
         ZIC   R1,OVMPERH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),OVMPER                                                   
         EX    R1,*+4                                                           
         MVC   LOWLEVEL,OVMPER                                                  
         OI    INPBITS,INPLOW                                                   
         EJECT                                                                  
***********************************************************************         
*        BUILD KEY                                                    *         
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
VK50     CLI   ACTEQU,ACTLIST       DON'T HAVE TO VALIDATE FOR LIST             
         BE    VK60                                                             
         CLI   ACTEQU,ACTADD        CLEAR BOTTOM SCREEN ON ADD                  
         BNE   *+8                                                              
         BAS   RE,CLRSCRN                                                       
         BAS   RE,VALOVER           MAKE SURE IT'S AN OVERHEAD ACCOUNT          
         BE    VK60                                                             
         LA    R2,OVMOFFH                                                       
         B     EINVACC                                                          
*                                                                               
VK60     XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,ACCNT                                                    
         CLC   SAVEKEY,BIGKEY                                                   
         BE    VKX                                                              
         MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
         MVC   SAVEKEY,BIGKEY                                                   
VKX      TM    OPTSTAT,OPTVK       DID WE COME FROM LIST RECS?                  
         BO    LR                  YES                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ACCOUNT IN ACCNT                                    *         
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
VALACCNT NTR1                                                                   
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,ACCNT                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'ACTKEY),BIGKEY                                         
         BNE   XNO                                                              
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OVERHEAD ACCOUNT IN ACCNT                           *         
***********************************************************************         
*                                                                               
VALOVER  NTR1                                                                   
         LA    R3,ACCNT                                                         
         ZIC   R1,LEVA1R                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=12CL1'9'   9'S FOR OFFICE                               
         BE    VOVERYES                                                         
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,LEVB1R                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=12CL1'9'   9'S FOR DEPT                                 
         BE    VOVERYES                                                         
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,LEVC1R                                                        
*        BCTR  R1,0                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        CLC   0(0,R3),=12CL1'9'   9'S FOR SUBDEPT                              
*        BE    VOVERYES                                                         
         AR    R3,R1                                                            
*                                                                               
         ZIC   R1,LEVD1R                                                        
         CH    R1,=H'3'            ONLY NEED 3 NINES IN LOW LEVEL               
         BL    *+8                                                              
         LA    R1,3                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=12CL1'9'   9'S FOR LOW LEVEL                            
         BE    VOVERYES                                                         
*                                                                               
VOVERNO  B     XNO                                                              
VOVERYES B     XYES                                                             
         EJECT                                                                  
***********************************************************************         
*        DISPLAY KEY                                                            
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
DK       LA    R6,BIGKEY                                                        
         MVC   SAVEKEY,BIGKEY                                                   
         MVC   OFFICE,SPACES                                                    
         MVC   DEPT,SPACES                                                      
         MVC   SUBDPT,SPACES                                                    
         MVC   LOWLEVEL,SPACES                                                  
         MVC   ACCNT,SPACES                                                     
         MVI   INPBITS,0                                                        
*                                                                               
         MVC   ACCNT,ACTKACT                                                    
         LA    R3,ACCNT                                                         
*                                                                               
         ZIC   R1,LEVA1R           OFFICE                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   OFFICE(0),0(R3)                                                  
         OC    OFFICE,SPACES                                                    
         CLC   OFFICE,SPACES                                                    
         BE    *+8                                                              
         OI    INPBITS,INPOFF                                                   
         MVC   OVMOFF,OFFICE                                                    
         OI    OVMOFFH+6,X'80'                                                  
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,LEVB1R           DEPARTMENT                                   
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   DEPT(0),0(R3)                                                    
         OC    DEPT,SPACES                                                      
         CLC   DEPT,SPACES                                                      
         BE    *+8                                                              
         OI    INPBITS,INPDEPT                                                  
         MVC   OVMDEPT,DEPT                                                     
         OI    OVMDEPTH+6,X'80'                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,LEVC1R           SUB-DEPARTMENT                               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SUBDPT(0),0(R3)                                                  
         OC    SUBDPT,SPACES                                                    
         CLC   SUBDPT,SPACES                                                    
         BE    *+8                                                              
         OI    INPBITS,INPSDPT                                                  
         MVC   OVMSDPT,SUBDPT                                                   
         OI    OVMSDPTH+6,X'80'                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,LEVD1R           LOW-LEVEL                                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   LOWLEVEL(0),0(R3)                                                
         OC    LOWLEVEL,SPACES                                                  
         CLC   LOWLEVEL,SPACES                                                  
         BE    *+8                                                              
         OI    INPBITS,INPLOW                                                   
         MVC   OVMPER,LOWLEVEL                                                  
         OI    OVMPERH+6,X'80'                                                  
*                                                                               
DKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                              *         
***********************************************************************         
*                                                                               
VR       CLI   ACTEQU,ACTDIS                                                    
         BE    DR                                                               
         OI    GENSTAT2,RETEQSEL   REDISPLAY BEFORE RETURN TO LIST              
*                                                                               
         BAS   RE,ELEM20                                                        
         BAS   RE,ELEM30                                                        
*                                                                               
         TM    INPBITS,INPLOW      LOW LEVEL REC                                
         BNO   VR10                                                             
         LA    R6,BIGKEY                                                        
         OI    ACTKSTA,ACTSABLP                                                 
         L     R6,AIO                                                           
         OI    ACTRSTA,ACTSABLP                                                 
         BAS   RE,ELEM32                                                        
         BAS   RE,ELEM33                                                        
*                                                                               
VR10     BAS   RE,VAL52BLK         UPDATE X'52' ELEMS                           
         OI    GENSTAT2,RETEQSEL   RETURN SAME SEL                              
VRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        UPDATE X''20' NAME ELEM FROM SCREEN                          *         
***********************************************************************         
*                                                                               
         USING NAMELD,R6                                                        
ELEM20   NTR1                                                                   
         LA    R2,OVMNAMEH                                                      
         CLI   OVMNAMEH+5,0                                                     
         BE    ERRMISS                                                          
*                                                                               
         MVI   ELCODE,NAMELQ       X'20'                                        
         GOTO1 REMELEM             REMOVE IF THERE                              
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ                                                     
         ZIC   R1,OVMNAMEH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   NAMEREC(0),OVMNAME                                               
         LA    R1,NAMLN1Q+1(R1)                                                 
         STC   R1,NAMLN                                                         
         BAS   RE,ADDIT            ADD ELEM                                     
EL20X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X '30' STATUS ELEM FROM SCREEN                        *         
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
ELEM30   NTR1                                                                   
         L     R6,AIO              CHANGE IF THERE                              
         MVI   ELCODE,RSTELQ       X'30'                                        
         BAS   RE,GETEL                                                         
         BNE   EL30B                                                            
*                                                                               
         ZIC   R2,RSTLN            MAKE ALL ELEMS THE LONGEST LENGTH            
         SH    R2,=H'1'                                                         
         EX    R2,*+4                                                           
         MVC   BLOCK(0),0(R6)      SAVE ELEM                                    
         MVI   ELCODE,RSTELQ                                                    
         GOTO1 REMELEM             DELETE SHORT ELEM                            
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         EX    R2,*+4                                                           
         MVC   ELEM(0),BLOCK       RESTORE ELEM (REMELEM CREAMS ELEM)           
         MVI   RSTLN,RSTLN3Q       UPDATE ELEM LENGTH                           
         B     EL30D                                                            
*                                                                               
EL30B    LA    R6,ELEM             ADD IF NOT THERE                             
         XC    ELEM,ELEM                                                        
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,RSTBDATE)                                   
         MVC   RSTTDATE,RSTBDATE                                                
*                                                                               
EL30D    MVC   RSTFILT1,OVMFLT1    UPDATE FILTERS                               
         OC    RSTFILT1,SPACES                                                  
         MVC   RSTFILT2,OVMFLT2                                                 
         OC    RSTFILT2,SPACES                                                  
         MVC   RSTFILT3,OVMFLT3                                                 
         OC    RSTFILT3,SPACES                                                  
         MVC   RSTFILT4,OVMFLT4                                                 
         OC    RSTFILT4,SPACES                                                  
         MVC   RSTFILT5,OVMFLT5                                                 
         OC    RSTFILT5,SPACES                                                  
*                                                                               
T        USING ACTRECD,RF                                                       
         LA    RF,BIGKEY                                                        
         MVC   T.ACTKSAF1,RSTFILT1                                              
         MVC   T.ACTKSAF2,RSTFILT2                                              
         MVC   T.ACTKSAF3,RSTFILT3                                              
         MVC   T.ACTKSAF4,RSTFILT4                                              
         MVC   T.ACTKSAF5,RSTFILT5                                              
*                                                                               
         L     RF,AIO                                                           
         MVC   T.ACTRSAF1,RSTFILT1                                              
         MVC   T.ACTRSAF2,RSTFILT2                                              
         MVC   T.ACTRSAF3,RSTFILT3                                              
         MVC   T.ACTRSAF4,RSTFILT4                                              
         MVC   T.ACTRSAF5,RSTFILT5                                              
         DROP  T                                                                
*                                                                               
         NI    RSTSTAT1,X'FF'-RSTSACIL                                          
         CLC   OVMLOCK,AC@YES      ACCOUNT IS LOCKED                            
         BNE   *+12                                                             
         OI    RSTSTAT1,RSTSACIL                                                
         B     EL30F                                                            
         CLC   OVMLOCK,AC@NO                                                    
         BE    EL30F                                                            
         LA    R2,OVMLOCKH                                                      
         B     ERRINV                                                           
*                                                                               
EL30F    BAS   RE,ADDIT            ADD ELEM                                     
EL30X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD X'32' ACCOUNT BALANCE ELEM IF ONE DOESN'T EXIST ON REC             
***********************************************************************         
*                                                                               
         USING ABLELD,R6                                                        
ELEM32   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'32' EXISTS                   
         BE    EL32X                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   ABLEL,ABLELQ        X'32'                                        
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         ZAP   ABLURG,=P'0'                                                     
         BAS   RE,ADDIT            ADD ELEM                                     
EL32X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD X'33' ACCOUNT PEEL-OFF ELEM IF ONE DOESN'T EXIST ON REC            
***********************************************************************         
*                                                                               
         USING APOELD,R6                                                        
ELEM33   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,APOELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'33' EXISTS                   
         BE    EL33X                                                            
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   APOEL,APOELQ        X'33'                                        
         MVI   APOLN,APOLN2Q                                                    
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         BAS   RE,ADDIT            ADD ELEM                                     
EL33X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE X'52' SALARY INFO                                   *         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
         USING EL52TABD,R4                                                      
VAL52BLK NTR1                                                                   
         LA    R4,EL52BLK                                                       
         AH    R4,STDISP                                                        
         LA    R2,OVMADDLH         ANYTHING IN ADD LINE                         
         MVI   ACTSTAT,ACTSADD     ACTION WOULD BE ADD                          
         BAS   RE,ANYINP           ANY DATA INPUT ON LINE                       
         BE    V52AA                                                            
         CLI   ACTEQU,ACTADD                                                    
         BNE   V52NXSCR            NO DATA ON CHANGE = NEXT LINE                
         TM    INPBITS,INPLOW                                                   
         BNO   V52NXSCR            NO DATA BUT NOT LOW LEVEL REC ANYWAY         
         LA    R1,DISMONSH                                                      
         LR    R2,R1                                                            
         B     ERRMISS             NO DATA ON ADD                               
*                                                                               
V52AA    TM    INPBITS,INPLOW      LOW LEVEL REC                                
         BNO   EONLYLOW            SAL ONLY ON LOW LEVEL                        
         B     V52C                YES                                          
*                                                                               
V52A     BAS   RE,ANYINP           ANY DATA INPUT ON LINE                       
         BNE   V52NXTAB            NO, NEXT TABLE ENTRY AND SCREEN LINE         
         TM    INPBITS,INPLOW      LOW LEVEL REC                                
         BNO   EONLYLOW            SAL ONLY ON LOW LEVEL                        
*                                                                               
         BAS   RE,VALACT           VALIDATE ACTION                              
V52C     BAS   RE,VALTYPE          VALIDATE TYPE                                
         BAS   RE,VALMONS          VALIDATE MONTHS                              
         BAS   RE,VALBASIS         VALIDATE BASIS                               
         BAS   RE,DUPCHK           CHECK FOR DUPLICATE ENTRIES                  
         BNE   *+12                                                             
         LA    R2,DISMONSH                                                      
         BE    ERRDUP                                                           
         BAS   RE,VALRATE          VALIDATE RATE                                
         MVC   ELSTDATE,STMONTH    FILL IN SOME FIELDS TO THE TABLE             
         MVC   ELENDATE,ENDMONTH   SO CAN CHECK FOR DUPLICATE ENTRIES           
         MVC   ELBASIS,BASIS                                                    
*                                                                               
         TM    ACTSTAT,ACTSADD      ADD NEW ELEM                                
         BO    V52ADD                                                           
         BAS   RE,DEL52            DELETE OLD X'52' ELEM                        
         TM    ACTSTAT,ACTSDEL     DELETE ONLY                                  
         BO    V52NXTAB                                                         
V52ADD   BAS   RE,ADD52            ADD NEW X'52' ELEM                           
*                                                                               
V52NXTAB LA    R4,ELTABLN(R4)      NEXT TABLE ENTRY                             
         LA    R1,EL52BLKX                                                      
         CR    R4,R1                                                            
         BNL   V52SORT                                                          
V52NXSCR LA    R2,DISLLEN(R2)      NEXT SCREEN LINE                             
         LA    R1,OVMENDH                                                       
         CR    R2,R1                                                            
         BNH   V52A                                                             
*                                                                               
V52SORT  BAS   RE,SORT52           SORT 52 ELEMS IN REC                         
         BAS   RE,DATECONF         CHECK FOR DATE CONFILCTS                     
V52X     B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ACTION AND SET ACTSTAT                              *         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
         USING ACTTABD,R3                                                       
VALACT   NTR1                                                                   
         MVI   ACTSTAT,0                                                        
         LA    R3,ACTTAB           MATCH WITH TABLE ENTRY                       
         ZIC   R1,DISACTH+5                                                     
         SH    R1,=H'1'                                                         
         BM    ERRMISS                                                          
VACT10   SR    RF,RF                                                            
         ICM   RF,3,ATACT                                                       
         LA    RF,SYSD(RF)                                                      
         EX    R1,*+8                                                           
         BE    VACT20                                                           
         CLC   DISACT(0),0(RF)                                                  
         LA    R3,ATABLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   VACT10                                                           
VACTINV  LA    R2,DISACTH          INVALID ACTION                               
         B     ERRINV                                                           
*                                                                               
VACT20   MVC   ACTSTAT,ATSTAT      SET ACTSTAT                                  
VACTX    B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TYPE                                                          
***********************************************************************         
*                                                                               
****** NO LONGER ENTER TYPE ON SCREEN.  DEFAULTS TO SALARY                      
*                                                                               
         USING TYPETABD,R3                                                      
VALTYPE  NTR1                                                                   
         MVI   TYPE,MSATSLRY       ONLY SALARY IS A VALID TYPE                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MONTHS                                                        
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
VALMONS  NTR1                                                                   
         XC    STMONTH,STMONTH                                                  
         XC    ENDMONTH,ENDMONTH                                                
         CLI   DISMONSH+5,0                                                     
         BE    VMONSERR                                                         
*                                                                               
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         LA    R1,DISSTMON                                                      
         ST    R1,DMCB                                                          
         ZIC   R1,DISMONSH+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,PVRCINV1     DATE 1 INVALID                               
         BE    VMONSERR                                                         
         TM    PVALASSM,PVALAEM    ONLY ONE DATE                                
         BO    VMONS30                                                          
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID                               
         BE    VMONSERR                                                         
*                                                                               
VMONS20  MVC   ENDMONTH,PVALPEND                                                
VMONS30  MVC   STMONTH,PVALPSTA                                                 
         SR    RF,RF                                                            
         IC    RF,DISMONSH         GET WHOLE FIELD LENGTH                       
         SH    RF,=H'9'            DEDUCT HEADER + 1 FOR EX                     
         EX    RF,*+4                                                           
         XC    DISSTMON(0),DISSTMON CLEAR FIELD FIRST                           
         OI    DISMONSH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(1,PVALPSTA),(6,DISSTMON)                            
         OC    ENDMONTH,ENDMONTH                  END MONTH                     
         BZ    VMONSX                                                           
         GOTO1 DATCON,DMCB,(1,PVALPEND),(6,DISNDMON)                            
         MVI   DISHYPH,C'-'                                                     
*                                                                               
VMONSX   B     XIT                                                              
VMONSERR LA    R2,DISMONSH                                                      
         B     EINVDATE                                                         
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE BASIS                                                         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
         USING BASETABD,R3                                                      
VALBASIS NTR1                                                                   
         XC    BASIS,BASIS                                                      
         LA    R3,BASISTAB         MATCH WITH TABLE ENTRY                       
         ZIC   R1,DISBASEH+5                                                    
         SH    R1,=H'1'                                                         
         BM    VBASEINV                                                         
*                                                                               
VBASE10  EX    R1,*+8                                                           
         B     *+4                                                              
         CLC   BTBASIS(0),DISBASE                                               
         BE    VBASE20                                                          
         LA    R3,BTABLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   VBASE10                                                          
VBASEINV LA    R2,DISBASEH         INVALID BASIS                                
         B     ERRINV                                                           
VBASEIN2 LA    R2,DISBASEH         INVALID BASIS                                
         B     EINVBASE            FOR THIS TYPE                                
*                                                                               
VBASE20  MVC   BASIS,BTBASISQ      SAVE 1ST CHARACTER                           
*                                                                               
VBASE40  CLI   BASIS,MSABYTD       YTD BASIS                                    
         BNE   VBASEX                                                           
         OC    ENDMONTH,ENDMONTH   END MONTH REQUIRED                           
         BNZ   VBASEX                                                           
         LA    R2,DISMONSH         END REQUIRED FOR YTD BASIS                   
         B     EENDREQD                                                         
*                                                                               
VBASEX   B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RATE                                                          
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
VALRATE  NTR1                                                                   
         ZAP   RATE,=P'0'                                                       
         MVI   RATESTAT,0                                                       
         ZAP   MAXRATE,=PL8'9900000000'  99,000,000.00                          
         OI    DISRATEH+6,X'80'                                                 
*                                                                               
         CLI   DISRATEH+5,0                                                     
         BE    VRATERR                                                          
         ZIC   R3,DISRATEH+5                                                    
         BCTR  R3,0                R3 = LEN-1                                   
         LA    R4,DISRATE(R3)      IS LAST CHAR A %                             
         CLI   0(R4),C'%'                                                       
         BE    VRATE10                                                          
         LA    R3,1(R3)            DOLLARS                                      
         B     VRATE40                                                          
*                                                                               
VRATE10  CH    R3,=H'3'            IS RATE LENGTH LESS THAN 3                   
         BL    VRATE30             THEN ASSUME 2 DECIMAL PLACES                 
*                                                                               
         LA    R1,DISRATE          A(RATE)                                      
         LR    R0,R3               LENGTH OF RATE                               
VRATE12  CLI   0(R1),C'.'          LOOK FOR DECIMAL PLACE                       
         BE    VRATE14                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VRATE12                                                       
         B     VRATE30             IF NONE = 2 DECIMAL PLACES                   
*                                                                               
VRATE14   LR    R1,R3               LEN OF RATE                                 
*                                                                               
VRATE30  MVI   RATESTAT,MSAS2DP    2 DECIMAL PLACES PERCENTAGE                  
VRATE40  MVC   BYTE,LANGCODE                                                    
         GOTO1 CASHVAL,DMCB,(X'82',DISRATE),(R3)                                
         CLI   DMCB,X'FF'                                                       
         BNE   VRATE100                                                         
         CLI   0(R4),C'%'          IS IT A PERCENTAGE                           
         BNE   VRATERR                                                          
VRATE50  MVI   RATESTAT,MSAS5DP    5 DECIMAL PLACES PERCENTAGE                  
         MVC   BYTE,LANGCODE                                                    
         GOTO1 CASHVAL,DMCB,(X'85',DISRATE),(R3)                                
         CLI   DMCB,X'FF'                                                       
         BE    VRATERR                                                          
*                                                                               
VRATE100 LA    R1,DMCB+4                                                        
         LA    R2,DISRATEH                                                      
         ZAP   RATE(6),0(8,R1)                                                  
         TM    RATESTAT,MSAS2DP                                                 
         BNO   VRATE110                                                         
         CP    RATE,=P'100000'       CAN'T BE HIGHER THAN 1000%                 
         BH    ERRATE                                                           
VRATE110 TM    RATESTAT,MSAS5DP                                                 
         BNO   VRATEX                                                           
         CP    RATE,=P'100000000'     CAN'T BE HIGHER THAN 1000%                
         BH    ERRATE                                                           
VRATEX   B     XIT                                                              
*                                                                               
VRATERR  LA    R2,DISRATEH                                                      
         B     ERRINV                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR DUPLICATE ENTRY                                    *         
*        NTRY - R4 POINTS TO CURRENT LINE BEING VALIDATED                       
***********************************************************************         
*                                                                               
         USING EL52TABD,R4                                                      
DUPCHK   NTR1                                                                   
         LR    R3,R4               SAVE THE CURRENT LINE                        
         LA    R4,EL52BLK                                                       
         B     DUP10                                                            
*                                                                               
DUPNX    LA    R4,ELTABLN(R4)      BUMP TO NEXT TABLE ENTRY                     
         LA    R1,EL52BLKX                                                      
         CR    R4,R1               COMPARE TO END OF TABLE                      
         BNL   DUPNO                                                            
*                                                                               
DUP10    CR    R3,R4               DON'T CHECK THE ONE BEING CHECKED            
         BE    DUPNX                                                            
         CLC   ELSTDATE,STMONTH    SAME START DATE?                             
         BNE   DUPNX                                                            
         CLC   ELENDATE,ENDMONTH   SAME END DATE?                               
         BNE   DUPNX                                                            
         CLC   ELBASIS,BASIS       SAME BASIS?                                  
         BNE   DUPNX                                                            
         B     DUPYES              FOUND A MATCH                                
*                                                                               
DUPNO    B     XNO                                                              
DUPYES   B     XYES                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD NEW X'52' ELEM FROM VALIDATED DATA                       *         
***********************************************************************         
*                                                                               
         USING EL52TABD,R4                                                      
         USING MSAELD,R6                                                        
ADD52    NTR1                                                                   
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   MSAEL,MSAELQ        X'52'                                        
         MVI   MSALN,MSALNQ                                                     
         ZAP   MSALARY,RATE                                                     
         MVC   MSABEG,STMONTH                                                   
         MVC   MSAEND,ENDMONTH                                                  
         MVC   MSATYPE,TYPE                                                     
         MVC   MSASTAT,RATESTAT                                                 
         MVC   MSABASIS,BASIS                                                   
         BAS   RE,ADDIT            ADD ELEM                                     
A52X     B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DELETE X'52' ELEM FROM TABLE ENTRY AT R4                     *         
***********************************************************************         
*                                                                               
         USING EL52TABD,R4                                                      
         USING MSAELD,R6                                                        
DEL52    NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,MSAELQ       X'52'                                        
         BAS   RE,GETEL                                                         
         B     D52A                                                             
D52ANX   BAS   RE,NEXTEL                                                        
D52A     BNE   D52X                                                             
         CLC   MSABEG,ELPRSTDT     SAME START MONTH                             
         BNE   D52ANX                                                           
         CLC   MSAEND,ELPRENDT     SAME END MONTH                               
         BNE   D52ANX                                                           
         CLC   MSATYPE,ELPRTYPE    SAME TYPE                                    
         BNE   D52ANX                                                           
         CLC   MSALARY,ELSALARY    SAME RATE                                    
         BNE   D52ANX                                                           
         CLC   MSASTAT,ELSTAT      SAME RATE STAT                               
         BNE   D52ANX                                                           
         CLC   MSABASIS,ELPRBAS    SAME BASIS                                   
         BNE   D52ANX                                                           
*                                                                               
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
D52X     B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        SORT X'52' ELEMS IN RECORD                                   *         
***********************************************************************         
*                                                                               
         USING MSAELD,R6                                                        
SORT52   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,MSAELQ       X'52'                                        
         SR    R3,R3               R3 = COUNTER                                 
         BAS   RE,GETEL                                                         
         BNE   S52X                                                             
         LR    R2,R6               R2 = A(FIRST ELEM)                           
S52A     LA    R3,1(R3)                                                         
         BAS   RE,NEXTEL                                                        
         BE    S52A                                                             
         CH    R3,=H'1'                                                         
         BNH   S52X                                                             
         GOTO1 VQSORT,DMCB,(C'N',(R2)),(R3),MSALNQ,5,8                          
S52X     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK RECORD FOR DATE CONFLICTS                              *         
***********************************************************************         
*                                                                               
MAIN     USING MSAELD,R6                                                        
BUMP     USING MSAELD,R2                                                        
DATECONF NTR1                                                                   
         XC    ERRBEG,ERRBEG       CLEAR ERRORS                                 
         XC    ERRBEG2,ERRBEG2                                                  
         XC    ERREND,ERREND                                                    
         XC    ERREND2,ERREND2                                                  
         XC    ERRTYPE,ERRTYPE                                                  
         XC    ERRTYPE2,ERRTYPE2                                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MSAELQ       X'52'                                        
         BAS   RE,GETEL                                                         
         BNE   DCONX                                                            
         LR    R2,R6                                                            
         B     DCON10                                                           
*                                                                               
DCONNX6  BAS   RE,NEXTEL                     BUMP MAIN POINTER R6               
         BNE   DCONX                                                            
         LR    R2,R6                                                            
*                                                                               
DCON10   CLI   MAIN.MSABASIS,MSABYTD         SKIP YTD                           
         BE    DCONNX6                                                          
*                                                                               
         MVC   YMD(L'MSABEG),MAIN.MSABEG     PREV MONTH IN YMD2                 
         MVI   YMD+L'MSABEG,1                                                   
         GOTO1 DATCON,DMCB,(1,YMD),(0,YYMMDD)                                   
         GOTO1 ADDAY,DMCB,YYMMDD,YYMMDD2,F'-1'                                  
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,YMD2)                                 
*                                                                               
DCONNX2  BAS   RE,NEXTEL2                    BUMP BUMP POINTER R2               
         BNE   DCONNX6                       NO MORE SO BUMP R6                 
*                                                                               
         CLC   MAIN.MSATYPE,BUMP.MSATYPE     SAME TYPE                          
         BNE   DCONNX2                                                          
         OC    BUMP.MSAEND,BUMP.MSAEND       ANY END DATE                       
         BZ    DCON20                                                           
         CLC   BUMP.MSAEND,YMD2                                                 
         BNH   DCONNX6                                                          
         B     DCONERR                                                          
DCON20   CLC   BUMP.MSABEG,YMD2                                                 
         BH    DCONERR                                                          
         MVC   BUMP.MSAEND,YMD2              FILL IN END DATE                   
         BNH   DCONNX6                                                          
*                                                                               
DCONERR  MVC   YMD(2),MAIN.MSABEG            EITHER COULD BE ON SCREEN          
         MVI   YMD+2,1                                                          
         GOTO1 DATCON,DMCB,(1,YMD),(6,ERRBEG)                                   
         MVC   ERRTYPE,MAIN.MSATYPE                                             
         OC    MAIN.MSAEND,MAIN.MSAEND ANY END DATE                             
         BZ    DCONERR1                                                         
         MVC   YMD(2),MAIN.MSAEND                                               
         MVI   YMD+2,1                                                          
         GOTO1 DATCON,DMCB,(1,YMD),(6,ERREND)                                   
*                                                                               
DCONERR1 MVC   YMD(2),BUMP.MSABEG            EITHER COULD BE ON SCREEN          
         MVI   YMD+2,1                                                          
         GOTO1 DATCON,DMCB,(1,YMD),(6,ERRBEG2)                                  
         MVC   ERRTYPE2,BUMP.MSATYPE                                            
         OC    BUMP.MSAEND,BUMP.MSAEND ANY END DATE                             
         BZ    DCONERR2                                                         
         MVC   YMD(2),BUMP.MSAEND                                               
         MVI   YMD+2,1                                                          
         GOTO1 DATCON,DMCB,(1,YMD),(6,ERREND2)                                  
DCONERR2 BAS   RE,FINDERR                                                       
         DC    H'0'                                                             
*                                                                               
DCONX    B     XIT                                                              
         DROP  MAIN,BUMP                                                        
         EJECT                                                                  
***********************************************************************         
*        FIND DATE ERROR TO FLAG ON SCREEN                            *         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
FINDERR  NTR1                                                                   
         LA    R2,OVMADDLH                                                      
*                                                                               
FERR10   CLC   ERRBEG,DISSTMON     COULD MATCH EITHER SET OF DATES              
         BNE   FERR20                                                           
         CLC   DISNDMON,SPACES                                                  
         BNH   FERR15                                                           
         CLC   ERREND,DISNDMON                                                  
         BNE   FERR20                                                           
FERR15   BAS   RE,VALTYPE                                                       
         CLC   TYPE,ERRTYPE                                                     
         BE    FERRERR                                                          
*                                                                               
FERR20   CLC   ERRBEG2,DISSTMON                                                 
         BNE   FERRNX                                                           
         CLC   DISNDMON,SPACES                                                  
         BNH   FERR25                                                           
         CLC   ERREND2,DISNDMON                                                 
         BNE   FERRNX                                                           
FERR25   BAS   RE,VALTYPE                                                       
         CLC   TYPE,ERRTYPE2                                                    
         BE    FERRERR                                                          
*                                                                               
FERRNX   LA    R2,DISLLEN(R2)                                                   
         LA    R1,OVMENDH                                                       
         CR    R2,R1                                                            
         BNH   FERR10                                                           
*                                                                               
FERRX    B     XIT                                                              
FERRERR  LA    R2,DISMONSH                                                      
         B     EDATECON            CONFLICTING DATES                            
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD ELEMENT - DELETE OLDEST X'52' IF REC TOO BIG             *         
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
ADDIT    NTR1                                                                   
ADD10    L     R6,AIO                                                           
         LH    R1,ACTRLEN                                                       
         ZIC   R0,ELEM+1                                                        
         AR    R1,R0                                                            
         CH    R1,=H'1950'                                                      
         BH    ADD20                                                            
         GOTO1 ADDELEM             ADD ELEM                                     
         B     ADDX                                                             
*                                                                               
ADD20    BAS   RE,SORT52           SORT - TO REMOVE OLDEST ELEMS                
         MVI   ELCODE,MSAELQ       X'52'                                        
         BAS   RE,GETEL            DIE IF THERE ARE NO 52'S TO REMOVE           
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,SVADDR                                                        
*                                                                               
ADD30NX  BAS   RE,NEXTEL                                                        
ADD30    BNE   ADD40                                                            
         ST    R6,SVADDR                                                        
         B     ADD30NX                                                          
*                                                                               
ADD40    L     R6,SVADDR                                                        
         MVI   0(R6),X'FF'                                                      
         MVC   BLOCK(L'ELEM),ELEM  REMELEM SCREWS UP ELEM                       
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         MVC   ELEM,BLOCK          RESTORE ELEM                                 
         B     ADD10                                                            
*                                                                               
ADDX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ANY INPUT ON LINE                                            *         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
ANYINP   NTR1                                                                   
         TM    DISACTH+1,X'20'     ACTION PROTECTED                             
         BO    AI10                                                             
         TM    DISACTH+4,X'20'     ACT PREVIOUSLY VALIDATED                     
         BNO   AIYES               NO, NEW INPUT                                
AI10     TM    DISMONSH+4,X'20'    MONTHS PREVIOUSLY VALIDATED                  
         BNO   AIYES                                                            
         TM    DISBASEH+4,X'20'    BASIS PREVIOUSLY VALIDATED                   
         BNO   AIYES                                                            
         TM    DISRATEH+4,X'20'    RATE PREVIOUSLY VALIDATED                    
         BNO   AIYES                                                            
AINO     B     XNO                                                              
AIYES    B     XYES                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RECORD                                               *         
***********************************************************************         
*                                                                               
DR       DS    0H                                                               
         BAS   RE,GETNAME          RETURNS NAME IN WORK                         
         MVC   OVMNAME,WORK                                                     
         OI    OVMNAMEH+6,X'80'                                                 
*                                                                               
         BAS   RE,DISEL30          DISPLAY STATUS X'30' INFO                    
*                                                                               
         BAS   RE,BLD52BLK         BUILD TABLE OF X'52' ELEMS                   
         BAS   RE,DIS52BLK         DISPLAY FROM TABLE                           
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        GETS NAME FROM X'20' ELEM                                    *         
*        NAME IN WORK ON EXIT                                         *         
***********************************************************************         
*                                                                               
         USING NAMELD,R6                                                        
GETNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,NAMELQ       X'20'                                        
         BAS   RE,GETEL                                                         
         BNE   GNX                                                              
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    GNX                                                              
         EX    R1,*+4                                                           
         MVC   WORK(0),NAMEREC                                                  
GNX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY X'30' STATUS ELEM INFO                               *         
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
DISEL30  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,RSTELQ       X'30'                                        
         BAS   RE,GETEL                                                         
         BNE   DIS30X                                                           
         MVC   OVMFLT1,RSTFILT1    FILTER 1                                     
         OI    OVMFLT1H+6,X'80'                                                 
         MVC   OVMFLT2,RSTFILT2    FILTER 2                                     
         OI    OVMFLT2H+6,X'80'                                                 
         MVC   OVMFLT3,RSTFILT3    FILTER 3                                     
         OI    OVMFLT3H+6,X'80'                                                 
         MVC   OVMFLT4,RSTFILT4    FILTER 4                                     
         OI    OVMFLT4H+6,X'80'                                                 
         MVC   OVMFLT5,SPACES      FILTER 5                                     
         OI    OVMFLT5H+6,X'80'                                                 
         CLI   RSTLN,RSTLN1Q                                                    
         BNH   DIS30A                                                           
         MVC   OVMFLT5,RSTFILT5    FILTER 5                                     
*                                                                               
DIS30A   MVC   OVMLOCK,AC@NO       ACCOUNT LOCKED                               
         OI    OVMLOCKH+6,X'80'                                                 
         TM    RSTSTAT1,RSTSACIL                                                
         BNO   *+10                                                             
         MVC   OVMLOCK,AC@YES                                                   
*                                                                               
DIS30X   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD TABLE OF X'52' MONTHLY SALARY ELEMENTS                 *         
***********************************************************************         
*                                                                               
         USING MSAELD,R6                                                        
         USING EL52TABD,R4                                                      
BLD52BLK NTR1                                                                   
         LA    R0,EL52BLK          CLEAR BLOCK                                  
         L     R1,=A(EL52BLKX-EL52BLK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SR    R3,R3               COUNTER                                      
         STH   R3,TABCOUNT                                                      
*                                                                               
         LA    R4,EL52BLK                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,MSAELQ       X'52'                                        
         BAS   RE,GETEL                                                         
         B     BLD10                                                            
BLD10NX  BAS   RE,NEXTEL                                                        
BLD10    BNE   BLDX                                                             
*                                                                               
         MVC   ELPRSTDT,MSABEG     PREVIOUS KEY TO ELEM                         
         MVC   ELPRENDT,MSAEND                                                  
         MVC   ELPRTYPE,MSATYPE                                                 
         MVC   ELPRBAS,MSABASIS                                                 
*                                                                               
         MVC   ELSTDATE,MSABEG     ELEM DATA                                    
         MVC   ELENDATE,MSAEND                                                  
         MVC   ELTYPE,MSATYPE                                                   
         MVC   ELSALARY,MSALARY                                                 
         MVC   ELSTAT,MSASTAT                                                   
         MVC   ELBASIS,MSABASIS                                                 
         MVC   ELINSURE,MSAINSUR                                                
*                                                                               
         LA    R3,1(R3)            TABLE ENTRY COUNTER                          
         STH   R3,TABCOUNT                                                      
*                                                                               
         LA    R4,ELTABLN(R4)      NEXT TABLE ENTRY                             
         LA    R1,EL52BLKX                                                      
         CR    R4,R1                                                            
         BL    BLD10NX                                                          
*                                                                               
BLDX     B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY X'52' SALARY INFO FROM TABLE                         *         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
         USING EL52TABD,R4                                                      
DIS52BLK NTR1                                                                   
         BAS   RE,CLRSCRN                                                       
         LA    R4,EL52BLK                                                       
         LA    R2,OVMLIN1H                                                      
*                                                                               
         CLI   PFKEY,0                                                          
         BNE   DIS5210                                                          
         CLI   ACTEQU,ACTSEL                                                    
         BE    DIS5225                                                          
         B     DIS5240                                                          
*                                                                               
DIS5210  CLI   PFKEY,PFKUP         PAGE UP                                      
         BNE   DIS5220                                                          
         MVC   STDISP,PRVSTDSP                                                  
         LA    R1,ELTABLN          ENTRY LEN                                    
         MH    R1,=H'10'          10 LINES ON SCREEN                            
         LH    R0,PRVSTDSP                                                      
         SR    R0,R1                                                            
         CH    R0,=H'0'                                                         
         BNL   *+6                                                              
         SR    R0,R0                                                            
         STH   R0,PRVSTDSP                                                      
         B     DIS5240                                                          
*                                                                               
DIS5220  CLI   PFKEY,PFKDOWN       PAGE DOWN                                    
         BNE   DIS5240                                                          
DIS5225  MVC   PRVSTDSP,STDISP                                                  
         MVC   STDISP,ENDISP       REDISPLAY LAST ON PREVIOUS PAGE              
         B     DIS5240                                                          
*                                                                               
DIS5240  LA    R0,ELTABLN          MAX FOR STDISP                               
         MH    R0,TABCOUNT         # OF ENTRIES IN TABLE                        
         LH    R1,STDISP                                                        
         CR    R0,R1                                                            
         BH    DIS5250                                                          
         LA    R1,0                                                             
DIS5250  AR    R4,R1                                                            
*                                                                               
DIS52NX  LA    R1,EL52BLK                                                       
         LR    R0,R4                                                            
         SR    R0,R1                                                            
         STH   R0,ENDISP           SAVE DISPLACEMENT INTO TABLE                 
*                                                                               
         OC    ELPRKEY(ELTABLN),ELPRKEY                                         
         BNZ   DIS5260                                                          
         NI    GENSTAT2,X'FF'-RETEQSEL GET NEXT SELECT                          
         LA    R0,0                                                             
         STH   R0,ENDISP           START FROM TOP NEXT ENTER                    
         B     DIS52X                                                           
*                                                                               
DIS5260  BAS   RE,DISPMONS                                                      
         BAS   RE,DISPBASE                                                      
         BAS   RE,DISPRATE                                                      
*                                                                               
         LA    R2,DISLLEN(R2)      NEXT SCREEN LINE                             
         LA    R1,OVMENDH                                                       
         CR    R2,R1                                                            
         BH    DIS52X                                                           
*                                                                               
         LA    R4,ELTABLN(R4)      NEXT TABLE ENTRY                             
         B     DIS52NX                                                          
*                                                                               
DIS52X   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY MONTHS                                               *         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
         USING EL52TABD,R4                                                      
DISPMONS NTR1                                                                   
         MVC   YMD(L'ELSTDATE),ELSTDATE           START MONTH                   
         MVI   YMD+2,X'00'                                                      
         GOTO1 DATCON,DMCB,(1,YMD),(6,DISSTMON)                                 
*                                                                               
         OC    ELENDATE,ELENDATE                  END MONTH                     
         BZ    DMONSX                                                           
         MVC   YMD(L'ELENDATE),ELENDATE                                         
         MVI   YMD+2,X'00'                                                      
         GOTO1 DATCON,DMCB,(1,YMD),(6,DISNDMON)                                 
         MVI   DISHYPH,C'-'                                                     
DMONSX   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY BASIS                                                *         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
         USING EL52TABD,R4                                                      
         USING BASETABD,R3                                                      
DISPBASE NTR1                                                                   
         LA    R3,BASISTAB                                                      
*                                                                               
DBASE10  CLC   ELBASIS,BTBASISQ    MATCH ON FIRST CHAR                          
         BE    DBASE20                                                          
         LA    R3,BTABLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   DBASE10                                                          
         B     DBASEX                                                           
*                                                                               
DBASE20  MVC   DISBASE,BTBASIS     DISPLAY                                      
DBASEX   B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RATE                                                 *         
***********************************************************************         
*                                                                               
         USING DISLINED,R2                                                      
         USING EL52TABD,R4                                                      
DISPRATE NTR1                                                                   
         CLI   ELSTAT,0            DOLLARS                                      
         BE    DRATE20                                                          
         TM    ELSTAT,MSAS5DP      5 DECIMAL PLACES PERCENTAGE                  
         BNO   DRATE10                                                          
         CURED (P6,ELSALARY),(L'DISRATE,DISRATE),5,ALIGN=LEFT,         X        
               TRAIL=%                                                          
         B     DRATEX                                                           
*                                                                               
DRATE10  CURED (P6,ELSALARY),(L'DISRATE,DISRATE),2,ALIGN=LEFT,         X        
               TRAIL=%                                                          
         B     DRATEX                                                           
*                                                                               
DRATE20  CURED (P6,ELSALARY),(L'DISRATE,DISRATE),2,COMMAS=YES,FLOAT=-, X        
               ALIGN=LEFT                                                       
*                                                                               
DRATEX   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        AFTER A PUT                                                            
***********************************************************************         
         SPACE 1                                                                
XP       MVI   IOOPT,C'Y'          NEED TO UPDATE STATUS OF KEY AND             
         GOTO1 WRITE               NEED TO DO THE WRITE MYSELF SINCE            
         MVI   IOOPT,C'N'          GENCON DOES NOT DO IT AFTER A PUTREC         
         B     DR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ON-SCREEN LIST                                               *         
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
         USING LSTLINED,R2                                                      
LR       NI    OPTSTAT,X'FF'-OPTVK                                              
         LA    R6,BIGKEY                                                        
         XC    ENDISP,ENDISP                                                    
         LA    R2,LISTAR                                                        
         MVI   NLISTS,14                                                        
         BAS   RE,VALOPTS                                                       
         CLC   OPTSTAT,SVOPSTAT    HAVE ANY OPTIONS BEEN ADDED?                 
         BE    LR05                NO                                           
         MVC   SVOPSTAT,OPTSTAT                                                 
         OI    OPTSTAT,OPTVK                                                    
         B     VK                                                               
*                                                                               
LR05     SR    R3,R3               R3 = ACCOUNT COMPARE LENGTH                  
         TM    INPBITS,INPOFF      SPECIFIC OFFICE ENTERED                      
         BNO   LR20                                                             
         ZIC   R3,LEVA1R                                                        
         TM    INPBITS,INPDEPT     SPECIFIC DEPT ENTERED                        
         BNO   LR10                                                             
         ZIC   R1,LEVB1R                                                        
         AR    R3,R1                                                            
         TM    INPBITS,INPSDPT     SPECIFIC SUBDEPT ENTERED                     
         BNO   LR10                                                             
         ZIC   R1,LEVC1R                                                        
         AR    R3,R1                                                            
         TM    INPBITS,INPLOW      SPECIFIC LOW LEVEL ENTERED                   
         BNO   LR10                                                             
         ZIC   R1,LEVD1R                                                        
         AR    R3,R1                                                            
LR10     BCTR  R3,0                                                             
LR20     STC   R3,COMPLEN                                                       
*                                                                               
         OC    BIGKEY,BIGKEY                                                    
         BNZ   LRNXT                                                            
         MVC   BIGKEY,SAVEKEY                                                   
*                                                                               
LRHI     GOTO1 HIGH                                                             
         CLC   KEYSAVE(ACTKACT-ACTKEY),BIGKEY   STILL 1R                        
         BNE   LRX                                                              
*                                                                               
         CLI   INPBITS,0           ANY ACCOUNT ENTERED                          
         BE    LR50                                                             
         ZIC   R3,COMPLEN          ACCOUNT COMPARE LENGTH                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   ACTKACT(0),ACCNT    ACCOUNT FILTER                               
         BNE   LRX                                                              
*                                                                               
LR50     MVC   ACCNT,ACTKACT                                                    
         BAS   RE,VALOVER          OVERHEAD ACCOUNT                             
         BNE   LRNXT                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTELD,R6                                                        
         MVI   ELCODE,RSTELQ       STATUS ELEM '30'                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    OPTSTAT,OPTLCKNO    IS THE LOCKED=NO OPTION IN USE               
         BZ    *+16                NO                                           
         TM    RSTSTAT1,RSTSACIL                                                
         BZ    LR60                                                             
         B     LRNXT                                                            
         TM    OPTSTAT,OPTLCKON    IS THE LOCKED=ONLY OPTION IN USE             
         BZ    LR60                                                             
         TM    RSTSTAT1,RSTSACIL                                                
         BZ    LRNXT                                                            
         DROP  R6                                                               
*                                                                               
LR60     LA    R3,ACCNT            DISPLAY ACCOUNT                              
         ZIC   R1,LEVA1R           OFFICE                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   LSTOFF(0),0(R3)                                                  
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,LEVB1R           DEPARTMENT                                   
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   LSTDEPT(0),0(R3)                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,LEVC1R           SUB-DEPARTMENT                               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   LSTSDPT(0),0(R3)                                                 
         LA    R3,1(R1,R3)                                                      
*                                                                               
         ZIC   R1,LEVD1R           LOW-LEVEL                                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   LSTLOW(0),0(R3)                                                  
*                                                                               
*        GOTO1 GETREC                                                           
         BAS   RE,GETNAME          RETURNS NAME IN WORK                         
         MVC   LSTNAME,WORK                                                     
*                                                                               
         GOTO1 LISTMON                                                          
LRNXT    LA    R6,BIGKEY                                                        
         USING ACTRECD,R6                                                       
         MVI   ACTKACT+L'ACTKACT,X'FF'                                          
         B     LRHI                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CLEAR SOME FIELDS                                                      
***********************************************************************         
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,OVMADDLH         CLEAR ALL FIELDS                             
         LA    R3,OVMPFKYH                                                      
*                                                                               
CLR10    TM    1(R2),X'20'         SKIP PROTECTED                               
         BO    CLR20                                                            
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         BM    CLR20                                                            
*                                                                               
         CLI   MODE,VALKEY         IN VALKEY                                    
         BE    CLR15                                                            
         EX    R1,*+4                                                           
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    4(R2),X'20'         VALIDATED                                    
         B     CLR20                                                            
*                                                                               
CLR15    EX    R1,*+8              VALIDATE ANY EMPTY FIELDS ON ADD             
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BH    CLR20                                                            
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
CLR20    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLR10               NO                                           
CLRX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ANY OPTIONS                                                   
***********************************************************************         
*                                                                               
VALOPTS  NTR1                                                                   
         MVI   OPTSTAT,0                                                        
         LA    R2,CONOPTH                                                       
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
         CLI   ACTEQU,ACTLIST      VALID ONLY FOR LIST                          
         BE    *+18                                                             
         MVC   CONOPT,SPACES                                                    
         OI    6(R2),X'80'         XMIT                                         
         B     OPTX                                                             
*                                                                               
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,CONOPTH,SCANBLK                                     
         CLI   DMCB+4,X'00'                                                     
         BE    EINVOPT                                                          
         LA    R4,SCANBLK                                                       
         USING SCANBLKD,R4                                                      
*                                                                               
OPT10    ZIC   R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,EXOPTL           OPTION LOCKED                                
         BNE   EINVOPT                                                          
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,EXOPTLNO         =NO                                          
         BNE   *+12                                                             
         OI    OPTSTAT,OPTLCKNO                                                 
         B     OPTX                                                             
         EX    R1,EXOPTLON         =ONLY                                        
         BNE   *+12                                                             
         OI    OPTSTAT,OPTLCKON                                                 
         B     OPTX                                                             
         EX    R1,EXOPTLYS         =YES(DEFAULT)                                
         BNE   EINVOPT                                                          
         B     OPTX                                                             
*                                                                               
EXOPTL   CLC   AC@LCKDU(0),SC1STFLD    OPTION LOCKED                            
EXOPTLNO CLC   AC@NOU(0),SC2NDFLD      =NO                                      
EXOPTLON CLC   AC@ONLYU(0),SC2NDFLD    =ONLY                                    
EXOPTLYS CLC   AC@YESU(0),SC2NDFLD     =YES (DEFAULT)                           
*                                                                               
OPTX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT2,RETEQSEL   RETURN SAME SELECTION (FOR PAGING)           
*                                                                               
****WANT TO SUPPRESS CARRY OVER FIELDS TO HIS/LIST AND PERSON/LIST              
*   FROM OVERHEAD/DIS                                                           
         CLI   ACTEQU,ACTLIST                                                   
         BNE   SETUP01                                                          
         CLI   PFKEY,3             HISTORY/LIST                                 
         BE    SETUP05                                                          
         B     SETUP04                                                          
SETUP01  CLI   PFKEY,1             PERSON/LIST                                  
         BE    SETUP04                                                          
         CLI   PFKEY,3                                                          
         BNE   *+8                                                              
SETUP04  NI    GENSTAT1,X'FF'-USKYMRG                                           
SETUP05  SR    R2,R2                                                            
         SR    R3,R3                                                            
         CLI   ACTEQU,ACTLIST                                                   
         BE    SET20                                                            
         LA    R3,OVMPFKYH                                                      
         CLI   PFKEY,PFKUP         UP                                           
         BE    INITPFKY                                                         
         CLI   PFKEY,PFKDOWN       DOWN                                         
         BE    INITPFKY                                                         
SET10    L     R2,=A(MPFTABLE)                                                  
         A     R2,RELO                                                          
         B     INITPFKY                                                         
SET20    L     R2,=A(LPFTABLE)                                                  
         A     R2,RELO                                                          
         LA    R3,OVLPFKYH                                                      
*                                                                               
INITPFKY GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
*                                                                               
         USING BASETABD,R3                                                      
         LA    R3,BASISTAB                                                      
         CLI   BTBASIS,X'40'       ALREADY DONE                                 
         BH    SETX                                                             
SETUP25  GOTO1 DICTATE,DMCB,C'SU  ',BTBASIS,0                                   
         LA    R3,BTABLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   SETUP25                                                          
         DROP  R3                                                               
*                                                                               
*        BAS   RE,VALOPTS          VALIDATE ANY OPTIONS                         
*                                                                               
SETX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        RANDOM STUFF                                                 *         
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
EONLYLOW MVC   GERROR,=AL2(ACENOLOW)   SAL TO LOW LEVEL ONLY                    
         B     ACCERRX                                                          
EENDREQD MVC   GERROR,=AL2(ACEENDRQ)   END REQD WITH YTD                        
         B     ACCERRX                                                          
ERRATE   MVC   GERROR,=AL2(ACERTEHI)   RATE NOT > 1000%                         
         B     ACCERRX                                                          
EINVBASE MVC   GERROR,=AL2(ACEINVBA)   INVALID BASIS FOR TYPE                   
         B     ACCERRX                                                          
EDATECON MVC   GERROR,=AL2(ACEDCONF)   CONFLICTING DATES                        
         B     ACCERRX                                                          
EINVACC  MVC   GERROR,=AL2(ACEACCT)                                             
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EMISHIGH MVC   GERROR,=AL2(ACEHIGH)    MISSING HIGHER LEVELS                    
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVTYPE MVC   GERROR,=AL2(ACEIVTYP)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
ERRDUP   MVC   GERROR,=AL2(ACEDUPEN)   DUPLICATE ENTRY                          
         B     ACCERRX                                                          
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         GETEL2 R2,DATADISP,ELCODE                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLE OF VALID TYPES                                                   
***********************************************************************         
                                                                                
TYPETAB  DC    AL1(MSATSLRY),CL4'SAL '                                          
         DC    AL1(MSATOVER),CL4'OT  '                                          
         DC    AL1(MSATTEMP),CL4'TEMP'                                          
         DC    AL1(MSATPENS),CL4'PEN '                                          
         DC    AL1(MSATBONU),CL4'BON '                                          
         DC    AL1(MSATBENE),CL4'BEN '                                          
         DC    AL1(MSATADMN),CL4'ADMN'                                          
         DC    AL1(MSATBUDG),CL4'BUD '                                          
         DC    AL1(MSATRATE),CL4'RTE '                                          
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
*        TABLE OF VALID BASIS                                                   
***********************************************************************         
*                                                                               
BASISTAB DCDD  AC#RSRMO,4          MON                                          
         DC    AL1(MSABMNTH)                                                    
         DCDD  AC#RSRQ1,4          QTR                                          
         DC    AL1(MSABQRTR)                                                    
         DCDD  AC#RSRY2,4          YTD                                          
         DC    AL1(MSABYTD)                                                     
         DCDD  AC#ANUAL,4          ANN(UAL)                                     
         DC    AL1(MSABANN)                                                     
**       DCDD  AC#CHR,4            HR     DELETED ON 10/23/01                   
**       DC    AL1(MSABHRL)                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLE OF VALID ACTIONS                                                 
***********************************************************************         
*                                                                               
ACTTAB   DC    AL2(AC@ADDU-SYSD),AL1(ACTSADD)                                   
         DC    AL2(AC@CHAU-SYSD),AL1(ACTSCHA)                                   
         DC    AL2(AC@DELU-SYSD),AL1(ACTSDEL)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        MAINTENANCE SCREEN PFKEY TABLE DEFINITIONS                             
***********************************************************************         
MPFTABLE DS    0C                                                               
*                                                                               
*        PERSON  LIST                                                           
*                                                                               
         DC    AL1(MPF01X-*,01,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
*        DC    AL1(KEYTYTWA,L'OVMPER-1),AL2(OVMPER-T61DFFD)                     
MPF01X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(MPF03X-*,03,PFTCPROG,(MPF03X-MPF03)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
MPF03    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
*        DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
*        DC    AL1(KEYTYTWA,L'OVMPER-1),AL2(OVMPER-T61DFFD)                     
MPF03X   EQU   *                                                                
*                                                                               
*        OVERHEAD LIST                                                          
*                                                                               
         DC    AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#COVER,8                                                       
         DCDD  AC#LIST,8                                                        
MPF04    DC    AL1(KEYTYTWA,L'OVMOFF-1),AL2(OVMOFF-T61DFFD)                     
MPF04X   EQU   *                                                                
*                                                                               
*        MAD INPUT                                                              
*                                                                               
         DC    AL1(MPF06X-*,06,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
MPF06X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        LIST SCREEN PFKEY TABLE DEFINITIONS                                    
***********************************************************************         
LPFTABLE DS    0C                                                               
*                                                                               
*        HISTORY DISPLAY                                                        
*                                                                               
         DC    AL1(LPF03X-*,03,PFTCPROG,0,0)                                    
         DCDD  AC#DSP,3                                                         
         DCDD  AC#HIST,8                                                        
         DCDD  AC#DSP,8                                                         
LPF03X   EQU   *                                                                
*                                                                               
*        PERSON  DISPLAY                                                        
*                                                                               
         DC    AL1(LPF02X-*,02,PFTCPROG,(LPF02X-LPF02)/KEYLNQ,0)                
         DCDD  AC#CPRSN,3                                                       
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#DSP,8                                                         
LPF02    DC    AL1(KEYTYTWA,L'OVLPER-1),AL2(OVLPER-T61DFFD)                     
LPF02X   EQU   *                                                                
*                                                                               
*        PERSON  LIST                                                           
*                                                                               
         DC    AL1(LPF01X-*,01,PFTCPROG,(LPF01X-LPF01)/KEYLNQ,0)                
         DC    CL3'PL '                                                         
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
LPF01    DC    AL1(KEYTYTWA,L'OVLPER-1),AL2(OVLPER-T61DFFD)                     
LPF01X   EQU   *                                                                
*                                                                               
*        MAD INPUT                                                              
*                                                                               
         DC    AL1(LPF06X-*,06,PFTCPROG,0,0)                                    
         DCDD  AC#MAD,3                                                         
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
LPF06X   EQU   *                                                                
*                                                                               
*        METHOD LIST                                                            
*                                                                               
         DC    AL1(LPF10X-*,10,PFTCPROG,0,0)                                    
         DCDD  AC#METH,3                                                        
         DCDD  AC#METH,8                                                        
         DCDD  AC#LIST,8                                                        
LPF10X   EQU   *                                                                
*                                                                               
*        OVERHEAD LAST (BACK PAGE)                                              
*                                                                               
         DC    AL1(LPF07X-*,07,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        '                                           
         DCDD  AC#LAST,8                                                        
LPF07X   EQU   *                                                                
*                                                                               
*        OVERHEAD NEXT (NEXT PAGE)                                              
*                                                                               
         DC    AL1(LPF08X-*,08,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        '                                           
         DCDD  AC#NXT,8                                                         
LPF08X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                     *         
***********************************************************************         
*                                                                               
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        DDPERVALD                                                              
*        DDSCANBLKD                                                             
*        FASECRETD                                                              
*        ACCAPWORKD                                                             
*        ACCAPDSECT                                                             
*        ACBMONVALD                                                             
*        ACGENFILE                                                              
*        ACDDEQUS                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREEENS                                                     *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPE7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPE6D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                          *         
***********************************************************************         
*                                                                               
STARTWRK DS    0F                                                               
RELO     DS    A                                                                
SVADDR   DS    F                                                                
STDISP   DS    H                   DISP TO FIRST ENTRY DISPLAYED                
PRVSTDSP DS    H                   DISP TO PREVIOUS START FOR PAGE UP           
ENDISP   DS    H                   DISP TO LAST ENTRY DISPLAYED                 
TABCOUNT DS    H                   COUNT OF TABLE ENTRIES                       
COMPLEN  DS    X                   COMPARE LENGTH                               
CURDEC   DS    X                                                                
*                                                                               
OFFICE   DS    CL2                 OFFICE CODE                                  
DEPT     DS    CL3                 DEPARTMENT CODE                              
SUBDPT   DS    CL3                 SUB DEPARTMENT CODE                          
LOWLEVEL DS    CL8                 LOW LEVEL OF ACCOUNT                         
*                                                                               
SCANBLK  DS    CL32                BLOCK FOR SCANNER                            
RATE     DS    PL6                                                              
MAXRATE  DS    PL8                                                              
STMONTH  DS    PL2                                                              
ENDMONTH DS    PL2                                                              
TYPE     DS    XL1                                                              
RATESTAT DS    XL1                                                              
BASIS    DS    CL1                                                              
*                                                                               
PFKUP    EQU   7                                                                
PFKDOWN  EQU   8                                                                
*                                                                               
INPBITS  DS    XL1                 WHAT LEVELS INPUT                            
INPOFF   EQU   X'80'                                                            
INPDEPT  EQU   X'40'                                                            
INPSDPT  EQU   X'20'                                                            
INPLOW   EQU   X'10'                                                            
*                                                                               
ACTSTAT  DS    XL1                 LINE ACTION                                  
ACTSADD  EQU   X'80'               ADD NEW ELEM                                 
ACTSCHA  EQU   X'40'               CHANGE  EXISTING ELEM                        
ACTSDEL  EQU   X'20'               DELETE ELEM                                  
*                                                                               
ERRBEG   DS    CL6                 ERROR START DATE                             
ERREND   DS    CL6                 ERROR END DATE                               
ERRTYPE  DS    XL1                 ERROR TYPE                                   
ERRBEG2  DS    CL6                 ERROR START DATE                             
ERREND2  DS    CL6                 ERROR END DATE                               
ERRTYPE2 DS    XL1                 ERROR TYPE                                   
*                                                                               
OPTSTAT  DS    XL1                 OPTION STATUS                                
OPTLCKNO EQU   X'80'               LOCKED=NO                                    
OPTLCKON EQU   X'40'               LOCKED=ONLY                                  
OPTVK    EQU   X'20'               DO VK TO RESET REC POINTER                   
*                                                                               
SVOPSTAT DS    CL1                 SAVE OPTION BIT                              
SBYTE    DS    CL1                 SECURITY BYTE                                
NAMEFLDQ EQU   1                   NAME FLIELD EQUATE FOR SECURITY              
*                                                                               
LEVS1R   DS    0CL4                LENGTHS OF ALL LEVELS                        
LEVA1R   DS    XL1                                                              
LEVB1R   DS    XL1                                                              
LEVC1R   DS    XL1                                                              
LEVD1R   DS    XL1                                                              
*                                                                               
YMD      DS    PL3                                                              
YMD2     DS    PL3                                                              
YYMMDD   DS    CL6                                                              
YYMMDD2  DS    CL6                                                              
*                                                                               
ACCNT    DS    CL12                MY TEMP ACCOUNT FIELD                        
SAVEKEY  DS    XL42                ACCFILE KEY                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
BLOCKSD  DSECT                     BLOCKS PUT IN SYSSPARE                       
EL52BLK  DS    120CL(ELTABLN)                                                   
EL52BLKX DS    C                                                                
         EJECT                                                                  
***********************************************************************         
*        TABLE DSECTS                                                           
***********************************************************************         
*                                                                               
BASETABD DSECT                     BASISTAB                                     
BTBASIS  DS    CL4                 BASIS                                        
BTBASISQ DS    CL1                 MSAEL VALUE                                  
BTABLNQ  EQU   *-BASETABD                                                       
*                                                                               
TYPETABD DSECT                     TYPETAB                                      
TTEQU    DS    XL1                 TYPE EQUATE FOR ELEM                         
TTTYPE   DS    CL4                 TYPE WORD                                    
TTABLNQ  EQU   *-TYPETABD                                                       
*                                                                               
ACTTABD  DSECT                     ACTTAB                                       
ATACT    DS    AL2                 DISPLACEMENT TO ACTION WORD                  
ATSTAT   DS    XL1                 ACTSTAT EQUATE                               
ATABLNQ  EQU   *-ACTTABD                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                                        
***********************************************************************         
*                                                                               
LSTLINED DSECT                     LIST LINE DSECT                              
         DS    CL4                                                              
LSTOFF   DS    CL2                                                              
         DS    CL4                                                              
LSTDEPT  DS    CL6                                                              
         DS    CL2                                                              
LSTSDPT  DS    CL6                                                              
         DS    CL2                                                              
LSTLOW   DS    CL8                                                              
         DS    CL2                                                              
LSTNAME  DS    CL36                                                             
LSTLEN   EQU   *-LSTLINED                                                       
         EJECT                                                                  
***********************************************************************         
*        DISPLAY LINE DSECT                                                     
***********************************************************************         
*                                                                               
DISLINED DSECT                     DISPLAY LINE DSECT                           
DISACTH  DS    CL8                                                              
DISACT   DS    CL3                                                              
DISMONSH DS    CL8                                                              
DISSTMON DS    CL6                                                              
DISHYPH  DS    CL1                                                              
DISNDMON DS    CL6                                                              
DISBASEH DS    CL8                                                              
DISBASE  DS    CL4                                                              
DISRATEH DS    CL8                                                              
DISRATE  DS    CL15                                                             
DISLLEN  EQU   *-DISLINED                                                       
         EJECT                                                                  
***********************************************************************         
*        TABLE OF 52 ELEMS DSECT                                                
***********************************************************************         
*                                                                               
EL52TABD DSECT                     TABLE OF '52' ELEMS                          
*                                                                               
ELPRKEY  DS    0CL(L'ELPRSTDT+L'ELPRENDT+L'ELPRTYPE+L'ELPRBAS)                  
ELPRSTDT DS    PL2                 PREVIOUS START DATE                          
ELPRENDT DS    PL2                 PREVIOUS END DATE                            
ELPRTYPE DS    XL1                 PREVIOUS TYPE                                
ELPRBAS  DS    CL1                 PREVIOUS BASIS                               
*                                                                               
ELSTDATE DS    PL2                                                              
ELENDATE DS    PL2                                                              
ELTYPE   DS    XL1                                                              
ELSALARY DS    PL6                                                              
ELSTAT   DS    XL1                                                              
ELBASIS  DS    CL1                                                              
ELINSURE DS    XL2                                                              
*                                                                               
ELTABLN  EQU   *-EL52TABD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACCAP16   02/19/13'                                      
         END                                                                    
