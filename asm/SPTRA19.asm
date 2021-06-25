*          DATA SET SPTRA19    AT LEVEL 031 AS OF 06/05/08                      
*PHASE T21619A                                                                  
*                                                                               
*  TITLE: T21619 - NETWORK TRAFFIC CLIENT LIST RECORD MAINTENANCE'    *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL MAINTAIN RECORDS CONTAINING LISTS OF   *         
*            PEOPLE TO BE NOTIFIED WHENENVER INSTRUCTIONS ARE RUN FOR *         
*            A GIVEN CLIENT.                                          *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATA MANAGER                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRAA9 (T216A9)                                *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: UPDATED CLIENT LIST RECORDS                              *          
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - ELEM CTR IN VR AND DR RTNS                            *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
*  LEV 20 ADD OPTIONAL NETWORK TO KEY                                 *         
*                                                                     *         
*  LEV 23 EJOR 13JUL93 FIX BUG IN LRR                                 *         
*  LEV 24 BGRI 31AUG93 ADD FAX NUMBER, CLEAR NAME LIST ON DISPLAY     *         
*  LEV 25 BGRI 17OCT01 ADD OFFICE SECURITY                            *         
*  LEV 26 SMUR 27JUN02 CLIENT STRING SECURITY                         *         
*  LEV 27 SMUR 19NOV03 BRAND LEVEL SECURITY                           *         
*  LEV 28 BGRI JAN15/04 CHGE SVSPARE TO TO SVSPAREX                   *         
*  LEV 30 SMUR FEB25/08 PRINT ??? FOR RECS WITH UNKNOWN CLIENT        *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21619 CLIENT LIST RECORD MAINTENANCE'                          
T21619   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21619**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE LIST RECORDS                         
         BE    LR                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
         SPACE                                                                  
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
VK01     DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
*                                                                               
         XC    QCLT,QCLT           CLIENT                                       
         LA    R2,TRACLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK08                                                             
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK05                 NO                                          
         SPACE                                                                  
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
         SPACE                                                                  
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         SPACE                                                                  
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
         SPACE                                                                  
         MVI   ERROR,0                                                          
*                                                                               
VK05     CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK10                                                             
*                                                                               
VK08     DS    0H                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT             VALIDATE CLIENT                              
         MVI   ERROPT,0                                                         
         SPACE                                                                  
         CLI   ERROR,0                                                          
         BE    VK10                                                             
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    TRAPERR              NO                                          
         CLI   ERROR,SECLOCK       ONLY VALID ERROR SEC-LOCKOUT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BO    TRAPERR                                                          
*                                                                               
VK10     XC    NETWORK,NETWORK     NETWORK                                      
         LA    R2,TRANETH                                                       
         CLI   5(R2),0             TEST NETWORK ENTERED                         
         BE    VK20                                                             
*                                                                               
         BAS   RE,VNET                                                          
*                                                                               
VK20     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTKEY,R4                                                        
         MVC   CLTKID,=XL2'0A41'   CLIENT NAME LIST RECORD                      
         MVC   CLTKAM,BAGYMD                                                    
         MVC   CLTKCLT,QCLT                                                     
         MVC   CLTKNET,NETWORK                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
*                                                                               
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    DS    0H                                                               
         CLI   ACTNUM,ACTSEL       (C)HANGE SELECT WAS ENTERED                  
         BNE   VR02                                                             
*                                                                               
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    *+12                                                             
         CLI   T216FFD+6,C'$'          TEST OFFICE LIST LOCKOUT                 
         BNE   VR02                                                             
*                                                                               
         L     R0,AIO2             SAVE RECORD                                  
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
*                                                                               
         XC    QCLT,QCLT                                                        
         LA    R2,TRACLTH                                                       
*                                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT             VALIDATE CLIENT                              
         MVI   ERROPT,0                                                         
         SPACE                                                                  
         CLI   ERROR,0                                                          
         BE    VR01                                                             
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    TRAPERR              NO                                          
         CLI   ERROR,SECLOCK       ONLY VALID ERROR SEC-LOCKOUT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BO    TRAPERR                                                          
*                                                                               
VR01     L     R0,AIO1             RESTORE RECORD                               
         L     RE,AIO2                                                          
         LA    RF,2000                                                          
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
VR02     L     R4,AIO                                                           
         USING CLTKEY,R4                                                        
         MVC   BAGYMD,CLTKAM                                                    
         MVC   QCLT,CLTKCLT                                                     
         DROP  R4                                                               
*                                                                               
VR04     TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VR10                                                             
*                                                                               
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    VR10                                                             
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
*                                                                               
VR10     XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING CLTCONEL,R6                                                      
         MVI   CLTCONEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   CLTCONLN,62         ELEMENT LENGTH                               
*                                                                               
         LA    R2,TRACONTH                                                      
         CLI   5(R2),0             TEST CONTACT NAME ENTERED                    
         BE    NOCONERR            REQUIRED                                     
*                                                                               
         GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         SPACE                                                                  
         MVC   CLTCONNM,WORK                                                    
*                                                                               
         LA    R2,TRATELH                                                       
         CLI   5(R2),0             TEST TELEPHONE NUMBER ENTERED                
         BE    NOTELERR            REQUIRED                                     
*                                                                               
         GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   CLTCONTL,WORK                                                    
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,TRAFAXH          FAX FIELD                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'14'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING CLTFAXEL,R6                                                      
         MVI   CLTFAXEL,X'14'      ELEMENT IDENTIFIER                           
         MVI   CLTFAXLN,26         ELEMENT LENGTH                               
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR35                                                             
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         MVC   CLTFAXTL,WORK                                                    
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
*MN                                                                             
VR35     DS    0H                                                               
         MVI   ELCODE,CLTEMLEQ     X'30' ELEMENT                                
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         LA    R2,TRAEMLH          FAX FIELD                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR40                                                             
         XC    ELEM,ELEM                                                        
         MVI   ELEM,CLTEMLEQ                                                    
         MVI   ELEM+1,CLTEMLNQ                                                  
         MVC   ELEM+2(L'CLTEMLAD),TRAEML                                        
         GOTO1 ADDELEM                                                          
*MN                                                                             
VR40     LA    R2,TRANAMEH         NAME FIELD                                   
         MVI   ELCODE,X'20'        NAME ELEMENT CODE                            
         GOTO1 REMELEM                                                          
         LA    R5,1                SET NAME COUNT                               
         LA    R6,ELEM                                                          
         USING CLTLSTEL,R6                                                      
         MVI   SEQNUM,0                                                         
*                                                                               
* FOR ADD, FORMAT DUMMY ELEMENT HEADER                                          
*                                                                               
VR50     CLI   5(R2),0             TEST FOR A NAME IN THIS FIELD                
         BE    VR60                                                             
*                                                                               
VR54     XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT ELEMENT SEQUENCE NUMBER            
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   CLTLSTEL,X'20'      ELEMENT IDENTIFIER                           
         MVI   CLTLSTLN,33         ELEMENT LENGTH                               
         MVC   CLTLSTSQ,SEQNUM     ELEMENT SEQUENCE NUMBER                      
*                                                                               
         CLM   R5,1,SEQNUM         NEED ANY BLANK PADDED ELEMS                  
         BE    VR56                                                             
         MVI   CLTLSTLN,4          ELEMENT LENGTH                               
         MVC   CLTLSTSQ,SEQNUM                                                  
         GOTO1 ADDELEM                                                          
         B     VR54                                                             
*                                                                               
VR56     GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   CLTLSTNM,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VR60     ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    R5,1(,R5)                                                        
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    VR50                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            GET CONTACT NAME AND ADDRESS                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CLTCONEL,R6                                                      
*                                                                               
         MVC   WORK(L'TRACONT),SPACES          CONTACT NAME                     
         MVC   WORK(L'CLTCONNM),CLTCONNM                                        
         MVC   TRACONT,WORK                                                     
         OI    TRACONTH+6,X'80'                                                 
*                                                                               
         MVC   WORK(L'TRATEL),SPACES           TELEPHONE                        
         MVC   WORK(L'CLTCONTL),CLTCONTL                                        
         MVC   TRATEL,WORK                                                      
         OI    TRATELH+6,X'80'                                                  
*                                                                               
         XC    TRAFAX,TRAFAX                                                    
         OI    TRAFAXH+6,X'80'                                                  
*                                                                               
         MVI   ELCODE,X'14'                                                     
         BAS   RE,NEXTEL           GET FAX, IF ANY                              
         BNE   DR25                                                             
*                                                                               
         USING CLTFAXEL,R6                                                      
*                                                                               
         MVC   WORK(L'TRAFAX),SPACES           TELEPHONE                        
         MVC   WORK(L'CLTFAXTL),CLTFAXTL                                        
         MVC   TRAFAX,WORK                                                      
         OI    TRAFAXH+6,X'80'                                                  
         DROP  R6                                                               
*MN                                                                             
DR25     DS    0H                                                               
         XC    TRAEML,TRAEML                                                    
         OI    TRAEMLH+6,X'80'                                                  
         USING CLTEMLEL,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,CLTEMLEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR50                                                             
         MVC   TRAEML(L'CLTEMLAD),CLTEMLAD                                      
         OI    TRAEMLH+6,X'80'                                                  
         DROP  R6                                                               
*MN                                                                             
DR50     L     R6,AIO                                                           
*                                                                               
         LA    R2,TRANAMEH         FIRST NAME FIELD                             
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            TEST ANY NAMES                               
         BNE   DR70                                                             
*                                                                               
         USING CLTLSTEL,R6                                                      
DR60     MVC   WORK(L'TRANAME),SPACES                                           
         CLI   CLTLSTLN,4                                                       
         BE    *+10                                                             
         MVC   WORK(L'CLTLSTNM),CLTLSTNM                                        
         MVC   8(L'TRANAME,R2),WORK   TRANSMIT NEW FIELD                        
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BNL   DRX                                                              
*                                                                               
         BAS   RE,NEXTEL           NEXT NAME                                    
         BE    DR60                                                             
*                                                                               
DR70     MVC   8(L'TRANAME,R2),SPACES    BLANK OUT REMAINING FIELDS             
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    DR70                                                             
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING CLTKEY,R4                                                        
*                                                                               
         MVC   TRACLT,CLTKCLT      CLIENT                                       
         OI    TRACLTH+6,X'80'                                                  
*                                                                               
         MVC   TRANET,CLTKNET      NETWORK                                      
         OI    TRANETH+6,X'80'                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST ROUTINE                                                           
*                                                                               
LR       LA    R4,KEY                                                           
         USING CLTKEY,R4                                                        
         USING CLTCONEL,R6                                                      
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVC   CLTKID,=X'0A41'     CLIENT LIST RECORD KEY                       
         MVC   CLTKAM,BAGYMD                                                    
         MVC   CLTKCLT,QCLT                                                     
         MVC   CLTKNET,NETWORK                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(3),SAVEKEY      TEST SAME TYPE/AGENCY/MEDIA                  
         BNE   LRX                                                              
*                                                                               
         OC    SAVEKEY+3(3),SAVEKEY+3   TEST CLIENT ENTERED                     
         BZ    LR40                                                             
         CLC   CLTKCLT,SAVEKEY+3        IF SO, TEST KEY MATCH                   
         BNE   LRX                                                              
*                                                                               
         OC    SAVEKEY+6(4),SAVEKEY+6   TEST NETWORK ENTERED                    
         BZ    LR40                                                             
         CLC   CLTKNET,SAVEKEY+6        IF SO, TEST KEY MATCH                   
         BNE   LR20                                                             
*                                                                               
LR40     DS   0H                                                                
         MVC   SVKEY,KEY                                                        
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),CLTKCLT CLIENT                                       
         LA    R2,FLDH                                                          
         SPACE                                                                  
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         MVI   ERROR,0                                                          
         SPACE                                                                  
         XC    DELCLT,DELCLT       CLEAR DELETED CLIENT                         
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         SPACE                                                                  
         CLI   ERROR,0                                                          
         BE    LR46                                                             
                                                                                
         CLI   ERROR,INVCLI                                                     
         BNE   *+14                                                             
         MVC   DELCLT,=C'???'                                                   
         B     LR46                                                             
                                                                                
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    LR44                                                             
*                                                                               
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    LR46                OFFICES MATCH, OK TO LIST                    
*                                                                               
LR44     MVI   CLTKCLT+3,X'FF'     FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
         B     LR30                                                             
         SPACE                                                                  
LR46     DS   0H                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'        CONTACT ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'00'                                                            
*                                                                               
         CLI   MODE,LISTRECS       ONLINE LIST                                  
         BE    LRL                                                              
         CLI   MODE,PRINTREP       OFFLINE LIST                                 
         BE    LRR                                                              
         DC    H'0'                                                             
LRX      B     EXIT                                                             
         EJECT                                                                  
* ONLINE LIST *                                                                 
         SPACE                                                                  
LRL      MVC   LISTAR,SPACES       FILL IN LIST LINE                            
         MVC   LSTCLT,CLTKCLT                                                   
                                                                                
         OC    DELCLT,DELCLT       WAS CLIENT RECORD FOUND FOR THIS             
         BZ    *+10                                                             
         MVC   LSTCLT,DELCLT       NO, PRINT ???                                
                                                                                
         MVC   LSTNET,CLTKNET                                                   
         MVC   LSTCONT,CLTCONNM                                                 
         MVC   LSTTEL,CLTCONTL                                                  
*                                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
         EJECT                                                                  
* OFFLINE LIST *                                                                
         SPACE                                                                  
         USING CLTKEY,R4                                                        
LRR      MVC   P,SPACES                                                         
         MVC   PCLT,CLTKCLT                                                     
                                                                                
         OC    DELCLT,DELCLT       WAS CLIENT RECORD FOUND FOR THIS             
         BZ    *+10                                                             
         MVC   PCLT,DELCLT         NO, PRINT ???                                
                                                                                
         MVC   PNET,CLTKNET                                                     
*                                                                               
         MVC   PCONNM,CLTCONNM                                                  
         MVC   PCONNM+132(24),CLTCONTL                                          
         DROP  R4                                                               
*                                                                               
         USING CLTLSTEL,R6                                                      
         MVI   ELCODE,X'20'        NAME LIST ELEMENT                            
         BAS   RE,NEXTEL                                                        
         BNE   LRR30                                                            
*** START NEW                                                                   
LRR10    LA    R0,4                                                             
         LA    R2,PNAME1                                                        
         LA    R3,PNAME2                                                        
LRR20    CLI   CLTLSTLN,4                                                       
         BE    *+10                                                             
         MVC   0(30,R2),CLTLSTNM                                                
         LA    R2,132(R2)                                                       
         BAS   RE,NEXTEL                                                        
         BNE   LRR30                                                            
         CLI   CLTLSTLN,4                                                       
         BE    *+10                                                             
         MVC   0(30,R3),CLTLSTNM                                                
         BAS   RE,NEXTEL                                                        
         BNE   LRR30                                                            
         LA    R3,132(R3)                                                       
         BCT   R0,LRR20                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRR10                                                            
LRR30    CLC   P,SPACES                                                         
         BE    LRR40                                                            
         OC    P,P                                                              
         BZ    LRR40                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R6                                                               
*MN                                                                             
LRR40    DS    0H                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'30'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         USING CLTEMLEL,R6                                                      
         BNE   LRR41               MUST BE THERE                                
         XC    P,P                                                              
         MVC   PCLT(8),=C'EMAIL : '                                             
         MVC   PCLT+8(L'CLTEMLAD),CLTEMLAD                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R6                                                               
*MN                                                                             
LRR41    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         DROP  R6                                                               
*** END NEW                                                                     
*         USING CLTLSTEL,R6                                                     
*         LA    R2,PNAME1                                                       
*         LA    R3,PNAME2+(3*132)                                               
*         LA    R4,PNAME2                                                       
*         LA    R5,PNAME1+132                                                   
*LRR10    CLI   CLTLSTLN,4                                                      
*         BE    *+10                                                            
*         MVC   0(30,R2),CLTLSTNM                                               
*                                                                               
*         LA    R2,33(R2)                                                       
*         CR    R2,R4                                                           
*         BNH   LRR12                                                           
*         LR    R2,R5                                                           
*         LA    R4,132(,R4)                                                     
*         LA    R5,132(,R5)                                                     
*         B     LRR14                                                           
*LRR12    CR    R2,R3                                                           
*         BNH   LRR14                                                           
*         LA    R2,PNAME1                                                       
*         LA    R3,PNAME2+(3*132)                                               
*         LA    R4,PNAME2                                                       
*         LA    R5,PNAME1+132                                                   
*         GOTO1 SPOOL,DMCB,(R8)                                                 
*LRR14    BAS   RE,NEXTEL                                                       
*         BE    LRR10                                                           
*         CLC   P,SPACES                                                        
*         BE    LRR16                                                           
*         OC    P,P                                                             
*         BZ    LRR16                                                           
*         GOTO1 SPOOL,DMCB,(R8)                                                 
*LRR16    GOTO1 SPOOL,DMCB,(R8)                                                 
*         B     LR20                                                            
*LRR20    GOTO1 SPOOL,DMCB,(R8)                                                 
*         GOTO1 (RF),(R1),(R8)                                                  
*         B     LR20                                                            
*         DROP  R4,R6                                                           
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
VNET     NTR1                                                                   
         USING STARECD,R4          LOOK UP NETWORK IN STATION RECORD            
*                                                                               
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIED          
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVI   STAKTYPE,C'S'       STATION RECORD TYPE                          
         MVI   STAKMED,C'N'        MEDIA NETWORK                                
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     R4,AIO                                                           
         CLC   0(9,R4),KEY         TEST NETWORK IS ON FILE                      
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,BNET             SAVE NETWORK MARKET NUMBER                   
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
NETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERRMS),NETERRMS                                     
         B     ERREXIT                                                          
*                                                                               
NOCONERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCONMS),NOCONMS                                       
         B     ERREXIT                                                          
*                                                                               
NOTELERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTELMS),NOTELMS                                       
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
NETERRMS DC    C'* ERROR * NETWORK NOT FOUND *'                                 
NOCONMS  DC    C'* ERROR * CONTACT REQUIRED *'                                  
NOTELMS  DC    C'* ERROR * TELEPHONE REQUIRED *'                                
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,33,C'CLIENT DISTRIBUTION LIST'                                
         SSPEC H2,33,C'------------------------'                                
         SSPEC H1,65,AGYNAME                                                    
         SSPEC H2,65,AGYADD                                                     
         SSPEC H4,78,RUN                                                        
         SSPEC H4,65,REPORT                                                     
         SSPEC H5,65,REQUESTOR                                                  
         SSPEC H5,95,PAGE                                                       
         SSPEC H8,3,C'CLIENT'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,10,C'NETWORK'                                                 
         SSPEC H9,10,C'-------'                                                 
         SSPEC H8,20,C'CONTACT NAME/TELEPHONE'                                  
         SSPEC H9,20,C'----------------------'                                  
         SSPEC H8,53,C'NAMES'                                                   
         SSPEC H9,53,C'-----'                                                   
         SSPEC H8,86,C'NAMES'                                                   
         SSPEC H9,86,C'-----'                                                   
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRNCLT                                                       
         PRINT OFF                                                              
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB9D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
BNET     DS    H                                                                
NETWORK  DS    CL4                                                              
SEQNUM   DS    XL1                                                              
NEWFIELD DS    CL1                                                              
SAVEKEY  DS    XL13                                                             
DELCLT   DS    XL3                 CLIENT RECORD DELETED BY MEDIA               
         SPACE 5                                                                
* OFFLINE REPORT LINE                                                           
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PCLT     DS    CL3                                                              
         DS    CL4                                                              
PNET     DS    CL4                                                              
         DS    CL5                                                              
PCONNM   DS    CL30                                                             
         DS    CL3                                                              
PNAME1   DS    CL30                                                             
         DS    CL3                                                              
PNAME2   DS    CL30                                                             
         SPACE 3                                                                
* ONLINE LIST LINE                                                              
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCLT   DS    CL3                                                              
         DS    CL3                                                              
LSTNET   DS    CL4                                                              
         DS    CL3                                                              
LSTCONT  DS    CL30                                                             
         DS    CL4                                                              
LSTTEL   DS    CL24                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPTRA19   06/05/08'                                      
         END                                                                    
