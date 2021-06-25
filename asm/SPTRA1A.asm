*          DATA SET SPTRA1A    AT LEVEL 027 AS OF 06/05/08                      
*PHASE T2161AA                                                                  
*                                                                               
*  TITLE: T2161A - NETWORK TRAFFIC PRODUCT LIST RECORD MAINTENANCE'   *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL MAINTAIN RECORDS CONTAINING LISTS OF   *         
*            PEOPLE TO BE NOTIFIED WHENENVER INSTRUCTIONS ARE RUN FOR *         
*            A GIVEN PRODUCT.                                         *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATA MANAGER                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRAAA (T216AA)                                *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: UPDATED PRODUCT LIST RECORDS                              *         
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
*                                                                     *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*                                                                     *         
*  LEV 18    MAR10/99 ADD CONTACT NAME AND NUMBER FIELDS              *         
*  LEV 19    AUG16/99 POINT R6 TO THE RECORD BEFORE REMELEM           *         
*  LEV 20    MAR27/01 PUT IN TRAFFIC OFFICE                           *         
*  LEV 21    SEP24/01 ADD NETWORK OR M= TO KEY                        *         
*  LEV 24 SMUR JUN27/02 CLIENT STRING SECURITY                        *         
*  LEV 25 SMUR OCT21/03 BRAND LEVEL SECURITY                          *         
*  LEV 26 SMUR JUL26/04 SOX                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2161A PRODUCT LIST RECORD MAINTENANCE'                         
T2161A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2161A*                                                       
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
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
         SPACE                                                                  
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
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
         XC    BCLT,BCLT           CLIENT                                       
         LA    R2,TRACLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK05                                                             
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK03                 NO                                          
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
         SPACE                                                                  
VK03     CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK10                                                             
*                                                                               
VK05     GOTO1 VALICLT                                                          
*                                                                               
VK10     XC    QPRD,QPRD           PRODUCT                                      
         LA    R2,TRAPRDH                                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
         B     VK40                                                             
*                                                                               
VK20     OC    BCLT,BCLT           PRODUCT REQUIRES CLIENT                      
         BNZ   VK30                                                             
         LA    R2,TRACLTH                                                       
         B     NOCLTERR                                                         
*                                                                               
VK30     CLC   =C'POL',TRAPRD      TEST POOL PRODUCT                            
         BE    PRODERR             POOL NOT PERMITTED IN NETWORK                
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK           ALPHA PRODUCT CODE                           
*                                                                               
VK40     LA    R2,TRANETH          NETWORK                                      
         XC    NETWORK,NETWORK                                                  
         CLI   5(R2),0             ANY NETWORK ENTERED                          
         BE    VK80                                                             
*                                                                               
         BAS   RE,VNT              VALIDATE NETWORK                             
*                                                                               
VK80     XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PRDKEY,R4                                                        
         MVC   PRDKID,=XL2'0A42'   PRODUCT NAME LIST RECORD                     
         MVC   PRDKAM,BAGYMD                                                    
         MVC   PRDKCLT,BCLT                                                     
         MVC   PRDKPRD,QPRD                                                     
         MVC   PRDKNET,NETWORK                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VR01     DS    0H                                                               
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         USING PRDKEY,R4                                                        
         MVC   BAGYMD,PRDKAM                                                    
         CLC   BCLT,PRDKCLT                                                     
         BE    VR02                YES                                          
         SPACE                                                                  
         BAS   RE,FCLT            GO GET CLIENT SVCLIST                         
         BE    *+6                 SHOULD BE OKAY                               
         DC    H'0'                                                             
VR02     DS    0H                                                               
         MVC   QPRD,PRDKPRD                                                     
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PRDCONEL,R6                                                      
         MVI   PRDCONEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   PRDCONLN,86         ELEMENT LENGTH                               
*                                                                               
         LA    R2,TRACONTH                                                      
         CLI   5(R2),0             CONTACT NAME ?                               
         BE    VR05                 NO                                          
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVC   PRDCONNM,WORK                                                    
*                                                                               
         CLI   TRATELH+5,0         TELEPHONE # REQUIRED                         
         BE    PHONEERR                                                         
*                                                                               
VR05     LA    R2,TRATELH                                                       
         CLI   5(R2),0             TELEPHONE NUMBER?                            
         BE    VR10                 NO                                          
*                                                                               
         CLI   TRACONTH+5,0                                                     
         BE    CONTERR             MUST HAVE CONTACT NAME                       
*                                                                               
         GOTO1 ANY                                                              
         MVC   PRDCONTL,WORK                                                    
*                                                                               
VR10     LA    R2,TRAFAXH          FAX FIELD                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR12                                                             
*                                                                               
         CLI   TRACONTH+5,0                                                     
         BE    CONTERR             MUST HAVE CONTACT NAME                       
*                                                                               
         GOTO1 ANY                                                              
         MVC   PRDFAXTL,WORK                                                    
*                                                                               
VR12     OC    2(84,R6),2(R6)      EMPTY ELEM?                                  
         BZ    VR13                 YES, NO NEED TO ADD ELEM                    
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
*MN                                                                             
VR13     DS    0H                                                               
         MVI   ELCODE,PRDEMLEQ     X'30' ELEMENT                                
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         LA    R2,TRAEMLH          FAX FIELD                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR15                                                             
         XC    ELEM,ELEM                                                        
         MVI   ELEM,PRDEMLEQ                                                    
         MVI   ELEM+1,PRDEMLNQ                                                  
         MVC   ELEM+2(L'PRDEMLAD),TRAEML                                        
         GOTO1 ADDELEM                                                          
*MN                                                                             
         SPACE                                                                  
VR15     LA    R2,TRANAMEH         NAME FIELD                                   
         MVI   ELCODE,X'20'        NAME ELEMENT CODE                            
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         LA    R5,1                SET NAME COUNT                               
         LA    R6,ELEM                                                          
         USING PRDLSTEL,R6                                                      
         MVI   SEQNUM,0                                                         
         MVI   NMFOUND,C'N'        NO NAMES FOUND YET                           
*                                                                               
* FOR ADD, FORMAT DUMMY ELEMENT HEADER                                          
*                                                                               
VR20     CLI   5(R2),0             TEST FOR A NAME IN THIS FIELD                
         BE    VR40                                                             
*                                                                               
         MVI   NMFOUND,C'Y'        SET NAME FOUND FLAG                          
VR30     XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT ELEMENT SEQUENCE NUMBER            
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   PRDLSTEL,X'20'      ELEMENT IDENTIFIER                           
         MVI   PRDLSTLN,33         ELEMENT LENGTH                               
         MVC   PRDLSTSQ,SEQNUM     ELEMENT SEQUENCE NUMBER                      
*                                                                               
         CLM   R5,1,SEQNUM         NEED ANY BLANK PADDED ELEMS                  
         BE    VR36                                                             
         MVI   PRDLSTLN,4          ELEMENT LENGTH                               
         MVC   PRDLSTSQ,SEQNUM                                                  
         GOTO1 ADDELEM                                                          
         B     VR30                                                             
*                                                                               
VR36     GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   PRDLSTNM,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VR40     ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    R5,1(,R5)           ADD TO NAME COUNT                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    VR20                                                             
*                                                                               
         CLI   NMFOUND,C'Y'        TEST FOR AT LEAST ONE NAME                   
         BNE   NONMERR             REQUIRED                                     
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         XC    TRACONT,TRACONT                                                  
         OI    TRACONTH+6,X'80'                                                 
         XC    TRATEL,TRATEL                                                    
         OI    TRATELH+6,X'80'                                                  
         XC    TRAFAX,TRAFAX                                                    
         OI    TRAFAXH+6,X'80'                                                  
*MN                                                                             
         XC    TRAEML,TRAEML                                                    
         OI    TRAEMLH+6,X'80'                                                  
*MN                                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            GET CONTACT ELEMENT                          
         BNE   DR05                                                             
         SPACE                                                                  
         USING PRDCONEL,R6                                                      
         SPACE                                                                  
         MVC   TRACONT(L'PRDCONNM),PRDCONNM CONTACT NAME                        
         MVC   TRATEL(L'PRDCONTL),PRDCONTL TELEPHONE                            
         MVC   TRAFAX(L'PRDFAXTL),PRDFAXTL  FAX                                 
         DROP  R6                                                               
*MN                                                                             
DR05     DS    0H                                                               
         USING PRDEMLEL,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,PRDEMLEQ     X'30' ELEMENT                                
         BAS   RE,GETEL            GET EMAIL ADDRESS ELEMENT                    
         BNE   DR10                                                             
         MVC   TRAEML(L'PRDEMLAD),PRDEMLAD  EMAIL ADDRESS                       
         DROP  R6                                                               
*MN                                                                             
DR10     L     R6,AIO                                                           
         USING PRDLSTEL,R6                                                      
*                                                                               
         LA    R2,TRANAMEH         FIRST NAME FIELD                             
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            TEST ANY NAMES                               
         BNE   DRX                                                              
*                                                                               
DR30     MVC   WORK(L'TRANAME),SPACES                                           
         CLI   PRDLSTLN,4                                                       
         BE    *+10                                                             
         MVC   WORK(L'PRDLSTNM),PRDLSTNM                                        
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
         BE    DR30                                                             
*                                                                               
DR40     MVC   8(L'TRANAME,R2),SPACES    BLANK OUT REMAINING FIELDS             
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    DR40                                                             
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING PRDKEY,R4                                                        
         SPACE                                                                  
         BAS   RE,FCLT GO GET CLIENT SVCLIST                                    
         BNE   EXIT                                                             
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,PRDKCLT,TRACLT                                       
         OI    TRACLTH+6,X'80'     CLIENT                                       
         MVC   TRAPRD,PRDKPRD                                                   
         OI    TRAPRDH+6,X'80'     PRODUCT                                      
         SPACE                                                                  
         MVC   TRANET,PRDKNET      PRESET NETWORK                               
         OI    TRANETH+6,X'80'                                                  
         SPACE                                                                  
         CLI   PRDKNET,X'00'       TEST IF MEDIA                                
         BNE   EXIT                                                             
         CLI   PRDKNET+2,X'FF'                                                  
         BNE   EXIT                                                             
         MVC   TRANET(2),=C'M='                                                 
         MVC   TRANET+2(1),PRDKNET+1                                            
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST ROUTINE                                                           
*                                                                               
LR       LA    R4,KEY                                                           
         USING PRDKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         MVC   PRDKID,=X'0A42'     PRODUCT LIST RECORD KEY                      
         MVC   PRDKAM,BAGYMD                                                    
         MVC   PRDKCLT,BCLT                                                     
         MVC   PRDKPRD,QPRD                                                     
         MVC   PRDKNET,NETWORK     PRESET NETWORK                               
         CLC   NETWORK,=C'M='      TEST IF MEDIA                                
         BNE   LR05                                                             
         XC    PRDKNET,PRDKNET                                                  
         MVI   PRDKNET+2,X'FF'                                                  
         MVC   PRDKNET+1(1),NETWORK+2                                           
*                                                                               
LR05     MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
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
         OC    SAVEKEY+3(2),SAVEKEY+3      TEST CLIENT ENTERED                  
         BZ    LR50                                                             
         CLC   PRDKCLT,SAVEKEY+3           IF SO, TEST KEY MATCH                
         BNE   LRX                                                              
*                                                                               
LR50     OC    SAVEKEY+5(3),SAVEKEY+5      TEST PRODUCT ENTERED                 
         BZ    LR55                                                             
         CLC   PRDKPRD,SAVEKEY+5           IF SO, TEST KEY MATCH                
         BNE   LRX                                                              
*                                                                               
LR55     DS    0H                                                               
         OC    SAVEKEY+8(4),SAVEKEY+8      TEST NETWORK ENTERED                 
         BZ    LR60                                                             
         CLC   PRDKNET,SAVEKEY+8           IF SO, TEST KEY MATCH                
         BNE   LRX                                                              
*                                                                               
LR60     DS    0H                                                               
         CLC   BCLT,PRDKCLT        IS CLIENT SAME                               
         BNE   LR65                                                             
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    LR65                                                             
         B     LR70                                                             
         SPACE                                                                  
LR65     BAS   RE,FCLT             GO GET CLIENT SVCLIST                        
         BNE   EXIT                 ALL DONE                                    
         SPACE                                                                  
LR70     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
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
LRL      LR    R6,R4                                                            
         MVI   ELCODE,X'20'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         USING PRDLSTEL,R6                                                      
         BE    *+6                 MUST BE THERE                                
         DC    H'00'                                                            
*                                                                               
         MVC   LISTAR,SPACES       FILL IN LIST LINE                            
         GOTO1 CLUNPK,DMCB,PRDKCLT,LSTCLT                                       
         MVC   LSTPRD,PRDKPRD                                                   
*                                                                               
         MVC   LSTNET,PRDKNET      PRESET NETWORK                               
*                                                                               
         CLI   PRDKNET,X'00'       TEST IF MEDIA                                
         BNE   LRL05                                                            
         CLI   PRDKNET+2,X'FF'                                                  
         BNE   LRL05                                                            
         MVC   LSTNET(2),=C'M='                                                 
         MVC   LSTNET+2(1),PRDKNET+1                                            
*                                                                               
LRL05    CLI   PRDLSTLN,4                                                       
         BE    *+10                                                             
         MVC   LSTNAME,PRDLSTNM                                                 
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'        CONTACT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   LRL20                                                            
*                                                                               
         MVC   LSTCONT,PRDCONNM                                                 
         MVC   LSTTEL,PRDCONTL                                                  
*                                                                               
LRL20    GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
         EJECT                                                                  
* OFFLINE LIST *                                                                
         SPACE                                                                  
LRR      DS    0H                                                               
*                                                                               
         CLC   PRDKCLT,BCLT                                                     
         BNE   LRR01                                                            
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    LRR01                                                            
         B     LRR02                                                            
         SPACE                                                                  
LRR01    MVC   BCLT,PRDKCLT                                                     
         BAS   RE,FCLT                                                          
         BNE   EXIT                                                             
         SPACE                                                                  
*NOP     MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
LRR02    MVC   P,SPACES                                                         
         SPACE                                                                  
         MVC   QPRD,PRDKPRD                                                     
         BAS   RE,PPRD                                                          
         SPACE                                                                  
         MVC   PPROD,QPRD                                                       
         MVC   PPRODNM,PRDNM                                                    
*                                                                               
         OC    PRDKNET,PRDKNET     ANY NETWORK                                  
         BZ    LRR04                                                            
*                                                                               
         MVC   PPROD+132(4),=C'NET='                                            
         MVC   PPROD+132+4(4),PRDKNET  PRESET NETWORK                           
*                                                                               
         CLI   PRDKNET,X'00'       TEST IF MEDIA                                
         BNE   LRR04                                                            
         CLI   PRDKNET+2,X'FF'                                                  
         BNE   LRR04                                                            
         MVC   PPROD+132(2),=C'M='                                              
         MVC   PPROD+132+2(1),PRDKNET+1                                         
         MVC   PPROD+132+2+1(5),SPACES                                          
*                                                                               
LRR04    LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR08                                                            
*                                                                               
         USING PRDCONEL,R6                                                      
*                                                                               
         MVC   PNAME1(9),=C'CONTACT= '                                          
         MVC   PNAME1+9(L'PRDCONNM),PRDCONNM                                    
*                                                                               
         LA    R2,PNAME1+132                                                    
         MVC   0(5,R2),=C'TEL= '                                                
         CLC   PRDCONTL,SPACES                                                  
         BNH   *+14                                                             
         MVC   5(L'PRDCONTL,R2),PRDCONTL                                        
         LA    R2,PNAME2+132                                                    
*                                                                               
         MVC   0(5,R2),=C'FAX= '                                                
         CLC   PRDFAXTL,SPACES                                                  
         BNH   *+14                                                             
         MVC   5(L'PRDFAXTL,R2),PRDFAXTL                                        
         B     LRR06                                                            
*                                                                               
         XC    0(5,R2),0(R2)                                                    
*                                                                               
LRR06    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LRR08    LR    R6,R4                                                            
         MVI   ELCODE,X'20'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         USING PRDLSTEL,R6                                                      
         BE    *+6                 MUST BE THERE                                
         DC    H'00'                                                            
*                                                                               
         LA    R2,PNAME1                                                        
         LA    R3,PNAME2                                                        
LRR10    CLI   PRDLSTLN,4                                                       
         BE    *+10                                                             
         MVC   0(30,R2),PRDLSTNM                                                
*                                                                               
         LA    R2,33(R2)                                                        
         CR    R2,R3                                                            
         BNH   LRR14                                                            
         LA    R2,PNAME1                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
LRR14    BAS   RE,NEXTEL                                                        
         BE    LRR10                                                            
         CLC   P,SPACES                                                         
         BE    LRR16                                                            
         OC    P,P                                                              
         BZ    LRR16                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R6                                                               
*MN                                                                             
LRR16    DS    0H                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'30'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         USING PRDEMLEL,R6                                                      
         BNE   LRR18               MUST BE THERE                                
         XC    P,P                                                              
         MVC   PPROD(8),=C'EMAIL : '                                            
         MVC   PPROD+8(L'PRDEMLAD),PRDEMLAD                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R6                                                               
*MN                                                                             
LRR18    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* FIND CLIENT NAME *                                                            
         SPACE                                                                  
FCLT     NTR1                                                                   
         SPACE                                                                  
         USING PRDKEY,R4                                                        
         SPACE                                                                  
         CLC   PRDKCLT,BCLT        DIFFERENT CLIENT                             
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       YES, FORCE TO NEXT PAGE                      
         SPACE                                                                  
FCLT10   DS    0H                                                               
         MVC   BCLT,PRDKCLT                                                     
         MVC   SVPRDPRD,PRDKPRD    SAVE MOVE IN PRODUCT                         
         DROP  R4                                                               
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         SPACE                                                                  
         MVC   ELEM(8),TRACLTH                                                  
         MVI   ELEM+5,3                                                         
         MVC   ELEM+8(3),QCLT                                                   
         LA    R2,ELEM                                                          
         SPACE                                                                  
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0            CLEAR                                        
         SPACE                                                                  
         CLI   ERROR,0             ANY ERRORS                                   
         BNE   FCLT15                                                           
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT30               NO                                          
         OC    SVPRDPRD,SVPRDPRD   ANY PRODUCT                                  
         BNZ   FCLT21                                                           
         B     FCLT30                                                           
         SPACE                                                                  
FCLT15   CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT25                                                           
         SPACE                                                                  
         OC    SVPRDPRD,SVPRDPRD   ANY PRODUCT                                  
         BNZ   FCLT21                                                           
         SPACE                                                                  
         CLI   ACTNUM,ACTLIST                                                   
         BE    FCLT22                                                           
         CLI   MODE,PRINTREP                                                    
         BE    FCLT22               NO, GO GET NEXT TEXT RECORD                 
         B     FCLT23                                                           
         SPACE                                                                  
* CHECK OUT BRAND LEVEL SECURITY                                                
         SPACE                                                                  
FCLT21   DS    0H                                                               
         L     RE,ASVNCLST                                                      
         LA    RF,NCLSTSIZ                                                      
         SPACE                                                                  
         CLC   SVPRDPRD,0(RE)      THIS PROD CODE?                              
         BE    FCLT30                                                           
         LA    RE,4(RE)            NEXT ENTRY                                   
         BCT   RF,*-14                                                          
         MVI   ERROR,SECLOCK       PRD IS NOT IN PRD LIST (SEC LOCK)            
         SPACE                                                                  
FCLT22   CLI   ACTNUM,ACTLIST                                                   
         BE    *+12                                                             
         CLI   MODE,PRINTREP                                                    
         BNE   FCLT23                                                           
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         SPACE                                                                  
         GOTO1 HIGH                DUMMY READ HI                                
         SPACE                                                                  
FCLT22C  GOTO1 SEQ                 FOR READ SEQ                                 
         OC    SVPRDPRD,SVPRDPRD   ANY PRODUCT                                  
         BZ    FCLT22F                                                          
         SPACE                                                                  
         CLC   SVPRDPRD,KEY+5      SAME PRD                                     
         BE    FCLT22C              YES GET NEXT RECORD                         
         B     FCLT27                                                           
         SPACE                                                                  
FCLT22F  CLC   KEY(8),KEYSAVE      SAME A/M/CLT AND STILL NO PROD               
         BE    FCLT22C             YES, GET NEXT RECORD                         
         B     FCLT27                                                           
         SPACE                                                                  
FCLT23   CLI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         BE    TRAPERR                                                          
         DC    H'0'                SOME OTHER ERROR???                          
         SPACE                                                                  
FCLT25   OC    SAVEKEY+3(2),SAVEKEY+3      TEST CLIENT ENTERED                  
         BNZ   EXIT                                                             
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
         SPACE                                                                  
FCLT27   CLC   KEY(3),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   EXIT                                                             
         SPACE                                                                  
* DO GETREC & THEN SAVE REC                                                     
         SPACE                                                                  
         GOTO1 GETREC                                                           
         B     FCLT10                                                           
         SPACE                                                                  
* MOVE PRODUCT REC BACK TO AIO1                                                 
         SPACE                                                                  
FCLT30   DS    0H                                                               
         SPACE                                                                  
         L     R0,AIO1                                                          
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    FCLT40              BYPASS                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
FCLT40   MVC   AIO,AIO1                                                         
         CR    RB,RB               SET COND CODE OKAY                           
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE NETWORK                                                              
         SPACE                                                                  
         DS    0H                                                               
VNT      NTR1                                                                   
*                                                                               
         CLC   =C'M=',8(R2)        WAS MEDIA ENTERED                            
         BNE   VNT10                                                            
         CLI   5(R2),3                                                          
         BNE   BADMED                                                           
*                                                                               
         CLI   10(R2),C'N'                                                      
         BE    *+28                                                             
         CLI   10(R2),C'C'                                                      
         BE    *+20                                                             
         CLI   10(R2),C'S'                                                      
         BE    *+12                                                             
         CLI   10(R2),C'O'                                                      
         BNE   BADMED                                                           
*                                                                               
* PUT MEDIA IN NETWORK PART OF KEY AS  X'00MMFF00'                              
*                                                                               
         MVC   NETWORK+1(1),10(R2)                                              
         MVI   NETWORK+2,X'FF'                                                  
         B     VNTX                                                             
*                                                                               
VNT10    GOTO1 ANY                                                              
         MVC   NETWORK,WORK                                                     
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    VNTX                                                             
         MVC   GERROR,=Y(NONET)                                                 
         B     VNTEREX2                                                         
*                                                                               
VNTX     XIT1                                                                   
*                                                                               
BADMED   MVC   GERROR,=Y(BADMEDIA)                                              
         SPACE                                                                  
VNTEREX2 GOTO1 VTRAERR                                                          
         EJECT                                                                  
* FIND 3 CHAR PROD AND GET PRODUCT NAME *                                       
         SPACE                                                                  
PPRD     NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING PRDHDRD,R1                                                       
         MVC   PRDNM,PNAME                                                      
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H4+10(3),QCLT                                                    
         MVC   H4+15(20),CLTNM                                                  
         B     EXIT                                                             
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
NOCLTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCLTMS),NOCLTMS                                       
         GOTO1 ERREX2                                                           
NOCLTMS  DC    C'* ERROR * PRODUCT REQUIRES CLIENT *'                           
*                                                                               
PHONEERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PHONEMS),PHONEMS                                       
         LA    R2,TRATELH                                                       
         GOTO1 ERREX2                                                           
PHONEMS  DC    C'* ERROR * TELEPHONE NUMBER REQUIRED *'                         
*                                                                               
CONTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CONTMS),CONTMS                                         
         LA    R2,TRACONTH                                                      
         GOTO1 ERREX2                                                           
CONTMS   DC    C'* ERROR * CONTACT NAME REQUIRED *'                             
*                                                                               
NONMERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NONMMS),NONMMS                                         
         LA    R2,TRANAMEH                                                      
         GOTO1 ERREX2                                                           
NONMMS   DC    C'* ERROR * AT LEAST ONE NAME REQUIRED *'                        
*                                                                               
PRODERR  MVI   ERROR,INVPROD                                                    
         GOTO1 ERREX                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,33,C'PRODUCT DISTRIBUTION LIST'                               
         SSPEC H2,33,C'-------------------------'                               
         SSPEC H1,65,AGYNAME                                                    
         SSPEC H2,65,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,78,RUN                                                        
         SSPEC H4,65,REPORT                                                     
         SSPEC H5,65,REQUESTOR                                                  
         SSPEC H5,95,PAGE                                                       
         SSPEC H8,3,C'PRODUCT'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,46,C'NAMES'                                                   
         SSPEC H9,46,C'-----'                                                   
         SSPEC H8,79,C'NAMES'                                                   
         SSPEC H9,79,C'-----'                                                   
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRNPRD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRABAD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
         SPACE                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
NMFOUND  DS    CL1                                                              
SEQNUM   DS    XL1                                                              
SAVEKEY  DS    XL13                                                             
NETWORK  DS    CL4                                                              
SVPRDPRD DS    CL3                 SAVE PRODUCT                                 
         SPACE 5                                                                
* OFFLINE REPORT LINE                                                           
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PPROD    DS    CL3                                                              
         DS    CL6                                                              
PPRODNM  DS    CL20                                                             
         DS    CL13                                                             
PNAME1   DS    CL30                                                             
         DS    CL3                                                              
PNAME2   DS    CL30                                                             
         SPACE 3                                                                
* ONLINE LIST LINE                                                              
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCLT   DS    CL3                                                              
         DS    CL2                                                              
LSTPRD   DS    CL3                                                              
         DS    CL2                                                              
LSTNET   DS    CL4                                                              
         DS    CL2                                                              
LSTNAME  DS    CL18                                                             
         DS    CL3                                                              
LSTCONT  DS    CL20                                                             
         DS    CL2                                                              
LSTTEL   DS    CL12                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SPTRA1A   06/05/08'                                      
         END                                                                    
