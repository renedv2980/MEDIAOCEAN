*          DATA SET SPTRA10    AT LEVEL 057 AS OF 02/15/07                      
*PHASE T21610B                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T21610 - STATION LABEL LIST MAINTENANCE AND LIST            *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL MAINTAIN RECORDS CONTAINING LISTS OF   *         
*            STATIONS FOR WHICH STATION LABELS ARE TO GENERATED.      *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: SEE SCREENS SPTRAA0 (T216A0)                               *         
*                      SPTRAB0 (T216B0)                               *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: UPDATED STATION LABEL STATION LIST RECORDS                *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
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
*  LEV 47    JAN08/87 ADDED OFFICE PROFILE                            *         
*  LEV 48    JUN17/92 ADDED AGENCY CODE TO REC                        *         
*  LEV 49    DEC02/92 CHANGED CODE FOR MSUNPK                         *         
*  LEV 50    MAR22/93 CABLE HEAD CODE                                 *         
*  LEV 51    MAY06/93 ADD TRAFFIC SYSTEM                              *         
*  LEV 52    JUL21/94 CHANGE TO FILENAME                              *         
*  LEV 53 BGRI MAR28/01 PUT IN TRAFFIC OFFICE                         *         
*  LEV 55 SMUR JUL15/02 CLIENT STRING SECURITY                        *         
*  LEV 56 SMUR JUL29/04 SOX                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21610 STATION LABEL LIST MAINTENANCE AND LIST'                 
T21610   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21610**                                                       
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
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         SPACE                                                                  
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
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
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
VK01     TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   SVMEDIA,QMED                                                     
*                                                                               
         XC    BCLT,BCLT           CLIENT                                       
         MVI   CLTALL,C'N'                                                      
         LA    R2,TRACLTH                                                       
*                                                                               
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK05                 NO                                          
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    *+14                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK05                                                             
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
VK05     CLI   5(R2),0                                                          
         BE    VK20                                                             
*                                                                               
         CLC   =C'ALL',TRACLT      TEST CLIENT 'ALL'                            
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      'ALL' VALID ONLY FOR LIST ACTION             
         BNE   BADALL                                                           
         MVI   CLTALL,C'Y'                                                      
         B     VK20                                                             
*                                                                               
VK10     GOTO1 VALICLT                                                          
*                                                                               
VK20     MVI   BPRD,0              PRODUCT                                      
         MVI   PRDALL,C'N'                                                      
         LA    R2,TRAPRDH                                                       
         CLI   5(R2),0                                                          
         BE    VK40                                                             
*                                                                               
         CLC   =C'ALL',TRAPRD      TEST PRODUCT 'ALL'                           
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST      'ALL' VALID ONLY FOR LIST ACTION             
         BNE   BADALL                                                           
         MVI   PRDALL,C'Y'                                                      
         B     VK40                                                             
*                                                                               
VK30     OC    BCLT,BCLT           PRODUCT REQUIRES CLIENT                      
         BZ    NOCLTERR                                                         
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   BPRD,WORK+3                                                      
*                                                                               
VK40     XC    DESCRIP,DESCRIP     DESCRIPTION                                  
         LA    R2,TRADESCH                                                      
         CLI   5(R2),0             TEST DESCRIPTION ENTERED                     
         BNE   *+12                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR             OTHERWISE REQUIRED                           
*                                                                               
         MVC   DESCRIP,TRADESC                                                  
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING LSTRECD,R4                                                       
         MVC   LSTKID,=XL2'0A2F'   STATION LABEL LIST RECORD                    
         MVC   LSTKAM,BAGYMD                                                    
         MVC   LSTKCLT,BCLT                                                     
         MVC   LSTKPRD,BPRD                                                     
         MVC   LSTKDESC,DESCRIP                                                 
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    DS    0H                                                               
         L     R4,AIO                                                           
         MVC   20(2,R4),AGENCY                                                  
         USING LSTKEY,R4                                                        
         MVC   BAGYMD,LSTKAM                                                    
         MVC   BCLT,LSTKCLT                                                     
         MVC   BPRD,LSTKPRD                                                     
         MVC   DESCRIP,LSTKDESC                                                 
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'10'        STATION ELEMENT CODE                         
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
*                                                                               
         MVC   SVKEY(13),KEY       SAVE RECORD (SO CALL TO VALISTA              
         L     R2,AIO3              DOESN'T BLOW IT AWAY)                       
         LA    R3,2000                                                          
         L     R6,AIO1                                                          
         LA    R7,2000                                                          
         MVCL  R2,R6                                                            
*                                                                               
         LA    R3,STATX            CLEAR LIST OF STATION CODES                  
         LA    R7,STATIONS                                                      
         XC    0(3,R7),0(R7)                                                    
         LA    R7,3(R7)                                                         
         CR    R3,R7                                                            
         BL    *-12                                                             
*                                                                               
         LA    R2,TRASTAH          STATION FIELD                                
         LA    R7,STATIONS                                                      
         LA    R6,ELEM                                                          
         USING LSTDTAEL,R6                                                      
*                                                                               
VR10     XC    BSTA,BSTA           CLEAR STATION CODE                           
         CLI   5(R2),0             TEST FOR A STATION IN THIS FIELD             
         BE    VR20                                                             
         TM    4(R2),X'20'         TEST FIELD VALIDATED PREVIOUSLY              
         BZ    VR15                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),8(R2)                                                    
         CLI   12(R2),C'/'         IS THIS LOCAL CABLE                          
         BNE   VR12                                                             
         MVC   WORK+4(4),12(R2)                                                 
         B     VR14                                                             
         SPACE                                                                  
VR12     CLI   12(R2),C'-'                                                      
         BE    *+18                                                             
         MVI   WORK+3,C' '                                                      
         MVC   WORK+4(1),12(R2)                                                 
         B     *+10                                                             
         MVC   WORK+4(1),13(R2)                                                 
VR14     GOTO1 MSPACK,DMCB,ZEROES,WORK,BMKTSTA                                  
         B     VR17                DON'T BOTHER VALIDATING                      
*                                                                               
VR15     GOTO1 VALISTA                                                          
*                                                                               
VR17     MVC   AIO,AIO3                                                         
         XC    ELEM,ELEM                                                        
         MVI   LSTDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   LSTDTALN,5          ELEMENT LENGTH                               
         MVC   LSTSTA,BSTA                                                      
         GOTO1 ADDELEM                                                          
*                                                                               
VR20     MVC   0(3,R7),BSTA        SAVE STATION CODE                            
         LA    R7,3(R7)                                                         
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    VR10                                                             
*                                                                               
         MVC   KEY(13),SVKEY       RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   ACTNUM,ACTADD       TEST ACTION ADD                              
         BE    VR30                                                             
*                                                                               
         GOTO1 HIGH                RESTORE DATAMGR KEY POINTER                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC              RESTORE DATAMGR RECORD POINTER               
*                                                                               
VR30     L     R2,AIO1             UPDATED VERSION OF RECORD                    
         LA    R3,2000                                                          
         L     R6,AIO3                                                          
         LA    R7,2000                                                          
         MVCL  R2,R6                                                            
*                                                                               
         L     R6,AIO              CHECK ELEMENTS IN NEW RECORD                 
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   NOSTAERR            MUST BE AT LEAST ONE STATION                 
*                                                                               
VR40     MVC   OLDSTA,LSTSTA       MAKE THIS THE OLD STATION                    
         BAS   RE,NEXTEL           NEXT STATION                                 
         BNE   VRX                 NO MORE STATIONS                             
*                                                                               
         CLC   OLDSTA,LSTSTA       CHECK FOR DUPLICATE STATIONS                 
         BNE   VR40                IF SO, ERROR                                 
*                                                                               
         LA    R2,TRASTAH          FIRST STATION FIELD                          
         LA    R7,STATIONS         SAVED STATION CODES                          
         MVI   MYFLAG,0                                                         
*                                                                               
VR50     CLC   OLDSTA,0(R7)        LOOK FOR THE OFFENDING STATION               
         BNE   VR60                THIS ISN'T THE ONE                           
*                                                                               
         CLI   MYFLAG,0            IS THIS THE FIRST DUPLICATE?                 
         BNE   DUPSTAER            THE SECOND -- PUT CURSOR THERE               
         MVI   MYFLAG,X'FF'        WE HAVE FOUND THE FIRST                      
*                                                                               
VR60     LA    R7,3(R7)            NEXT STATION CODE                            
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    VR50                                                             
         DC    H'00'               SOMETHING IS TERRIBLY WRONG                  
*                                                                               
VRX      B     DR                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         LA    R2,TRASTAH          FIRST STATION FIELD                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            TEST ANY STATIONS                            
         BNE   DRX                                                              
*                                                                               
DR10     XC    TMKT,TMKT                                                        
         MVC   TSTA,LSTSTA                                                      
         XC    STANET,STANET                                                    
         GOTO1 MSUNPK,DMCB,(X'80',TMKTSTA),QMKT,DUB                             
         SPACE                                                                  
         CLC   DUB+5(3),SPACES     CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
         SPACE                                                                  
         MVC   QSTA,DUB                                                         
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         MVC   STAPRNT,SPACES      FILL IN 6-CHARACTER STATION CODE             
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4      MOVE SUB-MEDIA                               
         SPACE                                                                  
         XC    8(L'TRASTA,R2),8(R2)   CLEAR ENTIRE FIELD                        
         MVC   8(6,R2),STAPRNT        TRANSMIT NEW FIELD                        
         SPACE                                                                  
         OC    STANET,STANET          CABLE HEAD                                
         BZ    *+10                                                             
         MVC   8(L'STANET,R2),STANET                                            
         SPACE                                                                  
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         FIELD IS VALID                               
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BNL   DRX                                                              
*                                                                               
         BAS   RE,NEXTEL           NEXT STATION                                 
         BE    DR10                                                             
*                                                                               
DR20     MVC   8(L'TRASTA,R2),SPACES    BLANK OUT REMAINING FIELDS              
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    DR20                                                             
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING LSTKEY,R4                                                        
*                                                                               
         MVC   TRAMED,SVMEDIA      MEDIA                                        
         OI    TRAMEDH+6,X'80'                                                  
*                                                                               
         OC    LSTKCLT,LSTKCLT     CLIENT                                       
         BNZ   *+14                                                             
         MVC   TRACLT,=C'   '                                                   
         B     DK10                                                             
         GOTO1 CLUNPK,DMCB,LSTKCLT,TRACLT                                       
DK10     OI    TRACLTH+6,X'80'                                                  
*                                                                               
         CLI   LSTKPRD,0           PRODUCT                                      
         BNE   *+14                                                             
         MVC   TRAPRD,=C'   '                                                   
         B     DK30                                                             
*                                                                               
         L     R3,ASVCLIST         PRODUCT LIST                                 
DK20     CLC   LSTKPRD,3(R3)       TEST PRODUCT CODE MATCH                      
         BNE   *+14                                                             
         MVC   TRAPRD,0(R3)                                                     
         B     DK30                                                             
*                                                                               
         LA    R3,4(R3)            NEXT PRODUCT                                 
         CLI   0(R3),C' '                                                       
         BNL   DK20                                                             
         DC    H'00'               PRODUCT CODE NOT FOUND                       
*                                                                               
DK30     OI    TRAPRDH+6,X'80'                                                  
*                                                                               
         MVC   TRADESC,LSTKDESC                                                 
         OI    TRADESCH+6,X'80'    DESCRIPTION                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST                                                                   
*                                                                               
LR       LA    R4,KEY                                                           
         USING LSTKEY,R4                                                        
         USING LSTDTAEL,R6                                                      
         XC    OLDCLT,OLDCLT                                                    
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         MVC   LSTKID,=X'0A2F'     PRODUCT LIST RECORD KEY                      
         MVC   LSTKAM,BAGYMD                                                    
         MVC   LSTKCLT,BCLT                                                     
         MVC   LSTKPRD,BPRD                                                     
         MVC   LSTKDESC,DESCRIP                                                 
         MVC   SAVEKEY,KEY                                                      
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
         BNZ   LR33                                                             
         CLI   CLTALL,C'Y'                                                      
         BE    LR35                                                             
*                                                                               
LR33     CLC   LSTKCLT,SAVEKEY+3   TEST CLIENT MATCH                            
         BNE   LRX                                                              
*                                                                               
LR35     CLI   SAVEKEY+5,0         TEST PRODUCT ENTERED                         
         BNE   LR37                                                             
         CLI   PRDALL,C'Y'                                                      
         BE    LR40                                                             
*                                                                               
LR37     CLC   LSTKPRD,SAVEKEY+5   IF SO, TEST KEY MATCH                        
         BNE   LR20                                                             
*                                                                               
LR40     OC    SAVEKEY+6(7),SAVEKEY+6      TEST DESCRIPTION ENTERED             
         BZ    *+14                                                             
         CLC   LSTKDESC,SAVEKEY+6  TEST DESCRIPTION MATCH                       
         BNE   LR20                                                             
*                                                                               
         MVC   LISTAR,SPACES       FILL IN LIST LINE                            
*                                                                               
         OC    LSTKCLT,LSTKCLT     CLIENT                                       
         BNZ   *+14                                                             
         MVC   LISTCLT,=C'   '                                                  
         B     LR60                                                             
*                                                                               
         GOTO1 CLUNPK,DMCB,LSTKCLT,LISTCLT                                      
         CLC   OLDCLT,LSTKCLT      DO WE ALREADY HAVE SVCLIST?                  
         BE    *+14                YES                                          
         MVC   OLDCLT,LSTKCLT                                                   
         BAS   RE,FCLT             GET SVCLIST                                  
         BNE   LR20                                                             
*                                                                               
         CLI   LSTKPRD,0           PRODUCT                                      
         BNE   *+14                                                             
         MVC   LISTPRD,=C'   '                                                  
         B     LR60                                                             
*                                                                               
         L     R3,ASVCLIST         PRODUCT LIST                                 
LR50     CLC   LSTKPRD,3(R3)       TEST PRODUCT CODE MATCH                      
         BNE   *+14                                                             
         MVC   LISTPRD,0(R3)                                                    
         B     LR60                                                             
*                                                                               
         LA    R3,4(R3)            NEXT PRODUCT                                 
         CLI   0(R3),C' '                                                       
         BNL   LR50                                                             
         DC    H'00'               PRODUCT CODE NOT FOUND                       
*                                                                               
LR60     MVC   LISTDESC,LSTKDESC   DESCRIPTION                                  
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         LA    R3,LISTSTA                                                       
         LA    R5,LISTMORE                                                      
         MVI   ELCODE,X'10'        STATION ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'00'                                                            
*                                                                               
LR70     XC    TMKT,TMKT                                                        
         MVC   TSTA,LSTSTA                                                      
         XC    STANET,STANET                                                    
         GOTO1 MSUNPK,DMCB,(X'80',TMKTSTA),QMKT,DUB                             
         SPACE                                                                  
         CLC   DUB+5(3),SPACES     CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
         SPACE                                                                  
         MVC   QSTA,DUB                                                         
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         MVC   STAPRNT,SPACES      FILL IN 6-CHARACTER STATION CODE             
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4      MOVE SUB-MEDIA                               
         MVC   0(6,R3),STAPRNT     PUT STATION NAME IN LIST LINE                
         SPACE                                                                  
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   0(L'STANET,R3),STANET                                            
         SPACE                                                                  
         LA    R3,9(R3)            NEXT POSITION                                
*                                                                               
         BAS   RE,NEXTEL           LOOK FOR ANOTHER STATION                     
         BNE   LR80                THERE ARE NO MORE                            
         CR    R3,R5               IS THERE ROOM LEFT ON THE SCREEN             
         BL    LR70                YES                                          
         MVC   LISTMORE,=C'MORE. . .'                                           
*                                                                               
LR80     GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*                                                                               
FCLT     NTR1                                                                   
         XC    SVCKEY,SVCKEY       KEY IS 00/AGMD/CLT                           
         MVC   SVCKEY+1(1),KEY+2                                                
         MVC   SVCKEY+2(2),KEY+3                                                
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         USING LSTKEY,R4                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,LSTKCLT,QCLT                                         
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
FCLT20   DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     R1,AIO                                                           
         MVC   WORK+11(1),COFFICE-CLTHDRD(R1)                                   
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         CLI   ERROR,0             SET CC FOR RETURN                            
         B     EXIT                                                             
         SPACE                                                                  
         DROP  R4                                                               
         EJECT                                                                  
ZEROES   DC    C'0000'                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
NOCLTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCLTMS),NOCLTMS                                       
         LA    R2,TRACLTH                                                       
         GOTO1 ERREX2                                                           
NOCLTMS  DC    C'* ERROR * PRODUCT REQUIRES CLIENT *'                           
*                                                                               
NOSTAERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSTAMS),NOSTAMS                                       
         LA    R2,TRASTAH                                                       
         GOTO1 ERREX2                                                           
NOSTAMS  DC    C'* ERROR * AT LEAST ONE STATION REQUIRED *'                     
*                                                                               
DUPSTAER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DUPSTAMS),DUPSTAMS                                     
         GOTO1 ERREX2                                                           
DUPSTAMS DC    C'* ERROR * STATION ALREADY IN LIST *'                           
*                                                                               
BADALL   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADALLMS),BADALLMS                                     
         GOTO1 ERREX2                                                           
BADALLMS DC    C'* ERROR * ''ALL'' VALID ONLY FOR LIST ACTION *'                
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRLBLS                                                       
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB0D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
SAVEKEY  DS    XL13                                                             
SVMEDIA  DS    CL1                                                              
SVCKEY   DS    CL13                                                             
TMKTSTA  DS    0XL5                                                             
TMKT     DS    XL2                                                              
TSTA     DS    XL3                                                              
OLDSTA   DS    XL3                                                              
OLDCLT   DS    XL2                                                              
DESCRIP  DS    CL7                                                              
MYFLAG   DS    XL1                                                              
CLTALL   DS    CL1                                                              
PRDALL   DS    CL1                                                              
STATIONS DS    105XL3                                                           
STATX    DS    0C                                                               
         SPACE 5                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LISTCLT  DS    CL3                                                              
         DS    CL2                                                              
LISTPRD  DS    CL3                                                              
         DS    CL2                                                              
LISTDESC DS    CL7                                                              
         DS    CL2                                                              
LISTSTA  DS    CL6                                                              
         DS    CL3                                                              
         DS    CL6                                                              
         DS    CL3                                                              
         DS    CL6                                                              
         DS    CL3                                                              
         DS    CL6                                                              
         DS    CL3                                                              
         DS    CL6                                                              
         DS    CL3                                                              
LISTMORE DS    CL9                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPTRA10   02/15/07'                                      
         END                                                                    
