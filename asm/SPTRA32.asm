*          DATA SET SPTRA32    AT LEVEL 031 AS OF 03/06/07                      
*PHASE T21632B                                                                  
         TITLE 'T21632 - CANADIAN COMMERCIAL PROFILE DISPLAY, CHANGE, AC        
               DD, DISPLAY, LIST'                                               
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - READ CML SEQ RECORD FOR ADDS (IN PAR RTN)                  
*                    READ IN PRDHDR FOR PROD NAMES IN OFFLINE LIST              
*             AIO3 - REC READ IN FOR CHANGE COMPARE                             
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
* LEV 20 - CHANGE ASC TO 00-00-00 FROM DATE                                     
* LEV 22 - JUL15/86 CHANGE VCML FOR NEW PROFILE OPTION & RESET VAL BITS         
* LEV 25 - NOV03/86 ALLOW ALPHA/NUM AND * IN CML CODES                          
* LEV 26   JAN08/87 ADD OFFICE PROFILE                                          
* LEV 27   MAY10/89 ADD CK FOR CANADIAN AGENCIES ONLY                           
* LEV 28   FEB07/90 CHANGE SPOTCAN FROM Y TO C                                  
* LEV 29 SMUR JUN01/01 USE TRAFFIC OFFICE                                       
*                                                                               
***********************************************************************         
         EJECT                                                                  
T21632   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CCML**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR32RR                                                      
         CLI   SPOTCAN,C'C'                                                     
         BNE   CANADER                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECADD         PRIOR TO ADD RECORD                          
         BE    PAR                                                              
         CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BE    AAR                                                              
         CLI   MODE,RECPUT         BEFORE REWRITING, CK IF REQ NEEDED           
         BE    PUT                                                              
         CLI   MODE,RECDEL         BEFORE DELETE RECORD (INVALID CML)           
         BE    DELREC                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE 3                                                                
VK       CLI   ACTNUM,ACTDEL       DELETE IS INVALID                            
         BE    DELREC                                                           
         LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         SPACE                                                                  
VK10     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0                                                          
         BNE   VK12                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK20                                                             
         B     MISSERR                                                          
VK12     GOTO1 VALICLT                                                          
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     R1,AIO                                                           
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
* READ T1 PROFILE *                                                             
         SPACE                                                                  
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),WORK,SVT1PROF,DATAMGR                                  
*                                                                               
         SPACE                                                                  
VK20     LA    R2,TRACMLH          COMMERCIAL IDENTIFICATION                    
         XC    HOLDCML,HOLDCML                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK22                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK24                ACCEPT WHATEVER ENTERED                      
         B     MISSERR                                                          
VK22     BAS   RE,VCML             VALIDATE COMMERCIAL                          
VK24     MVC   HOLDCML,TRACML                                                   
         SPACE                                                                  
VK30     LA    R2,TRAFLTRH         VALIDATE ANY FILTERS                         
         BAS   RE,VFTR                                                          
         EJECT                                                                  
* BUILD KEY                                                                     
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,HOLDCML                                                  
         CLI   ACTNUM,ACTLIST      IF LIST                                      
         BNE   EXIT                NOT, SO EXIT                                 
         SPACE                                                                  
* CHECK FOR ANY MISSING FIELDS (MUST ALL BE ENTERED, LEFT TO RIGHT)             
         SPACE                                                                  
         OC    HOLDCML,HOLDCML     IF ANY ENTRY                                 
         BZ    VK40                NO                                           
         OC    BCLT,BCLT           MUST HAVE ENTERED CLT                        
         BNZ   VK40                DID                                          
         LA    R2,TRACLTH          ERROR                                        
         B     MISSERR             MISSING CLIENT ENTRY                         
         SPACE                                                                  
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
         SPACE                                                                  
VK40     LA    R0,12               MAX KEY COMPARE (-1)                         
         LA    R1,KEY+12           START AT END OF CMLKCML                      
VK42     CLI   0(R1),0             NONZERO IS VALID COMPARAND                   
         BNE   VK44                FOUND END OF COMPARE KEY                     
         BCTR  R0,0                DECREMENT LENGTH                             
         BCT   R1,VK42                                                          
VK44     STC   R0,COMPKEYL         SAVE COMPARE LENGTH                          
         MVC   COMPKEY,KEY                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE 2                                                                
VR       L     R4,AIO              SAVE KEY CONTENTS                            
         USING CMLKEY,R4                                                        
         MVC   BAGYMD,CMLKAM                                                    
         CLC   BCLT,CMLKCLT        IS CLIENT SAME                               
         BE    VR10                YES                                          
         MVC   BCLT,CMLKCLT                                                     
         BAS   RE,FCLT             GO GET CLIENT CLIST                          
VR10     MVC   HOLDCML,CMLKCML                                                  
         DROP  R4                                                               
         SPACE                                                                  
         LA    R2,TRAPLSTH         PRODUCT LIST                                 
         MVI   ELCODE,X'20'        PROD LIST ELEM CODE                          
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BE    VR20                YES, ONLY VAL TITLE (PROD HOUSE)             
         GOTO1 =A(VPRDL),RR=SPTR32RR VALIDATE PROD LIST & BUILD ELEM            
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
VR20     MVI   ELCODE,X'10'        ADDRESS PART OF ELEMENT                      
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING CMLDTAEL,R6                                                      
         MVI   CMLDTAEL,X'10'      FORMAT ELEMENT IDENTIFIER                    
         MVI   CMLDTALN,CMLDTAX-CMLDTAEL   AND ELEMENT LENGTH FOR ADD           
         SPACE                                                                  
         LA    R2,TRATLEH          COMMERCIAL TITLE                             
         GOTO1 ANY                                                              
         CLC   DELETE,WORK         SOFT DELETE THIS CML                         
         BNE   VR22                NO                                           
         OI    CMLSTAT,X'80'       SOFT DELETE RECORD                           
         B     VR26                DO NOT OVERLAY TITLE                         
VR22     CLC   =C'RESTORE',WORK    RESTORE SOFT DELETE?                         
         BNE   VR24                NO                                           
         TM    CMLSTAT,X'80'       WAS CML SOFT DELETED                         
         BZ    RSTDELER            CAN'T RESTORE                                
         NI    CMLSTAT,X'FF'-X'80' SET OFF SOFT DELETE                          
         B     VR26                                                             
VR24     MVC   CMLTITLE,WORK                                                    
VR26     GOTO1 =A(VHSE),RR=SPTR32RR  GO VALIDATE PROD HOUSE IF NEEDED           
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BNE   VR28                YES, ONLY VAL TITLE (PROD HOUSE)             
         GOTO1 ADDELEM                                                          
         B     VR80                                                             
         SPACE                                                                  
VR28     LA    R2,TRASLNH                                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         GOTO1 VALISLN             COMMERCIAL LENGTH - REQUIRED                 
         MVC   CMLSLN,WORK                                                      
         EJECT                                                                  
         LA    R2,TRARLSEH         RELEASE DATE - REQUIRED                      
         GOTO1 DATVAL,DMCB,(0,TRARLSE),DATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,CMLRLSE)                                 
         SPACE                                                                  
         LA    R2,TRARCLH          RECALL DATE                                  
         CLC   =CL3'UFN',TRARCL    UNTIL FURTHUR NOTICE                         
         BNE   VR30                NO, PROCESS IT                               
         MVC   CMLRCL,=XL3'FFFFFF' FORCE IT                                     
         B     VR34                DONE                                         
VR30     GOTO1 DATVAL,DMCB,(0,TRARCL),DATE                                      
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,CMLRCL)                                  
         CLC   CMLRLSE,CMLRCL      CAN'T RECALL BEFORE RELEASE                  
         BH    DATERR                                                           
         SPACE                                                                  
VR34     LA    R2,TRATYPEH         TYPE                                         
         BAS   RE,VTYP                                                          
         MVC   CMLTYPE,WORK        STORE TYPE                                   
         SPACE                                                                  
         LA    R2,TRACLTNH         CLIENT COMMERCIAL NUMBER                     
         CLI   5(R2),0                                                          
         BE    VR36                                                             
         GOTO1 ANY                                                              
         MVC   CMLCLTNO,WORK                                                    
         SPACE                                                                  
VR36     LA    R2,TRASOLOH         PIGGYBACK/SOLO                               
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         CLI   8(R2),C'S'                                                       
         BE    VR38                                                             
         CLI   8(R2),C'P'                                                       
         BNE   SOLERR                                                           
VR38     MVC   CMLSOLO,8(R2)                                                    
         SPACE                                                                  
VR40     LA    R2,TRACLSH          CLASS                                        
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         CLI   5(R2),4                                                          
         BH    CLASIZER                                                         
         MVC   CMLCLASS,8(R2)                                                   
         SPACE                                                                  
VR50     GOTO1 ADDELEM                                                          
         EJECT                                                                  
* VALIDATE ALL CANADIAN COMMERCIAL REGISTRATION NUMBERS *                       
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'50'        REGISTRATION ELEMENT                         
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMLNOEL,R6                                                       
         SPACE                                                                  
         LA    R2,TRACRTCH         C.R.T.C.                                     
         CLI   5(R2),0                                                          
         BE    VR52                                                             
         BAS   RE,VCRTC            VALIDATE C.R.T.C. NO                         
         SPACE                                                                  
VR52     LA    R2,TRAREGNH         REGISTRATION NUMBER                          
         CLI   5(R2),0                                                          
         BE    VR54                                                             
         BAS   RE,VREG             VALIDATE REG. NO                             
         SPACE                                                                  
VR54     LA    R2,TRATCNOH         T.C. NUMBER                                  
         CLI   5(R2),0                                                          
         BE    VR56                                                             
         BAS   RE,VTC              VALIDATE T.C. NO                             
         SPACE                                                                  
VR56     LA    R2,TRACBCH          CBC. NUMBER                                  
         CLI   5(R2),0                                                          
         BE    VR58                                                             
         BAS   RE,VCBC             VALIDATE CBC. NO                             
         SPACE                                                                  
VR58     LA    R2,TRAASCH          ASC. NUMBER                                  
         CLI   5(R2),0                                                          
         BE    VR60                                                             
         BAS   RE,VASC             VALIDATE ASC. NO                             
         SPACE                                                                  
VR60     CLI   ELEM,0              WAS ANY NUMBER ENTERED                       
         BE    VR70                NO                                           
         MVI   CMLNOLN,CMLNOLEN    SET ELEM LENGTH                              
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
VR70     XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'52'        TALENT CYCLE ELEMENT                         
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING CMLTCEL,R6                                                       
         LA    R2,TRATC1H          FIRST START-END DATE PAIR                    
         SPACE                                                                  
         BAS   RE,VTAL             VALIDATE TALENT CYCLE                        
         SPACE                                                                  
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
VR80     CLI   SVPROF+9,C'Y'       AUTO TURNAROUND                              
         BE    VR84                YES                                          
         CLI   SVPROF+9,C'D'       AUTO TURNAROUND                              
         BE    VR84                NO                                           
         MVC   CHREASON,=C'NC'                                                  
         B     *+10                                                             
VR84     MVC   CHREASON,=C'TC'     SET UP AS MAINT CHANGE                       
         CLI   ACTNUM,ACTADD       UNLESS ADD                                   
         BNE   VR86                                                             
         MVI   CHREASON+1,C'A'                                                  
VR86     B     DR                  NOW DISPLAY VALIDATED RECORD                 
         SPACE                                                                  
VRPACK   PACK  DUB,WORK(1)                                                      
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE 3                                                                
DR       L     R6,AIO                                                           
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S (PROD HSE KEY)                
         BE    DR60                YES, ONLY DISPL TITLE (PROD HOUSE)           
         BAS   RE,PPRD             GO PRINT PROD LIST                           
         CLC   TRAPLST,WORK                                                     
         BE    *+14                                                             
         MVC   TRAPLST,WORK                                                     
         OI    TRAPLSTH+6,X'80'                                                 
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE COMMERCIAL ELEMENT                 
         USING CMLDTAEL,R6                                                      
         SPACE                                                                  
         CLC   TRATLE,CMLTITLE     TITLE                                        
         BE    *+14                                                             
         MVC   TRATLE,CMLTITLE                                                  
         OI    TRATLEH+6,X'80'                                                  
         SPACE                                                                  
         TM    CMLSTAT,X'80'       IS RECORD SOFT DELETED                       
         BZ    DR04                NO                                           
         MVC   TRADEL,DELMSG                                                    
         B     DR06                                                             
DR04     OC    TRADEL,TRADEL                                                    
         BZ    DR08                                                             
         XC    TRADEL,TRADEL                                                    
DR06     OI    TRADELH+6,X'80'                                                  
         SPACE                                                                  
DR08     XC    PARAS(24),PARAS                                                  
         EDIT  (1,CMLSLN),(5,PARAS),ZERO=BLANK,ALIGN=LEFT                       
         CLC   TRASLN,PARAS       COMMERCIAL LENGTH                             
         BE    *+14                                                             
         MVC   TRASLN,PARAS                                                     
         OI    TRASLNH+6,X'80'                                                  
         SPACE                                                                  
         XC    WORK(L'TRARLSE),WORK                                             
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,WORK)                                 
         CLC   TRARLSE,WORK        RELEASE DATE                                 
         BE    *+14                                                             
         MVC   TRARLSE,WORK                                                     
         OI    TRARLSEH+6,X'80'                                                 
         SPACE                                                                  
         MVC   WORK(L'TRARCL),WORK                                              
         CLC   CMLRCL,=XL3'FFFFFF'                                              
         BNE   DR10                                                             
         MVC   WORK(3),=CL3'UFN'                                                
         XC    WORK+3(5),WORK+3    CLEAR PREV DATE                              
         B     DR12                                                             
         EJECT                                                                  
DR10     GOTO1 (RF),(R1),(3,CMLRCL),(5,WORK)                                    
DR12     CLC   TRARCL,WORK         RECALL DATE                                  
         BE    *+14                                                             
         MVC   TRARCL,WORK                                                      
         OI    TRARCLH+6,X'80'                                                  
         SPACE                                                                  
         XC    WORK(L'TRATYPE),WORK                                             
         MVC   WORK(L'CMLTYPE),CMLTYPE                                          
         CLC   TRATYPE,WORK        COMMERCIAL TYPE                              
         BE    *+14                                                             
         MVC   TRATYPE,WORK                                                     
         OI    TRATYPEH+6,X'80'                                                 
         SPACE                                                                  
DR22     CLC   TRACLTN,CMLCLTNO    CLIENT COMMERCIAL NUMBER                     
         BE    *+14                                                             
         MVC   TRACLTN,CMLCLTNO                                                 
         OI    TRACLTNH+6,X'80'                                                 
         SPACE                                                                  
         MVC   WORK(1),CMLSOLO                                                  
         MVI   WORK+1,0                                                         
         CLC   TRASOLO,WORK                                                     
         BE    *+14                                                             
         MVC   TRASOLO,WORK                                                     
         OI    TRASOLOH+6,X'80'                                                 
         SPACE                                                                  
         MVC   WORK(4),CMLCLASS                                                 
         MVI   WORK+4,0                                                         
         CLC   TRACLS,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLS,WORK                                                      
         OI    TRACLSH+6,X'80'                                                  
         SPACE                                                                  
* DISPLAY CANADIAN REGISTRATION NUMBERS *                                       
         SPACE                                                                  
         MVI   ELCODE,X'50'                                                     
         BAS   RE,NEXTEL                                                        
         BE    DR30                                                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMLNOEL,R6                                                       
DR30     XC    WORK(L'TRACRTC),WORK                                             
         MVC   WORK(L'CMLNOCR),CMLNOCR                                          
         CLC   TRACRTC,WORK                                                     
         BE    *+14                                                             
         MVC   TRACRTC,WORK                                                     
         OI    TRACRTCH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK(L'TRACDTE),WORK                                             
         OC    CMLNOCRD,CMLNOCRD   IS THERE A DATE                              
         BZ    DR32                                                             
         GOTO1 DATCON,DMCB,(3,CMLNOCRD),(5,WORK)                                
         SPACE                                                                  
DR32     CLC   TRACDTE,WORK                                                     
         BE    *+14                                                             
         MVC   TRACDTE,WORK                                                     
         OI    TRACDTEH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK(L'TRAREGN),WORK                                             
         MVC   WORK(L'CMLNORE),CMLNORE                                          
         CLC   TRAREGN,WORK                                                     
         BE    *+14                                                             
         MVC   TRAREGN,WORK                                                     
         OI    TRAREGNH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK(L'TRATCNO),WORK                                             
         MVC   WORK(L'CMLNOTC),CMLNOTC                                          
         CLC   TRATCNO,WORK                                                     
         BE    *+14                                                             
         MVC   TRATCNO,WORK                                                     
         OI    TRATCNOH+6,X'80'                                                 
         SPACE                                                                  
         XC    WORK(L'TRACBC),WORK                                              
         MVC   WORK(L'CMLNOCB),CMLNOCB                                          
         CLC   TRACBC,WORK                                                      
         BE    *+14                                                             
         MVC   TRACBC,WORK                                                      
         OI    TRACBCH+6,X'80'                                                  
         SPACE                                                                  
         XC    WORK(L'TRAASC),WORK                                              
         MVC   WORK(L'CMLNOAS),CMLNOAS                                          
         CLC   TRAASC,WORK                                                      
         BE    *+14                                                             
         MVC   TRAASC,WORK                                                      
         OI    TRAASCH+6,X'80'                                                  
         SPACE                                                                  
         XC    WORK(L'TRAADTE),WORK                                             
         OC    CMLNOASD,CMLNOASD   IS THERE A DATE                              
         BZ    DR34                                                             
         GOTO1 DATCON,DMCB,(3,CMLNOASD),(5,WORK)                                
         MVI   WORK+9,0                                                         
         CLC   TRAADTE,WORK                                                     
         BE    *+14                                                             
DR34     MVC   TRAADTE,WORK                                                     
         OI    TRAADTEH+6,X'80'                                                 
         SPACE                                                                  
DR40     MVI   ELCODE,X'52'                                                     
         LA    R3,4                CURRENT TALENT CYCLES                        
         LA    R2,TRATC1H                                                       
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   DR50                                                             
         USING CMLTCEL,R6                                                       
         ZIC   R5,CMLTCLN                                                       
         SR    R4,R4                                                            
         D     R4,=F'6'                                                         
         CH    R4,=H'2'                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,CMLTLCYS                                                      
DR44     LTR   R3,R3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         XC    WORK(L'TRATC1),WORK                                              
         GOTO1 DATCON,DMCB,(3,(R4)),(5,WORK)                                    
         MVI   WORK+8,C'-'                                                      
         GOTO1 (RF),(R1),(3,3(R4)),(5,WORK+9)                                   
         CLC   8(L'TRATC1,R2),WORK                                              
         BE    *+14                                                             
         MVC   8(L'TRATC1,R2),WORK                                              
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCTR  R3,0                                                             
         LA    R4,6(,R4)           NEXT TALENT CYCLE PAIR                       
         BCT   R5,DR44                                                          
DR50     LTR   R3,R3               ANY REMAINING                                
         BZ    EXIT                                                             
         XC    WORK,WORK           BLANK ANY REMAINING                          
         SPACE                                                                  
DR52     CLC   8(L'TRATC1,R2),WORK                                              
         BE    *+14                                                             
         MVC   8(L'TRATC1,R2),WORK                                              
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R3,DR52                                                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY PROD HOUSE KEY ONLY (ALL 9'S CML ID) AND CLEAR ALL OTHER FLDS         
         SPACE                                                                  
DR60     XC    WORK,WORK                                                        
         LA    R0,8                NUMBER OF FLDS TO CLEAR                      
         LA    R1,TRADELH          1ST FLD                                      
DR62     ZIC   RE,0(R1)            GET FLD LEN                                  
         SH    RE,=H'9'                                                         
         EX    RE,DRCLC                                                         
         BE    *+12                                                             
         EX    RE,DRMVC                                                         
         OI    6(R1),X'80'                                                      
         LA    R1,9(RE,R1)                                                      
         ZIC   RE,0(,R1)                                                        
         AR    R1,RE                                                            
         BCT   R0,DR62                                                          
         LA    R0,4                NUMBER OF FLDS TO CLEAR                      
         LA    R1,TRATC1H          1ST FLD                                      
DR64     ZIC   RE,0(R1)            GET FLD LEN                                  
         SH    RE,=H'9'                                                         
         EX    RE,DRCLC                                                         
         BE    *+12                                                             
         EX    RE,DRMVC                                                         
         OI    6(R1),X'80'                                                      
         LA    R1,9(RE,R1)                                                      
         ZIC   RE,0(,R1)                                                        
         AR    R1,RE                                                            
         BCT   R0,DR64                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE COMMERCIAL ELEMENT                 
         USING CMLDTAEL,R6                                                      
         SPACE                                                                  
         MVC   TRATLE,CMLTITLE     TITLE                                        
         OI    TRATLEH+6,X'80'                                                  
         B     EXIT                                                             
DRCLC    CLC   8(0,R1),WORK                                                     
DRMVC    MVC   8(0,R1),WORK                                                     
         EJECT                                                                  
* GET NEXT SEQUENCE NUMBER FROM MASTER RECORD FOR ADDS                          
         SPACE                                                                  
PAR      MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF CMML SEQ REC FOUND                        
         BE    PAR10               GO SAVE CMML SEQ NUMBER                      
         SPACE                                                                  
         DROP  R4                                                               
         SPACE                                                                  
* NOW MUST ADD CMML SEQ REC FOR AGENCY/MEDIA/CLT-ONCE FOR EACH A/M/CLT          
         SPACE                                                                  
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         L     R6,AIO                                                           
         USING CMLKEY,R6                                                        
         XC    CMLRECD(256),CMLRECD                                             
         MVC   CMLKEY,KEY                                                       
         XC    ELEM+2(CMLDTAX-CMLDTAEL),ELEM+2                                  
         LA    R6,ELEM                                                          
         USING CMLDTAEL,R6                                                      
         MVI   CMLDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   CMLDTALN,CMLDTAX-CMLDTAEL                                        
         MVC   CMLSEQ,=XL3'000001' START SEQ NUMBER ONE IN MASTER               
         MVC   HOLDSEQ,CMLSEQ      START SEQ NUMBER ONE IN DSECT                
         MVC   CMLTITLE,=CL15'CMML SEQ RECORD'                                  
         GOTO1 ADDELEM             ADD CMML DATA ELEMENT                        
         GOTO1 ADDREC                                                           
         B     PAR20                                                            
PAR10    GOTO1 GETREC                                                           
         L     R6,AIO2                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   HOLDSEQ,CMLSEQ                                                   
PAR20    MVC   KEY(L'SVKEY),SVKEY       RESTORE KEY AND AIO                     
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        NOW GET DATA ELEM                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   CMLSEQ,HOLDSEQ      AND PUT COMMERCIAL SEQ # IN IT               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* UPDATE SEQUENCE NUMBER IN MASTER RECORD FOR LAST ADD                          
         SPACE                                                                  
AAR      MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
         GOTO1 GETREC                                                           
AAR10    L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         ICM   R1,7,CMLSEQ         GET SEQ                                      
         LA    R1,1(,R1)                   AND ADD 1                            
         STCM  R1,7,CMLSEQ                           FOR ADDED REC              
         GOTO1 PUTREC                                                           
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,HOLDCML                                                  
         GOTO1 HIGH                GET DISK ADDR FOR ADDED REC                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    CMLKID+1,X'80'       CHANGE 21 TO A1                             
         MVC   CMLKCML(3),HOLDSEQ                                               
         XC    CMLKCML+3(5),CMLKCML+3                                           
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND AIO                          
         MVC   AIO,AIO1                                                         
         GOTO1 =A(GENR),RR=SPTR32RR GO GENERATE AUTO-TURNAROUND REQ             
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
         SPACE                                                                  
PUT      MVC   AIO,AIO3                                                         
         L     R2,AIO1                                                          
         MVC   KEY,0(R2)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO3                                                          
         CLC   14(2,R2),14(R4)     COMPARE RECORD LENGTHS                       
         BNE   PUT10                                                            
         LH    R3,14(,R2)          GET LENGTH                                   
         LR    R5,R3                                                            
         CLCL  R2,R4                                                            
         BNE   PUT10                                                            
         MVI   IOOPT,C'Y'                                                       
         B     PUT12                                                            
PUT10    GOTO1 =A(GENR),RR=SPTR32RR GO GENERATE AUTO-TURNAROUND REQ             
         SPACE                                                                  
PUT12    MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         SPACE 3                                                                
* DELETE RECORD INVALID FOR COMMERCIALS                                         
         SPACE                                                                  
DELREC   MVI   ERROR,INVACT        DELETE IS INVALID                            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         B     TRAPERR                                                          
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE                                                                  
DK       L     R4,AIO                                                           
         USING CMLKEY,R4                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,CMLKCLT,QCLT                                         
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
         CLC   TRACLT,WORK                                                      
         BE    DK12                                                             
DK10     MVC   TRACLT,WORK                                                      
         OI    TRACLTH+6,X'80'                                                  
         SPACE                                                                  
DK12     XC    WORK(L'TRACML),WORK                                              
         MVC   WORK(L'CMLKCML),CMLKCML                                          
         CLC   TRACML,WORK                                                      
         BE    *+14                                                             
         MVC   TRACML,WORK                                                      
         OI    TRACMLH+6,X'80'                                                  
         SPACE                                                                  
         MVC   BAGYMD,CMLKAM                                                    
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    DK20                YES                                          
         MVC   BCLT,CMLKCLT        SAVE KEY CONTENTS                            
         BAS   RE,FCLT             GO GET CLIENT CLIST                          
DK20     MVC   HOLDCML,CMLKCML                                                  
         DROP  R4                                                               
         SPACE                                                                  
* PRINT OUT ANY FILTERS                                                         
         SPACE                                                                  
         OC    FILTERS,FILTERS     ANY FILTERS                                  
         BZ    EXIT                NO                                           
         LA    R3,FLD              OUTPUT AREA                                  
         LR    R4,R3               COMPARAND                                    
         XC    FLD,FLD                                                          
         OC    DATEFTR,DATEFTR     DATE FILTER                                  
         BZ    DK28                                                             
         MVI   0(R3),C'D'                                                       
         CLI   DATESFTR,0          ANY GREATER/LESS THAN                        
         BE    *+14                NO                                           
         MVC   1(1,R3),DATESFTR                                                 
         LA    R3,1(,R3)                                                        
         MVI   1(R3),C'='                                                       
         LA    R3,2(,R3)                                                        
         GOTO1 DATCON,DMCB,(3,DATEFTR),(5,(R3))                                 
         LA    R3,8(,R3)                                                        
DK28     OC    DELFTR,DELFTR       SHOW DELETED CML'S ONLY                      
         BZ    DK30                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(3,R3),DELETE                                                   
         LA    R3,4(,R3)                                                        
         EJECT                                                                  
DK30     OC    PRODFTR,PRODFTR     PRODUCT FILTER                               
         BZ    DK32                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'P'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(3,R3),PRODFTR                                                  
         CLI   4(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,5(,R3)                                                        
DK32     OC    TYPEFTR,TYPEFTR     TYPE FILTER                                  
         BZ    DK34                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'T'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(3,R3),TYPEFTR                                                  
         CLI   4(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,5(,R3)                                                        
DK34     OC    SLNFTR,SLNFTR       SPOT LENGTH FILTER                           
         BZ    DK36                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'L'                                                       
         MVI   1(R3),C'='                                                       
         LA    R3,2(,R3)                                                        
         EDIT  (B1,SLNFTR),(3,(R3)),ALIGN=LEFT                                  
         CLI   2(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,3(,R3)                                                        
DK36     MVC   TRAFLTR,FLD                                                      
         OI    TRAFLTRH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GO DO READ HIGH                          
         XC    RECCT,RECCT         ZERO RECORD CT                               
         L     R1,=A(HEADING)      HEADING LINE FOR REPORT                      
         A     R1,SPTR32RR                                                      
         ST    R1,SPECS                                                         
         L     R1,=A(HDHK)          HEADING ROUTINE FOR REPORT                  
         A     R1,SPTR32RR                                                      
         ST    R1,HEADHOOK                                                      
         MVC   CMLKID(2),=XL2'0A21' BUILD KEY FOR READ HI                       
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,HOLDCML                                                  
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   COMPKEY(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
         B     LRNONE                                                           
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
LR22     CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   LRANY               YES, SEE IF ANY SELECTED                     
         MVC   DUB(2),=XL2'0A21'   TO INSURE                                    
         MVC   DUB+2(1),BAGYMD     BUILD KEY                                    
         CLC   DUB(3),KEY          ONLY WANTED KEYS ARE PASSED                  
         BL    EXIT                                                             
         BH    LR20                                                             
         ZIC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,LRCLC            SEE IF PAST KEY                              
         BNE   LRANY               YES, ALL DONE                                
LR30     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING CMLKEY,R4                                                        
         OC    CMLKCML(8),CMLKCML  IF ALL BIN ZEROS                             
         BZ    LR20                THEN CMML SEQ # REC, BYPASS                  
         CLC   CMLKCML,=8C'9'      IF ALL 9'S                                   
         BE    LR20                THEN PROD HSE REC, BYPASS                    
         LR    R6,R4                                                            
         BAS   RE,FTR              GO FILTER RECS                               
         BNE   LR20                GOT FILTERED OUT                             
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         USING CMLDTAEL,R6                                                      
         LH    R1,RECCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,RECCT                                                         
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                MUST BE ON/OFFLINE                           
LRCLC    CLC   COMPKEY(1),KEY                                                   
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
         SPACE                                                                  
LRR      LA    R5,P                PRINT LINE ADDRESS                           
         USING PRTLINE,R5                                                       
         CLC   BCLT,CMLKCLT        SAME CLIENT                                  
         BE    LRR10               YES                                          
         MVC   BCLT,CMLKCLT        SAVE KEY CONTENTS                            
         BAS   RE,FCLT             GO GET CLIENT CLIST                          
         MVI   FORCEHED,C'Y'                                                    
LRR10    MVC   PCML,CMLKCML                                                     
         DROP  R4                                                               
         SPACE                                                                  
         CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   LRR18               NO                                           
         MVI   ELCODE,C'1'         CK ACTIVITY ELEMENT                          
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   LRR16                                                            
         GOTO1 DATCON,DMCB,(5,0),(3,WORK) GET TODAY'S DATE                      
         USING ACTVD,R6                                                         
         CLC   ACTVADDT,WORK       WAS ADD TODAY                                
         BNE   LRR12               NO, CK CHANGE                                
         MVC   PCML+8+132(3),=C'ADD'                                            
         B     LRR14                                                            
LRR12    CLC   ACTVCHDT,WORK       WAS CHANGE TODAY                             
         BNE   LRR16                                                            
         MVC   PCML+8+132(3),=C'CHG'                                            
LRR14    MVI   PCML+9,C'*'                                                      
         SPACE                                                                  
LRR16    MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    LRR18                                                            
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
LRR18    EDIT  (1,CMLSLN),(3,PSLN),ZERO=BLANK                                   
         MVC   PTITLE,CMLTITLE                                                  
         CLI   CMLSOLO,0           ANY ENTRY                                    
         BE    LRR20               NO                                           
         MVC   PSOLO,CMLSOLO                                                    
         SPACE                                                                  
LRR20    GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,PRELSE)                               
         CLC   CMLRCL,=XL3'FFFFFF'                                              
         BNE   LRR22                                                            
         MVC   PRECALL+3(3),=CL3'UFN'                                           
         B     LRR24                                                            
LRR22    GOTO1 (RF),(R1),(3,CMLRCL),(5,PRECALL)                                 
         SPACE                                                                  
LRR24    MVC   PTYPE,CMLTYPE                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLPRDEL,R6                                                      
         MVC   SVKEY,KEY                                                        
         CLI   CMLPRDS,X'FF'       IS THIS PRD=ALL                              
         BNE   LRR26               NO                                           
         MVC   PLIST(7),PRDALL                                                  
         MVI   PRDCTR,0                                                         
         B     LRR28                                                            
LRR26    ZIC   R3,CMLPRDLN         GET PROD LIST ELEM LEN                       
         BCTR  R3,0                                                             
         BCTR  R3,0                NOW # PRODS IN LIST                          
         LA    R4,CMLPRDS          POINT TO START BIN PROD LIST                 
         MVC   AIO,AIO2                                                         
         STC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         ST    R4,PRDPTR           SAVE PROD LIST PTR                           
         BAS   RE,PPRDS                                                         
         LA    R5,P1               RESTORE R5 TO P1                             
LRR28    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       SOFT DELETE                                  
         BZ    LRR30               NO                                           
         MVC   PTITLE+132(9),DELMSG                                             
LRR30    OC    CMLPROD,CMLPROD     PRODUCTION HOUSE?                            
         BZ    LRR40               NO                                           
         MVC   PMISC(18),=CL18'PRODUCTION HOUSE ='                              
         MVC   PMISC+19(L'CMLPROD),CMLPROD                                      
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
LRR40    OC    CMLCLTNO,CMLCLTNO                                                
         BZ    LRR50                                                            
         MVC   PMISC(7),=CL7'CLIENT='                                           
         MVC   PMISC+7(L'CMLCLTNO),CMLCLTNO                                     
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
LRR50    OC    CMLCLASS,CMLCLASS   ANY COMM CLASS                               
         BZ    LRR60                                                            
         MVC   PMISC(6),=CL6'CLASS='                                            
         MVC   PMISC+6(4),CMLCLASS                                              
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
* DISPLAY CANADIAN REGISTRATION NUMBERS *                                       
         SPACE                                                                  
LRR60    MVI   ELCODE,X'50'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   LRR70                                                            
         USING CMLNOEL,R6                                                       
         OC    CMLNOCR,CMLNOCR                                                  
         BZ    LRR62                                                            
         MVC   PMISC(13),=C'C.R.T.C. NO.='                                      
         MVC   PMISC+13(L'CMLNOCR),CMLNOCR                                      
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
         MVC   PMISC+2(11),=C'EXPIRATION-'                                      
         GOTO1 DATCON,DMCB,(3,CMLNOCRD),(5,PMISC+13)                            
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
LRR62    OC    CMLNORE,CMLNORE                                                  
         BZ    LRR64                                                            
         MVC   PMISC(9),=C'REG. NO.='                                           
         MVC   PMISC+9(L'CMLNORE),CMLNORE                                       
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
LRR64    OC    CMLNOTC,CMLNOTC                                                  
         BZ    LRR66                                                            
         MVC   PMISC(9),=C'T.C. NO.='                                           
         MVC   PMISC+9(L'CMLNOTC),CMLNOTC                                       
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
LRR66    OC    CMLNOCB,CMLNOCB                                                  
         BZ    LRR68                                                            
         MVC   PMISC(8),=C'CBC NO.='                                            
         MVC   PMISC+8(L'CMLNOCB),CMLNOCB                                       
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
LRR68    OC    CMLNOAS,CMLNOAS                                                  
         BZ    LRR70                                                            
         MVC   PMISC(8),=C'ASC NO.='                                            
         MVC   PMISC+8(L'CMLNOAS),CMLNOAS                                       
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
         MVC   PMISC+2(11),=C'EXPIRATION-'                                      
         GOTO1 DATCON,DMCB,(3,CMLNOASD),(5,PMISC+13)                            
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         SPACE                                                                  
LRR70    MVI   ELCODE,X'52'                                                     
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   LRR80                                                            
         USING CMLTCEL,R6                                                       
         ZIC   R3,CMLTCLN                                                       
         SR    R2,R2                                                            
         D     R2,=F'6'                                                         
         CH    R2,=H'2'                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,CMLTLCYS                                                      
         MVC   PMISC(12),=C'TALENT CYCLE'                                       
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
LRR74    GOTO1 DATCON,DMCB,(3,(R4)),(5,PMISC+3)                                 
         MVI   PMISC+11,C'-'                                                    
         GOTO1 (RF),(R1),(3,3(R4)),(5,PMISC+12)                                 
         BAS   RE,NXL              POINT TO NEXT PRINT LINE                     
         LA    R4,6(,R4)           NEXT TALENT CYCLE PAIR                       
         BCT   R3,LRR74                                                         
LRR80    CLI   PRDCTR,0            WAS PROD LIST DONE                           
         BE    LRR90               YES                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         MVC   AIO,AIO2                                                         
         BAS   RE,PPRDS                                                         
         B     LRR80                                                            
         SPACE                                                                  
LRR90    MVC   AIO,AIO1                                                         
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
         B     LR20                                                             
         SPACE 3                                                                
* INCREMENT TO NEXT LINE AND PRINT IF ALL USED *                                
         SPACE                                                                  
NXL      NTR1                                                                   
         LA    R5,132(,R5)                                                      
         LA    R0,P4                                                            
         CR    R5,R0                                                            
         BNH   NXLX                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         CLI   PRDCTR,0            WAS PROD LIST DONE                           
         BE    NXLX                YES                                          
         MVC   AIO,AIO2                                                         
         BAS   RE,PPRDS                                                         
NXLX     XIT1  REGS=(R5)                                                        
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
         SPACE                                                                  
         USING CMLKEY,R4                                                        
         USING CMLDTAEL,R6                                                      
LRL      MVC   LISTAR,SPACES                                                    
         GOTO1 CLUNPK,DMCB,CMLKCLT,LCLT                                         
         TM    CMLSTAT,X'80'       SOFT DELETE                                  
         BZ    *+8                 NO                                           
         MVI   LCLT+3,C'*'                                                      
         MVC   LCML,CMLKCML                                                     
         EDIT  (1,CMLSLN),(2,LLEN),ZERO=BLANK                                   
         MVC   LTITLE,CMLTITLE                                                  
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,LRELSE)                               
         CLC   CMLRCL,=XL3'FFFFFF'                                              
         BNE   LRL10                                                            
         MVC   LRECALL(3),=CL3'UFN'                                             
         B     LRL12                                                            
LRL10    GOTO1 (RF),(R1),(3,CMLRCL),(5,LRECALL)                                 
LRL12    MVC   LTYPE,CMLTYPE                                                    
         SPACE                                                                  
LRL20    GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
         DROP  R4,R5,R6                                                         
         SPACE                                                                  
LRANY    OC    RECCT,RECCT         WERE ANY RECS SELECTED                       
         BNZ   EXIT                                                             
LRNONE   CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   LRNONEA                                                          
         MVC   P(30),=CL30'* NOTE * NO RECORDS SELECTED *'                      
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
LRNONEA  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         LA    R2,TRAMEDH                                                       
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
* VALIDATE 8 CHARACTER COMMERCIAL CODE *                                        
         SPACE                                                                  
* FIRST 4 CHARACTERS MUST BE ALPHA, 5TH NUMERIC OR -, LAST 3 NUMERIC *          
         SPACE                                                                  
VCML     CLI   5(R2),8             MUST BE LEN OF 8                             
         BNE   CMLLENER                                                         
         CLC   TRACML(8),=8C'9'    CML ID ALL 9'S (PROD HSE KEY)                
         BER   RE                                                               
         SPACE                                                                  
         LA    R0,8                                                             
         LA    R1,TRACML                                                        
VCML10   CLI   0(R1),C' '                                                       
         BE    CMLBLKER                                                         
         CLI   0(R1),C'0'                                                       
         BL    VCML14                                                           
         CLI   0(R1),C'9'                                                       
         BH    BADCMLER                                                         
         B     VCML16                                                           
         SPACE                                                                  
VCML14   CLI   0(R1),C'*'                                                       
         BE    VCML16                                                           
         SPACE                                                                  
         CLI   0(R1),C'A'                                                       
         BL    BADCMLER                                                         
         CLI   0(R1),C'Z'                                                       
         BH    BADCMLER                                                         
VCML16   LA    R1,1(,R1)                                                        
         BCT   R0,VCML10                                                        
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - DATE, PROD, TYPE, LEN *                            
         SPACE                                                                  
VFTR     NTR1                                                                   
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    EXIT                NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELP),FTRHELP                                       
         B     ERREXIT                                                          
VFTR08   GOTO1 SCANNER,DMCB,TRAFLTRH,(5,BLOCK)                                  
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         DATE                                         
         BNE   VFTR20                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,DATEFTR)                                 
         MVC   DATESFTR,HOLDSIGN                                                
         B     VFTR70                                                           
VFTR20   EX    R1,VFTRCLCB         PRD (PRODUCT)                                
         BE    VFTR22                                                           
         EX    R1,VFTRCLCC         PROD (PRODUCT)                               
         BNE   VFTR30                                                           
VFTR22   OC    BCLT,BCLT           CLIENT MUST HAVE BEEN ENTERED                
         BZ    MISSCLT                                                          
         CLC   ALL,22(R4)          IS PRD=ALL                                   
         BE    VFTR28              YES, SAVE IT                                 
         LA    R0,256              MAX COUNT BUG CATCHER                        
         L     R1,ASVCLIST         TABLE OF CLIENT PROD CODES                   
VFTR24   CLI   0(R1),0             AT END OF TABLE?                             
         BE    PRDERR              YES, ERROR                                   
         CLC   0(3,R1),22(R4)      THIS A VALID PROD CODE                       
         BE    VFTR26                                                           
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,VFTR24                                                        
         DC    H'0'                                                             
VFTR26   MVC   PRODFTR(4),0(R1)    PROD AND PRD                                 
         B     VFTR70                                                           
VFTR28   MVC   PRODFTR(3),ALL      MOVE IN ALL, LEAVE PRD ZERO                  
         B     VFTR70                                                           
VFTR30   EX    R1,VFTRCLCD         TYPE                                         
         BNE   VFTR40                                                           
         MVC   WORK(12),22(R4)                                                  
         BAS   RE,VTYPF                                                         
         MVC   TYPEFTR,WORK                                                     
         B     VFTR70                                                           
VFTR40   EX    R1,VFTRCLCE         SPOT LEN                                     
         BNE   VFTR50                                                           
         TM    3(R4),X'80'         WAS SPOT LEN NUMERIC                         
         BZ    NUMERR                                                           
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      SPOT LEN                                     
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERR             GO PRINT ERROR                               
         MVC   SLNFTR,WORK                                                      
         B     VFTR70                                                           
VFTR50   EX    R1,VFTRCLCG         DELETED RECS                                 
         BNE   VFTR80                                                           
         MVI   DELFTR,1                                                         
VFTR70   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         B     EXIT                                                             
VFTR80   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELP),FTRMSG                               
         B     ERREXIT                                                          
FTRMSG   DC    C'* ERROR *'                                                     
FTRHELP  DC    C'VALID FILTERS - DELETED,DATE/PRD/TYPE/LEN='                    
VFTRCLCA CLC   12(0,R4),=CL4'DATE'                                              
VFTRCLCB CLC   12(0,R4),=CL4'PRD'                                               
VFTRCLCC CLC   12(0,R4),=CL4'PROD'                                              
VFTRCLCD CLC   12(0,R4),=CL4'TYPE'                                              
VFTRCLCE CLC   12(0,R4),=CL3'LEN'                                               
VFTRCLCG CLC   12(0,R4),DELETED                                                 
         EJECT                                                                  
* VALIDATE COMMERCIAL TYPE                                                      
         SPACE                                                                  
VTYP     NTR1                                                                   
         GOTO1 ANY                                                              
         B     VTYP04                                                           
VTYPF    NTR1                                                                   
VTYP04   CLI   WORK+3,C' '                                                      
         BH    VTYPER                                                           
         CLI   SPOTNETF,C'S'       THIS SPOT SYSTEM                             
         BNE   VTYP06                                                           
         LA    R0,CTYPTBCS                                                      
         L     R1,=A(CTYPTABS)                                                  
         B     VTYP08                                                           
VTYP06   CLI   SPOTNETF,C'N'       THIS NETWORK SYSTEM                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,CTYPTBCN                                                      
         L     R1,=A(CTYPTABN)                                                  
VTYP08   A     R1,SPTR32RR                                                      
VTYP10   CLC   WORK(3),0(R1)                                                    
         BE    VTYP20                                                           
         LA    R1,3(,R1)                                                        
         BCT   R0,VTYP10                                                        
         B     VTYPER                                                           
VTYP20   B     EXIT                                                             
         EJECT                                                                  
* VALIDATE C.R.T.C. NO *                                                        
         SPACE                                                                  
         USING CMLNOEL,R6                                                       
VCRTC    NTR1                                                                   
         CLI   5(R2),6             MAX LEN                                      
         BH    CRTCER                                                           
         LA    R0,6                                                             
         LA    R1,8+5(R2)                                                       
VCRTC10  CLI   0(R1),C'0'                                                       
         BL    CRTCER                                                           
         CLI   0(R1),C'9'                                                       
         BH    CRTCER                                                           
         BCTR  R1,0                                                             
         BCT   R0,VCRTC10                                                       
         MVC   CMLNOCR,8(R2)                                                    
         LA    R2,TRACDTEH                                                      
         SPACE                                                                  
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),DATE                                            
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,CMLNOCRD)                                
         MVI   ELEM,X'50'                                                       
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 3                                                                
* VALIDATE REG. NO *                                                            
         SPACE                                                                  
         USING CMLNOEL,R6                                                       
VREG     CLI   5(R2),9             MAX LEN                                      
         BH    REGNOER                                                          
         LA    R0,4                                                             
         LA    R1,8+8(R2)                                                       
VREG10   CLI   0(R1),C'0'                                                       
         BL    REGNOER                                                          
         CLI   0(R1),C'9'                                                       
         BH    REGNOER                                                          
         BCTR  R1,0                                                             
         BCT   R0,VREG10                                                        
         CLI   0(R1),C'-'                                                       
         BNE   REGNOER                                                          
         BCTR  R1,0                                                             
         SPACE                                                                  
         LA    R0,4                                                             
VREG14   CLI   0(R1),C'0'                                                       
         BL    REGNOER                                                          
         CLI   0(R1),C'9'                                                       
         BH    REGNOER                                                          
         BCTR  R1,0                                                             
         BCT   R0,VREG14                                                        
         SPACE                                                                  
         MVC   CMLNORE,8(R2)                                                    
         MVI   ELEM,X'50'                                                       
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE TC. NO *                                                             
         SPACE                                                                  
         USING CMLNOEL,R6                                                       
VTC      CLI   5(R2),9             MAX LEN                                      
         BH    TCER                                                             
         LA    R0,4                                                             
         LA    R1,8(R2)                                                         
VTC10    CLI   0(R1),C'0'                                                       
         BL    TCER                                                             
         CLI   0(R1),C'9'                                                       
         BH    TCER                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,VTC10                                                         
         CLI   0(R1),C'-'                                                       
         BNE   TCER                                                             
         LA    R1,1(,R1)                                                        
         SPACE                                                                  
         LA    R0,2                                                             
VTC14    CLI   0(R1),C'0'                                                       
         BL    TCER                                                             
         CLI   0(R1),C'9'                                                       
         BH    TCER                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,VTC14                                                         
         CLI   0(R1),C'E'                                                       
         BE    VTC20                                                            
         CLI   0(R1),C'F'                                                       
         BE    VTC20                                                            
         CLI   0(R1),C' '                                                       
         BNE   TCER                                                             
         SPACE                                                                  
VTC20    MVC   CMLNOTC,8(R2)                                                    
         MVI   ELEM,X'50'                                                       
         BR    RE                                                               
         DROP  R6                                                               
         SPACE 3                                                                
* VALIDATE CBC. NO *                                                            
         SPACE                                                                  
         USING CMLNOEL,R6                                                       
VCBC     CLI   5(R2),5             MAX LEN                                      
         BH    CBCER                                                            
         LA    R0,5                                                             
         LA    R1,8+4(R2)                                                       
VCBC10   CLI   0(R1),C'0'                                                       
         BL    CBCER                                                            
         CLI   0(R1),C'9'                                                       
         BH    CBCER                                                            
         BCTR  R1,0                                                             
         BCT   R0,VCBC10                                                        
         MVC   CMLNOCB,8(R2)                                                    
         MVI   ELEM,X'50'                                                       
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE ASC. NO *                                                            
         SPACE                                                                  
         USING CMLNOEL,R6                                                       
VASC     NTR1                                                                   
         CLI   5(R2),8             MAX LEN                                      
         BNE   ASCER                                                            
         LA    R1,8(,R2)                                                        
         LA    R0,2                                                             
VASC10   CLI   0(R1),C'0'                                                       
         BL    ASCER                                                            
         CLI   0(R1),C'9'                                                       
         BH    ASCER                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,VASC10                                                        
         CLI   0(R1),C'-'                                                       
         BNE   ASCER                                                            
         LA    R1,1(,R1)                                                        
         LA    R0,2                                                             
VASC20   CLI   0(R1),C'0'                                                       
         BL    ASCER                                                            
         CLI   0(R1),C'9'                                                       
         BH    ASCER                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,VASC20                                                        
         CLI   0(R1),C'-'                                                       
         BNE   ASCER                                                            
         LA    R1,1(,R1)                                                        
         LA    R0,2                                                             
VASC30   CLI   0(R1),C'0'                                                       
         BL    ASCER                                                            
         CLI   0(R1),C'9'                                                       
         BH    ASCER                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,VASC30                                                        
         SPACE                                                                  
         MVC   CMLNOAS,8(R2)                                                    
         SPACE                                                                  
         LA    R2,TRAADTEH                                                      
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),DATE                                            
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,CMLNOASD)                                
         MVI   ELEM,X'50'                                                       
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE TALENT CYCLE *                                                       
         SPACE                                                                  
         USING CMLTCEL,R6                                                       
VTAL     NTR1                                                                   
         LA    R4,CMLTLCYS                                                      
         MVI   CMLTCLN,2           SET EMPTY ELEM LENGTH                        
VTAL10   CLI   5(R2),0             ANY ENTRY                                    
         BE    VTAL30                                                           
         SPACE                                                                  
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),DATE                                            
         L     RF,DMCB             GET LENGTH OF FIELD                          
         LTR   RF,RF                                                            
         BZ    DATERR                                                           
         LA    R3,1(RF,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,DATE),(3,(R4))                                    
         GOTO1 DATVAL,(R1),(R3),DATE                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,3(R4))                                   
         CLC   0(3,R4),3(R4)       DATES MUST BE IN SEQ                         
         BH    DATSQER                                                          
         ZIC   R1,CMLTCLN                                                       
         LA    R1,6(R1)                                                         
         STC   R1,CMLTCLN                                                       
         MVI   CMLTCEL,X'52'       SET ELEM TO INDICATE SOME DATE               
         LA    R1,CMLTLCYS                                                      
VTAL20   CR    R1,R4                                                            
         BNL   VTAL26                                                           
         CLC   0(6,R1),0(R4)       CHECK EXISTING TO NEW                        
         BL    VTAL22                                                           
         BE    DATOVLER            IF SAME, OVERLAP ERROR                       
         CLC   0(3,R1),3(R4)       EXIST START TO NEW END                       
         BNH   DATOVLER                                                         
         B     VTAL24                                                           
VTAL22   CLC   3(3,R1),0(R4)       EXIST END TO START NEW                       
         BNL   DATOVLER                                                         
VTAL24   LA    R1,6(,R1)                                                        
         B     VTAL20                                                           
         SPACE                                                                  
VTAL26   LA    R4,6(,R4)                                                        
         SPACE                                                                  
VTAL30   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,TRATAGH                                                       
         CR    R2,R0                                                            
         BL    VTAL10                                                           
         CLI   ELEM,0              WERE ANY TALENT CYCLES ENTERED               
         BE    TALCYCER            NO                                           
         CLI   CMLTCLN,8           IF ONLY 1, DONE                              
         BE    EXIT                                                             
         ZIC   R3,CMLTCLN                                                       
         SR    R2,R2                                                            
         D     R2,=F'6'                                                         
         CH    R2,=H'2'            REMAINDER MUST BE 2                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 XSORT,DMCB,CMLTLCYS,(R3),6,6,0                                   
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* FIND, EDIT, AND PRINT PRODUCT LIST                                            
         SPACE                                                                  
PPRD     NTR1                                                                   
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLPRDEL,R6                                                      
         MVC   WORK,SPACES                                                      
         CLI   CMLPRDS,X'FF'       IS THIS PRD=ALL                              
         BNE   PPRD06              NO                                           
         MVC   WORK(7),PRDALL                                                   
         B     EXIT                                                             
PPRD06   ZIC   R3,CMLPRDLN         GET PROD LIST ELEM LEN                       
         BCTR  R3,0                                                             
         BCTR  R3,0                NOW # PRODS IN LIST                          
         LA    R4,CMLPRDS          POINT TO START BIN PROD LIST                 
         LA    R5,WORK             OUTPUT AREA                                  
PPRD10   L     R1,ASVCLIST                                                      
PPRD12   CLI   0(R1),C' '                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),3(R1)                                                    
         BE    PPRD14                                                           
         LA    R1,4(R1)                                                         
         B     PPRD12                                                           
PPRD14   MVC   0(3,R5),0(R1)       STORE IN WORK AREA                           
         LA    R5,2(,R5)                                                        
PPRD20   CLI   0(R5),C' '          FIND END OF PROD CODE                        
         BNH   PPRD22                                                           
         LA    R5,1(,R5)                                                        
PPRD22   MVI   0(R5),C','          SEPARATE PRODUCTS                            
         LA    R4,1(,R4)           PRODUCT LIST POINTER                         
         LA    R5,1(,R5)                                                        
         BCT   R3,PPRD10                                                        
         BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* PRINT PROD LIST OF PROD/NAMES                                                 
         SPACE 2                                                                
         USING PRTLINE,R5                                                       
PPRDS    NTR1                                                                   
         ZIC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         L     R4,PRDPTR           SAVE PROD LIST PTR                           
PPRDS10  LA    R1,P4                                                            
         CR    R1,R5                                                            
         BL    PPRDS20                                                          
         L     R1,ASVCLIST                                                      
PPRDS14  CLI   0(R1),C' '                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),3(R1)                                                    
         BE    PPRDS16                                                          
         LA    R1,4(R1)                                                         
         B     PPRDS14                                                          
PPRDS16  MVC   PLIST(3),0(R1)       STORE IN WORK AREA                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD AND BCLT                                         
         MVC   KEY+4(3),0(R1)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING PRDHDRD,R1                                                       
         MVC   PLIST+4(20),PNAME                                                
         DROP  R1                                                               
         LA    R5,132(,R5)                                                      
         LA    R4,1(,R4)           POINT TO NEXT IN PROD LIST                   
         BCT   R3,PPRDS10                                                       
PPRDS20  STC   R3,PRDCTR           SAVE ZERO, OR ANY REMAINING CT               
         ST    R4,PRDPTR           SAVE PROD LIST PTR                           
         B     EXIT                                                             
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
FCLT     NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         LA    R1,KEY                                                           
         USING CLTHDR,R1                                                        
         XC    KEY,KEY                                                          
         MVC   CKEYAM(3),BAGYMD AND BCLT                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING CLTHDR,R1                                                        
         MVC   CLTNM,CNAME                                                      
         LA    R2,CLIST                                                         
         DROP  R1                                                               
         LA    R3,880                                                           
         L     RE,ASVCLIST                                                      
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
* FILTER CMMLS FOR LIST FUNCTION                                                
         SPACE                                                                  
FTR      NTR1                                                                   
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING CMLDTAEL,R6                                                      
         OC    DELFTR,DELFTR       ONLY DELETED CML'S                           
         BZ    FTR04               NO                                           
         TM    CMLSTAT,X'80'       THIS DELETED                                 
         BZ    FTRNO                                                            
         B     FTR06                                                            
FTR04    TM    CMLSTAT,X'80'                                                    
         BNZ   FTRNO                                                            
         SPACE                                                                  
FTR06    OC    TYPEFTR,TYPEFTR     TYPE FILTER                                  
         BZ    FTR10               NO                                           
         CLC   TYPEFTR,CMLTYPE     THIS IT                                      
         BNE   FTRNO                                                            
         SPACE                                                                  
FTR10    OC    SLNFTR,SLNFTR       SPOT LEN FILTER                              
         BZ    FTR20               NO                                           
         CLC   SLNFTR,CMLSLN       DOES THIS MATCH                              
         BNE   FTRNO               NO                                           
         SPACE                                                                  
FTR20    OC    DATEFTR,DATEFTR     DATE FILTER                                  
         BZ    FTR30                                                            
         OC    DATESFTR,DATESFTR                                                
         BZ    FTR24                                                            
         CLI   DATESFTR,X'4C'      GREATER THAN                                 
         BNE   FTR22               MUST BE LESS THAN                            
         CLC   DATEFTR,CMLRCL      FILTER TO RECALL                             
         BNH   FTRNO               BYPASS                                       
         B     FTR30               CK NEXT FILTER                               
FTR22    CLC   DATEFTR,CMLRCL                                                   
         BNL   FTRNO                                                            
         B     FTR30                                                            
FTR24    CLC   DATEFTR,CMLRLSE                                                  
         BL    FTRNO                                                            
         CLC   DATEFTR,CMLRCL                                                   
         BH    FTRNO                                                            
         SPACE                                                                  
FTR30    DS    0H                                                               
         EJECT                                                                  
FTR40    OC    PRODFTR,PRODFTR     IS THERE A PROD FILTER                       
         BZ    FTR60               NO                                           
         MVI   ELCODE,X'20'                                                     
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PRODFTR,ALL                                                      
         BE    FTR44                                                            
         USING CMLPRDEL,R6                                                      
         ZIC   R0,CMLPRDLN                                                      
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         LA    R1,CMLPRDS          START OF PROD LIST                           
FTR42    CLC   PRDFTR,0(R1)                                                     
         BE    FTR60                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,FTR42                                                         
         B     FTRNO                                                            
FTR44    ZIC   R1,CMLPRDLN                                                      
         BCTR  R1,0                                                             
         CLC   CMLPRDLN,SVPRDLST+1 IS PROD LIST SAME                            
         BNE   FTR46                                                            
         EX    R1,FTRCLC                                                        
         BE    FTR60                                                            
FTR46    EX    R1,FTRMVC                                                        
         MVI   FORCEHED,C'Y'                                                    
FTR60    CR    R1,R1               SET COND CODE FILTERED OK                    
         B     EXIT                                                             
FTRNO    CR    RB,RD               SET COND CODE NO FILTER                      
         B     EXIT                                                             
FTRCLC   CLC   SVPRDLST(0),CMLPRDEL                                             
FTRMVC   MVC   SVPRDLST(0),CMLPRDEL                                             
         EJECT                                                                  
*        ERROR ROUTINES                                                         
         SPACE                                                                  
CMLLENER L     R1,=A(CMLLENMS)     CML MORE THAN 8                              
         B     ERREXITA                                                         
         SPACE                                                                  
CMLBLKER L     R1,=A(CMLBLKMS)     EMBEDDED BLANK IN COMML                      
         B     ERREXITA                                                         
         SPACE                                                                  
BADCMLER L     R1,=A(BADCMLMS)     CML NOT PROPER FORMAT                        
         B     ERREXITA                                                         
         SPACE                                                                  
CLASIZER L     R1,=A(CLASIZMS)     CLASS SIZE ERROR 1-4                         
         B     ERREXITA                                                         
         SPACE                                                                  
SOLERR   MVC   GERROR,=Y(SOLOERR)                                               
         GOTO1 VTRAERR                                                          
*OLOERR  L     R1,=A(SOLOMS)       MUST BE P OR S                               
*        B     ERREXITA                                                         
         SPACE                                                                  
TALCYCER L     R1,=A(TALCYCMS)     MUST BE AT LEAST 1 TALENT CYCLE              
         LA    R2,TRATC1H                                                       
         B     ERREXITA                                                         
         SPACE                                                                  
ASCER    L     R1,=A(ASCMS)                                                     
         B     ERREXITA                                                         
         SPACE                                                                  
CRTCER   L     R1,=A(CRTCMS)                                                    
         B     ERREXITA                                                         
         SPACE                                                                  
REGNOER  L     R1,=A(REGNOMS)                                                   
         B     ERREXITA                                                         
         SPACE                                                                  
TCER     L     R1,=A(TCMS)         T.C. NO. ERR - SHOULD BE 0000-00E            
         B     ERREXITA                                            F            
*                                                              OR BLANK         
CBCER    L     R1,=A(CBCMS)        CBC ERR - SHOULD BE 5 DIGITS                 
         B     ERREXITA                                                         
         SPACE                                                                  
DATSQER  L     R1,=A(DATSQMS)      TALENT CYCLE DATE PAIR OUT OF SEQ            
         B     ERREXITA                                                         
         SPACE                                                                  
DATOVLER L     R1,=A(DATOVLMS)     TALENT CYCLE DATE PAIR OVERLAPS              
         B     ERREXITA                                                         
         SPACE                                                                  
CANADER  L     R1,=A(CANADMS)      ONLY CANADIAN AGENCIES USE THIS              
         LA    R2,CONREC                                                        
         B     ERREXITA                                                         
         SPACE                                                                  
RSTDELER L     R1,=A(RSTDELMS)                                                  
         SPACE                                                                  
ERREXITA A     R1,SPTR32RR                                                      
         MVC   CONHEAD+10(50),0(R1)                                             
         MVC   CONHEAD(10),=C'* ERROR * '                                       
         B     ERREXIT                                                          
         SPACE                                                                  
ERREXIT  GOTO1 ERREX2                                                           
VTYPER   MVI   ERROR,INVTYPE       INVALID CMML TYPE-CTYPTAB                    
         B     TRAPERR                                                          
BADLENER MVI   ERROR,INVCMMLN      COMML ID MUST BE 8 CHAR                      
         B     TRAPERR                                                          
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
PRDERR   MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRDALL   DS    0CL7                                                             
         DC    CL4'PRD='                                                        
ALL      DC    CL3'ALL'                                                         
DELMSG   DS    0CL9                                                             
         DC    C'*'                                                             
DELETED  DS    0CL7                                                             
DELETE   DC    CL6'DELETE'                                                      
         DC    C'D*'                                                            
SOLOMS   DC    CL50'MUST BE BLANK, P(IGGYBACK) OR S(OLO) *'                     
RSTDELMS DC    CL50'CAN NOT RESTORE REC, NOT DELETED *'                         
CLASIZMS DC    CL50'CLASS MUST BE 1-4 CHARACTERS *'                             
BADCMLMS DC    CL50'COMML MUST BE ALPHA, NUMERIC, OR * **'                      
CMLLENMS DC    CL50'COMML MUST BE 8 CHARACTERS *'                               
CMLBLKMS DC    CL50'NO EMBEDDED BLANKS IN COMML CODE *'                         
DATOVLMS DC    CL50'DATE PAIR OVERLAPS OTHER DATE PAIR *'                       
DATSQMS  DC    CL50'DATE PAIR OUT OF SEQUENCE *'                                
TALCYCMS DC    CL50'MUST BE AT LEAST 1 TALENT CYCLE *'                          
ASCMS    DC    CL50'ASC NO MUST BE 2 DIGITS - 2 DIGITS - 2 DIGITS *'            
CRTCMS   DC    CL50'CRTC NO MUST BE 6 DIGITS *'                                 
REGNOMS  DC    CL50'REG NO MUST BE 4 DIGITS - 4 DIGITS *'                       
TCMS     DC    CL50'TC NO MUST BE 4 DIGITS - YR AND E OR F *'                   
CBCMS    DC    CL50'CBC NO MUST BE 5 DIGITS *'                                  
CANADMS  DC    CL50'ONLY CANADIAN AGENCIES SHOULD USE CANADIAN COMML *'         
CTYPTABS DC    CL3'CF8'            COLOR 8MM FILM                               
         DC    CL3'BF8'            B/W 8MM FILM                                 
         DC    CL3'CF6'            COLOR 16MM FILM                              
         DC    CL3'BF6'            B/W 16MM FILM                                
         DC    CL3'CF5'            COLOR 35MM FILM                              
         DC    CL3'CM8'            COLOR MAGNETIC 8MM FILM                      
         DC    CL3'BM8'            B/W MAGNETIC 8MM FILM                        
         DC    CL3'CM6'            COLOR MAGNETIC 16MM FILM                     
         DC    CL3'BM6'            B/W MAGNETIC 16MM FILM                       
         DC    CL3'ARR'            REEL TO REEL TAPE                            
         DC    CL3'HVR'            HIGH BAND VIDEOTAPE REEL                     
         DC    CL3'LVR'            LOW BAND VIDEOTAPE REEL                      
         DC    CL3'HVT'            HIGH BAND VIDEOTAPE REEL                     
         DC    CL3'H34'            HIGH BAND VIDEOTAPE 3/4 INCH REEL            
         DC    CL3'HV1'            HIGH BAND VIDEOTAPE 1 INCH REEL              
         DC    CL3'HV2'            HIGH BAND VIDEOTAPE 2 INCH REEL              
         DC    CL3'LVT'            LOW BAND VIDEOTAPE REEL                      
         DC    CL3'VC '            VIDEOTAPE CARTRIDGE                          
         DC    CL3'SCR'            LIVE SCRIPT                                  
         DC    CL3'AT '            AUDIO TAPE                                   
         DC    CL3'AC '            AUDIO CARTRIDGE                              
         DC    CL3'ET '            ELECTRICAL TRANSCRIPTION                     
         DC    CL3'HMR'            HIGH BAND MULTIREEL TAPE                     
         DC    CL3'LMR'            LOW BAND MULTIREEL TAPE                      
         DC    CL3'S  '            SLIDE                                        
         DC    CL3'BF5'            B/W 35MM FILM                                
CTYPTBCS EQU   (*-CTYPTABS)/3                                                   
CTYPTABN DC    CL3'CLR'            CLR/VTR                                      
         DC    CL3'SC '            SLIDE COPY                                   
         DC    CL3'CPY'            COPY                                         
         DC    CL3'ABB'            AUDIO BILLBOARD                              
         DC    CL3'SSL'            SUPERSLIDE                                   
         DC    CL3'CSL'            COLOR SLIDE                                  
         DC    CL3'FBB'            FILM BILLBOARD                               
CTYPTBCN EQU   (*-CTYPTABN)/3                                                   
         DC    X'00'               END OF TABLE MARKER                          
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'C O M M E R C I A L  L I S T'                            
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,35,C'----------------------------'                            
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'COMML ID'                                                 
         SSPEC H9,3,C'--------'                                                 
         SSPEC H8,13,C'LEN'                                                     
         SSPEC H9,13,C'---'                                                     
         SSPEC H8,18,C'TITLE'                                                   
         SSPEC H9,18,C'---------------'                                         
         SSPEC H8,34,C'P'                                                       
         SSPEC H9,34,C'S'                                                       
         SSPEC H8,36,C'RELEASE'                                                 
         SSPEC H9,36,C'--DATE--'                                                
         SSPEC H8,46,C'RECALL'                                                  
         SSPEC H9,45,C'--DATE--'                                                
         SSPEC H8,54,C'TYPE'                                                    
         SSPEC H9,54,C'----'                                                    
         SSPEC H8,60,C'PRODUCT LIST'                                            
         SSPEC H9,60,C'------------------------'                                
         SSPEC H8,85,C'OTHER DETAILS'                                           
         SSPEC H9,85,C'--------------------------'                              
         EJECT                                                                  
* VALIDATE PRODUCT LIST AND BUILD PROD LIST ELEMENT                             
         SPACE                                                                  
         DROP  R7,RB,RC                                                         
VPRDL    NMOD1 0,**VPRDL*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERRA            NO                                           
         LA    R6,ELEM                                                          
         USING CMLPRDEL,R6                                                      
         MVI   CMLPRDEL,X'20'      ELEM CODE                                    
         GOTO1 ANY                                                              
         CLC   =CL7'PRD=ALL',WORK                                               
         BNE   VPRDL06                                                          
         MVI   CMLPRDLN,3                                                       
         MVI   CMLPRDS,X'FF'                                                    
         B     EXIT2                                                            
VPRDL06  MVI   CMLPRDLN,2          ELEM LENGTH EMPTY                            
         MVC   FLDH,TRAPLSTH       SAVE HEADER                                  
         MVC   FLD(L'TRAPLST),TRAPLST                                           
         GOTO1 SCANNER,DMCB,FLDH,(12,BLOCK),0                                   
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3                                                            
         BZ    MISSERRA                                                         
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         LA    R5,ELEM+2           SAVE PRODUCT CODE HERE                       
         LR    RE,R5                                                            
VPRDL10  LA    R0,256              MAX COUNT BUG CATCHER                        
         L     R1,ASVCLIST         TABLE OF CLIENT PROD CODES                   
VPRDL12  CLI   0(R1),0             AT END OF TABLE?                             
         BE    PRDERRA             YES, ERROR                                   
         CLC   0(3,R1),12(R4)      THIS A VALID PROD CODE                       
         BE    VPRDL14                                                          
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,VPRDL12                                                       
         DC    H'0'                                                             
VPRDL14  CLC   12(3,R4),=C'POL'    THIS ILLEGAL                                 
         BE    PRDINV                                                           
         CLC   12(3,R4),=C'AAA'    THIS ILLEGAL                                 
         BE    PRDINV                                                           
         LR    RF,RE                                                            
VPRDL16  CR    R5,RF               THIS FIRST/CURR ENTRY                        
         BE    VPRDL18             YES, NO DUPES                                
         CLC   0(1,RF),3(R1)       THIS A DUPE                                  
         BE    DUPRDER                                                          
         LA    RF,1(,RF)                                                        
         B     VPRDL16                                                          
VPRDL18  MVC   0(1,R5),3(R1)       SAVE BINARY PRODUCT CODE                     
         LA    R4,32(,R4)          NEXT SCANNER BLOCK                           
         LA    R5,1(,R5)           BUMP ELEMENT POINTER                         
         IC    R1,ELEM+1           GET ELEM LENGTH                              
         LA    R1,1(,R1)                                                        
         STC   R1,ELEM+1                                                        
         BCT   R3,VPRDL10                                                       
         CLI   CMLPRDLN,2          WERE ANY PROD CODES FOUND                    
         BE    MISSERRA            NO                                           
         B     EXIT2                                                            
MISSERRA MVI   ERROR,MISSING                                                    
         B     TRAPERRA                                                         
PRDERRA  MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         B     TRAPERRA                                                         
PRDINV   MVI   ERROR,INVPRDCD      POL & AAA INVALID PROD                       
TRAPERRA GOTO1 ERREX                                                            
DUPRDER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DPRDMS),DPRDMS                                         
         BCTR  RE,0                MAKE ALL FLDS RELATIVE TO 1                  
         SR    R5,RE               CURRENT DUP                                  
         SR    RF,RE               PREV DUP                                     
         LA    R1,CONHEAD+14                                                    
         EDIT  (RF),(2,(R1))                                                    
         LA    R1,CONHEAD+19                                                    
         EDIT  (R5),(2,(R1))                                                    
         GOTO1 ERREX2                                                           
         DROP  R6                                                               
DPRDMS   DC    C'* ERROR * FLD XX = YY IN PRODUCT LIST'                         
         EJECT                                                                  
* VALIDATE PRODUCTION HOUSE                                                     
         SPACE                                                                  
         DROP  RB,RC                                                            
         USING CMLDTAEL,R6                                                      
VHSE     NMOD1 0,**VHSE**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLC   HOLDCML,=8C'9'      CML ID ALL 9'S                               
         BNE   EXIT2               NO,                                          
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRHKEY,R4                                                        
         MVC   PRHKID,=XL2'0A29'                                                
         MVC   PRHKAM,BAGYMD                                                    
         MVC   PRHKPRH,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRDHSER                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVC   CMLTITLE+6(9),=CL9'=PROD HSE'                                    
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    VHSE10              NO GETREC NEEDED                             
         GOTO1 GETREC                                                           
VHSE10   MVC   AIO,AIO1                                                         
         B     EXIT2                                                            
PRDHSER  MVI   ERROR,INVPRHSE      NO PROD HOUSE ON FILE                        
         GOTO1 ERREX                                                            
         DROP  R4,R6,RB,RC                                                      
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   HDHK10                 NO                                        
         MVC   H3+42(11),=C'TURN-AROUND'                                        
HDHK10   CLC   PRODFTR,=C'ALL'                                                  
         BNE   EXIT2                                                            
         MVC   H4+44(8),=C'PROD=ALL'                                            
         B     EXIT2                                                            
         DROP  RB,RC                                                            
         EJECT                                                                  
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
         SPACE                                                                  
GENR     NMOD1 0,**GENR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   SVPROF+9,C'Y'       AUTO TURNAROUND                              
         BE    GENR10              YES                                          
         CLI   SVPROF+9,C'D'       AUTO TURNAROUND                              
         BNE   EXIT2               NO                                           
GENR10   XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVC   REQUEST(2),=C'TZ'                                                
         MVC   REQUEST+2(2),AGENCY                                              
         MVC   REQUEST+4(23),=CL23'*.COM.LIST..DDS,T/A....'                     
         MVC   REQUEST+27(1),QMED                                               
         MVI   REQUEST+28,C'.'                                                  
         MVC   REQUEST+29(3),QCLT                                               
         MVI   REQUEST+32,C'.'                                                  
         MVC   REQUEST+33(4),HOLDCML                                            
         MVC   REQUEST+37(2),=C'.*'                                             
         CLI   SVPROF+9,C'D'       IF REQ IS DDS                                
         BE    *+10                BYPASS                                       
         MVC   REQHDR+11(2),T216FFD+17                                          
         XC    FLD,FLD                                                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',FLD,REQHDR                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
EXIT2    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPRH                                                        
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAE4D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
* OFFLINE REPORT                                                                
         SPACE                                                                  
PRTLINE  DSECT                                                                  
         DS    CL2                                                              
PCML     DS    CL8                                                              
         DS    CL2                                                              
PSLN     DS    CL3                                                              
         DS    CL2                                                              
PTITLE   DS    CL15                                                             
         DS    CL1                                                              
PSOLO    DS    CL1                                                              
         DS    CL1                                                              
PRELSE   DS    CL8                                                              
         DS    CL1                                                              
PRECALL  DS    CL8                                                              
         DS    CL2                                                              
PTYPE    DS    CL4                                                              
         DS    CL1                                                              
PLIST    DS    CL24                                                             
         DS    CL1                                                              
PMISC    DS    CL26                                                             
         SPACE 3                                                                
* ONLINE LIST                                                                   
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    C                                                                
LCML     DS    CL8                                                              
         DS    C                                                                
LLEN     DS    CL2                                                              
         DS    C                                                                
LTITLE   DS    CL15                                                             
         DS    C                                                                
LRELSE   DS    CL8                                                              
         DS    C                                                                
LRECALL  DS    CL8                                                              
         DS    CL2                                                              
LTYPE    DS    CL3                                                              
         DS    C                                                                
         DS    CL2                                                              
         DS    CL15                                                             
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR32RR DS    F                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
PRDPTR   DS    F                                                                
PRDCTR   DS    CL1                                                              
RECCT    DS    H                                                                
SCANCT   DS    XL1                                                              
DATE     DS    CL6                                                              
HOLDSEQ  DS    XL3                                                              
HOLDCML  DS    CL8                                                              
COMPKEY  DS    CL13                COMPARE KEY FOR ONLINE LIST                  
COMPKEYL DS    CL1                                                              
FILTERS  DS    0CL14                                                            
PRODFTR  DS    CL3                 PRODFTR MUST BE FOLLOWED BY PRDFTR           
PRDFTR   DS    XL1                                                              
TYPEFTR  DS    CL4                                                              
SLNFTR   DS    CL1                                                              
DATEFTR  DS    CL3                                                              
DATESFTR DS    CL1                                                              
DELFTR   DS    XL1                 NON-ZERO SHOW ONLY DELETED CML'S             
HOLDSIGN DS    CL1                                                              
REQHDR   DS    CL26                REQUEST HEADER FOR TURNAROUND REPORT         
REQUEST  DS    CL80                                                             
SVPRDLST DS    CL256                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPTRA32   03/06/07'                                      
         END                                                                    
