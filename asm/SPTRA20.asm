*          DATA SET SPTRA20    AT LEVEL 012 AS OF 02/15/05                      
*PHASE T21620A                                                                  
         TITLE 'T21620 NETWORK TRAFFIC - PROG DAY SPREAD MAINT/LIST'            
*  TITLE: T21620 - TRAFFIC NETWORK PROGRAM DAY SPREAD MAINT/LIST      *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG                                              *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
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
*                                                                     *         
*  LEV  6    OCT31/90 SHOW NEW PROGRAM ROTATION FIELD                 *         
*  LEV  7    FEB01/91 CHANGE TRAFFIC OVERRIDE ELEM FROM DD TO E3      *         
*  LEV  8    SEP10/92 UPDATE 92 ELEM DESC                             *         
*  LEV  9 EJOR 30JUN93 ALLOW UPDATE OF FAX NUMBER                     *         
*  LEV 10 BGRI  9FEB95 ADD FAX NUMBER TO PRINTED REPORT               *         
*  LEV 11 SMUR 26JUL04 SOX                                            *         
*  LEV 12 BGRI 15FEB05 FIX BLANKING FAX IF NO ENTRY                   *         
*                                                                     *         
***********************************************************************         
T21620   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1620**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR20RR                                                      
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BE    ADDELER                                                          
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEYS                                                                 
         SPACE                                                                  
VK       CLI   ACTNUM,ACTDEL       ACTION DELETE INVALID                        
         BE    ADDELER                                                          
         CLI   ACTNUM,ACTADD       ACTION ADD INVALID                           
         BE    ADDELER                                                          
         SPACE                                                                  
VK10     BAS   RE,VMD              FAKE VALIDATE MEDIA                          
         SPACE                                                                  
         LA    R2,TRANETH          NETWORK                                      
         BAS   RE,VNET                                                          
         SPACE                                                                  
         LA    R2,TRAPROGH         PROGRAM                                      
         XC    PROGRAM,PROGRAM                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK34                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK40                                                             
         B     MISSERR                                                          
VK34     BAS   RE,VPROG                                                         
         SPACE                                                                  
VK40     LA    R2,TRADTEH          END DATE                                     
         XC    DATES,DATES                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK44                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK50                                                             
         B     MISSERR                                                          
VK44     BAS   RE,VDTE                                                          
         SPACE                                                                  
* VALIDATE FILTERS *                                                            
VK50     LA    R2,TRAFLTRH                                                      
         BAS   RE,VFTR                                                          
         SPACE                                                                  
* BUILD KEY *                                                                   
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,PROGRAM                                                 
         MVC   NPGKEND,DATEP                                                    
         EJECT                                                                  
         CLI   ACTNUM,ACTLIST      ONLY SET COMPARE KEY                         
         BNE   EXIT                FOR LIST                                     
         SPACE                                                                  
* CHECK FOR ANY MISSING FIELDS (MUST ALL BE ENTERED LEFT TO RIGHT)              
         SPACE                                                                  
         OC    PROGRAM,PROGRAM                                                  
         BZ    VK70                                                             
         OC    NETWORK,NETWORK                                                  
         BNZ   VK70                                                             
         LA    R2,TRANETH                                                       
         B     MISSERR                                                          
         SPACE 3                                                                
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
         SPACE                                                                  
VK70     LA    R0,12               MAX KEY COMPARE (-1)                         
         LA    R1,KEY+12           START AT END OF KEY                          
VK72     CLI   0(R1),0             NONZERO IS VALID COMPARAND                   
         BNE   VK74                FOUND END OF COMPARE KEY                     
         BCTR  R0,0                DECREMENT LENGTH                             
         BCT   R1,VK72                                                          
VK74     STC   R0,COMPKEYL         SAVE COMPARE LENGTH                          
         MVC   COMPKEY,KEY                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD *                                                             
         SPACE                                                                  
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
         CLI   ACTNUM,ACTADD       ACTION ADD INVALID                           
         BE    ADDELER                                                          
         CLI   ACTNUM,ACTDEL       ACTION DELETE INVALID                        
         BE    ADDELER                                                          
         SPACE                                                                  
         MVI   ELCODE,X'E3'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
* VALIDATE DAY SPREAD HERE                                                      
         SPACE                                                                  
         LA    R2,TRADAYSH                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR20                                                             
         ZIC   R3,5(R2)                                                         
         SPACE                                                                  
         GOTO1 DAYVAL,DMCB,((R3),8(R2)),TRDAYS,TRSTDAY                          
         CLI   TRDAYS,0                                                         
         BE    DSERR                                                            
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING NPGELE3,R6                                                       
         MVI   NPGTELEM,X'E3'                                                   
         MVI   NPGTLEN,16                                                       
         MVC   NPGTDAY,TRDAYS                                                   
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
* VALIDATE FAX NUMBER (IF ANY)                                                  
         SPACE                                                                  
VR20     LA    R2,TRAFAXH                                                       
         CLI   5(R2),0             ANY ENTRY?                                   
         BE    VR25                 NO, BLK FAX #                               
         TM    4(R2),X'08'         FIELD NUMERIC?                               
         BNZ   *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
VR25     L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    VR30                                                             
         SPACE                                                                  
* ADD BLANK ELEM                                                                
         SPACE                                                                  
         CLI   5(R2),0             NO ELEM - NO FAX                             
         BE    VRX                  JUST GET OUT                                
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'0328'                                                 
         GOTO1 ADDELEM                                                          
         B     VR25                                                             
         SPACE                                                                  
VR30     DS   0H                                                                
         XC    NPGTRFAX-NPGEL03(L'NPGTRFAX,R6),NPGTRFAX-NPGEL03(R6)             
         CLI   5(R2),0             NO ELEM - NO FAX                             
         BE    VRX                  JUST GET OUT                                
         SPACE                                                                  
         MVC   NPGTRFAX-NPGEL03(L'NPGTRFAX,R6),TRAFAX                           
VRX      B     DR                                                               
         EJECT                                                                  
* DISPLAY KEY *                                                                 
         SPACE                                                                  
DK       L     R4,AIO                                                           
         USING NPGKEY,R4                                                        
         SPACE                                                                  
         XC    FLD(L'TRAPROG),FLD                                               
         MVC   FLD(6),NPGKPROG                                                  
         CLC   TRAPROG,FLD                                                      
         BE    *+14                                                             
         MVC   TRAPROG,FLD                                                      
         OI    TRAPROGH+6,X'80'                                                 
         SPACE                                                                  
         XC    FLD(L'TRADTE),FLD                                                
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,FLD)                                  
         CLC   TRADTE,FLD                                                       
         BE    *+14                                                             
         MVC   TRADTE,FLD                                                       
         OI    TRADTEH+6,X'80'                                                  
         SPACE                                                                  
* DISPLAY FILTER HERE *                                                         
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY RECORD *                                                              
         SPACE                                                                  
DR       L     R4,AIO                                                           
         USING NPGKEY,R4                                                        
         XC    FLD(L'TRADAYS),FLD                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'E3'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         USING NPGELE3,R6                                                       
         GOTO1 UNDAY,DMCB,(2,NPGTDAY),(5,FLD)                                   
DR10     CLC   TRADAYS,FLD                                                      
         BE    *+14                                                             
         MVC   TRADAYS,FLD                                                      
         OI    TRADAYSH+6,X'80'                                                 
         SPACE                                                                  
         LR    R6,R4                                                            
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R6                                                       
         XC    FLD(L'TRAPRGN),FLD                                               
         MVC   FLD(L'NPGNAME),NPGNAME                                           
         CLC   TRAPRGN,FLD                                                      
         BE    *+14                                                             
         MVC   TRAPRGN,FLD                                                      
         OI    TRAPRGNH+6,X'80'                                                 
         SPACE                                                                  
         XC    FLD(L'TRANDYS),FLD                                               
         GOTO1 UNDAY,DMCB,(2,NPGDAY),(5,FLD)                                    
         CLC   TRANDYS,FLD                                                      
         BE    *+14                                                             
         MVC   TRANDYS,FLD                                                      
         OI    TRANDYSH+6,X'80'                                                 
         SPACE                                                                  
         XC    FLD(L'TRANROT),FLD                                               
         CLI   NPGROT,0            IS THERE ANY ROTATION                        
         BE    DR20                                                             
         GOTO1 UNDAY,DMCB,(2,NPGROT),(5,FLD)                                    
DR20     CLC   TRANROT,FLD                                                      
         BE    *+14                                                             
         MVC   TRANROT,FLD                                                      
         OI    TRANROTH+6,X'80'                                                 
         SPACE                                                                  
         XC    FLD(L'TRATIME),FLD                                               
         GOTO1 UNTIME,DMCB,NPGTIME,FLD                                          
         CLC   TRATIME,FLD                                                      
         BE    *+14                                                             
         MVC   TRATIME,FLD                                                      
         OI    TRATIMEH+6,X'80'                                                 
         SPACE                                                                  
         MVC   TRAFAX,SPACES                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         USING NPGEL03,R6                                                       
         OC    NPGTRFAX,NPGTRFAX                                                
         BZ    DR30                                                             
         MVC   TRAFAX,NPGTRFAX                                                  
DR30     OI    TRAFAXH+6,X'80'     XMIT FIELD                                   
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GET THIS KEY                             
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         SPACE                                                                  
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,PROGRAM                                                 
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
         MVC   P(21),=CL21'NO PROGRAM RECS FOUND'                               
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   EXIT                YES                                          
LR22     ZIC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,LRCLC            SEE IF PAST KEY                              
         BNE   EXIT                YES, ALL DONE                                
LR30     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         BAS   RE,LFTR             FILTER                                       
         BNE   LR20                                                             
         SPACE                                                                  
         LR    R6,R4                                                            
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
LRCLC    CLC   KEY(0),COMPKEY                                                   
         EJECT                                                                  
* FORMAT OFFLINE REPORT                                                         
         SPACE                                                                  
LRR      MVC   PPROG,NPGKPROG                                                   
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,PEDATE)                               
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R6                                                       
         SPACE                                                                  
         MVC   PPROGN,NPGNAME                                                   
         SPACE                                                                  
         GOTO1 UNTIME,DMCB,NPGTIME,PTIME                                        
         GOTO1 UNDAY,(R1),NPGDAY,PMDAYS                                         
         CLI   NPGROT,0            IS THERE ANY ROTATION                        
         BE    LRR10                NO                                          
         GOTO1 (RF),(R1),NPGROT,PMROT                                           
         SPACE                                                                  
LRR10    LR    R6,R4                                                            
         MVI   ELCODE,X'E3'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR16                                                            
         USING NPGELE3,R6                                                       
         GOTO1 UNDAY,DMCB,NPGTDAY,PTDAYS                                        
         SPACE                                                                  
LRR16    LR    R6,R4                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR20                                                            
         USING NPGSPEL,R6                                                       
         OC    NPGTRFAX,NPGTRFAX                                                
         BZ    LRR20                                                            
         MVC   PFAX,NPGTRFAX                                                    
         SPACE                                                                  
LRR20    GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         DROP  R6                                                               
         EJECT                                                                  
* FORMAT ONLINE LIST                                                            
         SPACE                                                                  
LRL      MVC   LISTAR,SPACES                                                    
*         MVC   LNET,NETWORK                                                    
         SPACE                                                                  
         MVC   LPROG,NPGKPROG                                                   
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,LEDATE)                               
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R6                                                       
         SPACE                                                                  
         MVC   LPROGN,NPGNAME                                                   
         MVC   BYTE,NPGDAY                                                      
         CLI   NPGROT,0                                                         
         BE    *+10                                                             
         MVC   BYTE,NPGROT                                                      
         GOTO1 UNDAY,DMCB,BYTE,LMDAYS                                           
         GOTO1 UNTIME,DMCB,NPGTIME,LTIME                                        
         LR    R6,R4                                                            
         MVI   ELCODE,X'E3'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRL10                                                            
         USING NPGELE3,R6                                                       
         GOTO1 UNDAY,DMCB,NPGTDAY,LTDAYS                                        
         SPACE                                                                  
LRL10    MVC   LFAX,SPACES                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRL20                                                            
         USING NPGEL03,R6                                                       
         OC    NPGTRFAX,NPGTRFAX                                                
         BZ    LRL20                                                            
         MVC   LFAX,NPGTRFAX                                                    
LRL20    GOTO1 LISTMON                                                          
         B     LR20                                                             
         EJECT                                                                  
* FAKE VALIDATE MEDIA *                                                         
         SPACE                                                                  
VMD      NTR1                                                                   
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
         B     EXIT                                                             
         SPACE 3                                                                
* VALIDATE NETWORK                                                              
         SPACE                                                                  
VNET     NTR1                                                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, MUST BE ENTRY                            
         GOTO1 ANY                                                              
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
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         L     R4,AIO                                                           
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,NETMKT                                                        
         B     EXIT                                                             
         EJECT                                                                  
VPROG    NTR1                                                                   
         SPACE                                                                  
         OC    NETWORK,NETWORK     WAS NETWORK ENTERED                          
         BZ    MISSNET                                                          
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PROGERR                                                          
         SPACE                                                                  
         MVC   PROGRAM,WORK                                                     
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 3                                                                
* VALIDATE DATE - END DATE FOR PROGRAM *                                        
         SPACE                                                                  
VDTE     NTR1                                                                   
         SPACE                                                                  
         SPACE                                                                  
         LA    R4,TRADTE                                                        
         GOTO1 DATVAL,DMCB,TRADTE,STDATE                                        
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,STDATE),(2,DATEP)                                 
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE FILTERS *                                                            
         SPACE                                                                  
VFTR     NTR1                                                                   
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTR74              NO                                           
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
         MVC   CONHEAD(L'OPTHELP),OPTHELP                                       
         B     ERREXIT                                                          
VFTR08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(5,BLOCK+64)                          
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK+64            ADDRESS OF FIRST BLOCK                    
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         MVI   HOLDSIGN,0                                                       
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         DATE                                         
         BNE   VFTR80                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(2,DATEOPT)                                 
         CLM   R6,1,1(R4)          WAS THERE ONLY 1 DATE                        
         BE    VFTR18              YES                                          
         LA    R5,1(R6,R5)                                                      
         GOTO1 DATVAL,(R1),(0,(R5)),DATE                                        
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(2,DATE2OPT)                                
         B     VFTR70                                                           
VFTR18   MVC   DATESOPT,HOLDSIGN                                                
VFTR70   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTR74   OI    4(R2),X'20'         SET VALIDATED                                
         B     EXIT                                                             
VFTR80   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTMSG+L'OPTHELP),OPTMSG                               
         B     ERREXIT                                                          
OPTMSG   DC    C'* ERROR *'                                                     
OPTHELP  DC    C'VALID FILTERS - DATE='                                         
VFTRCLCA CLC   12(0,R4),=CL4'DATE'                                              
         EJECT                                                                  
* FILTER UNITS ON ENTRIES IN OPTION FIELD WHILE DOING LIST FUNCTIONS *          
         SPACE                                                                  
LFTR     NTR1                                                                   
         OC    FILTERS,FILTERS                                                  
         BZ    EXIT                                                             
         OC    DATEOPT,DATEOPT                                                  
         BZ    EXIT                                                             
         CLI   DATESOPT,0                                                       
         BE    LFTR12                                                           
         CLI   DATESOPT,X'6E'      GREATER THAN                                 
         BE    LFTR14                                                           
         CLI   DATESOPT,X'4C'      LESS THAN                                    
         BE    LFTR16                                                           
         DC    H'0'                                                             
LFTR12   OC    DATE2OPT,DATE2OPT                                                
         BNZ   LFTR18                                                           
         CLC   DATEOPT,NPGKEND                                                  
         BE    LFTR20                                                           
         B     LFTRNE                                                           
LFTR14   CLC   DATEOPT,NPGKEND                                                  
         BNH   LFTR20                                                           
         B     LFTRNE                                                           
LFTR16   CLC   DATEOPT,NPGKEND                                                  
         BNL   LFTR20                                                           
         B     LFTRNE                                                           
LFTR18   CLC   DATEOPT,NPGKEND                                                  
         BH    LFTRNE                                                           
         CLC   DATE2OPT,NPGKEND                                                 
         BL    LFTRNE                                                           
         SPACE                                                                  
LFTR20   CR    R1,R1                                                            
         B     EXIT                                                             
LFTRNE   LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         OC    STDATE(12),STDATE                                                
         BZ    HDHK10                                                           
         MVC   H3+28(6),=C'PERIOD'                                              
         GOTO1 DATCON,DMCB,(0,STDATE),(8,H3+35)                                 
         MVC   H3+44(2),=C'TO'                                                  
         GOTO1 DATCON,(R1),(0,ENDATE),(8,H3+48)                                 
HDHK10   MVC   H4+12(4),NETWORK                                                 
         B     EXIT                                                             
         SPACE 2                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
ADDELER  LA    R1,ADDELMS                                                       
         B     ERREXITM                                                         
DSERR    LA    R1,DSERMS                                                        
         B     ERREXITM                                                         
PROGERR  LA    R1,PROGERMS                                                      
         LA    R2,TRAPROGH                                                      
         B     ERREXITM                                                         
PRGDATER LA    R1,PRGDATMS                                                      
         LA    R2,TRADTEH                                                       
         B     ERREXITM                                                         
NETERR   LA    R1,NETERMS                                                       
ERREXITM MVC   CONHEAD(10),=C'* ERROR * '                                       
         MVC   CONHEAD+10(50),0(R1)                                             
ERREXIT  GOTO1 ERREX2                                                           
         SPACE 3                                                                
         B     TRAPERR                                                          
MISSNET  LA    R2,TRANETH                                                       
         B     MISSERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ADDELMS  DC    CL50'NO ACTION ADD OR DELETE *'                                  
PROGERMS DC    CL50'NO PROGRAM FOUND *'                                         
NETERMS  DC    CL50'NO NETWORK FOUND *'                                         
PRGDATMS DC    CL50'NO PROGRAM RECORD WITH END DATE FOR THIS DATE *'            
DSERMS   DC    CL50'ERROR IN DAY SPREAD ENTRY *'                                
         SPACE                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,36,C'TRAFFIC PROGRAMS LIST'                                   
         SSPEC H2,36,C'---------------------'                                   
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'NETWORK'                                                  
         SSPEC H4,85,RUN                                                        
         SSPEC H4,73,REPORT                                                     
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'PROGRAM'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,13,C'PROGRAM NAME'                                            
         SSPEC H9,13,C'------------'                                            
         SSPEC H8,33,C'END DATE'  37                                            
         SSPEC H9,33,C'--------'                                                
         SSPEC H8,45,C'TIME'      53                                            
         SSPEC H9,45,C'-----'                                                   
         SSPEC H8,59,C'MEDIA DAY' 70                                            
         SSPEC H9,59,C'---------'                                               
         SSPEC H8,71,C'MEDIA ROT' 84                                            
         SSPEC H9,71,C'---------'                                               
         SSPEC H8,83,C'TRAFFIC DAY' 99                                          
         SSPEC H9,83,C'-----------'                                             
         SSPEC H8,95,C'PROGRAM FAX'                                             
         SSPEC H9,95,C'-----------'                                             
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         PRINT OFF                                                              
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRADED                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR20RR DS    F                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
NETMKT   DS      H                                                              
         SPACE                                                                  
DATE     DS    CL6                                                              
         SPACE                                                                  
* STDATE, ENDATE, STDATEP, AND ENDATEP MUST BE TOGETHER AND IN ORDER            
         SPACE                                                                  
DATES    DS    0CL18                                                            
DATEP    DS    XL2                                                              
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
STDATEP  DS    XL2                                                              
ENDATEP  DS    XL2                                                              
NETWORK  DS    CL4                                                              
PROGRAM  DS    CL6                                                              
PROGDAY  DS    XL1                                                              
FILTERS  DS   0CL5                                                              
DATEOPT  DS    XL2                                                              
DATE2OPT DS    XL2                                                              
DATESOPT DS    CL1                                                              
HOLDSIGN DS    CL1                                                              
COMPKEY  DS    CL13                                                             
COMPKEYL DS    XL1                                                              
TRDAYS   DS    XL1                                                              
TRSTDAY  DS    XL1                                                              
         EJECT                                                                  
* OFFLINE REPORT LINE                                                           
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PPROG    DS    CL6                                                              
         DS    CL4                                                              
PPROGN   DS    CL16                                                             
         DS    CL4 8                                                            
PEDATE   DS    CL8                                                              
         DS    CL4 8                                                            
PTIME    DS    CL10                                                             
         DS    CL4 7                                                            
PMDAYS   DS    CL9                                                              
         DS    CL3 5                                                            
PMROT    DS    CL9                                                              
         DS    CL3 6                                                            
PTDAYS   DS    CL9                                                              
         DS    CL3                                                              
PFAX     DS    CL12                                                             
         SPACE 3                                                                
* ONLINE LIST LINE                                                              
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LPROG    DS    CL6                                                              
         DS    CL1                                                              
LPROGN   DS    CL16                                                             
         DS    CL1                                                              
LEDATE   DS    CL8                                                              
         DS    CL1                                                              
LMDAYS   DS    CL8                                                              
         DS    CL1                                                              
LTDAYS   DS    CL8                                                              
         DS    CL1                                                              
LTIME    DS    CL11                                                             
         DS    CL1                                                              
LFAX     DS    CL12                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPTRA20   02/15/05'                                      
         END                                                                    
