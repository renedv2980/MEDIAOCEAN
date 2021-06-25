*          DATA SET SPTRA05    AT LEVEL 020 AS OF 08/28/09                      
*PHASE T21605B                                                                  
         TITLE 'T21605 SHIPPING RECORD DISPLAY, CHANGE, ADD'                    
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30) SHIPPING RECORD TO BE MAINTAINED         
*             AIO2 - PRIMARY CMML REC                                           
*             AIO3 - PARTNER CMML REC, IF ANY                                   
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
*        R7 - WORK                                                              
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
***********************************************************************         
*                                                                     *         
*  LEV  13    FEB24/88 FIX BUG NOT SHOWING NO SHIP ON SCREEN          *         
*  LEV  14    JUL27/92 ADD LOCKET                                     *         
*  LEV  15  SMUR NOV10/99 USE RECUP FROM FACPAK                       *         
*  LEV  16  SMUR APR10/01 USE TRAFFIC OFFICE                          *         
*  LEV  17  BGRI JUL29/04 SOX                                         *         
*  LEV  18  MNAS APR08/07 CHANGE VALILOC CALLS TO INCLUDE PARAM1 OF 0 *         
*  LEV  19  MHER JUL/09   ADID SUPPORT                                          
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21605   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21605*,RR=R3                                                 
         LA    R7,2048(,RB)                                                     
         LA    R7,2048(,R7)                                                     
         USING T21605,RB,R7                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR05RR                                                      
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORD ILLEGAL                        
         BE    DELMODE                                                          
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       CLI   ACTNUM,ACTDEL       IF ACTION DEL, INVALID                       
         BE    DELMODE                                                          
         XC    FLD(ENDHOLD-FLD),FLD                                             
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, MUST BE ENTRY                            
         GOTO1 VALICLT                                                          
*                                                                               
* READ T1 PROFILE FOR LOCKET CALLS IN CONTROLLER *                              
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T1'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVT1PROF,DATAMGR                               
*                                                                               
         LA    R2,TRASTAH          STATION                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALISTA                                                          
*                                                                               
         LA    R2,TRACMLH          COMMERCIAL                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         CLI   5(R2),8                                                          
         BL    BADCMMLN                                                         
         CLI   5(R2),12            MUST BE 8 CHAR                               
         BH    BADCMMLN            NO, ERROR                                    
         GOTO1 ANY                 MOVE INPUT TO WORK                           
*                                                                               
         MVI   SVCMMLF,C'N'                                                     
         MVC   SVCMML,WORK                                                      
         XC    SVCMMLP,SVCMMLP                                                  
* ALWAYS TRY TO PACK CMML                                                       
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SVCMMLP                                 
         BNE   VK2                                                              
         CLI   5(R2),8             IF ISCI, DO NOT SET ADID FLAG                
         BE    VK2                                                              
         MVI   SVCMMLF,C'Y'        SET CMML IS ADID                             
*                                                                               
VK2      BRAS  RE,VCML                                                          
*                                                                               
* NOW BUILD KEY                                                                 
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING SHPKEY,R4                                                        
         MVC   SHPKID,=XL2'0A25'                                                
         MVC   SHPKAM,BAGYMD       A-M                                          
         MVC   SHPKCLT,BCLT        CLT                                          
         MVC   SHPKMKT,BMKT        MKT                                          
         MVC   SHPKSTA,BSTA        STA                                          
         MVC   SHPKCSEQ,SVSEQ                                                   
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         GOTO1 VSOXERR                                                          
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
*                                                                               
* SEE IF LOCKET HAS SHIP RECS (0A25) LOCKED OUT FOR A SOON JOB *                
*                                                                               
VR       MVC   DUB,=X'E3010A2500000000' T=TEST, 01 = 1 ENTRY                    
         GOTO1 VALILOC,0                 0A25 RECS                              
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
         GOTO1 VSOXERR                                                          
*                                                                               
VR01     DS    0H                                                               
         L     R4,AIO                                                           
         MVC   SVKEY,KEY                                                        
         USING SHPKEY,R4                                                        
         MVC   BAGYMD,SHPKAM                                                    
         MVC   BCLT,SHPKCLT                                                     
         MVC   BMKT,SHPKMKT                                                     
         MVC   BSTA,SHPKSTA                                                     
         MVC   SVSEQ,SHPKCSEQ                                                   
         DROP  R4                                                               
         LA    R2,TRAACTNH         FIRST FLD                                    
         MVI   ELCODE,X'10'        DATA ELEMENT                                 
         L     R6,AIO                                                           
         BAS   RE,GETEL            GET SHIP DATA ELEM                           
         BNE   VR30                NONE IN RECORD                               
         USING SHPDTAEL,R6                                                      
*                                                                               
VR10     MVC   SVSHPFTD,SHPFTD                                                  
         MVC   SVSHPLTD,SHPLTD                                                  
* SEE WHAT ACTION NEEDED - CHA DEL (NO ENTRY = NO CHANGE)                       
         ST    R2,LASTACTN                                                      
         CLI   5(R2),0             ANY DATA ENTERED (ACTION FLD)                
         BE    VR12                NO                                           
         GOTO1 ANY                                                              
         CLI   WORK,C'C'           CHANGE                                       
         BE    VR40                                                             
         CLI   WORK,C'D'           DELETE                                       
         BE    VR60                                                             
         CLI   WORK,C'R'           RESHIP                                       
         BE    VR70                                                             
         CLI   WORK,C'A'           ADD                                          
         BE    BADADDER            CAN'T ADD OVER EXISTING ELEM                 
         B     INVALOPT            ADD,CHANGE,DELETE ONLY VALID                 
*                                                                               
VR12     GOTO1 VSAME               GO VALIDATE NO CHANGED ENTRIES               
         L     R2,DMCB             GET UPDATED R2                               
         LA    R1,TRATAGH          SEE IF AT END OF SCREEN                      
         CR    R2,R1                                                            
         BL    VR14                                                             
         DC    H'0'                                                             
*                                                                               
VR14     MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   VR30                NO MORE ELEMENTS                             
*                                                                               
VR16     CLI   SHPPIG,0            IS THIS AN INVERTED INFO ONLY                
         BNE   VR14                YES, BYPASS IT                               
         B     VR10                                                             
         EJECT                                                                  
*===============================================================                
* CHECKED ALL EXISTING ELEMENTS,                                                
* NOW CAN ONLY HAVE ADDS OR BLANK LINES                                         
*===============================================================                
                                                                                
VR30     XC    ELEM,ELEM                                                        
         XC    SVCMML2,SVCMML2                                                  
         XC    SVSEQ2,SVSEQ2                                                    
         XC    SVOLDCML,SVOLDCML                                                
         LA    R6,ELEM                                                          
         USING SHPDTAEL,R6                                                      
         MVI   SHPDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   SHPDTALN,SHPDTAX-SHPDTAEL ELEMENT LENGTH                         
         MVC   SHPCMML,SVCMML      CMML ID                                      
         CLI   SVCMMLF,C'Y'        TEST TO USE ADID                             
         BNE   *+10                                                             
         MVC   SHPCMML,SVCMMLP                                                  
*                                                                               
VR32     CLI   5(R2),0             ANY DATA ENTERED (ACTION FLD)                
         BE    VR34                NO                                           
         ST    R2,LASTACTN                                                      
         GOTO1 ANY                                                              
         CLI   WORK,C'C'           CHANGE                                       
         BE    INVELMER            CAN'T CHANGE, NO ELEM                        
         CLI   WORK,C'D'           DELETE                                       
         BE    INVELMER            CAN'T DELETE, NO ELEM                        
         CLI   WORK,C'A'           ADD                                          
         BE    VR80                GO VALIDATE ADD                              
         B     INVALOPT            ADD,CHANGE,DELETE ONLY VALID                 
*                                                                               
VR34     GOTO1 VBLK                GO VALIDATE NO DATA ENTRY                    
         L     R2,DMCB             GET UPDATED R2                               
         LA    R1,TRATAGH          SEE IF AT END OF SCREEN                      
         CR    R2,R1                                                            
         BL    VR32                                                             
         MVC   KEY(L'SVKEY),SVKEY                                               
         CLI   RECRDSW,0           WERE ANY RECS READ                           
         BE    DR                  NO, NO NEED FOR GETREC                       
         MVC   AIO,AIO2            DO NOT DESTROY UPDATED                       
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            NOW BACK TO REAL REC                         
         B     DR                  GO DISPLAY UPDATED RECORD                    
         EJECT                                                                  
* CHANGE ELEMENT (LINE)                                                         
*                                                                               
VR40     LLC   R0,0(R2)            GET LEN OF THIS FLD                          
         AR    R2,R0               POINT TO PARTNER CMML                        
         XC    FLD(L'TRACML2),FLD                                               
*                                                                               
         OC    SHPCMML2,SHPCMML2   WAS THERE A PIGGY-BACK                       
         BZ    VR42                NO                                           
         MVC   FLD(8),SHPCMML2                                                  
         MVC   FLD+8(4),SPACES                                                  
         TM    SHPNOSHP,SHPISADI   TEST ADI                                     
         BZ    VR42                                                             
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML2),FLD                                 
*                                                                               
VR42     CLC   8(12,R2),FLD                                                     
         BE    VR44                NO CHANGE                                    
*                                                                               
         GOTO1 ANY                                                              
         CLC   FLD,WORK            WAS THE CHANGE NO CHANGE                     
         BE    VR44                YES                                          
*                                                                               
         MVC   SVOLDCML,SHPCMML2   SAVE OLD PIGGY-BACK (FOR DEL)                
         CLC   =C'DELETE ',WORK    DELETE OLD PIGGY BACK                        
         BNE   *+14                NO                                           
         XC    SHPCMML2,SHPCMML2   DELETE                                       
         B     VR44                                                             
*                                                                               
         GOTO1 VCML2               GO VALIDATE COMMERCIAL                       
         MVC   SHPCMML2,SVCMML2                                                 
         CLI   SVCMMLF,C'Y'        TEST TO USE ADID                             
         BNE   *+10                                                             
         MVC   SHPCMML2,SVCMML2P                                                
*                                                                               
VR44     LLC   R0,0(R2)            GET LEN OF THIS FLD                          
         AR    R2,R0               POINT TO NEXT                                
*                                                                               
* NO CHANGE TO 1ST TELECAST DATE                                                
*                                                                               
         XC    FLD(L'TRAFTCD),FLD                                               
         GOTO1 DATCON,DMCB,(3,SHPFTD),(5,FLD)                                   
         CLC   8(L'TRAFTCD,R2),FLD                                              
         BNE   INVALFLD                                                         
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    FLD(L'TRALTCD),FLD                                               
         GOTO1 DATCON,DMCB,(3,SHPLTD),(5,FLD)                                   
         CLC   8(L'TRALTCD,R2),FLD                                              
         BE    VR46                                                             
*                                                                               
         GOTO1 VDTE                VALIDATE LAST TELECAST DATE                  
         CLC   SHPFTD,NEWDATE      MUST BE AFTER 1ST TELECAST                   
         BNL   DATERR                                                           
         MVC   SHPLTD,NEWDATE                                                   
*                                                                               
VR46     LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
* NO CHANGE ALLOWED TO INSTRUCT DATE                                            
*                                                                               
         XC    FLD(L'TRAINDT),FLD                                               
         OC    SHPQDATE,SHPQDATE   IS THERE AN INSTR DATE                       
         BZ    VR47                NO                                           
         GOTO1 DATCON,DMCB,(3,SHPQDATE),(5,FLD)                                 
*                                                                               
VR47     CLC   8(L'TRAINDT,R2),FLD                                              
         BNE   INVALFLD                                                         
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    FLD(L'TRASHDT),FLD                                               
         OC    SHPSHPDT,SHPSHPDT   IS THERE A SHIPPING DATE                     
         BZ    VR48                NO                                           
         GOTO1 DATCON,DMCB,(3,SHPSHPDT),(5,FLD)                                 
*                                                                               
VR48     CLC   8(L'TRASHDT,R2),FLD                                              
         BE    VR52                                                             
         GOTO1 VDTE                VALIDATE NEW SHIPPING DATE                   
         MVC   SHPSHPDT,NEWDATE                                                 
*                                                                               
VR52     LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         OC    SVCMML2,SVCMML2     IS THERE A PIGGY-BACK                        
         BZ    VR54                NO                                           
         XC    ELEM,ELEM                                                        
         MVC   ELEM(SHPDTAX-SHPDTAEL),0(R6)                                     
         GOTO1 ASHP                GO ADD PIGGY BACK ELEMENT                    
*                                                                               
VR54     OC    SVOLDCML,SVOLDCML   WAS THERE AN OLD PIGGY-BACK                  
         BZ    VR56                NO                                           
         GOTO1 DSHP                                                             
*                                                                               
VR56     B     VR14                                                             
         EJECT                                                                  
* DELETE THIS ELEMENT (LINE)                                                    
*                                                                               
VR60     OC    SHPCMML2,SHPCMML2   WAS THERE A PIGGY BACK CMML                  
         BZ    VR62                NO                                           
         MVC   SVOLDCML,SHPCMML2                                                
         GOTO1 DSHP                GO DELETE PIGGY BACK SHIP ELEM               
*                                                                               
VR62     GOTO1 VRECUP,DMCB,AIO,(R6)                                             
*                                                                               
         LA    R0,7                NUMBER OF FLDS TO NEXT LINE                  
VR64     LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R0,VR64                                                          
         CLC   ELCODE,0(R6)        NEXT ELEMENT SAME                            
         BE    VR16                YES, BYPASS NEXTEL                           
         B     VR30                END OF ELEMENTS                              
*                                                                               
* RESHIP ZEROS SHIP DATE, ALL ELSE SHOULD BE SAME                               
*                                                                               
VR70     XC    SHPSHPDT,SHPSHPDT   ZERO SHIP DATE                               
         LR    RE,R2                                                            
         LA    R0,5                SKIP OUT TO SHIPPED DATE FIELD               
VR74     LLC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         BCT   R0,VR74                                                          
         LLC   RF,0(RE)                                                         
         SH    RF,=H'9'                                                         
         OI    6(RE),X'80'         FORCE SEND FLD                               
         EX    RF,VR76                                                          
         B     VR12                GO CK ALL REST IS SAME                       
VR76     XC    8(0,RE),8(RE)       ZERO SHIP FLD ON SCREEN                      
         EJECT                                                                  
* ADD ELEMENT                                                                   
*                                                                               
VR80     LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             ANY PIGGY BACK CMML ENTERED                  
         BE    VR82                NO                                           
         GOTO1 ANY                                                              
         GOTO1 VCML2                                                            
*                                                                               
VR82     LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             NEED FIRST TELECAST DATE                     
         BE    MISSERR                                                          
         GOTO1 VDTE                VALIDATE 1ST TELECAST DATE                   
         MVC   SHPFTD,NEWDATE                                                   
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
* VALIDATE NEW LAST TELECAST DATE HERE                                          
*                                                                               
         GOTO1 VDTE                VALIDATE LAST TELECAST DATE                  
         CLC   SHPFTD,NEWDATE      MUST BE AFTER 1ST TELECAST                   
         BNL   DATERR                                                           
         MVC   SHPLTD,NEWDATE                                                   
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
* VALIDATE NEW CMML INSTR DATE HERE                                             
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VR83                YES                                          
         GOTO1 DATCON,DMCB,(5,0),(3,NEWDATE)                                    
         B     VR84                USE TODAYS'S DATE                            
*                                                                               
VR83     GOTO1 VDTE                VALIDATE INSTRUCTION DATE                    
*                                                                               
VR84     MVC   SHPQDATE,NEWDATE                                                 
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
* VALIDATE NEW SHIPPING DATE HERE                                               
         CLI   5(R2),0             ANY SHIPPING DATE                            
         BE    VR86                NO                                           
         GOTO1 VDTE                VALIDATE SHIPPING DATE                       
         MVC   SHPSHPDT,NEWDATE                                                 
*                                                                               
VR86     LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
* TEST HERE FOR PIGGY BACK, AND IF THERE WAS ADD ELEM                           
         OC    SVCMML2,SVCMML2     WAS THERE A PIGGY-BACK                       
         BZ    VR88                NO                                           
         GOTO1 ASHP                GO ADD PIGGY BACK ELEMENT                    
*                                                                               
VR88     MVI   SHPNOSHP,X'40'      SET UP AS MANUAL SHIP                        
         GOTO1 ADDELEM                                                          
         B     VR30                GO LOOK FOR MORE ADDS                        
         DROP  R6                                                               
         EJECT                                                                  
*===========================================================                    
* DISPLAY RECORD                                                                
*===========================================================                    
                                                                                
DR       LA    R2,TRAACTNH                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR30                GO CLEAR THE REST OF THE SCREEN              
         USING SHPDTAEL,R6                                                      
*                                                                               
DR10     CLI   SHPPIG,0            THIS THE INVERTED PIGGY BACK ELEM            
         BNE   DR20                YES, SKIP                                    
*                                                                               
         XC    8(L'TRAACTN,R2),8(R2)  CLEAR THE ACTION FIELD                    
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         BRAS  RE,DSPELEM          GO DISPLAY ELEMENT                           
         L     R2,DMCB             POINT TO NEXT DISPLAY LINE                   
*                                                                               
DR20     BAS   RE,NEXTEL                                                        
         BE    DR10                                                             
*                                                                               
DR30     GOTO1 CLR                                                              
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=============================================================                  
* DISPLAY KEY                                                                   
*=============================================================                  
                                                                                
DK       LA    R2,TRAMEDH                                                       
         L     R4,AIO                                                           
         USING SHPKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
*                                                                               
         XC    WORK(L'TRACLT),WORK                                              
         GOTO1 CLUNPK,DMCB,SHPKCLT,WORK                                         
         CLC   TRACLT,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLT,WORK         MOVE IN CLIENT                               
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         MVC   QCLT,WORK                                                        
         MVC   BCLT,SHPKCLT                                                     
*                                                                               
         MVC   BMKT,SHPKMKT                                                     
         MVC   BSTA,SHPKSTA                                                     
         XC    WORK(L'TRASTA),WORK                                              
         MVC   WORK(L'QSTA),QSTA                                                
         CLC   TRASTA,WORK                                                      
         BE    *+14                                                             
         MVC   TRASTA,WORK         MOVE IN PROD                                 
         OI    TRASTAH+6,X'80'     SET ON TRANSMIT BIT                          
*                                                                               
         DROP  R4                                                               
         B     EXIT                                                             
*                                                                               
* DELETE RECORD FUNCTION ILLEGAL                                                
*                                                                               
DELMODE  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
*======================================================                         
* VALIDATE DATE                                                                 
*======================================================                         
                                                                                
VDTE     NTR1                                                                   
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,WORK,DATE                                            
         OC    DMCB,DMCB           CK FOR ERROR                                 
         BZ    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,DATE),(3,NEWDATE)                                 
         B     EXIT                                                             
                                                                                
*======================================================                         
* VALIDATE COMMERCIAL                                                           
*======================================================                         
                                                                                
VCML     NTR1                                                                   
         MVC   AIO,AIO2            USE I/O 2                                    
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML,SVCMML                                                   
         CLI   SVCMMLF,C'Y'        TEST ADID                                    
         BNE   *+10                                                             
         MVC   CMLKCML,SVCMMLP                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCMLERR                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   SVSEQ,CMLSEQ                                                     
         MVC   SVTYPE,CMLTYPE                                                   
*                                                                               
         MVI   ELCODE,X'20'        GET PRODUCT LIST                             
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* PUT LOCKER TEST LOOP HERE FOR ALL PRODUCTS IN LIST - IF POOL,                 
*  ALL PRODUCTS IN CLIST!!!!                                                    
*                                                                               
         MVC   AIO,AIO1            RESTORE TO AIO1                              
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*==============================================================                 
* VALIDATE PIGGY-BACK COMMERCIAL                                                
*==============================================================                 
                                                                                
VCML2    NTR1                                                                   
         CLI   5(R2),8                                                          
         BL    BADCMMLN                                                         
         CLI   5(R2),12                                                         
         BH    BADCMMLN                                                         
*                                                                               
         GOTO1 ANY                                                              
         MVC   SVCMML2,WORK                                                     
         XC    SVCMML2P,SVCMML2P                                                
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SVCMML2P                                
         BE    VCML2A                                                           
*                                                                               
         CLI   SVCMMLF,C'Y'        CMML2 NOT ADID, TEST CMML1 IS ADID           
         BE    VCMLERR             IF ADIDFLAG SET, BAD PAIR                    
         B     VCML2X                                                           
*                                                                               
* CMML2 IS VALID ADID                                                           
*                                                                               
VCML2A   CLI   SVCMMLF,C'Y'        TEST CMML1 VALID ADID                        
         BE    VCML2X              YES                                          
         CLI   5(R2),8             ELSE TEST CMML2 IS VALID ISCI                
         BNE   VCMLERR             NO - THEN BAD PAIR                           
         B     VCML2X                                                           
*                                                                               
VCML2X   CLC   SVCMML,SVCMML2      CAN'T BE EQUAL                               
         BE    EQCMLER                                                          
         CLC   SVCMMLP,SVCMML2P                                                 
         BE    EQCMLER                                                          
*                                                                               
         MVC   AIO,AIO3            USE AIO3 (CMML IS IN AIO2)                   
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML,SVCMML2                                                  
         CLI   SVCMMLF,C'Y'                                                     
         BNE   *+10                                                             
         MVC   CMLKCML,SVCMML2P                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCMLERR                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   RECRDSW,1           SET FOR RESET GETREC LATER                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         CLC   SVTYPE,CMLTYPE      TYPE MUST BE SAME AS PRD                     
         BNE   BADCMLTP            ERROR                                        
         CLC   SVLMTCD,CMLLMTCD    RUN LIMIT CODE MUST BE SAME                  
         BNE   BADCMLMT            ERROR                                        
         CLC   SVSHPFTD,CMLRLSE    IS 1ST TELECAST BEFORE RELEASE               
         BL    BADCMLD                                                          
         CLC   SVSHPLTD,CMLRCL     IS LAST TELECAST AFTER RECALL                
         BH    BADCMLD                                                          
         MVC   SVSEQ2,CMLSEQ       SAVE CMML SEQ                                
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*============================================================                   
* FIND PIGGY BACK SHIPPING REC AND DELETE ELEMENT                               
*============================================================                   
                                                                                
DSHP     NTR1                                                                   
         MVC   AIO,AIO3            CMML IS IN AIO2                              
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML,SVOLDCML                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   RECRDSW,1           SET FOR RESET GETREC LATER                   
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SHPKEY,R4                                                        
         MVC   SHPKID,=XL2'0A25'                                                
         MVC   SHPKAM,BAGYMD                                                    
         MVC   SHPKCLT,BCLT        CLT                                          
         MVC   SHPKMKT,BMKT        MKT                                          
         MVC   SHPKSTA,BSTA        STA                                          
         MVC   SHPKCSEQ,CMLSEQ     SEQ                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SHPDTAEL,R6                                                      
DSHP10   CLI   SHPPIG,C'Y'                                                      
         BNE   DSHP20                                                           
         CLC   SHPCMML,SVOLDCML                                                 
         BE    DSHP20                                                           
         BAS   RE,NEXTEL                                                        
         BE    DSHP10                                                           
         DC    H'0'                                                             
*                                                                               
DSHP20   GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         GOTO1 PUTREC                                                           
         MVI   ELCODE,X'10'                                                     
         MVC   AIO,AIO1            RESTORE TO AIO1                              
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*=================================================================              
* FIND PIGGY BACK SHIPPING REC AND ADD ELEMENT, IF NONE ADD REC                 
*=================================================================              
                                                                                
ASHP     NTR1                                                                   
         MVC   AIO,AIO3            CMML IS IN AIO2                              
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
*                                                                               
         XC    KEY,KEY                                                          
         USING SHPKEY,R4                                                        
         MVC   SHPKID,=X'0A25'                                                  
         MVC   SHPKAM,BAGYMD                                                    
         MVC   SHPKCLT,BCLT        CLT                                          
         MVC   SHPKMKT,BMKT        MKT                                          
         MVC   SHPKSTA,BSTA        STA                                          
         MVC   SHPKCSEQ,SVSEQ2                                                  
*                                                                               
         GOTO1 HIGH                                                             
         LA    R6,ELEM                                                          
         USING SHPDTAEL,R6                                                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ASHP10                                                           
         GOTO1 GETREC                                                           
         MVI   RECRDSW,1           SET FOR RESET GETREC LATER                   
*                                                                               
* DATES WERE FILLED IN BY VR80 RTN                                              
*                                                                               
ASHP10   MVI   ELCODE,X'10'              RESTORE ELEMENT                        
         MVI   SHPDTAEL,X'10'            ELEMENT IDENTIFIER                     
         MVI   SHPDTALN,SHPDTAX-SHPDTAEL ELEMENT LENGTH                         
*                                                                               
         MVI   SHPNOSHP,0                                                       
         MVC   SHPCMML,SVCMML2                                                  
         MVC   SHPCMML2,SVCMML                                                  
         CLI   SVCMMLF,C'Y'        TEST TO USE ADID                             
         BNE   ASHP12                                                           
         MVC   SHPCMML,SVCMML2P                                                 
         MVC   SHPCMML2,SVCMMLP                                                 
         OI    SHPNOSHP,SHPISADI                                                
*                                                                               
ASHP12   MVI   SHPPIG,C'Y'                                                      
         OI    SHPNOSHP,X'40'      SET UP AS MANUAL SHIP                        
         MVC   SVDATES,SHPFTD                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ASHP20                                                           
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
*                                                                               
ASHP14   MVI   ELCODE,X'10'        RESTORE ELEMENT                              
         MVI   SHPDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   SHPDTALN,SHPDTAX-SHPDTAEL ELEMENT LENGTH                         
         CLI   SVCMMLF,C'Y'        TEST TO USE ADID                             
         BNE   ASHP16                                                           
         MVC   SHPCMML,SVCMMLP                                                  
         MVC   SHPCMML2,SVCMML2P                                                
*                                                                               
ASHP16   MVI   SHPPIG,0                                                         
         MVC   SHPFTD(12),SVDATES                                               
         MVI   SHPNOSHP,X'40'      SET UP AS MANUAL SHIP                        
*                                                                               
         CLI   SVCMMLF,C'Y'                                                     
         BNE   ASHP18                                                           
         OI    SHPNOSHP,SHPISADI   SET ADID FLAG                                
         MVC   SHPCMML,SVCMMLP                                                  
         MVC   SHPCMML2,SVCMML2P                                                
         MVC   AIO,AIO1            RESTORE TO AIO1                              
*                                                                               
ASHP18   B     EXIT                                                             
*                                                                               
* ADD REC HERE                                                                  
*                                                                               
ASHP20   L     R4,AIO3                                                          
         XC    0(256,R4),0(R4)                                                  
         USING SHPKEY,R4                                                        
         MVC   KEY,KEYSAVE                                                      
         MVC   SHPKEY,KEY                                                       
         MVI   ELCODE,X'10'        RESTORE ELEMENT                              
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         MVI   RECRDSW,1           SET FOR RESET GETREC LATER                   
         B     ASHP14                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
*============================================================                   
* VALIDATE NO CHANGED DATA IN UNPROTECTED FIELDS                                
*============================================================                   
                                                                                
         USING SHPDTAEL,R6                                                      
VSAME    NTR1                                                                   
         ST    R2,LASTACTN         SAVE ACTION FIELD ADDRESS                    
         LA    RF,6                                                             
*                                                                               
VSAME2   LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    4(R2),X'20'         TEST FIELD CHANGED                           
         BZ    INVALCHG                                                         
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,VSAME2                                                        
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,DMCB             ON EXIT, SET A(NEXT ACTION FIELD)            
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*===========================================================                    
* MAKE SURE ALL FIELDS ON THIS LINE ARE BLANK                                   
*===========================================================                    
                                                                                
VBLK     NTR1                                                                   
         LA    RF,6                SET FOR 6 FIELDS                             
*                                                                               
VBLK2    LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LLC   RE,0(R2)                                                         
         AHI   RE,-9               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BNZ   INVALENT                                                         
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,VBLK2                                                         
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO START OF NEXT LINE                  
         ST    R2,DMCB                                                          
         B     EXIT                                                             
         EJECT                                                                  
*================================================================               
* CLEAR REST OF SCREEN                                                          
*================================================================               
                                                                                
CLR      NTR1                                                                   
         LA    RF,TRATAGH          BLANK REST OF SCREEN                         
*                                                                               
CLR10    CR    R2,RF               AT END OF SCREEN                             
         BNL   EXIT                YES                                          
         LLC   R0,0(R2)                                                         
         LR    R1,R0                                                            
         AHI   R1,-9               GET FLD LEN-1                                
         EX    R1,CLRXC             MAKE FLD BLANK                              
         OI    6(R2),X'80'                                                      
*                                                                               
CLR14    AR    R2,R0                                                            
         B     CLR10                                                            
*                                                                               
CLRXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*                                                                               
INVALCHG XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVCHGMS),INVCHGMS                                     
         B     INVALRST            RESTORE FLD                                  
*                                                                               
INVALFLD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVFLDMS),INVFLDMS                                     
*                                                                               
INVALRST L     RE,LASTACTN         GET ADDRESS OF ACTION FIELD                  
         CLI   8(RE),C'A'          TEST ADD                                     
         BE    *+8                 YES - THEN NO REDISPLAY                      
         BRAS  RE,DSPELEM          GO REDISPLAY ELEMENT                         
         B     ERREXIT                                                          
*                                                                               
BADCMLD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADATMS),BADATMS                                       
         B     ERREXIT                                                          
*                                                                               
INVALENT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVENTMS),INVENTMS                                     
         B     ERREXIT                                                          
*                                                                               
INVELMER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVELMMS),INVELMMS                                     
         B     ERREXIT                                                          
*                                                                               
BADADDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVADDMS),INVADDMS                                     
         B     ERREXIT                                                          
*                                                                               
INVALOPT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOPTMS),INVOPTMS                                     
*                                                                               
ERREXIT  GOTO1 ERREX2                                                           
         DC    H'0'                                                             
*                                                                               
EQCMLER  MVI   ERROR,INVEQCML      CMML/CMML2 ARE THE SAME                      
         B     TRAPERR                                                          
*                                                                               
BADCMLTP MVI   ERROR,UNMCMLTP      UNMATCHED CMML TYPES                         
         B     TRAPERR                                                          
*                                                                               
BADCMLMT MVI   ERROR,INVCMLMT      2 CMML'S HAVE DIFF RUN LIMITS                
         B     TRAPERR                                                          
*                                                                               
BADCMMLN MVC   GERROR,=Y(NOT812)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VCMLERR  MVI   ERROR,INVCOMM                                                    
         B     TRAPERR                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
BADATMS  DC    C'* ERROR * CML REL/RECALL DATES DON''T SPAN TELECAST *'         
INVCHGMS DC    C'* ERROR * NO CHANGE WITHOUT ENTRY IN ACTION FIELD *'           
INVFLDMS DC    C'* ERROR * NO CHANGE TO 1ST TELECAST OR INSTR DATES *'          
INVENTMS DC    C'* ERROR * NO ENTRY WITHOUT ENTRY IN ACTION FIELD *'            
INVELMMS DC    C'* ERROR * CAN NOT CHANGE/DELETE BLANK SHIP LINE *'             
INVADDMS DC    C'* ERROR * NO ADD OVER EXISTING LINE *'                         
INVOPTMS DC    C'* ERROR * CODES = A(DD)/C(HANGE)/D(ELETE)/R(ESHIP) *'          
         EJECT                                                                  
*=================================================================              
* DISPLAY AN ELEMENT                                                            
*=================================================================              
*                                                                               
         USING SHPDTAEL,R6                                                      
DSPELEM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(L'TRACML2,R2),8(R2)                                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    SHPCMML2,SHPCMML2   IS THERE A PIGGY-BACK                        
         BZ    *+8                                                              
         BRAS  RE,DISCMML2         DISPLAY 2ND CMML                             
*                                                                               
DSPEL2   LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,SHPFTD),(5,8(R2))                                 
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,SHPLTD),(5,8(R2))                                 
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(L'TRAINDT,R2),8(R2)                                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    SHPQDATE,SHPQDATE   IS THERE AN INSTR DATE                       
         BZ    DSPEL4              NO                                           
         GOTO1 DATCON,DMCB,(3,SHPQDATE),(5,8(R2))                               
*                                                                               
DSPEL4   LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(L'TRASHDT,R2),8(R2)                                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    SHPSHPDT,SHPSHPDT   IS THERE A SHIPPING DATE                     
         BZ    DSPEL6              NO                                           
         GOTO1 DATCON,DMCB,(3,SHPSHPDT),(5,8(R2))                               
*                                                                               
DSPEL6   LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(L'TRAMSHP,R2),8(R2)                                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         TM    SHPNOSHP,X'80'                                                   
         BZ    *+10                                                             
         MVC   8(2,R2),=CL2'NS'                                                 
*                                                                               
         TM    SHPNOSHP,X'40'                                                   
         BZ    *+10                                                             
         MVC   8(2,R2),=CL2'MS'                                                 
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,DMCB                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* CODE TO FIGURE OUT FOR PIGGYBACKS WHICH COMMERCIAL TO DISPLAY                 
* IT IS NEVER THE ONE THAT IS IN THE KEY FIELD ON THE SCREEN                    
*==================================================================             
                                                                                
DISCMML  NTR1  BASE=*,LABEL=*                                                   
         USING SHPDTAEL,R6                                                      
         OC    SHPCMML2,SHPCMML2   IS THERE A PIGGY-BACK                        
         BZ    DISCMMLX                                                         
* NEED TO FIGURE OUT WHICH CMML TO DISPLAY AS PARTNER                           
         MVC   FLD(8),SHPCMML2                                                  
         MVC   FLD+8(4),SPACES                                                  
*                                                                               
         TM    SHPNOSHP,SHPISADI   TEST ADID                                    
         BZ    DISCMML2                                                         
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML2),FLD                                 
*                                                                               
DISCMML2 MVC   8(L'TRACML2,R2),FLD                                              
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    FLD+12(12),FLD+12   NEED TO COMPARE TO CMML IN KEY               
         MVC   FLD+12(12),TRACML                                                
         OC    FLD+12(12),SPACES                                                
         CLC   FLD(12),FLD+12      TEST KEY CMML MATCHES CMML2                  
         BNE   DISCMMLX            NO - SO CMML2 IS THE ONE TO DSPLY            
*                                                                               
         MVC   FLD(8),SHPCMML                                                   
         MVC   FLD+8(4),SPACES                                                  
         MVC   8(8,R2),SHPCMML     ELSE DISPLAY CMML1                           
         TM    SHPNOSHP,SHPISADI   TEST ADID                                    
         BZ    DISCMMLX                                                         
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML),8(R2)                                
DISCMMLX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF5D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR05RR DS    F                                                                
VTRPACK  DS    A                                                                
LASTACTN DS    A                   A(LAST ACTION FIELD)                         
         DS    0D                                                               
SVCMML   DS    CL8                                                              
SVCMML2  DS    CL8                                                              
SVCMMLP  DS    XL8                                                              
SVCMML2P DS    XL8                                                              
SVCMMLF  DS    C                                                                
DATE     DS    CL6                                                              
         DS    C                                                                
         DS    0D                                                               
FLD      DS    CL60                                                             
NEWDATE  DS    XL3                                                              
SVSEQ    DS    XL3                                                              
SVCMLRLS DS    XL3                                                              
SVCMLRCL DS    XL3                                                              
SVTYPE   DS    CL4                                                              
SVLMTCD  DS    CL1                                                              
SVSEQ2   DS    XL3                                                              
SVOLDCML DS    CL8                                                              
SVDATES  DS    CL12                                                             
SVSHPFTD DS    XL3                                                              
SVSHPLTD DS    XL3                                                              
RECRDSW  DS    XL1                                                              
ENDHOLD  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPTRA05   08/28/09'                                      
         END                                                                    
