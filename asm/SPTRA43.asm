*          DATA SET SPTRA43    AT LEVEL 074 AS OF 09/24/09                      
*PHASE T21643C                                                                  
         TITLE 'T21643 - PATTERN RECAP '                                        
**********************************************************************          
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - PRODUCT RECORDS, MARKET RECS, STATION ADDRESS,             
*                    WORK AREA TO BUILD COMML LIST                              
*             AIO3 - READ STATION AND ESTIMATE RECS                             
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG                                                          
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - UNUSED                                                            
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
*                                                                               
*********************************************                                   
* THIS PROGRAM GIVES ON LINE PATTERN RECAP  *                                   
*                    OFF LINE PATTERN RECAP *                                   
*********************************************                                   
         EJECT                                                                  
***********************************************************************         
*                                                                               
*  LEV 65 SMUR OCT11/00 FIX TBA REFS                                  *         
*  LEV 66 SMUR DEC04/00 FIX ALL REFS REQUEST                          *         
*  LEV 68 SMUR MAY29/01 USE TRAFFIC OFFICE, CHANGE DUMMY              *         
*  LEV 70 BGRI DEC09/02 NEW INSTR RECAP RECS                          *         
*  LEV 71 BGRI DEC09/02 FIX BAD DATES                                 *         
*  LEV 73 SMUR DEC19/08 PRINT NO CMML ON REPORT FOR INCPLETE PAT RECS *         
*  LEV 74 MHER JUL/09   ADID SUPPORT                                            
*                                                                     *         
***********************************************************************         
T21643   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1643**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR43RR                                                      
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       ON-LINE RECAP                                
         BE    RL                                                               
         CLI   MODE,PRINTREP       OFF-LINE RECAP                               
         BE    OFLP                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE *                                                    
*                                                                               
VK       LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
*                                                                               
         BRAS  RE,FPRO              GET PROFILE                                 
*                                                                               
         LA    R2,TRAPRLNH         PRODUCT-LEN                                  
         XC    BPRD(4),BPRD        ZERO PRD, BSLN, BPRD2, BSLN2                 
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK        PRD POL                                      
         BE    PRDINV              INVALID                                      
         CLC   =C'AAA',WORK        PRD AAA                                      
         BE    PRDINV              INVALID                                      
         MVC   QPRD,WORK           SAVE 3 CHAR PRD                              
         MVC   BPRD(2),WORK+3      SAVE BPRD AND BSLN                           
*                                                                               
         LA    R2,TRAPTLNH         PARTNER-LEN                                  
         CLI   5(R2),0                                                          
         BE    VK06                                                             
         CLC   =C'NONE',8(R2)      ONLY SINGLE PRODS                            
         BNE   VK04                                                             
         MVI   BPRD2,X'FF'                                                      
         B     VK06                                                             
*                                                                               
VK04     GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK        PRD POL                                      
         BE    PRDINV              INVALID                                      
         CLC   =C'AAA',WORK        PRD AAA                                      
         BE    PRDINV              INVALID                                      
         MVC   BPRD2(2),WORK+3     SAVE PARTNER BPRD2, BSLN2                    
*                                                                               
VK06     LA    R2,TRACODEH         CODE                                         
*                                                                               
         BRAS  RE,VCC                                                           
*                                                                               
VK10     LA    R2,TRAREFH          REF NUMBER                                   
         CLI   5(R2),0                                                          
         BNE   VK12                                                             
         TM    WHEN,X'C0'      ** OX RTN(REF=ALL) IS ONLY OVERNIGHT             
         BNZ   REFERR                                                           
         MVI   SVREF,X'FF'                                                      
         B     VK20                                                             
*                                                                               
VK12     CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'TBA',8(R2)       THIS A TBA REQUEST                           
         BE    VK16                                                             
*                                                                               
         TM    4(R2),X'08'         WAS FIELD NUMERIC                            
         BZ    NUMERR              MUST BE NUMERIC                              
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,TRAREF(0)                                                    
         CVB   R0,DUB                                                           
         STCM  R0,3,SVREF                                                       
         XC    SVREF,=X'3FFF'                                                   
         B     VK20                                                             
*                                                                               
VK16     MVC   SVREF,=X'FFFF'                                                   
*                                                                               
VK20     LA    R2,TRAPERH          PERIOD-ALLOW ONE OR TWO DATES                
*                                                                               
         BRAS  RE,VPER                GO VALIDATE PERIOD                        
*                                                                               
         LA    R2,TRAFLTRH         FILTERS                                      
*                                                                               
         BRAS  RE,VFTR                GO VALIDATE FILTERS                       
*                                                                               
* NOW BUILD KEY                                                                 
*                                                                               
VK30     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'     INSTRUCTION RECAP REC ID                     
         MVC   KEY+2(4),BAGYMD  BCLT/BPRD                                       
         MVC   SVKEY,KEY                                                        
         MVC   COMPKEY,KEY         ID/A-M/CLT/PRD                               
         B     EXIT                                                             
         EJECT                                                                  
* ON-LINE PATTERN RECAP *                                                       
*                                                                               
RL       LA    R2,TRAREFH          ON-LINE MUST HAVE REF NUM INPUT              
         CLI   5(R2),0                                                          
         BE    REFERR                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
*                                                                               
* GET INSTRUCTION RECAP RECORD *                                                
*                                                                               
         OC    KEY(13),KEY                                                      
         BNZ   RL10                NO, FIND POSITION IN LIST                    
         MVC   KEY(6),COMPKEY                                                   
RL10     MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         B     RL30                                                             
*                                                                               
RL20     MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 SEQ                                                              
*                                                                               
RL30     CLC   KEY(6),COMPKEY      ID/A-M/CLT/PRD                               
         BNE   EXIT                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,KEY+INSKMKT-INSKEY                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVSTAMKT,DUB                                                     
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION                          
         BE    *+14                 YES                                         
         OC    FTRAFF,FTRAFF       FILTERING ON AFFILIATE                       
         BZ    *+12                                                             
         BAS   RE,CKSTA            GO CHECK STATION                             
         BNE   RL20                                                             
*                                                                               
         BRAS  RE,GETMKT               GOTO GET MKT NAME                        
*                                                                               
         L     R6,AIO                                                           
         LR    R5,R6                                                            
         USING INSKEY,R5                                                        
         USING INSDTAEL,R6                                                      
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         BRAS  RE,GETEL                                                         
         BNE   RL20                                                             
*                                                                               
         CLI   SVCODE,0                ANY CODE                                 
         BE    *+14                                                             
         CLC   SVCODE,INSKCOPY         TEST CODE                                
         BNE   RL20                    (NO-GET NXT RECORD)                      
         MVC   HOLDCOPY,INSKCOPY                                                
         DROP  R5                                                               
*                                                                               
* SEE IF THIS ELEMENT NEEDED                                                    
*                                                                               
RL32     CLC   INSPRD1,BPRD         TEST ELEM = INPUT (PRD,PRT)                 
         BE    *+6                     (NO-GET NXT ELEM)                        
         DC    H'0'                                                             
*                                                                               
         CLI   BSLN,0              ANY SPOT LEN ENTRY                           
         BE    *+14                NO                                           
         CLC   INSSLN1,BSLN                                                     
         BNE   RL60                                                             
*                                                                               
         CLI   BPRD2,0             ANY PRD2 ENTRY                               
         BE    RL34                 NO                                          
         CLI   BPRD2,X'FF'         DON'T WANT ANY P/B                           
         BNE   *+16                                                             
         CLI   INSPRD2,0                                                        
         BE    RL34                                                             
         B     RL60                    (NO-GET NXT ELEM)                        
*                                                                               
         CLC   INSPRD2,BPRD2        TEST ELEM = INPUT (PRD,PRT)                 
         BNE   RL60                    (NO-GET NXT ELEM)                        
*                                                                               
         CLI   BSLN2,0             ANY SPOT LEN ENTRY                           
         BE    *+14                 NO                                          
         CLC   INSSLN2,BSLN2                                                    
         BNE   RL60                                                             
*                                                                               
* LOOK IN ELEM FOR REQUESTED REF *                                              
*                                                                               
RL34     MVC   LISTAR,SPACES                                                    
         SR    R0,R0                   TEST REF NUMBER                          
         LLC   R1,1(R6)                                                         
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         D     R0,=A(INSSUBEL)          VARIABLE NUM OF 7 BYTE DATA             
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R1                  -R3 NOW HAS NUM OF VARIABLE DATA          
         LA    R5,INSPTTN                                                       
*                                                                               
RL36     TM    SVREF,X'80'         IS THIS TBA'S ONLY                           
         BZ    RL38                 NO                                          
         CLC   =X'FFFFFF',0(R5)                                                 
         BE    RL40                                                             
         B     RL56                                                             
*                                                                               
RL38     SR    R0,R0                                                            
         ICM   R0,7,0(R5)                                                       
         SRL   R0,10                                                            
         N     R0,=X'00003FFF'                                                  
         CLM   R0,3,SVREF                                                       
         BNE   RL56                                                             
*                                                                               
* IF NO DATE ENTERED, USE ANY LAST TELECAST DATE FROM 30 DAYS AGO ON            
*                                                                               
RL40     CLI   SVPER2,0          IS THERE ONLY ONE INPUT DATE                   
         BNE   RL44                  -IN PERIOD                                 
         CLC   SVPER1P,5(R5)      (YES)   INSLTD                                
         BH    RL56                                                             
         CLC   SVPER1P,3(R5)              INSFTD                                
         BL    RL56                                                             
         B     RL46                                                             
*                                                                               
RL44     CLC   SVPER1P,5(R5)                                                    
         BH    RL56                START-PERIOD GREATER THAN INSLTD             
         CLC   SVPER2P,3(R5)                                                    
         BL    RL56                END-PERIOD LESS THAN INSFTD                  
*                                                                               
* FORMAT LINE  *                                                                
*                                                                               
RL46     MVC   LSTA,STAPRNT                                                     
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   LSTA(8),STANET                                                   
*                                                                               
         MVC   LMKT,MKTNM                                                       
*                                                                               
         LA    R0,INSPRD1                                                       
         LA    R1,LPRDSLN                                                       
         BAS   RE,FPRD             GO FORMAT PRD/SLN                            
*                                                                               
         LA    R0,INSPRD2                                                       
         LA    R1,LPTRSLN                                                       
         BAS   RE,FPRD             GO FORMAT PRD/SLN                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,INSDATE),(5,LINSDTE)                              
         GOTO1 (RF),(R1),(2,3(R5)),(5,LFTD)                                     
         MVI   LFTD+5,C'-'                                                      
         GOTO1 (RF),(R1),(2,5(R5)),(5,LLTD)                                     
*                                                                               
         CLI   HOLDCOPY,0                                                       
         BE    RL50                                                             
         MVC   LCOPY+1(1),HOLDCOPY                                              
         TM    INSFLAG,X'20'       COPY CODE = EST                              
         BZ    RL50                 NO                                          
         LLC   R0,HOLDCOPY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LCOPY,DUB                                                        
*                                                                               
RL50     GOTO1 LISTMON                                                          
*                                                                               
RL56     LA    R5,INSSUBEL(,R5)                                                 
         BCT   R3,RL36             USE ALL FLDS IN ELEMENT                      
*                                                                               
RL60     BRAS  RE,NEXTEL                                                        
         BE    RL32                                                             
         B     RL20                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* OFF-LINE RECAP *                                                              
*                                                                               
OFLP     LA    R0,HEADING                                                       
         ST    R0,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MDHK                                                          
         ST    R1,MIDHOOK                                                       
         XC    SPECTLE,SPECTLE                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(3,SVPER1),USERQSTR                                  
         MVC   WORK(3),SVPER2                                                   
         OC    SVPER2,SVPER2       WAS PERIOD 2 ENTERED                         
         BNZ   OL06                 YES                                         
         MVC   WORK,SVPER1                                                      
OL06     GOTO1 (RF),(R1),(3,WORK),USERQEND                                      
         GOTO1 (RF),(R1),(0,USERQEND),(3,SVPER2)                                
         GOTO1 (RF),(R1),(0,USERQEND),(2,SVPER2P)                               
*                                                                               
OL08     CLI   SVREF,X'FF'         IS REF NUM ALL OR TBA                        
         BE    OX                                                               
*                                                                               
         MVI   FORCEHED,C'Y'       READ AND PRINT PATTERN RECORD                
         MVI   RCSUBPRG,0                                                       
*                                                                               
         OC    KEY(13),KEY                                                      
         BNZ   OL10                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(7),BAGYMD     A-M/CLT/PRD-LEN/PTR/LEN                      
         MVC   KEY+9(1),SVCODE     COPY CODE                                    
         SR    R0,R0                                                            
         ICM   R0,12,SVREF         (SVREF=2BYTES                                
         SLL   R0,2                REF NUM=14BITS)                              
         N     R0,=X'FFFC0000'                                                  
         STCM  R0,12,KEY+10                                                     
         STCM  R0,12,SVREF2                                                     
         EJECT                                                                  
OL10     MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLI   SVCODE,0            ANY COPY CODE                                
         BE    OL20                                                             
         CLC   KEY(10),KEYSAVE     COPY CODE FOUND                              
         BE    OL20                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+9,0                                                          
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         B     OL20                                                             
*                                                                               
OL15     MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 SEQ                                                              
*                                                                               
OL20     CLC   KEY+2(3),COMPKEY+2    A-M/CLT                                    
         BNE   OR80                                                             
         CLC   KEY+5(1),BPRD       PRD-LEN                                      
         BNE   OL15                                                             
         CLI   BSLN,0              ANY SPOT LEN ENTRY                           
         BE    *+14                NO                                           
         CLC   KEY+6(1),BSLN                                                    
         BNE   OL15                                                             
         CLI   BPRD2,0             PARTNER                                      
         BE    OL26                                                             
         CLI   BPRD2,X'FF'         DON'T WANT ANY P/B                           
         BNE   OL24                                                             
         CLI   KEY+7,0                                                          
         BE    OL26                                                             
         B     OL15                    (NO-GET NXT ELEM)                        
OL24     CLC   KEY+7(1),BPRD2      PTR-LEN                                      
         BNE   OL15                                                             
OL26     CLI   BSLN2,0             ANY SPOT LEN ENTRY                           
         BE    *+14                NO                                           
         CLC   KEY+8(1),BSLN2                                                   
         BNE   OL15                                                             
         CLI   SVCODE,0                ANY CODE                                 
         BE    *+14                                                             
         CLC   KEY+9(1),SVCODE                                                  
         BNE   OL15                                                             
*                                                                               
         SR    R0,R0               REF NUM                                      
         ICM   R0,12,KEY+10                                                     
         N     R0,=X'FFFC0000'                                                  
         CLM   R0,12,SVREF2                                                     
         BNE   OL15                                                             
         MVC   SVREFSUB,KEY+10     SAVE REF/SUB FOR COMPARE                     
*                                                                               
         BAS   RE,PPAT             GO PRINT PATTERN                             
*                                                                               
         MVI   P+2,C'*'                                                         
         MVC   P+4(102),P+2                                                     
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* GET INSTRUCTION RECAP RECORD *                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         USING PATKEY,R6                                                        
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         XC    SVMKTNM,SVMKTNM                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),COMPKEY                                                   
         DROP  R6                                                               
*                                                                               
OR10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     OR30                                                             
*                                                                               
OR20     MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
OR30     CLC   KEY(6),COMPKEY      ID/A-M/CLT/PRD                               
         BNE   OR80                                                             
*                                                                               
         LA    R5,KEY                                                           
         USING INSKEY,R5                                                        
         CLI   SVCODE,0                                                         
         BE    *+14                                                             
         CLC   SVCODE,INSKCOPY     TEST CODE                                    
         BNE   OR20                 (NO-GET NXT RECORD)                         
*                                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,KEY+INSKMKT-INSKEY                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVSTAMKT,DUB                                                     
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION                          
         BE    *+14                 YES                                         
         OC    FTRAFF,FTRAFF       FILTERING ON AFFILIATE                       
         BZ    *+12                                                             
         BAS   RE,CKSTA            GO CHECK STATION                             
         BNE   OR20                                                             
*                                                                               
         L     R6,AIO                                                           
         LR    R5,R6                                                            
         USING INSDTAEL,R6                                                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,GETEL                                                         
         BNE   OR20                                                             
*                                                                               
OR34     CLC   INSPRD1(4),SVPRD    TEST ELEM = INPUT (PRD,PRT)                  
         BNE   OR74                 (NO-GET NXT ELEM)                           
*                                                                               
         SR    R0,R0               GET NUMBER OF SUB-ELS                        
         LLC   R1,1(R6)                                                         
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         D     R0,=A(INSSUBEL)          VARIABLE NUM OF 7 BYTE DATA             
         LA    R3,INSPTTN                                                       
OR40     STC   R1,INSFLDCT         SAVE NUM OF PATTERN REFS.                    
         SR    R0,R0                                                            
         ICM   R0,7,0(R3)                                                       
         SRL   R0,10                                                            
         N     R0,=X'00003FFF'                                                  
         CLM   R0,3,SVREF                                                       
         BNE   OR70                                                             
*                                                                               
         CLC   SVPATSTR,5(R3)      PAT START AFTER THIS END                     
         BH    OR70                 YES                                         
         CLC   SVPATEND,3(R3)      PAT END BEFORE THIS START                    
         BL    OR70                 YES                                         
         CLI   SVPER1,0            TEST PERIOD INPUT BLANK                      
         BNE   OR46                                                             
         MVC   WRKDT,5(R3)             6(R3) IS INSLTD (LAST TELE DTE)          
         GOTO1 DATCON,DMCB,(2,WRKDT),WORK                                       
         LA    R0,30                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)    ADD 30 TO LAST TELE DTE           
         GOTO1 DATCON,(R1),(5,0),WORK+12      GET TODAY'S DATE                  
         CLC   WORK+6(6),WORK+12          IF LAST TELE DTE IS 30                
         BL    OR70                        -DAYS PRIOR TO TODAY, SKIP           
         B     OR50              O.K. GET MKT                                   
*                                                                               
OR46     CLI   SVPER2,0          IS THERE ONLY ONE PERIOD INPUT DATE            
         BNE   OR48                                                             
         CLC   SVPER1P,5(R3)      (YES)   INSLTD                                
         BH    OR70                                                             
         CLC   SVPER1P,3(R3)              INSFTD                                
         BL    OR70                                                             
         B     OR50                                                             
*                                                                               
OR48     CLC   SVPER1P,5(R3)                                                    
         BH    OR70                START-PERIOD GREATER THAN INSLTD             
         CLC   SVPER2P,3(R3)                                                    
         BL    OR70                END-PERIOD LESS THAN INSFTD                  
*                                                                               
OR50     BRAS  RE,GETMKT               GOTO GET MKT NAME                        
*                                                                               
* FORMAT FOR MID-LINES *                                                        
*                                                                               
         CLC   SVMKTNM,MKTNM       TEST NEW MARKET                              
         BE    OR54                                                             
         CLI   OFSW,0              IS P LINE EMPTY                              
         BE    OR56                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   OFSW,0                                                           
         B     OR56                                                             
OR54     LA    R2,40(R2)           INCREMENT TO 2ND HALF OF P LINE              
         CLI   OFSW,0              TEST 1ST HALF P LINE USED                    
         BNE   OR58                                                             
OR56     LA    R2,P                                                             
         CLC   SVMKTNM,MKTNM                                                    
         BE    *+10                                                             
         MVC   2(24,R2),MKTNM                                                   
OR58     MVC   29(7,R2),STAPRNT                                                 
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   29(8,R2),STANET                                                  
*                                                                               
         CLC   SVREFSUB,0(R3)                                                   
         BE    *+8                                                              
         MVI   36(R2),C'*'                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,INSDATE),(5,38(R2))                               
*                                                                               
** TWX CODE NO-OPED 10/16/90  - TWX REPLACED WITH PERIOD                        
**       BAS   RE,GETWX                                                         
**       MVC   48(10,R2),SVTWX                                                  
         GOTO1 (RF),(R1),(2,3(R3)),(5,48(R2))                                   
         MVI   56(R2),C'-'                                                      
         GOTO1 (RF),(R1),(2,5(R3)),(5,132+48(R2))                               
**                                                                              
         CLI   INSKCOPY,0          WAS THIS STATION COPY CODED                  
         BE    OR64                 NO                                          
         TM    INSFLAG,X'20'       COPY CODE = EST                              
         BZ    OR60                 NO                                          
         LLC   R0,INSKCOPY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  62(3,R2),DUB                                                     
         B     OR64                                                             
OR60     MVC   63(1,R2),INSKCOPY                                                
*                                                                               
OR64     IC    R1,OFSW             INCREMENT P LINE SWITCH                      
         LA    R1,1(R1)                                                         
         STC   R1,OFSW                                                          
         MVC   SVMKTNM,MKTNM       SAVE MKT NAME                                
*                                                                               
         CLI   OFSW,2              TEST P LINE FULL                             
         BNE   OR74                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   OFSW,0                                                           
         B     OR74                                                             
*                                                                               
OR70     LA    R3,INSSUBEL(,R3)                                                 
         LLC   R1,INSFLDCT         GET NUM OF PATTERN REFS.                     
         BCT   R1,OR40                                                          
*                                                                               
OR74     BRAS  RE,NEXTEL                                                        
         BE    OR34                                                             
         B     OR20                                                             
OR80     CLI   OFSW,0              TEST P LINE EMPTY                            
         BE    OLXX                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
OLXX     B     EXIT                                                             
         EJECT                                                                  
***************************************************                             
* ROUTINE FOR PATTERN RECAP IF REF NUM = ALL      *                             
* FIRST READ THROUGH INST RECAP RECS, CONSTRUCT   *                             
* KEY OF REQUIRED INFO IN XLIST, THEN SORT XLIST, *                             
* THEN GET PATTERN REC BASED ON SORTED KEYS       *                             
***************************************************                             
*                                                                               
* GET INSTRUCTION RECAP RECORD *                                                
*                                                                               
OX       MVI   ELCODE,X'10'                                                     
         OC    KEY(13),KEY                                                      
         BNZ   OX10                NO, FIND POSITION IN LIST                    
         MVC   KEY(6),COMPKEY                                                   
*                                                                               
         SR    R2,R2                                                            
         CLI   OFFLINE,C'Y'                                                     
         BE    OX00                                                             
         LA    R4,XLISTSTR        DUMMY IS XLIST STORE AREA                     
         ST    R4,AXLIST                                                        
         LR    RE,R4                                                            
         LR    R1,R4                                                            
         L     RF,LSYSD                                                         
         SR    R1,R9                                                            
         SR    RF,R1                                                            
         LA    R0,0(RE,RF)                                                      
         ST    R0,AXLISTND                                                      
         XCEF                                                                   
         B     OX001                                                            
*                                                                               
OX00     L     R4,VADUMMY         DUMMY IS XLIST STORE AREA                     
         ST    R4,AXLIST                                                        
         LR    RE,R4                                                            
         L     RF,=F'149600'                                                    
         LA    R0,0(RE,RF)                                                      
         ST    R0,AXLISTND                                                      
         XCEF                                                                   
*                                                                               
OX001    L     R1,AXLISTND                                                      
         SH    R1,=AL2(XLISTLN)                                                 
         ST    R1,AXLISTND                                                      
         USING XLIST,R4                                                         
         TM    FILTERSW,FILINACT   SHOW INACTIVE PATTERNS                       
         BZ    OX10                 NO                                          
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(7),BAGYMD     A-M/CLT/PRD-LEN/PTR-LEN                      
         MVC   KEY+9(1),SVCODE     COPY CODE                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
OX02     CLC   KEY(6),KEYSAVE                                                   
         BNE   OX08                                                             
         CLC   KEY+5(1),BPRD       PRD-LEN                                      
         BNE   OX04                                                             
         CLI   BSLN,0              ANY SPOT LEN ENTRY                           
         BE    *+14                NO                                           
         CLC   KEY+6(1),BSLN                                                    
         BNE   OX04                                                             
         CLI   BPRD2,0             PARTNER                                      
         BE    OX03                                                             
         CLI   BPRD2,X'FF'         DON'T WANT ANY P/B                           
         BNE   *+16                                                             
         CLI   INSPRD2,0                                                        
         BE    OX03                                                             
         B     OX04                    (NO-GET NXT ELEM)                        
         CLC   KEY+7(1),BPRD2      PTR-LEN                                      
         BNE   OX04                                                             
OX03     CLI   BSLN2,0             ANY SPOT LEN ENTRY                           
         BE    *+14                NO                                           
         CLC   KEY+8(1),BSLN2                                                   
         BNE   OX04                                                             
         CLI   SVCODE,0                ANY CODE                                 
         BE    *+14                                                             
         CLC   KEY+9(1),SVCODE                                                  
         BNE   OX04                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         CLC   SVPER1,PATEND       CK IF PATTERN IN PERIOD                      
         BH    OX04                                                             
         CLC   SVPER2,PATSTART     CK IF PATTERN IN PERIOD                      
         BL    OX04                                                             
         DROP  R6                                                               
         MVC   XPRD1(5),KEY+5      PRD1, SLN1, PRD2, SLN, COPY                  
         SR    R0,R0                                                            
         MVC   XREF,KEY+10                                                      
         MVC   XMKT,=X'FFFF'                                                    
         LA    R4,XNEXT                                                         
         C     R4,AXLISTND         PAST END OF STORAGE                          
         BH    SIZERR                                                           
         LA    R2,1(R2)            COUNTER OF XLIST RECS                        
*                                                                               
* FORCE NEXT SUBLINE *                                                          
*                                                                               
OX04     SR    R1,R1                                                            
         ICM   R1,7,KEY+10                                                      
         SRL   R1,10               RIGHT JUSTIFY, DROPPING SUBLINE              
         LA    R1,1(,R1)                                                        
         SLL   R1,10               BACK TO REF                                  
         STCM  R1,7,KEY+10                                                      
         OC    KEY+10(3),KEY+10    AT END                                       
         BZ    OX08                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     OX02                                                             
*                                                                               
OX08     MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
OX10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     OX22                                                             
*                                                                               
OX20     MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
OX22     CLC   KEY(6),COMPKEY        ID/A-M/CLT/PRD                             
         BNE   OX42                                                             
*                                                                               
         LA    R5,KEY                                                           
         CLI   SVCODE,0            WAS CODE SELECTION ENTERED                   
         BE    *+14                NO                                           
         CLC   SVCODE,INSKCOPY         TEST CODE                                
         BNE   OX20                    (NO-GET NXT RECORD)                      
*                                                                               
         USING INSKEY,R5                                                        
*                                                                               
         L     R6,AIO                                                           
         LR    R5,R6                                                            
         USING INSDTAEL,R6                                                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,GETEL                                                         
         BNE   OX20                                                             
*                                                                               
OX24     CLC   INSPRD1,BPRD         TEST ELEM = INPUT (PRD,PRT)                 
         BNE   OX40                    (NO-GET NXT ELEM)                        
         CLI   BSLN,0              ANY SPOT LEN ENTRY                           
         BE    *+14                NO                                           
         CLC   INSSLN1,BSLN                                                     
         BNE   OX40                    (NO-GET NXT ELEM)                        
         CLI   BPRD2,0             ANY PRD2 ENTRY                               
         BE    *+14                NO                                           
         CLC   INSPRD2,BPRD2        TEST ELEM = INPUT (PRD,PRT)                 
         BNE   OX40                    (NO-GET NXT ELEM)                        
         CLI   BSLN2,0             ANY SPOT LEN ENTRY                           
         BE    *+14                NO                                           
         CLC   INSSLN2,BSLN2                                                    
         BNE   OX40                    (NO-GET NXT ELEM)                        
*                                                                               
         SR    R0,R0               GET NUMBER OF SUB-ELS                        
         LLC   R1,1(R6)                                                         
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         D     R0,=A(INSSUBEL)          VARIABLE NUM OF 7 BYTE DATA             
         LA    R3,INSPTTN                                                       
OX26     STC   R1,INSFLDCT         SAVE NUM OF PATTERN REFS.                    
*                                                                               
         OC    0(3,R3),0(R3)       IS REF/SUB ZERO                              
         BZ    OX38                 HIATUS, BYPASS                              
*                                                                               
*                                                                               
         CLI   SVPER1,0            TEST ANY PERIOD DATE INPUT                   
         BNE   OX32                                                             
         MVC   WRKDT,5(R3)             6(R3) IS INSLTD (LAST TELE DTE)          
         GOTO1 DATCON,DMCB,(2,WRKDT),WORK                                       
         GOTO1 (RF),(R1),(5,0),WORK+12        GET TODAY'S DATE                  
         LA    R0,30                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)    ADD 30 TO LAST TELE DTE           
         CLC   WORK+6(6),WORK+12                                                
         BL    OX38              LAST TELE DTE=30 DAYS PRIOR TO TODAY           
         B     OX36              O.K. SET UP XKEY                               
*                                                                               
OX32     CLI   SVPER2,0          IS THERE ONLY ONE INPUT DATE                   
         BNE   OX34                  -IN PERIOD                                 
         CLC   SVPER1P,5(R3)      (YES)   INSLTD                                
         BH    OX38                                                             
         CLC   SVPER1P,3(R3)              INSFTD                                
         BL    OX38                                                             
         B     OX36                                                             
*                                                                               
OX34     CLC   SVPER1P,5(R3)                                                    
         BH    OX38                START-PERIOD GREATER THAN INSLTD             
         CLC   SVPER2P,3(R3)                                                    
         BL    OX38                END-PERIOD LESS THAN INSFTD                  
*                                                                               
         CLC   SVREF,=X'FFFF'      TBA REFS ONLY                                
         BE    *+12                                                             
         CLI   SVREF,X'FF'         ALL REFS REQUEST                             
         BE    OX36                                                             
         CLC   =X'FFFFFF',0(R3)                                                 
         BNE   OX38                                                             
*                                                                               
* SET UP KEY IN XLIST *                                                         
*                                                                               
OX36     MVC   XREF,0(R3)          SAVE REF/SUB                                 
         MVC   XPRD1(4),INSPRD1                                                 
         MVC   XCOPY,INSKCOPY                                                   
         MVC   XINSDT,INSDATE                                                   
         MVC   XMKT(5),INSKMKT      MKT/STA                                     
         MVC   XFLAG,INSFLAG                                                    
         MVC   XDTS,3(R3)           FTD/LTD                                     
         LA    R4,XNEXT                  INCREMENT XLIST                        
         LA    R2,1(R2)            COUNTER OF XLIST RECS                        
         C     R4,AXLISTND                                                      
         BH    SIZERR                                                           
OX38     LA    R3,INSSUBEL(,R3)    INCREMENT 7 BYTE PAT REF NUMS                
         LLC   R1,INSFLDCT         SAVE NUM OF PATTERN REFS.                    
         BCT   R1,OX26                                                          
*                                                                               
OX40     BRAS  RE,NEXTEL                                                        
         BE    OX24                                                             
         B     OX20                                                             
         EJECT                                                                  
* SORT XLIST *                                                                  
*                                                                               
OX42     L     R4,AXLIST                                                        
         OC    0(2,R4),0(R4)       END OF XLISTKEY                              
         BNZ   OX44                                                             
         MVC   P(34),=C'NO INSTRUCTIONS RUN FOR PATTERN(S)'                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
OX44     GOTO1 XSORT,DMCB,(R4),(R2),XLISTLN,13,0                                
*                                                                               
* BUILD KEY FOR PATTERN REC *                                                   
*                                                                               
OX50     MVC   SVXKEY,XPRD1        SAVE  XLIST KEY                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         XC    SVMKTNM,SVMKTNM                                                  
*                                                                               
         OC    XREF,XREF           HIATUS                                       
         BZ    OX68                                                             
*                                                                               
         CLC   XREF,=X'FFFFFF'     TBA                                          
         BE    OX68                                                             
*                                                                               
* FIND CURRENT REF/SUB FOR THIS REF *                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),XPRD1      PRD-LEN+PTR-LEN+COPY+REF/SUB                 
         NC    KEY+11(2),=X'FC00'  SET SUB TO ZERO                              
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     A-M/CLT/PRD/PRT/COPY                         
         BE    OX54                                                             
         CLI   XCOPY,0                                                          
         BE    OX64                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+9,0                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     A-M/CLT/PRD/PRT/COPY                         
         BNE   OX64                GO PRINT PATTERN RECORD LINE                 
OX54     SR    R0,R0                                                            
         ICM   R0,7,XREF                                                        
         SR    R1,R1                                                            
         ICM   R1,7,KEY+10                                                      
         N     R0,=XL4'FFFFFC00'                                                
         N     R1,=XL4'FFFFFC00'                                                
         CR    R0,R1                                                            
         BE    OX60                                                             
         CLI   XCOPY,0                                                          
         BE    OX64                                                             
         CLI   KEY+9,0                                                          
         BE    OX64                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+9,0                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     A-M/CLT/PRD/PRT/COPY                         
         BNE   OX64                GO PRINT PATTERN RECORD LINE                 
         SR    R0,R0                                                            
         ICM   R0,7,XREF                                                        
         SR    R1,R1                                                            
         ICM   R1,7,KEY+10                                                      
         N     R0,=XL4'FFFFFC00'                                                
         N     R1,=XL4'FFFFFC00'                                                
         CR    R0,R1                                                            
         BNE   OX64                                                             
OX60     MVC   SVREFSUB,KEY+10                                                  
*                                                                               
* FIND THIS REF/SUB PATTERN *                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),XPRD1      PRD-LEN+PTR-LEN+COPY+REF/SUB                 
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     A-M/CLT/PRD/PRT/COPY                         
         BE    OX70                                                             
         CLI   XCOPY,0                                                          
         BE    OX64                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+9,0                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     A-M/CLT/PRD/PRT/COPY                         
         BE    OX70                GO PRINT PATTERN RECORD LINE                 
*                                                                               
OX64     MVC   P(13),KEYSAVE                                                    
         GOTO1 HEXOUT,DMCB,KEYSAVE,P+14,13                                      
         MVC   P+45(40),=C'** SYSTEM ERROR, CALL DDS-NO PTTN KEY **'            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DC    H'0'                                                             
*                                                                               
* PRINT HIATUS PATTERN *                                                        
*                                                                               
OX68     BAS   RE,PHAT                                                          
         CLC   XREF,=X'FFFFFF'     TBA                                          
         BE    *+14                                                             
         MVC   P+3(47),=C'NO INSTRUCTION ACTIVITY KEPT FOR HIATUS PATTEC        
               RN'                                                              
         B     OX72                                                             
         MVC   P+3(44),=C'NO PATTERN FOR COMMERCIALS (TO BE ANNOUNCED)'         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   P+2,C'*'                                                         
         MVC   P+4(102),P+2                                                     
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     OX74                                                             
*                                                                               
* GET XLIST KEY AND FORMAT P LINE *                                             
*                                                                               
OX70     BAS   RE,PPAT             GO PRINT PATTERN                             
*                                                                               
         MVI   P+2,C'*'                                                         
         MVC   P+4(102),P+2                                                     
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLC   XMKT,=X'FFFF'       THIS INACTIVE PATTERN                        
         BNE   OX74                                                             
         MVC   P+3(40),=C'NO INSTRUCTION ACTIVITY FOR THIS PATTERN'             
*                                                                               
         CLI   HIATUS,C'Y'         THIS HIATUS PATTERN                          
         BNE   *+10                                                             
         MVC   P+3(42),=C'NO INSTRUCTION ACTIVITY FOR HIATUS PATTERN'           
*                                                                               
OX72     MVI   OFSW,0              SET P LINE EMPTY                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     OX90                                                             
*                                                                               
OX74     MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
OX76     L     R5,AIO              RESTORE R5 AS KEY POINTER                    
         MVC   INSKMKT(5),XMKT                                                  
         MVC   INSDATE(2),XINSDT                                                
         MVC   KEY,0(R5)                                                        
         BRAS  RE,GETMKT               GOTO GET MKT NAME                        
*                                                                               
* FORMAT FOR MID-LINES *                                                        
*                                                                               
         CLC   SVMKTNM,MKTNM       TEST NEW MARKET                              
         BE    OX82                                                             
         CLI   OFSW,0              IS P LINE EMPTY                              
         BE    OX84                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   OFSW,0                                                           
         B     OX84                                                             
OX82     LA    R2,40(R2)           INCREMENT TO 2ND HALF OF P LINE              
         CLI   OFSW,0              TEST 1ST HALF P LINE USED                    
         BNE   OX86                                                             
OX84     LA    R2,P                                                             
         CLC   SVMKTNM,MKTNM                                                    
         BE    *+10                                                             
         MVC   2(24,R2),MKTNM                                                   
OX86     MVC   29(7,R2),STAPRNT                                                 
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   29(8,R2),STANET                                                  
*                                                                               
         CLC   SVREFSUB,XREF   SEE IF INSTR REFERS TO CURR PAT SUB              
         BE    *+8                                                              
         MVI   36(R2),C'*'                                                      
         GOTO1 DATCON,DMCB,(2,INSDATE),(5,38(R2))                               
*                                                                               
** TWX CODE NO-OPED 10/16/90  TWX REPLACED WITH PERIOD                          
**       BAS   RE,GETWX                                                         
**       MVC   48(10,R2),SVTWX                                                  
         GOTO1 (RF),(R1),(2,XFTD),(5,48(R2))                                    
         MVI   56(R2),C'-'                                                      
         GOTO1 (RF),(R1),(2,XLTD),(5,132+48(R2))                                
*                                                                               
         MVC   62(1,R2),XCOPY                                                   
         TM    XFLAG,X'20'         COPY CODE = EST                              
         BZ    OX88                                                             
         LLC   R0,XCOPY                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  62(3,R2),DUB                                                     
*                                                                               
OX88     IC    R1,OFSW             INCREMENT P LINE SWITCH                      
         LA    R1,1(R1)                                                         
         STC   R1,OFSW                                                          
         MVC   SVMKTNM,MKTNM       SAVE MKT NAME                                
*                                                                               
         CLI   OFSW,2              TEST P LINE FULL                             
         BNE   OX90                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   OFSW,0                                                           
*                                                                               
OX90     LA    R4,XNEXT            INCREMENT TO NXT KEY IN XLIST                
         OC    0(4,R4),0(R4)       END OF XLIST                                 
         BZ    OX94                                                             
         CLC   SVXKEY,0(R4)        SAME PATTRN REC KEY (AND MKT/STA)            
         BE    OX90                 YES, SKIP                                   
         CLC   SVXKEY(8),0(R4)     SAME PATTRN REC KEY                          
         BNE   OX92                                                             
         CLC   XMKT,=X'FFFF'       DUMMY PATTERN REC                            
         BNE   OX76                 NO, PRT NXT MKT STA                         
         B     OX90                 YES, SKIP IT                                
OX92     CLI   OFSW,0              NO.  TEST P LINE EMPTY                       
         BE    OX50                          YES.GET NEW PAT KEY                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   OFSW,0                            SET SWITCH TO EMPTY            
         B     OX50                              NOW GET NEW PAT KEY            
*                                                                               
OX94     CLI   OFSW,0                                                           
         BE    OXX                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
OXX      B     EXIT                                                             
         EJECT                                                                  
* READ STATION MASTER REC TO GET STATION AFFILIATE *                            
*                                                                               
         DS    0H                                                               
CKSTA    NTR1                                                                   
         MVC   INSTKEY,KEY                                                      
         GOTO1 MSUNPK,DMCB,(X'80',INSKMKT-INSKEY+INSTKEY),FULL,DUB              
*                                                                               
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES                                                  
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
*                                                                               
         MVC   QSTA,DUB                                                         
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION                          
         BE    *+10                 YES                                         
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         L     R3,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,(R3)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STAMASD,R3                                                       
         MVC   SVAFFIL,SNETWRK                                                  
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION                          
         BNE   CKSTA10              YES                                         
         MVC   SVSTAMKT,SMKT                                                    
*                                                                               
CKSTA10  MVC   KEY,INSTKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH              TO RESTORE PROPER REC FOR READ SEQ             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    FTRAFF,FTRAFF       FILTERING ON AFFILIATE                       
         BZ    EXIT                 ALSO SETS = CC                              
         CLC   SVAFFIL,FTRAFF                                                   
         B     EXIT                                                             
         EJECT                                                                  
* PRINT PATTERN RECORD LINE                                                     
*                                                                               
         DS    0H                                                               
PPAT     NTR1                                                                   
         L     R6,AIO                                                           
         USING PATKEY,R6                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVPRD(4),PATKPRD      SAVE FOR READING                           
*                                                                               
         LA    R3,PATKPRD          * PRD-LEN                                    
         LA    R5,SVPRDNM                                                       
         BAS   RE,PPRD             PROD AT R5, PROD NAME NEXT LINE              
*                                                                               
         MVC   CURPRD,PATKPRD                                                   
*                                                                               
         CLI   PATKPRD2,0          IS THERE A PARTNER                           
         BE    PPAT02                NO                                         
         LA    R3,PATKPRD2         * PTR-LEN                                    
         LA    R5,SVPRDNM2                                                      
         BAS   RE,PPRD             PROD AT R5, PROD NAME NEXT LINE              
*                                                                               
         MVC   CURPRD,PATKPRD      FOR ESTIMATE DESC LOOKUP                     
         B     PPAT04                                                           
*                                                                               
PPAT02   MVC   SVPRDNM2,SPACES                                                  
*                                                                               
PPAT04   MVC   SVCCODE,SPACES                                                   
         MVC   SVCCODE(1),PATKCODE  * COPY CODE                                 
*                                                                               
         MVC   SVREFSUB,PATKREF    SAVE REFSUB                                  
         SR    R3,R3                * REF NUM                                   
         ICM   R3,7,PATKREF                                                     
         SRL   R3,10                                                            
         X     R3,=X'00003FFF'     COMPLEMENT TO NON-COMPLMNT                   
         LA    R5,P+PREF-PLINE                                                  
         EDIT  (R3),(4,(R5))                                                    
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL             GET PATTERN DATA ELEMENT                    
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
         CLI   PATDTALN,38         OLD ELEM                                     
         BE    PPAT06                                                           
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
         TM    PATSTAT1-PATDTAEL(R6),PATSADID                                   
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         TM    PATSTAT,X'10'       IS THIS A COPY CODE = EST PATTERN            
         BZ    PPAT05                                                           
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,SVCCODE                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         EDIT  (R5),(3,SVCCODE),ALIGN=LEFT                                      
*                                                                               
         STC   R5,CUREST                                                        
*                                                                               
         BAS   RE,PESTD            GET EST DISCRIPTION                          
         B     PPAT06                                                           
*                                                                               
PPAT05   MVC   SVEDESC,SPACES                                                   
         MVI   SVEST,0                                                          
*                                                                               
PPAT06   LA    R3,P+PPERST-PLINE                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,PATSTART),(2,SVPATSTR)                            
         GOTO1 (RF),(R1),(3,PATEND),(2,SVPATEND)                                
*                                                                               
         GOTO1 (RF),(R1),(3,PATSTART),(5,(R3))    * PERIOD                      
         LA    R3,P+PPEREND-PLINE                                               
         MVC   2(3,R3),=CL3'UFN'                                                
         CLC   PATEND,=XL3'FFFFFF'                                              
         BE    PPAT10                                                           
         GOTO1 (RF),(R1),(3,PATEND),(5,(R3))                                    
PPAT10   MVI   P+PPERDASH-PLINE,C'-'                                            
*                                                                               
         MVC   P+PDESC-PLINE(16),PATDESC        * DESCRIPTION                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL            PATTERN LIST ELEM                            
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PMSL             DO MARKET/STATION LIST                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'        * COMMENT ELEM                               
         BRAS  RE,GETEL                                                         
         BNE   PPAT20                                                           
         BAS   RE,PCMNT                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PPAT20   MVI   ELCODE,X'30'        * COMMERCIAL LIST ELEM                       
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    PPAT21                                                           
         MVC   P+33(13),=C'**NO CMMLS **'                                       
         B     PPAT21C                                                          
*                                                                               
PPAT21   MVI   HIATUS,C'N'                                                      
         CLC   =C'HIATUS',PATCML-PATCMLEL(R6)                                   
         BNE   PPAT24                                                           
         MVI   HIATUS,C'Y'                                                      
         MVC   P+33(6),=C'HIATUS'                                               
PPAT21C  MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                         - AND GOTO INST RECAP RECS          
*                                                                               
PPAT24   BRAS  RE,PCML                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'32'        * ROTATION(COML PATRN ELEM)                  
         USING PATPTNEL,R6                                                      
         BRAS  RE,GETEL            IS THERE A ROTATION ELEM                     
         BE    PPAT30                                                           
         DC    H'0'                                                             
*                                                                               
PPAT30   GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+2(16),=C'ROTATION PATTERN'                                     
         LLC   R1,PATPTNLN                                                      
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+31(0),PATPTN-PATPTNEL(R6)                                      
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT ABSURD PERCENT ROTATION ELEMENT, IF ANY *                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'36'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PPAT40                                                           
         USING PATPCTEL,R6                                                      
         BRAS  RE,PCT              FORMAT PERCENT ELEMENT TO ELEM               
*                                                                               
         LLC   R1,PATPCTLN                                                      
         SH    R1,=H'2'            GET PCT LEN                                  
         SR    R0,R0                                                            
         D     R0,=F'3'            DIVIDE BY THREE (GET ENTRIES)                
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT ZERO                      
         LR    R0,R1               GET NUMBER OF COMMAS NEEDED                  
         BCTR  R0,0                                                             
         SLL   R1,2                * 4 (A=00)                                   
         AR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+31(0),ELEM                                                     
         MVC   P+2(17),=C'ROTATION PERCENTS'                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT PERCENT ROTATION ELEMENT, IF ANY *                                      
*                                                                               
PPAT40   L     R6,AIO                                                           
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         BNE   EXIT                                                             
         USING PATPCTEL,R6                                                      
         BRAS  RE,PCT              FORMAT PERCENT ELEMENT TO ELEM               
*                                                                               
         LLC   R1,PATPCTLN                                                      
         SH    R1,=H'2'            GET PCT LEN                                  
         SR    R0,R0                                                            
         D     R0,=F'3'            DIVIDE BY THREE (GET ENTRIES)                
         LTR   R0,R1                                                            
         BZ    EXIT                                                             
         BCTR  R0,0                                                             
         SLL   R1,2                * 4 (A=00)                                   
         AR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+31(0),ELEM                                                     
         MVC   P+2(17),=C'ROTATION PERCENTS'                                    
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
* PRINT ESTIMATE DESCRIPTION *                                                  
*                                                                               
         DS    0H                                                               
PESTD    NTR1                                                                   
*                                                                               
         CLC   SVESTPRD(2),CURPRD  IS THIS AND PREV SAME                        
         BE    EXIT                 YES                                         
*                                                                               
         MVC   SVESTPRD(2),CURPRD                                               
         MVC   INSTKEY,KEY                                                      
*                                                                               
         MVC   SVEDESC,SPACES                                                   
         MVC   SVEDESC(7),=C'UNKNOWN'                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),PATKAM-PATKEY+INSTKEY                                   
*                                                                               
         LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
PESTD10  CLC   3(1,R1),CURPRD                                                   
         BE    PESTD20                                                          
         CLI   0(R1),0                                                          
         BNH   PESTD14                                                          
         LA    R1,4(,R1)                                                        
         BCT   R0,PESTD10                                                       
PESTD14  DC    H'0'                                                             
*                                                                               
PESTD20  MVC   KEY+4(3),0(R1)                                                   
         MVC   KEY+7(1),CUREST                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'     SWITCH TO SPOT SYSTEM                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PESTD50                                                          
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'     SWITCH TO SPOT SYSTEM                  
         GOTO1 GETREC                                                           
         MVC   SVEDESC,EDESC-ESTHDR(R6)                                         
         MVC   AIO,AIO1                                                         
*                                                                               
PESTD50  MVC   KEY,INSTKEY                                                      
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   SVREFSUB,=X'FFFFFF' THIS A HIATUS                                
         BE    EXIT                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH              TO RESTORE PROPER REC FOR READ SEQ             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* PRINT HIATUS PATTERN RECORD LINE *                                            
*                                                                               
         DS    0H                                                               
PHAT     NTR1                                                                   
         USING XLIST,R4                                                         
*                                                                               
         LA    R3,XPRD1            * PRD-LEN                                    
         LA    R5,SVPRDNM                                                       
         BAS   RE,PPRD             PROD AT R5, PROD NAME NEXT LINE              
*                                                                               
         CLI   XPRD2,0             IS THERE A PARTNER                           
         BE    PHAT02                NO                                         
         LA    R3,XPRD2            * PTR-LEN                                    
         LA    R5,SVPRDNM2                                                      
         BAS   RE,PPRD             PROD AT R5, PROD NAME NEXT LINE              
         B     PHAT04                                                           
*                                                                               
PHAT02   MVC   SVPRDNM2,SPACES                                                  
*                                                                               
PHAT04   MVC   SVCCODE,SPACES                                                   
         MVC   SVCCODE(1),XCOPY     * COPY CODE                                 
*                                                                               
         TM    XFLAG,X'20'         COPY CODE = EST                              
         BZ    PHAT05                                                           
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,SVCCODE                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         EDIT  (R5),(3,SVCCODE),ALIGN=LEFT                                      
*                                                                               
         STC   R5,CUREST                                                        
         MVC   CURPRD,XPRD1                                                     
         MVC   SVREFSUB,XREF                                                    
         BAS   RE,PESTD                                                         
         B     PHAT06                                                           
*                                                                               
PHAT05   MVC   SVEDESC,SPACES                                                   
         MVI   SVEST,0                                                          
*                                                                               
PHAT06   LA    R3,P+PPERST-PLINE                                                
         GOTO1 DATCON,DMCB,(2,XFTD),(5,(R3))    * PERIOD                        
         MVI   P+PPERDASH-PLINE,C'-'                                            
         GOTO1 (RF),(R1),(2,XLTD),(5,9(R3))                                     
*                                                                               
         MVC   P+PDESC-PLINE(27),=C'HIATUS PATTERN, NO ROTATION'                
*                                                                               
         CLC   XREF,=X'FFFFFF'     NO PATTERN                                   
         BNE   *+10                                                             
         MVC   P+PDESC-PLINE(27),=CL27'NO PATTERN, NO ROTATION'                 
*                                                                               
         MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT PRODUCT CODE AND SPOT LEN *                                            
*                                                                               
         DS    0H                                                               
FPRD     NTR1                                                                   
         LR    R2,R0                                                            
         LR    R3,R1                                                            
         CLI   0(R2),0             ANY PROD TO PRINT                            
         BE    FPRDX                NO                                          
         LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
FPRD10   CLI   0(R1),C' '                                                       
         BNH   FPRDBAD                                                          
         CLC   0(1,R2),3(R1)                                                    
         BE    FPRD12                                                           
         LA    R1,4(R1)                                                         
         BCT   R0,FPRD10                                                        
FPRDBAD  DC    H'0'                                                             
FPRD12   MVC   0(3,R3),0(R1)                                                    
         LA    R3,2(,R3)                                                        
         CLI   0(R3),C' '                                                       
         BNH   FPRD14                                                           
         LA    R3,1(R3)                                                         
FPRD14   MVI   0(R3),C'-'                                                       
         EDIT  (B1,1(R2)),(3,1(R3)),ALIGN=LEFT                                  
FPRDX    B     EXIT                                                             
         EJECT                                                                  
* PRINT PRODUCT CODE, SPOT LEN, AND PRODUCT NAME *                              
*                                                                               
         DS    0H                                                               
PPRD     NTR1                                                                   
         CLI   0(R3),0                                                          
         BE    PPRDX                                                            
*                                                                               
         L     R1,ASVCLIST                                                      
PPRD10   CLI   0(R1),C' '                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),3(R1)                                                    
         BE    PPRD12                                                           
         LA    R1,4(R1)                                                         
         B     PPRD10                                                           
PPRD12   CLC   0(3,R5),0(R1)                                                    
         BE    PPRDX                                                            
         MVC   0(3,R5),0(R1)                                                    
*                                                                               
* GET PRODUCT REC AND SAVE PROD NAME IN WORK                                    
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD & BCLT                                           
         MVC   KEY+4(3),0(R5)      MOVE IN ALPHA PROD                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'     SWITCH TO SPOT SYSTEM                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'     SWITCH TO SPOT SYSTEM                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
         LA    R5,2(R5)                                                         
         CLI   0(R5),C' '                                                       
         BNH   PPRD14                                                           
         LA    R5,1(R5)                                                         
PPRD14   CLI   1(R3),0             ANY SPOT LEN                                 
         BE    PPRD20                                                           
         MVI   0(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
         EDIT  (B1,1(R3)),(3,(R5)),ALIGN=LEFT                                   
*                                                                               
         LA    R5,2(R5)                                                         
         CLI   0(R5),C' '                                                       
         BNH   PPRD20                                                           
         LA    R5,1(R5)                                                         
PPRD20   LA    R5,1(R5)                                                         
         MVC   0(L'PNAME,R5),PNAME                                              
         DROP  R6                                                               
PPRDX    B     EXIT                                                             
         EJECT                                                                  
* MARKET/STATION LIST *                                                         
*                                                                               
         USING PATLSTEL,R6                                                      
         DS    0H                                                               
PMSL     NTR1                                                                   
*                                                                               
         LA    R2,P+31                                                          
         LA    R3,77(,R2)          HIGH LIMIT                                   
         LLC   R1,PATLSTLN                                                      
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         D     R0,=F'5'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R1               R4 HAS NUM OF MKT/STA                        
         LA    R5,PATLST                                                        
         LR    R0,R2               SAVE PRINT LINE START ADDRESS                
         MVC   P+2(16),=C'MARKETS/STATIONS'                                     
         CLI   PATLSTTY,C'T'       STATION TYPE                                 
         BNE   PMS20                                                            
         MVC   0(5,R2),=C'TYPE='                                                
PMS10    MVC   5(1,R2),0(R5)                                                    
         BCT   R4,PMS16                                                         
         B     PMS70                                                            
PMS16    MVI   6(R2),C','                                                       
         LA    R2,2(,R2)                                                        
         LA    R5,5(,R5)           INCREMENT TO NXT IN PATLST                   
         B     PMS10                                                            
PMS20    CLI   PATLSTTY,C'A'       AFFILIATE                                    
         BNE   PMS30                                                            
         MVC   0(4,R2),=C'AFF='                                                 
PMS24    MVC   4(3,R2),0(R5)                                                    
         BCT   R4,PMS26                                                         
         B     PMS70                                                            
PMS26    MVI   7(R2),C','                                                       
         LA    R2,4(,R2)                                                        
         LA    R5,5(,R5)           INCREMENT TO NXT IN PATLST                   
         B     PMS24                                                            
PMS30    CLI   PATLSTTY,C'M'                                                    
         BNE   PMS40                                                            
         OC    PATLST(5),PATLST                                                 
         BNZ   PMS40                                                            
         MVC   P+31(3),=C'ALL'                                                  
         B     PMS70                                                            
*                                                                               
PMS40    CLI   PATLSTTY,C'C'       COMBINED                                     
         BE    PMS42                YES                                         
         CLI   PATLSTTY,C'M'       MARKET                                       
         BE    PMS42                YES                                         
         CLI   PATLSTTY,C'S'       STATION                                      
         BE    PMS46                YES                                         
         CLI   PATLSTTY,C'G'       MARKET GROUP                                 
         BE    PMS48                YES                                         
         DC    H'0'                                                             
PMS42    CLI   0(R5),0             MARKET                                       
         BNE   PMS44                NO, AFFILIATE                               
         SR    R1,R1                                                            
         ICM   R1,3,3(R5)                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB+5(3)                                                 
         MVC   0(4,R2),WORK+1                                                   
         LA    R2,4(,R2)                                                        
         B     PMS50                                                            
*                                                                               
PMS44    MVC   0(3,R2),0(R5)                                                    
         LA    R2,3(R2)                                                         
         B     PMS50                                                            
*                                                                               
PMS46    OC    0(2,R5),0(R5)       THIS A CABLE HEAD                            
         BNZ   PMS47               NO                                           
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',(R5)),WORK,WORK+4                             
*                                                                               
         MVC   0(8,R2),WORK+4                                                   
         MVI   4(R2),C'/'                                                       
         LA    R2,7(,R2)                                                        
         CLI   0(R2),C' '                                                       
         BNH   PMS50                                                            
         LA    R2,1(,R2)                                                        
         B     PMS50                                                            
PMS47    MVC   0(4,R2),0(R5)       MOVE IN 1ST 4 LETTERS                        
         LA    R2,3(,R2)                                                        
         CLI   0(R2),C' '          SEE IF ONLY THREE LETTERS                    
         BNH   *+8                                                              
         LA    R2,1(,R2)           BUMP OVER 4TH LETTER                         
         CLI   4(R5),C'T'          IF TV                                        
         BE    PMS50                ONLY PRINT STATION LETTERS                  
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),4(R5)                                                    
         MVI   2(R2),C'M'                                                       
         LA    R2,3(,R2)                                                        
         B     PMS50                                                            
*                                                                               
PMS48    MVC   0(1,R2),0(R5)       MOVE IN LETTER OF MARKET GRP SCHEME          
         UNPK  DUB(5),1(3,R5)      GET 4 DIGITS                                 
         MVC   1(4,R2),DUB         DISPLAY THEM                                 
         LA    R2,5(,R2)                                                        
*                                                                               
PMS50    LA    R5,5(,R5)           INCREMENT TO NXT IN PATLST                   
         BCT   R4,PMS54                                                         
         B     PMS70                                                            
PMS54    CR    R3,R2                                                            
         BNH   PMS60                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         B     PMS40                                                            
PMS60    LR    R2,R0               GET START OF PRINT                           
         LA    R2,132(,R2)         INCREMENT TO NXT P LINE                      
         LR    R0,R2               SAVE PRINT LINE START ADDRESS                
         LA    R3,77(,R2)          RESET MAX MKT/STA PER LINE COUNTER           
         B     PMS40                                                            
PMS70    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* GET COMMENTS *                                                                
*                                                                               
         USING PATCMTEL,R6                                                      
         DS    0H                                                               
PCMNT    NTR1                                                                   
         LA    R3,4                4=MAX CMNT LINES                             
         MVC   P+2(8),=C'COMMENTS'                                              
         LA    R2,P+31                                                          
         B     CM20                                                             
CM10     BRAS  RE,NEXTEL                                                        
         BNE   CMX                                                              
CM20     LLC   R1,PATCMTLN                                                      
         SH    R1,=H'4'            ACTUAL COMNT LEN-1                           
         EX    R1,CMVC                                                          
         LA    R2,132(R2)          INCRMNT TO NXT P LINE                        
         BCT   R3,CM10                                                          
         BRAS  RE,NEXTEL           5TH CMNT                                     
         BNE   CMX                                                              
         DC    H'0'                                                             
CMX      B     EXIT                                                             
*                                                                               
CMVC     MVC   0(0,R2),PATCMT-PATCMTEL(R6)                                      
         DROP  R6                                                               
         EJECT                                                                  
SIZERR   L     R1,=A(SIZERRMS)                                                  
         LA    R2,TRAPERH                                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    ERREXIT                                                          
         L     R1,=A(SIZERRMA)                                                  
         B     ERREXIT                                                          
         B     ERREXIT                                                          
REFERR   LA    R2,TRAREFH                                                       
         L     R1,=A(REFERMSG)                                                  
ERREXIT  MVC   CONHEAD(10),=CL10'** ERROR **'                                   
         A     R1,SPTR43RR                                                      
         MVC   CONHEAD+10(50),0(R1)                                             
         GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
PRDINV   MVI   ERROR,INVPRDCD      POL & AAA INVALID PROD                       
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
* HEAD HOOK ROUTINE *                                                           
*                                                                               
HDHK     DS    0H                                                               
         MVC   H2+10(L'MEDNM),MEDNM                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
         MVC   H5+14(L'SVPRDNM),SVPRDNM                                         
         MVC   H6+14(L'SVPRDNM2),SVPRDNM2                                       
         MVC   H7+14(3),SVCCODE                                                 
         MVC   H7+18(L'EDESC),SVEDESC                                           
         MVC   H4+42(18),SPECTLE                                                
*                                                                               
         OC    FTRAFF,FTRAFF       FILTERING ON AFFILIATE                       
         BZ    HDHK10                                                           
         MVC   H6+45(5),=C'AFF ='                                               
         MVC   H6+51(3),FTRAFF                                                  
*                                                                               
HDHK10   TM    FILTERSW,FILTA      THIS AUTO T/A FROM INSTR                     
         BZ    HDHK30                                                           
         TM    FILTERSW,FILINACT   SHOW INACTIVE PATTERNS                       
         BO    HDHK20                                                           
         MVC   H5+50(3),=C'T/A'                                                 
         BR    RE                                                               
HDHK20   MVC   H5+46(12),=C'T/A,INACTIVE'                                       
         BR    RE                                                               
HDHK30   TM    FILTERSW,FILINACT   SHOW INACTIVE PATTERNS                       
         BZR   RE                                                               
         MVC   H5+48(8),=C'INACTIVE'                                            
HDHKX    BR    RE                                                               
*                                                                               
MDHK     DS    0H                                                               
         MVI   RCSUBPRG,3          FORCE BOTH HEAD AND MID LINES                
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
         EJECT                                                                  
HEADING  SPROG 0                                                                
         SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,39,C'P A T T E R N   R E C A P'                               
         SSPEC H2,39,C'-------------------------'                               
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H5,3,C'PRODUCT-LEN'                                              
         SSPEC H6,3,C'PARTNER-LEN'                                              
         SSPEC H7,3,C'COPY CODE'                                                
         SSPEC H3,39,PERIOD                                                     
         SSPEC H3,77,RUN                                                        
         SSPEC H4,77,REQUESTOR                                                  
         SSPEC H4,103,PAGE                                                      
*                                                                               
         SSPEC H9,6,C'REF'                                                      
         SSPEC H9,25,C'PERIOD'                                                  
         SSPEC H10,20,C'-----------------'                                      
         SSPEC H9,40,C'DESCRIPTION'                                             
*                                                                               
         SPROG 1                                                                
         SSPEC M1,3,C'MARKET'        *************MID LINES********             
         SSPEC M1,30,C'STATION'                                                 
         SSPEC M1,39,C'INSTRUCT'                                                
         SSPEC M2,41,C'DATE'                                                    
         SSPEC M1,49,C'PERIOD'                                                  
         SSPEC M1,62,C'COPY'                                                    
         SSPEC M2,62,C'CODE'                                                    
         SSPEC M1,70,C'STATION'                                                 
         SSPEC M1,79,C'INSTRUCT'                                                
         SSPEC M2,81,C'DATE'                                                    
         SSPEC M1,89,C'PERIOD'                                                  
         SSPEC M1,102,C'COPY'                                                   
         SSPEC M2,102,C'CODE'                                                   
*                                                                               
         SPROG 3                                                                
         SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,39,C'P A T T E R N   R E C A P'                               
         SSPEC H2,39,C'-------------------------'                               
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H5,3,C'PRODUCT-LEN'                                              
         SSPEC H6,3,C'PARTNER-LEN'                                              
         SSPEC H7,3,C'COPY CODE'                                                
         SSPEC H3,39,PERIOD                                                     
         SSPEC H3,77,RUN                                                        
         SSPEC H4,77,REQUESTOR                                                  
         SSPEC H4,103,PAGE                                                      
         SSPEC M1,3,C'MARKET'        *************MID LINES********             
         SSPEC M1,30,C'STATION'                                                 
         SSPEC M1,39,C'INSTRUCT'                                                
         SSPEC M2,41,C'DATE'                                                    
         SSPEC M1,49,C'PERIOD'                                                  
         SSPEC M1,62,C'COPY'                                                    
         SSPEC M2,62,C'CODE'                                                    
         SSPEC M1,70,C'STATION'                                                 
         SSPEC M1,79,C'INSTRUCT'                                                
         SSPEC M2,81,C'DATE'                                                    
         SSPEC M1,89,C'PERIOD'                                                  
         SSPEC M1,102,C'COPY'                                                   
         SSPEC M2,102,C'CODE'                                                   
         DC    X'00'                                                            
*                                                                               
SIZERRMS DC   CL50'TOO MUCH DATA FOR REQUEST, REQUEST SMALLER PERIOD *'         
SIZERRMA DC   CL50'TOO MUCH DATA ONLINE, REQUEST OVERNITE/SOON *'               
REFERMSG DC    CL50'ONLY OV(ERNIGHT) CAN HAVE REF BLANK (ALL) *'                
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
* VALIDATE PERIOD *                                                             
*                                                                               
         DS    0H                                                               
VPER     NMOD1 0,**+VPE**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         XC    SVPER,SVPER                                                      
         CLI   5(R2),0             MUST ENTER DATES                             
         BE    MISSERRV                                                         
         CLI   5(R2),2             MAY BE ES                                    
         BE    VPER20                                                           
VPER10   MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    TRAPERRV                                                         
         A     R2,0(R1)                                                         
         LA    R2,1(R2)                                                         
         GOTO1 DATCON,(R1),(0,WORK),(3,SVPER1)                                  
         GOTO1 (RF),(R1),(0,WORK),(2,SVPER1P)                                   
         OC    8(2,R2),8(R2)                                                    
         BZ    VPERX                                                            
         GOTO1 DATVAL,(R1),(0,8(R2)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    TRAPERRV                                                         
         GOTO1 DATCON,(R1),(0,WORK),(3,SVPER2)                                  
         GOTO1 (RF),(R1),(0,WORK),(2,SVPER2P)                                   
         CLC   SVPER1,SVPER2                 START NOT GREATER END              
         BH    TRAPERRV                                                         
         B     VPERX                                                            
*                                                                               
VPER20   CLC   =C'ES',8(R2)                                                     
         BNE   VPER10                                                           
         CLI   SVCODE,0                                                         
         BE    ESTENTER                                                         
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   ESTENTER                                                         
         B     VPER34                                                           
VPER30   CLI   SVCODE,0            IF COPY CODE = EST, USE EST DATES            
         BE    VPERX                NO                                          
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPERX                                                            
VPER34   MVC   SVPER,ESTDTS        MAKE REQ PERIOD EST DATES                    
         GOTO1 DATCON,DMCB,(3,ESTSTR),(5,TRAPER)                                
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,ESTEND),(5,TRAPER+9)                                
         MVI   TRAPERH+5,17                                                     
         OI    TRAPERH+6,X'80'                                                  
VPERX    XIT1                                                                   
ESTENTER MVC   CONHEAD(10),=C'* ERROR * '                                       
         MVC   CONHEAD+10(50),ESTENTMS                                          
         GOTO1 ERREX2                                                           
MISSERRV MVI   ERROR,MISSING                                                    
TRAPERRV GOTO1 ERREX                                                            
ESTENTMS DC    CL50'MUST ENTER COPY CODE = ESTIMATE FOR PERIOD = ES *'          
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE PERIOD *                                                             
*                                                                               
         DS    0H                                                               
VFTR     NMOD1 0,**+VFT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             MUST ENTER DATES                             
         BE    VFTRX                                                            
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTRHLP                                                          
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,TRAFLTRH,(2,BLOCK)                                  
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRA             NO                                          
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12               YES, SAVE IT                                
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14               NO, NETHER                                  
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         T/A                                          
         BNE   VFTR20                                                           
         CLI   OFFLINE,C'Y'        ONLY VALID FOR OFFLINE                       
         BNE   VFTRHLP                                                          
         OI    FILTERSW,FILTA                                                   
         B     VFTR80                                                           
*                                                                               
VFTR20   EX    R1,VFTRCLCB         INACTIVE                                     
         BNE   VFTR30                                                           
         CLI   SVREF,X'FF'         THIS AN ALL REF REQ                          
         BNE   VFTR90                                                           
         OI    FILTERSW,FILINACT                                                
         B     VFTR80                                                           
*                                                                               
VFTR30   EX    R1,VFTRCLCC         AFFILIATE                                    
         BNE   VFTR90                                                           
         CLI   1(R4),0             MUST HAVE ENTRY                              
         BE    AFFERR                                                           
         MVC   FTRAFF,22(R4)                                                    
VFTR80   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    XIT1                                                                   
*                                                                               
VFTRCLCA CLC   12(0,R4),=C'T/A '                                                
VFTRCLCB CLC   12(0,R4),=C'INACTIVE '                                           
VFTRCLCC CLC   12(0,R4),=C'AFFILIATE '                                          
*                                                                               
VFTRHLP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'VFTRHPMS),VFTRHPMS                                     
         B     ERREXITA                                                         
AFFERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(L'AFFERRMS),AFFERRMS                                  
         B     VFTR94                                                           
VFTR90   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(37),=C'INACTIVE ONLY FOR ALL REF OVERNIGHT *'         
VFTR94   MVC   CONHEAD(9),=CL9'* ERROR *'                                       
*                                                                               
ERREXITA GOTO1 ERREX2                                                           
MISSERRA MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
AFFERRMS DC    CL50'ENTER 3 CHARACTER AFFILIATE *'                              
VFTRHPMS DC    C'* FILTERS = AFF/INACTIVE FOR ALL REF OVERNIGHT *'              
         DROP  RB,RC                                                            
         EJECT                                                                  
* GET PROFILE REC(S)                                                            
*                                                                               
         DS    0H                                                               
FPRO     NMOD1 0,**+FPR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
* READ T0 PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* AND T1 PROFILE *                                                              
*                                                                               
         MVI   WORK+3,C'1'         T1 PROFILE                                   
*                                                                               
         GOTO1 (RF),(R1),,SVT1PROF,DATAMGR                                      
*                                                                               
* AND TS PROFILE *                                                              
*                                                                               
         MVI   WORK+3,C'S'         TS PROFILE                                   
*                                                                               
         GOTO1 (RF),(R1),,SVTSPROF,DATAMGR                                      
*                                                                               
         XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* SET TO READ MARKET REC TO GET MARKET NAME *                                   
*                                                                               
         DS    0H                                                               
GETMKT   NMOD1 0,*GETMKT*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVC   SVDSKADD,DMDSKADD                                                
         MVC   SVKEY,KEY                                                        
         LA    R5,KEY                                                           
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',INSKMKT-INSKEY+SVKEY),WORK,DUB                
*                                                                               
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES                                                  
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
*                                                                               
         MVC   QSTA,DUB                                                         
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION                          
         BNE   *+10                 NO                                          
         MVC   WORK(4),SVSTAMKT     USE MARKET FROM GENERIC STATION REC         
*                                                                               
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    GM36                                                             
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    GM36                                                             
         MVI   3(RE),C' '                                                       
*                                                                               
GM36     CLC   QMKT,WORK           TEST SAME MARKET AS PREV                     
         BE    GMXX                                                             
         MVC   QMKT,WORK           SAVE MARKET                                  
*                                                                               
         MVI   KEY,C'0'            READ MARKET RECORD                           
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         L     R3,AIO2                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R3)                     
*                                                                               
         MVC   MKTNM,=CL24'**** UNKNOWN ****'                                   
         CLC   KEY(8),0(R3)                                                     
         BNE   GMX                                                              
*                                                                               
         MVC   MKTNM,MKTNAME-MKTRECD(R3)                                        
*                                                                               
GMX      MVC   KEY(L'SVKEY),SVKEY                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH              TO RESTORE PROPER REC FOR READ SEQ             
         MVC   DMDSKADD,SVDSKADD                                                
GMXX     XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* FORMAT PERCENT ELEMENT *                                                      
*                                                                               
PCT      NMOD1 0,**+PCT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         USING PATPCTEL,R6                                                      
         SR    R0,R0                                                            
         LLC   R1,PATPCTLN                                                      
         D     R0,=F'3'                                                         
         LR    R5,R1                                                            
         SR    R1,R1                                                            
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,PATPCTLT                                                      
         B     PCT20                                                            
*                                                                               
PCT10    MVI   0(R2),C','                                                       
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
*                                                                               
PCT20    MVC   0(1,R2),0(R3)                                                    
         MVI   1(R2),C'='                                                       
*                                                                               
         LLC   R0,2(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R2),DUB                                                      
         LA    R2,4(R2)                                                         
         CHI   R0,100                                                           
         BL    PCT22                                                            
         AHI   R2,-4                                                            
         UNPK  2(3,R2),DUB                                                      
         LA    R2,5(R2)                                                         
*                                                                               
PCT22    LA    R1,4(R1)                                                         
         LA    R3,3(R3)                                                         
         BCT   R5,PCT10                                                         
         XIT1                                                                   
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* VALIDATE COPY CODE (MAY BE ESTIMATE IF T1 PROFILE 12 ON) *                    
*                                                                               
         DS    0H                                                               
VCC      NMOD1 0,**+VCC**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVI   SVCODE,0                                                         
         MVI   CODESW,C'N'                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VCCX                                                             
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VCC30                                                            
*                                                                               
         GOTO1 VALINUM                                                          
*                                                                               
         MVC   SVCODE,ACTUAL                                                    
         MVI   CODESW,C'Y'                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM(3),BAGYMD & BCLT                                          
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,SVCODE                                                   
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         MVC   FILENAME,=CL8'SPTDIR'     SWITCH TO SPOT SYSTEM                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ESTCDER                                                          
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         MVC   FILENAME,=CL8'SPTFIL'     SWITCH TO SPOT SYSTEM                  
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(3,ESTSTR)                                
         GOTO1 (RF),(R1),(0,EEND),(3,ESTEND)                                    
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         B     VCCX                                                             
         DROP  R4                                                               
*                                                                               
VCC30    CLI   5(R2),1                                                          
         BNE   CPYCODER                                                         
         GOTO1 ANY                                                              
*                                                                               
         MVC   SVCODE,WORK                                                      
VCCX     XIT1                                                                   
ESTCDER  MVC   CONHEAD,ESTCDMS                                                  
         B     VCCERX                                                           
CPYCODER MVC   CONHEAD,CPYCODMS                                                 
VCCERX   GOTO1 ERREX2                                                           
ESTCDMS  DC    CL60'* ERROR * ESTIMATE NOT FOUND *'                             
CPYCODMS DC    CL60'* ERROR * COPY CODE CAN ONLY BE 1 CHARACTER *'              
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* PRINT COMMERCIAL LIST                                                         
*==========================================================                     
                                                                                
         USING PATCMLEL,R6                                                      
PCML     NTR1  BASE=*,LABEL=*                                                   
         MVI   ALPHA,X'C1'         COMMERCIAL PAIRS A-L                         
*                                                                               
         LLC   R3,PATCMLLN         ELEM LEN                                     
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         SRL   R3,4                DIVIDE BY 16=NUM OF COMML PAIRS              
         LA    R2,PATCML           1ST COMMERCIAL                               
         MVC   P+2(11),=C'COMMERCIALS'                                          
*                                                                               
PCML02   MVC   P+31(1),ALPHA                                                    
         MVI   P+32,C'='                                                        
         MVC   P+33(8),0(R2)                                                    
         CLC   =X'5C00',0(R2)                                                   
         BE    PCML12                                                           
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PCML10                                                           
         GOTO1 VTRPACK,DMCB,(C'U',(R2)),P+33                                    
*                                                                               
PCML10   LA    R4,P+46                                                          
         BAS   RE,PCDESC                                                        
*                                                                               
PCML12   LA    R2,8(R2)                                                         
*                                                                               
         OC    0(8,R2),0(R2)                                                    
         BZ    PCML20                                                           
         MVC   P+72(8),0(R2)                                                    
         CLC   =X'5C00',0(R2)                                                   
         BE    PCML20                                                           
*                                                                               
PCML14   CLI   ADIDFLAG,C'Y'                                                    
         BNE   PCML16                                                           
         GOTO1 VTRPACK,DMCB,(C'U',0(R2)),P+72                                   
*                                                                               
PCML16   LA    R4,P+86                                                          
         BAS   RE,PCDESC                                                        
*                                                                               
PCML20   LA    R2,8(R2)                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   ALPHA,X'C9'                                                      
         BNE   PCML24                                                           
         MVI   ALPHA,X'D1'                                                      
         B     PCML26                                                           
*                                                                               
PCML24   LLC   R5,ALPHA                                                         
         LA    R5,1(R5)                                                         
         STC   R5,ALPHA                                                         
*                                                                               
PCML26   BCT   R3,PCML02                                                        
*                                                                               
PCMLX    J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================              
* PRINT COMML DESCRIPTION                                                       
*=================================================================              
                                                                                
PCDESC   NTR1                                                                   
*                                                                               
         CLC   =X'5C00',0(R2)  DELETED COMML                                    
         BE    PCDESC50                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),0(R2)      CML CODE                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   0(L'CMLTITLE,R4),CMLTITLE                                        
         MVC   WORK(L'CMLCLTNO),CMLCLTNO                                        
         LA    R4,132(,R4)                                                      
         MVI   ELCODE,X'30'                                                     
*                                                                               
PCDESC10 BRAS  RE,NEXTEL                                                        
         BNE   PCDESC20                                                         
         MVC   0(L'CMLDSC,R4),3(R6)                                             
         LA    R4,132(,R4)                                                      
         B     PCDESC10                                                         
*                                                                               
PCDESC20 OC    WORK(L'CMLCLTNO),WORK                                            
         JZ    EXIT                                                             
         MVI   1(R4),C'('                                                       
         MVC   2(L'CMLCLTNO,R4),WORK                                            
         LA    R1,22(R4)                                                        
*                                                                               
PCDESC30 CLI   0(R1),C' '                                                       
         BH    PCDESC34                                                         
         BCT   R1,PCDESC30                                                      
*                                                                               
PCDESC34 MVI   1(R1),C')'                                                       
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
PCDESC50 MVC   0(20,R4),=C'DELETED FROM PATTERN'                                
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
*        INCLUDES SPTRCMML                                                      
*                 SPGENMKT                                                      
*                 SPGENCLT                                                      
*                 SPGENPRD                                                      
*                 SPGENEST                                                      
*                 SPGENSTA                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
STAMASD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAD3D                                                       
* INCLUDED DSECTS                                                               
* INCLUDE SPTRAWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0F                                                               
SPTR43RR DS    F                                                                
AXLIST   DS    F                                                                
AXLISTND DS    F                                                                
VTRPACK  DS    A                                                                
*                                                                               
SVTSPROF DS    CL16                                                             
SVTSPRO1 EQU   SVTSPROF+0          USE GENERIC STATION FOR MARKET               
SVTSPRO2 EQU   SVTSPROF+1          PAGE BREAK BY STATION AFFILIATE              
SVTSPRO3 EQU   SVTSPROF+2          FORCE 1 COMMERCIAL PER LIST                  
SVTSPRO4 EQU   SVTSPROF+3          SUPPRESS COMMERCIAL TYPE                     
SVTSPRO5 EQU   SVTSPROF+4          SUPPRESS COMMERCIAL COUNTS                   
*                                                                               
INSFLDCT DS    XL1                 SAVE NUM OF PATTERN REFS.                    
HIATUS   DS    CL1                 Y=HIATUS PATTERN                             
SVCODE   DS    XL1                                                              
CODESW   DS    XL1                                                              
ESTDTS   DS    0XL6                                                             
ESTSTR   DS    XL3                                                              
ESTEND   DS    XL3                                                              
*                                                                               
CURPRD   DS    XL1                                                              
CUREST   DS    XL1                                                              
SVESTPRD DS    XL1                                                              
SVEST    DS    XL1                                                              
SVEDESC  DS    CL20                                                             
SVCCODE  DS    CL3                                                              
*                                                                               
SVPRDNM  DS    CL28                                                             
SVPRDNM2 DS    CL28                                                             
*                                                                               
SVREF    DS    XL2                                                              
SVREF2   DS    XL2                                                              
SVREFSUB DS    XL3                                                              
*                                                                               
*                                  SAVED DATES FROM PATTERN                     
SVPATSTR DS    XL2                                                              
SVPATEND DS    XL2                                                              
SVPRD    DS    XL4                 PRD/SLN/PRD2/SLN2 FROM PAT FOR INST          
SVAFFIL  DS    CL3                                                              
SVDSKADD DS    CL4                                                              
COMPKEY  DS    CL6                                                              
SVMKTNM  DS    CL24                                                             
SVTWX    DS    CL20                                                             
SVTWXAB  DS    CL20                                                             
SVSTAMKT DS    CL4                                                              
SPECTLE  DS    CL18           =C'LAST TELECAST DATE' IF NO DATE SELECT          
OFSW     DS    CL1                 P LINE SWITCH                                
ALPHA    DS    CL1                                                              
SVXKEY   DS    CL13   PRD1, SLN1, PRD2, SLN2, COPY, REFSUB MKT/STA              
SVPER    DS    0XL6                                                             
SVPER1   DS    XL3                                                              
SVPER2   DS    XL3                                                              
SVPER1P  DS    XL2                                                              
SVPER2P  DS    XL2                                                              
WRKDT    DS    CL2                                                              
HOLDCOPY DS    XL1                                                              
INSTKEY  DS    CL17                                                             
*                                                                               
FILTERS  DS   0CL5                                                              
FILTERSW DS    CL1                                                              
FILTA    EQU   X'80'               TURN/AROUND FROM OVERNITE INSTR              
FILINACT EQU   X'40'               SHOW INACTIVE PATTERNS                       
FTRAFF   DS    CL3                                                              
HOLDSIGN DS    CL1                                                              
*                                                                               
*                                                                               
XLISTSTR DS    0D                                                               
*                                                                               
XLIST    DSECT                                                                  
XPRD1    DS    CL1                                                              
XP1LN    DS    CL1                                                              
XPRD2    DS    CL1                                                              
XP2LN    DS    CL1                                                              
XCOPY    DS    CL1                                                              
XREF     DS    CL3                                                              
XMKT     DS    CL2                                                              
XSTA     DS    CL3                                                              
XINSDT   DS    CL2                                                              
XFLAG    DS    XL1                                                              
XDTS     DS    0XL4                                                             
XFTD     DS    XL2                                                              
XLTD     DS    XL2                                                              
XNEXT    EQU   *                                                                
XLISTLN  EQU   *-XLIST                                                          
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTA     DS    CL7                                                              
         DS    CL2                                                              
LMKT     DS    CL23                                                             
         DS    CL1                                                              
LPRDSLN  DS    CL7                                                              
         DS    CL1                                                              
LPTRSLN  DS    CL7                                                              
         DS    CL1                                                              
LINSDTE  DS    CL8                                                              
         DS    CL2                                                              
LFTD     DS    CL6                                                              
LLTD     DS    CL8                                                              
         DS    CL1                                                              
LCOPY    DS    CL3                                                              
*                                                                               
* PRINT LINE DSECT FOR REPORTS *                                                
*                                                                               
PLINE    DSECT                                                                  
         DS    CL4                                                              
PREF     DS    CL5                                                              
         DS    CL10                                                             
PPERST   DS    CL8                                                              
PPERDASH DS    CL1                                                              
PPEREND  DS    CL8                                                              
         DS    CL3                                                              
PDESC    DS    CL16                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074SPTRA43   09/24/09'                                      
         END                                                                    
