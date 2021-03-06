*          DATA SET ACCAP10    AT LEVEL 011 AS OF 12/11/09                      
*PHASE T61D10A                                                                  
*INCLUDE QSORT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP10 -- COSTING RATES DISPLAY                     *         
*                                                                     *         
*  COMMENTS:     DISPLAYS COSTING RATES                               *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN: ACCAPF0                                      *         
*                                                                     *         
*  OUTPUTS:      JUST DISPLAYS                                        *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
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
* DCUR L 006 CHANGE EDIT CALL TO INCLUDE MINUS SIGN AND OC W/ SPACES  *         
*            WHEN READING METHOD REC BY METHOD CODE                   *         
***********************************************************************         
         TITLE 'T61D10 - COSTING RATES DISPLAY'                                 
T61D10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D10**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                                                      
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         LA    R2,PPFTABLE         YES, USE SEND PFKEY TABLE                    
         LA    R3,CRAPFKYH                                                      
*              PROGRAM INITIALIZATION - DON'T CLEAR WORKING STORAGE             
INITPFKY GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE KEY                              *         
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         MVC   PERCODE,SPACES      PERSON CODE                                  
         MVI   BITS,0                                                           
*                                                                               
         GOTO1 GETLDG,DMCB,C'1R'   GET ACCOUNT LEVEL LENGTHS                    
*                                                                               
         BAS   RE,VALOPTS          VALIDATE OPTIONS                             
*                                                                               
*----------------------------------------------------------------------         
*                METHOD CODE -- DEFAULTS TO METHOD 1                            
*----------------------------------------------------------------------         
*                                                                               
VK10     DS    0H                                                               
         XC    METHCODE,METHCODE                                                
         MVI   METHNUM,C'1'        DEFAULT TO METHOD 1                          
*                                                                               
         LA    R2,CRAMETHH                                                      
         CLI   CRAMETHH+5,0        ANY DATA?                                    
         BE    VK11                DEFAULT TO METHOD 1                          
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   VK20                                                             
*                                                                               
*                                                                               
*----------------------------------------------------------------------         
*                                  READ RECORD BY METHOD NUMBER                 
*                                                                               
         CLI   5(R2),1             ONLY 1-9                                     
         BNE   EINVMETH                                                         
         MVC   METHNUM,8(R2)                                                    
*                                                                               
VK11     LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM    METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         B     VK30                                                             
         DROP  R6                                                               
*                                                                               
*----------------------------------------------------------------------         
*                                  READ RECORD BY METHOD CODE                   
*                                                                               
VK20     MVC   METHCODE,CRAMETH    SAVE METHOD CODE                             
*                                                                               
         LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CMTRECD,R6                                                       
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CMTKSUB,CMTKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CMTKCPY,CMPY        COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD CODE                                  
         OC    CMTKMTHD,SPACES                                                  
         B     VK30                                                             
         DROP  R6                                                               
*                                                                               
*----------------------------------------------------------------------         
*                                  GET BOTH METHNUM AND METHCODE                
*                                                                               
VK30     DS    0H                                                               
         MVC   AIO,AIO3            METHOD RECORD IN AIO3                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ENOMETH                                                          
         GOTO1 GETREC                                                           
         GOTO1 GETNME,DMCB,AIO,CRAMENMH     PUT OUT NAME                        
         MVC   AIO,AIO1                                                         
         USING METELD,R6                                                        
         L     R6,AIO3                                                          
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   METHNUM,METNUM      SAVE BOTH NUMBER AND CODE                    
         MVC   METHCODE,METCODE                                                 
         MVC   CRAMETH,METCODE     PUT OUT CODE                                 
         OI    CRAMETHH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
*----------------------------------------------------------------------         
*                      PERSON CODE                                              
*----------------------------------------------------------------------         
*                                                                               
         LA    R2,CRACODEH                                                      
         CLI   CRACODEH+5,0        ANY DATA?                                    
         BE    ERRPLS              MISSING DATA                                 
         CLC   CRACODEH+5(1),ABCDLEN+3        RIGHT LENGTH?                     
         BH    ETOOLONG                                                         
*        BL    ETOOSHRT                                                         
         MVC   PERCODE,SPACES                                                   
         ZIC   R1,CRACODEH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERCODE(0),CRACODE     SAVE PERSON CODE                          
*                                                                               
         LA    R6,BIGKEY                                                        
         USING PERRECD,R6                                                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CMPY                                                     
         MVC   PERKCODE,PERCODE                                                 
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   EINVPER                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVC   OFFICE,SPACES                                                    
         MVC   DEPT,SPACES                                                      
         MVC   SUBDPT,SPACES                                                    
*                                                                               
         TM    CRAOFFH+4,X'80'     OFFICE CODE ENTERED THIS TIME                
         BO    VK32                VALIDATE LOCATION ENTERED                    
         TM    CRACODEH+4,X'80'    PERSON CODE ENTERED THIS TIME                
         BO    VK36                GET MOST RECENT CODE                         
*                                                                               
VK32     LA    R2,CRAOFFH                                                       
         CLC   CRAOFFH+5(1),ABCDLEN         SHOULD = LEN LEVEL A ACC            
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFICE(0),CRAOFF    SAVE OFFICE                                  
*                                                                               
         LA    R2,CRADEPTH         ANY DEPARTMENT                               
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         CLC   5(1,R2),ABCDLEN+1                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEPT(0),8(R2)                                                    
*                                                                               
         LA    R2,CRASDPTH         ANY SUB DEPT FILTER                          
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         CLC   5(1,R2),ABCDLEN+2                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SUBDPT(0),8(R2)                                                  
*                                                                               
         LA    R2,CRAOFFH                                                       
         BAS   RE,VALLOC           VALIDATE LOCATION FOR PERSON                 
         BNE   EINVLOC                                                          
         B     VK42                                                             
*                                                                               
VK36     L     R6,AIO                                                           
         MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         USING LOCELD,R6                                                        
         ST    R6,STADDR           WANT TO FIND LAST ELEMENT                    
VK40LP   BAS   RE,NEXTEL                                                        
         BNE   VK40                                                             
         ST    R6,STADDR                                                        
         B     VK40LP                                                           
*                                                                               
*----------------------------------------------------------------------         
*     BUILD ACCOUNT FROM MOST RECENT ELEMENT IN PERSON REC                      
*----------------------------------------------------------------------         
*                                                                               
VK40     L     R6,STADDR                                                        
         MVC   OFFICE,LOCOFF                                                    
         OC    OFFICE,SPACES                                                    
         MVC   DEPT,LOCDEPT                                                     
         OC    DEPT,SPACES                                                      
         MVC   SUBDPT,LOCSUB                                                    
         OC    SUBDPT,SPACES                                                    
         DROP  R6                                                               
*                                                                               
VK42     MVC   MYACCT,SPACES                                                    
         LA    R4,MYACCT           BUILD ACCOUNT HERE                           
*                                                                               
         ZIC   R1,ABCDLEN          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),OFFICE                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CRAOFF(0),OFFICE                                                 
         OI    CRAOFFH+6,X'80'                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 GTLEVNM,DMCB,C'1R',MYACCT,CRAOFNMH                               
         MVC   AIO,AIO1                                                         
*                                                                               
         ZIC   R1,ABCDLEN          LENGTH OF LEVEL A                            
         AR    R4,R1               BUMP MYACCT                                  
         ZIC   R1,ABCDLEN+1        LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),DEPT                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CRADEPT(0),DEPT                                                  
         OI    CRADEPTH+6,X'80'                                                 
         MVC   AIO,AIO2                                                         
         GOTO1 GTLEVNM,DMCB,C'1R',MYACCT,CRADPNMH                               
         MVC   AIO,AIO1                                                         
*                                                                               
         ZIC   R1,ABCDLEN+1        LENGTH OF LEVEL B                            
         AR    R4,R1               BUMP MYACCT                                  
         ZIC   R1,ABCDLEN+2        LENGTH OF LEVEL C                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SUBDPT                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CRASDPT(0),SUBDPT                                                
         OI    CRASDPTH+6,X'80'                                                 
         MVC   AIO,AIO2                                                         
         GOTO1 GTLEVNM,DMCB,C'1R',MYACCT,CRASDNMH                               
         MVC   AIO,AIO1                                                         
*                                                                               
         ZIC   R1,ABCDLEN+2        LENGTH OF LEVEL C                            
         AR    R4,R1               BUMP MYACCT                                  
         ZIC   R1,ABCDLEN+3        LENGTH OF LEVEL D                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PERCODE                                                  
         MVC   CRACODE,PERCODE                                                  
         OI    CRACODEH+6,X'80'                                                 
*                                                                               
*                                                                               
*                                                                               
*----------------------------------------------------------------------         
*                   DISPLAY PERSON'S NAME                                       
*----------------------------------------------------------------------         
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         BAS   RE,FLDSEC           MUST BE AUTHORIZED TO SEE NAME               
         BNE   VK55                                                             
*                                                                               
         L     R6,AIO                                                           
         LA    R3,WORK                                                          
         MVI   ELCODE,GPNELQ       GENERAL PURPOSE NAME ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING GPNELD,R6                                                        
VK50     ZIC   R1,GPNLN                                                         
         SH    R1,=H'4'            3 FOR CODE,LEN,AND TYPE + 1 FOR EX           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),GPNNME                                                   
         CLI   GPNTYP,GPNTLST                                                   
         BNE   VK55                                                             
         AR    R3,R1               BUMP LENGTH                                  
         LA    R3,1(R3)            ADD 1 MORE FOR EX                            
         MVI   0(R3),C','                                                       
         LA    R3,2(R3)            1 FOR , THEN 1 MORE                          
*                                                                               
         BAS   RE,NEXTEL           GET FIRST NAME                               
         BNE   VK55                                                             
         B     VK50                                                             
*                                                                               
VK55     DS    0H                                                               
         MVC   CRANAME,WORK                                                     
         OI    CRANAMEH+6,X'80'                                                 
*                                                                               
         BAS   RE,CLRSCR                                                        
*                                                                               
         XC    BIGKEY,BIGKEY       SECURITY CHECK                               
         LA    R6,BIGKEY                                                        
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),=C'1R'   1R                                           
         MVC   ACTKACT,MYACCT      ACCOUNT                                      
         MVC   AIO,AIO2                                                         
         GOTO1 TSTSEC              SECURITY CHECK OF 1R IN BIGKEY               
         MVC   AIO,AIO1                                                         
         DROP  R6                                                               
*                                                                               
*----------------------------------------------------------------------         
*           BUILD KEY TO COSTING PERSONAL RATES RECORD                          
*----------------------------------------------------------------------         
*                                                                               
VK60     LA    R4,BIGKEY           BUILD KEY                                    
         USING CPRRECD,R4                                                       
*                                                                               
         MVC   BIGKEY,SPACES                                                    
         MVI   CPRKTYP,CPRKTYPQ    COSTING PERSONAL RATES RECORD                
         MVI   CPRKSUB,CPRKSUBQ    AND SUB TYPE                                 
         MVC   CPRKCPY,CMPY        COMPANY                                      
         MVI   CPRKUNT,C'1'        UNIT                                         
         MVI   CPRKLDG,C'R'        LEDGER                                       
         MVC   CPRKACT,MYACCT      ACCOUNT                                      
*&&US*&& MVC   CPRKMTHD,METHNUM    METHOD ** NEW AS OF 10/02 **                 
         MVC   SAVEKEY,BIGKEY                                                   
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*       VALIDATE ANY OPTIONS                                                    
***********************************************************************         
*                                                                               
VALOPTS  NTR1                                                                   
         NI    BITS,X'FF'-ONEMNTH                                               
         XC    STDATE,STDATE       OPTIONAL START DATE                          
         XC    ENDATE,ENDATE       OPTIONAL END DATE                            
         LA    R2,CONOPTH          ANY START DATE FILTER?                       
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
         GOTO1 SCANNER,DMCB,(20,(R2)),(1,SCANBLK)                               
         CLI   DMCB+4,0                                                         
         BE    EINVOPT                                                          
         LA    R3,SCANBLK                                                       
         CLC   AC@MNTHU(2),12(R3)    MO=                                        
         BNE   EINVOPT                                                          
         CLI   1(R3),0                                                          
         BE    OPTX                                                             
*                                                                               
         XC    WORK,WORK           MOVE DATE EXPRESSION INTO WORK               
         ZIC   R1,1(R3)            NEED FAKE FIELD HEADER                       
         STC   R1,WORK+5                                                        
         AH    R1,=H'8'                                                         
         STC   R1,WORK                                                          
*                                                                               
         ZIC   R1,1(R3)            NEED FAKE FIELD HEADER                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),22(R3)                                                 
         GOTO1 SCANNER,DMCB,(0,WORK),(2,SCANBLK),C',=-='                        
         CLI   DMCB+4,0                                                         
         BE    EINVOPT                                                          
         CLI   DMCB+4,X'01'                                                     
         BNE   *+8                                                              
         OI    BITS,ONEMNTH                                                     
         LA    R4,SCANBLK                                                       
*                                                                               
         CLI   0(R4),0             ANY FIRST DATE                               
         BE    OPT10                                                            
         ZIC   R6,0(R4)                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         GOTO1 PERVAL,DMCB,((R6),12(R4)),(X'20',BLOCK)                          
         CLI   DMCB+4,PVRCINV1     DATE#1 INVALID                               
         BE    EINVDATE                                                         
         CLI   DMCB+4,PVRCMISS     MISSING                                      
         BE    EINVDATE                                                         
         MVC   STDATE,PVALPSTA                                                  
*                                                                               
OPT10    LA    R4,32(R4)           NEXT LINE                                    
         CLI   0(R4),0             ANY END DATE                                 
         BE    OPTX                                                             
         CLI   0(R4),C' '          ANY END DATE                                 
         BE    OPTX                                                             
         ZIC   R6,0(R4)                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         GOTO1 PERVAL,DMCB,((R6),12(R4)),(X'20',BLOCK)                          
         CLI   DMCB+4,PVRCINV1     DATE#1 INVALID                               
         BE    EINVDATE                                                         
         CLI   DMCB+4,PVRCMISS     MISSING                                      
         BE    EINVDATE                                                         
         MVC   ENDATE,PVALPSTA                                                  
*                                                                               
OPTX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*       CHECK FIELD SECURITY TO SEE WHETHER TO DISPLAY PERSON NAME              
***********************************************************************         
FLDSEC   NTR1                                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   SBYTE,NAMEFLDQ                                                   
         LA    R2,SBYTE                                                         
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES        WRITE ACCESS?                                
         BE    XYES                                                             
         CLI   DMCB,SECPREAD       READ ACCESS?                                 
         BE    XYES                                                             
         B     XNO                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LOCATION IN PERSON REC IN AIO                       *         
*        INPUT                     OFFICE = OFFICE CODE               *         
*        INPUT                     DEPT   = DEPATMENT CODE            *         
*        INPUT                     SUBDPT = SUB-DEPARTMENT CODE       *         
*        OUTPUT                    CC = EQUAL IF VALID, ELSE NOT      *         
***********************************************************************         
*                                                                               
VALLOC   NTR1                      VALIDATE LOCATION ENTEREDD                   
*                                                                               
         USING LOCELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   VLNO                                                             
         B     VL20                                                             
*                                                                               
VL10LP   BAS   RE,NEXTEL                                                        
         BNE   VLNO                                                             
*                                                                               
VL20     DS    0H                                                               
         CLC   OFFICE,LOCOFF                                                    
         BNE   VL10LP                                                           
         CLC   DEPT,LOCDEPT                                                     
         BNE   VL10LP                                                           
         CLC   SUBDPT,LOCSUB                                                    
         BNE   VL10LP                                                           
*                                                                               
         OC    STDATE,STDATE                                                    
         BZ    VL25                                                             
         CLC   STDATE,LOCSTART     MUST FIT PERIOD ENTERED                      
         BL    VL10LP                                                           
         OC    LOCEND,LOCEND                                                    
         BZ    VL25                                                             
         CLC   STDATE,LOCEND                                                    
         BH    VL10LP                                                           
*                                                                               
VL25     DS    0H                                                               
*                                                                               
VLYES    B     XYES                                                             
VLNO     B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                      CLEAR DISPLAY LINES                                      
***********************************************************************         
*                                                                               
CLRSCR   NTR1                                                                   
         LA    R2,CRAHDLNH         HEADLINE                                     
         LA    R3,CRAENDH                                                       
DRCLR    ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
DRCLR5   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BNH   DRCLR               NO                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
*                                                                               
DR       DS    0H                                                               
         BAS   RE,VALOPTS          VALIDATE OPTIONS                             
         BAS   RE,CLRSCR           CLEAR DSPLAY LINES                           
*                                                                               
*----------------------------------------------------------------------         
*                       DISPLAY HEADLINES                                       
*----------------------------------------------------------------------         
*                                                                               
         L     R6,AIO                                                           
         USING LINED,R2                                                         
         LA    R2,CRAHDLNH                                                      
*                                                                               
         MVC   LDATE(L'AC@DATE),AC@DATE                                         
         MVC   LCOL1+2(L'AC@SAL),AC@SAL                                         
         MVC   LCOL2+1(L'AC@BEN),AC@BEN                                         
         MVC   LCOL3+1(L'AC@PEN),AC@PEN                                         
         MVC   LTOTAL+3(L'AC@TOTAL),AC@TOTAL                                    
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT SCREEN LINE                             
         LA    R1,CRAENDH                                                       
*                                                                               
*----------------------------------------------------------------------         
*   DISPLAY PERSONAL HOURLY RATE ELEMENTS FOR A PARTICULAR METHOD               
*----------------------------------------------------------------------         
*                                                                               
         LA    R3,RATETABL         RATE INFO TABLE                              
         USING RATED,R3                                                         
*                                                                               
         LA    R0,RATETABL         CLEAR BLOCK                                  
         L     R1,=A(RATEEND-RATETABL)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    TABCOUNT,TABCOUNT                                                
*                                                                               
         USING PHRELD,R6                                                        
         MVI   ELCODE,PHRELQ       PERSONAL HOURLY RATE ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   DR100                                                            
         ST    R6,STADDR                                                        
         B     DR30                                                             
*                                                                               
DR20NX   DS    0H                                                               
         L     R6,STADDR                                                        
         BAS   RE,NEXTEL                                                        
         BNE   DR69                                                             
         ST    R6,STADDR                                                        
*                                                                               
DR30     DS    0H                  MAKE TABLE OF ELEMENTS TO SORT               
         CLC   PHRMTH,METHNUM      SAME METHOD?                                 
         BNE   DR20NX                                                           
*                                                                               
         TM    BITS,ONEMNTH        DISPLAY ONLY THE 1 MONTH REQUESTED           
         BNO   DR30A                                                            
         CLC   PHRYR(2),STDATE     ANY START AT FILTER                          
         BNE   DR20NX                                                           
         B     DR31                                                             
*                                                                               
DR30A    OC    STDATE,STDATE                                                    
         BZ    DR31A                                                            
         CLC   PHRYR(2),STDATE     ANY START AT FILTER                          
         BL    DR20NX                                                           
DR31A    OC    ENDATE,ENDATE                                                    
         BZ    DR31                                                             
         CLC   PHRYR(2),ENDATE     ANY START AT FILTER                          
         BH    DR69                                                             
*                                                                               
DR31     MVC   RATEDATE,PHRYR      SAVE DATE                                    
*                                                                               
         ZAP   RATESAL,=P'0'                                                    
         ZAP   RATEBEN,=P'0'                                                    
         ZAP   RATEPEN,=P'0'                                                    
         ZAP   RATETOT,=P'0'                                                    
*                                                                               
         ZIC   R4,PHRNUM           NUMBER OF SUBELEMENTS                        
         AH    R6,=Y(PHRLN1Q)      BUMP TO FIRST SUB ELEMENT                    
         USING PHRNTRY,R6                                                       
         B     DR33                                                             
*                                                                               
DR32     AH    R6,=Y(PHRLN2Q)      BUMP TO NEXT SUB ELEMENT                     
DR33     ZAP   WORK(5),PHRRATE(5)                                               
         SRP   WORK(5),62,5                                                     
*                                                                               
         CLI   PHRTYPE,PHRTSAL     SALARY                                       
         BE    DR40                                                             
         CLI   PHRTYPE,PHRTBEN     BENEFIT                                      
         BE    DR42                                                             
         CLI   PHRTYPE,PHRTPEN     PENSION                                      
         BE    DR44                                                             
         CLI   PHRTYPE,PHRTADM     ADMINISTRATION                               
         BE    DR46                                                             
         CLI   PHRTYPE,PHRTTMP     TEMPORARY                                    
         BE    DR48                                                             
         CLI   PHRTYPE,PHRTBON     BONUS                                        
         BE    DR50                                                             
         CLI   PHRTYPE,PHRTOVT     OVERTIME                                     
         BE    DR52                                                             
         CLI   PHRTYPE,PHRTTOT     TOTAL                                        
         BE    DR54                                                             
         DC    H'0'                                                             
*                                                                               
DR40     DS    0H                                                               
         MVC   RATESAL,WORK                                                     
         B     DR60                                                             
DR42     DS    0H                                                               
         MVC   RATEBEN,WORK                                                     
         B     DR60                                                             
DR44     DS    0H                                                               
         MVC   RATEPEN,WORK                                                     
         B     DR60                                                             
DR46     DS    0H                                                               
         B     DR60                                                             
DR48     DS    0H                                                               
         B     DR60                                                             
DR50     DS    0H                                                               
         B     DR60                                                             
DR52     DS    0H                                                               
         B     DR60                                                             
DR54     DS    0H                                                               
         MVC   RATETOT,WORK                                                     
         B     DR60                                                             
*                                                                               
DR60     DS    0H                                                               
         BCT   R4,DR32             MORE SUB ELEMENTS                            
*                                                                               
         LH    R1,TABCOUNT         KEEP COUNT OF ENTRIES FO SORT                
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
*                                                                               
         LA    R3,RATELEN(R3)      NEXT IN TABLE                                
         LA    R1,RATEEND                                                       
         CR    R3,R1                                                            
         BL    DR20NX              NEXT ELEMENT                                 
*                                                                               
DR69     LH    R4,TABCOUNT                                                      
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(C'N',RATETABL),(R4),RATELEN,L'RATEDATE,      X        
               RATEDATE-RATED                                                   
*                                                                               
         LA    R3,RATETABL                                                      
DR70     OC    0(RATELEN,R3),0(R3) ANY ENTRY                                    
         BZ    DR100                                                            
*                                                                               
         XC    YRMON,YRMON         PUT OUT INFO                                 
         MVC   YRMON(2),RATEDATE                                                
         GOTO1 DATCON,DMCB,(1,YRMON),(9,LDATE)                                  
*                                                                               
         EDIT  (P5,RATESAL),(8,LCOL1),2,ZERO=NOBLANK,FLOAT=-,          +        
               ALIGN=RIGHT                                                      
         EDIT  (P5,RATEBEN),(8,LCOL2),2,ZERO=NOBLANK,FLOAT=-,          +        
               ALIGN=RIGHT                                                      
         EDIT  (P5,RATEPEN),(8,LCOL3),2,ZERO=NOBLANK,FLOAT=-,          +        
               ALIGN=RIGHT                                                      
         EDIT  (P5,RATETOT),(8,LTOTAL),2,ZERO=NOBLANK,FLOAT=-,         +        
               ALIGN=RIGHT                                                      
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT SCREEN LINE                             
         LA    R1,CRAENDH                                                       
         CR    R2,R1                                                            
         BH    DR100                                                            
*                                                                               
         LA    R3,RATELEN(R3)      NEXT ENTRY                                   
         B     DR70                                                             
*                                                                               
DR100    DS    0H                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                        RANDOM STUFF                                 *         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
*                                                                               
ERRX     DS    0H                                                               
         MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
EINVLOC  MVC   GERROR,=AL2(ACEIVLOC)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EINVMETH MVC   GERROR,=AL2(ACEIVMET)                                            
         B     ACCERRX                                                          
ENOMETH  MVC   GERROR,=AL2(ACENOMET)                                            
         B     ACCERRX                                                          
EINVPER  MVC   GERROR,=AL2(ACEIVPER)                                            
         B     ACCERRX                                                          
*                                                                               
ACCERRX  DS    0H                                                               
         MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*                           LTORG                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE DEFINITIONS                                                
***********************************************************************         
*                                                                               
PPFTABLE DS    0C                                                               
*                                                                               
*        PERSON DISPLAY                                                         
*                                                                               
         DC    AL1(PPF02X-*,02,PFTCPROG,(PPF02X-PPF02)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#DSP,8                                                         
PPF02    DC    AL1(KEYTYTWA,L'CRACODE-1),AL2(CRACODE-T61DFFD)                   
PPF02X   EQU   *                                                                
*                                                                               
*        PERSON LIST                                                            
*                                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,(PPF01X-PPF01)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
PPF01    DC    AL1(KEYTYTWA,L'CRACODE-1),AL2(CRACODE-T61DFFD)                   
PPF01X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(PPF03X-*,03,PFTCPROG,(PPF03X-PPF03)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
PPF03    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'CRACODE-1),AL2(CRACODE-T61DFFD)                   
PPF03X   EQU   *                                                                
*                                                                               
*        MAD INPUT                                                              
*                                                                               
         DC    AL1(PPF06X-*,06,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
PPF06X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*                          INCLUDES                                   *         
***********************************************************************         
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FASECRETD                                                                     
* ACCAPWORKD                                                                    
* ACCAPDSECT                                                                    
* ACGENFILE                                                                     
* ACDDEQUS                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*                          SCREENS                                    *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPF0D                                                       
         EJECT                                                                  
***********************************************************************         
*                   REMAINING WORK AREA                               *         
***********************************************************************         
*                                                                               
RELO     DS    A                                                                
STADDR   DS    F                   START ADDRESS                                
TABCOUNT DS    H                                                                
PERCODE  DS    CL8                 PERSON CODE                                  
OFFICE   DS    CL2                                                              
DEPT     DS    CL3                                                              
SUBDPT   DS    CL3                                                              
STDATE   DS    CL3                 START DATE (OPTIONAL)                        
ENDATE   DS    CL3                 END DATE (OPTIONAL)                          
METHCODE DS    CL3                 METHOD CODE                                  
METHNUM  DS    X                   METHOD NUMBER                                
BITS     DS    XL1                                                              
ONEMNTH  EQU   X'80'               DISPLAY ONLY ONE MONTH                       
YRMON    DS    CL3                                                              
MYACCT   DS    CL12                MY ACCOUNT                                   
SAVEKEY  DS    XL42                ACCFILE KEY                                  
MYKEY    DS    XL42                ACCFILE KEY                                  
SCANBLK  DS    XL32                SCANNER BLOCK                                
SBYTE    DS    CL1                 SECURITY BYTE                                
NAMEFLDQ EQU   1                   NAME FIELD NUMBER                            
         EJECT                                                                  
***********************************************************************         
*                         DSECTS                                      *         
***********************************************************************         
*                                                                               
BLOCKSD  DSECT                     TABLE                                        
RATETABL DS    100CL(RATELEN)                                                   
RATEEND  DS    0C                                                               
*                                                                               
RATED    DSECT                     TO COVER RATE TABLE                          
RATEDATE DS    PL2                 PACKED YR-MON                                
RATESAL  DS    PL5                 SALARY RATE                                  
RATEBEN  DS    PL5                 BENEFIT RATE                                 
RATEPEN  DS    PL5                 PENSION RATE                                 
RATETOT  DS    PL5                 TOTAL RATE                                   
RATELEN  EQU   *-RATED                                                          
*                                                                               
*                                                                               
LINED    DSECT                                                                  
         DS    CL8                 FOR FIELD HEADER                             
LDATE    DS    CL6                 DATE                                         
         DS    CL1                                                              
LCOL1    DS    CL8                 COLUMN ONE                                   
         DS    CL1                                                              
LCOL2    DS    CL8                                                              
         DS    CL1                                                              
LCOL3    DS    CL8                                                              
         DS    CL1                                                              
LCOL4    DS    CL8                                                              
         DS    CL1                                                              
LTOTAL   DS    CL8                                                              
         DS    CL1                                                              
LCOL6    DS    CL8                                                              
         DS    CL1                                                              
LCOL7    DS    CL8                                                              
         DS    CL1                                                              
LCOL8    DS    CL8                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACCAP10   12/11/09'                                      
         END                                                                    
