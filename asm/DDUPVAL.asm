*          DATA SET DDUPVAL    AT LEVEL 044 AS OF 07/29/09                      
*PHASE T00A13A,+0                                                               
         SPACE 1                                                                
**********************************************************************          
*                                                                    *          
* PARAMETER LIST AS FOLLOWS                                          *          
*        ...0...      ...1-3...                                      *          
*                                                                    *          
*    P1  MAX EXP      A(FLDHDR)                                      *          
*    P2  DEC SUPPORT  Y=1 DEC UPGRADES SUPPORTED                     *          
*                     A(RAVLNEL BUILD AREA) ...SEE REGENAVL          *          
*    P3  SPCL CHAR    A(COMFACS)                                     *          
*                                                                    *          
*   'SPCL CHAR' IS USED IN PLACE OF ',' AS EOF CHAR TO SCANNER       *          
*                                                                    *          
**********************************************************************          
         SPACE 3                                                                
**********************************************************************          
*                                                                    *          
* NOTE: FOR TYPE NUMBER 1-6 UPGRADES                                 *          
*                X'40' BIT IS ON FOR DECIMAL PRECISION               *          
*       FOR SPECIFIC DEMO UPGRADES                                   *          
*                X'40' IS OFF FOR DEC. PRECISION                     *          
*       XI    UPGRADETYPE,B'01000000'                                *          
*              WILL CONVERT THE UPGRADE TYPE TO THE                  *          
*              OLD NON-DECIMAL PRECISION CODING SCHEME.              *          
*                                                                    *          
*    VALID EXPRESSIONS             BOOK TYPE CAT. OP1  OP2  OP3  OP4 *          
*    -----------------             TYPE  NO. ---- ---  ---  ---  --- *          
*                                                                    *          
*    HOMES,N,N,N(,N)                     01        N    N    N   (N) *          
*    RATING,N(,N)                        02        N   (N)           *          
*    HUT,N(,N)                           03        N   (N)           *          
*    HUT,N,BOOK                          03        N   YYMM          *          
*    HUT,BOOK,BOOK                  BT   03       YYMM YYMM          *          
*    SHARE,N                             03             N            *          
*    PUT,BOOK,BOOK                  BT   03   P   YYMM YYMM          *          
*    INDEX,N                             04        N                 *          
*    SVI,MONTH                           05       00MM               *          
*    HPT,BOOK(,BOOK),N,(N)          BT   06       YYMM YYMM  N N     *          
*    HPV,BOOK(,BOOK),N,(N)          BT   06   V   YYMM YYMM  H S     *          
*    MIN,N                               07       N                  *          
*    Y4S,BOOK                            08       YYMM               *          
*    MMU,BOOK,BOOK                       09       YYMM YYMM          *          
*    LPM,BOOK,STATION,(ALPHA MKT)        09       YYMM               *          
*    LPS,BOOK,BOOK                       09       YYMM               *          
*    USED BY DEM32 FOR IMS               0A       DONT REUSE         *          
*  ********NOTE X'20' BIT FOR 0B-0E INDICATES USE REP INVENTORY      *          
*    PINDEX,BOOK,BOOK(,B3)(,B4)          0B       YYMM YYMM  YYMM    *          
*    PIM,BOOK,BOOK(,B3)(,B4)             0B   M   YYMM YYMM  YYMM    *          
*    PIY,BOOK,N                          0B   Y   YYMM  N            *          
*    PIQ,BOOK,N                          0B   Q   YYMM  N            *          
*    PIB,BOOK,N                          0B   B   YYMM  N            *          
*    SINDEX,BOOK,BOOK(,B3)(,B4)          0C       YYMM YYMM  YYMM    *          
*    SIM,BOOK,BOOK(,B3)(,B4)             0C   M   YYMM YYMM  YYMM    *          
*    SIY,BOOK,N                          0C   Y   YYMM  N            *          
*    SIQ,BOOK,N                          0C   Q   YYMM  N            *          
*    SIB,BOOK,N                          0C   B   YYMM  N            *          
*    PAVG,BOOK,BOOK(,B3)(,B4)            0D       YYMM YYMM  YYMM    *          
*    PAY,BOOK,N                          0D   Y   YYMM  N            *          
*    PAQ,BOOK,N                          0D   Q   YYMM  N            *          
*    PAB,BOOK,N                          0D   B   YYMM  N            *          
*    SAVG,BOOK,BOOK(,B3)(,B4)            0E       YYMM YYMM  YYMM    *          
*    SAY,BOOK,N                          0E   Y   YYMM  N            *          
*    SAQ,BOOK,N                          0E   Q   YYMM  N            *          
*    SAB,BOOK,N                          0E   B   YYMM  N            *          
*    USED BY DEM32 FOR MULTIWEEK AVG     0F       DONT REUSE         *          
*    RW1849,N                            R    42   N                 *          
*    PW1849,N,N                          P    42   N    N            *          
*                                                                    *          
*    NOTE THAT BOOK EXPRESSIONS MAY OPTIONALLY BE WRITTEN AS:        *          
*           NOV86   NOV86-1  NOV86-2  NOV86-3  NOV86-4               *          
*    IN WHICH CASE THE TOP NIBBLE OF THE MONTH NUMBER WILL BE:       *          
*                0        1        2        3        4               *          
*                                                                    *          
**********************************************************************          
         TITLE 'UPVAL - VALIDATION OF UPGRADE EXPRESSIONS'                      
UPVAL    CSECT                                                                  
         DS    2000C                                                            
         ORG   UPVAL                                                            
         PRINT NOGEN                                                            
         NMOD1 UPDX-UPD,**UPVL**,CLEAR=YES                                      
         USING UPD,RC                                                           
         USING RAVLNEL,R3                                                       
         LM    R2,R4,0(R1)         A(HEADER) A(LIST AREA) A(COMFACS)            
         STM   R2,R4,PARMS                                                      
         LR    R8,R4               R8=A(COMFACS)                                
         USING COMFACSD,R8                                                      
         MVC   MAX,0(R1)                                                        
         MVC   USERCHAR,8(R1)      SAVE SPECIAL USER STOP CHAR                  
         MVC   PRECOK,4(R1)                                                     
         LR    R9,R1                                                            
         MVI   SOFAR,0                                                          
         MVI   END,C'N'                                                         
         MVI   PRECSW,0                                                         
         MVI   0(R9),0                                                          
         SPACE 1                                                                
UP2      XC    PARAS(8),PARAS                                                   
         MVC   PARAS+4(4),=X'D9000A00'                                          
         GOTO1 CCALLOV,PARAS                                                    
         CLI   4(R1),X'FF'         TEST FOR ERROR                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   BOOKVAL,0(R1)                                                    
         SPACE 1                                                                
         EJECT                                                                  
*              SCAN STRING AND SELECT ROUTINE                                   
         SPACE 3                                                                
SCAN     CLI   USERCHAR,C'/'       TEST VALID USER CHAR SPECIFIED               
         BNE   *+10                NO                                           
         MVC   PARAS+8(4),=C',=/='                                              
         GOTO1 CSCANNER,PARAS,(R2),(20,BLOCK)                                   
         LA    R2,BLOCK                                                         
         ZIC   R4,PARAS+4                                                       
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         BAS   RE,START                                                         
         SPACE 1                                                                
SCAN2    BAS   RE,WHAT                                                          
         SPACE 1                                                                
WHERE    CLI   TYPE,C'B'                                                        
         BE    BUK                                                              
         CLI   TYPE,C'D'                                                        
         BE    DEM                                                              
         CLI   TYPE,C'K'                                                        
         BNE   XIT                                                              
         LH    RF,VAL                                                           
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     BRANCH(RF)                                                       
         SPACE 1                                                                
BRANCH   B     HOM                 1                                            
         B     RAT                 2                                            
         B     HUT                 3                                            
         B     SHR                 4                                            
         B     PUT                 5                                            
         B     INX                 6                                            
         B     SVI                 7                                            
         B     HPT                 8                                            
         B     MIN                 9                                            
         B     Y4S                 10                                           
         B     PINDX               11                                           
         B     SINDX               12                                           
         B     PAVG                13                                           
         B     SAVG                14                                           
         B     MMU                 15                                           
         B     HPV                 16                                           
         B     LPM                 17                                           
         B     LPS                 17                                           
         SPACE 1                                                                
BUK      OC    RAVLNBKS,VAL+1                                                   
         BAS   RE,WHAT                                                          
         B     WHERE                                                            
         SPACE 1                                                                
WRAP     BAS   RE,FINISH                                                        
         B     SCAN2                                                            
         EJECT                                                                  
*              SPECIFIC ROUTINES - HOMES AND RATINGS                            
         SPACE 3                                                                
HOM      MVI   RAVLNTYP,X'01'      HOMES,N1,N2,N3(,N4)                          
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         MVC   PRECSWP,PRECSW                                                   
         STH   R1,RAVLNOP1                                                      
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         CLC   PRECSWP,PRECSW                                                   
         BNE   XIT                                                              
         STH   R1,RAVLNOP2                                                      
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         CLC   PRECSWP,PRECSW                                                   
         BNE   XIT                                                              
         STH   R1,RAVLNOP3                                                      
         CLI   END,C'Y'                                                         
         BE    FINISH                                                           
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   HOM2                                                             
         CLC   PRECSWP,PRECSW                                                   
         BNE   XIT                                                              
         STH   R1,RAVLNOP4                                                      
         B     WRAP                                                             
         SPACE 1                                                                
HOM2     BAS   RE,FINISH                                                        
         B     WHERE                                                            
*              SPECIFIC ROUTINES - PUT OR SHARE INDEX                           
         SPACE 3                                                                
PINDX    MVI   RAVLNTYP,X'0B'      VARIES                                       
         B     PSINDX                                                           
SINDX    MVI   RAVLNTYP,X'0C'      VARIES                                       
         B     PSINDX                                                           
         SPACE 3                                                                
PAVG     MVI   RAVLNTYP,X'0D'      VARIES                                       
         B     PSINDX                                                           
SAVG     MVI   RAVLNTYP,X'0E'      VARIES                                       
         B     PSINDX                                                           
         SPACE 1                                                                
PSINDX   MVC   RAVLNCAT(1),BLOCK+14                                             
*                                                                               
         CLI   RAVLNCAT,C'M'       BOOKS SPECIFIED (PIM)                        
         BE    PSBOOKS                                                          
*                                                                               
         CLI   RAVLNCAT,C'V'       BOOKS SPECIFIED (PAVG)                       
         BNE   *+8                                                              
         MVI   RAVLNCAT,C'N'                                                    
*                                                                               
         CLI   RAVLNCAT,C'N'       BOOKS SPECIFIED                              
         BE    PSBOOKS                                                          
                                                                                
         CLI   RAVLNCAT,C'Y'       YEARLY INDEX                                 
         BE    PSINDX2                                                          
         CLI   RAVLNCAT,C'Q'       QUARTERLY INDEX                              
         BE    PSINDX2                                                          
         CLI   RAVLNCAT,C'S'       SWEEP INDEX                                  
         BE    PSINDX2                                                          
         CLI   RAVLNCAT,C'B'       BOOK  INDEX                                  
         BE    PSINDX2                                                          
         B     XIT                                                              
         SPACE 1                                                                
PSINDX2  BAS   RE,WHAT             BOOK FOLLOWED BY NUMBER                      
         CLI   TYPE,C'K'                                                        
         BNE   PSINDX2A                                                         
         CLC   VAL,=H'6'                                                        
         BNE   XIT                                                              
         OI    RAVLNTYP,X'20'      INDICATE INVENTORY SOURCE                    
         BAS   RE,WHAT                                                          
*                                                                               
PSINDX2A CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP1,VAL                                                     
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         CH    R1,=H'2'            NEED 2                                       
         BL    XIT                                                              
         CH    R1,=H'4'            BUT NOT MORE THAN 4                          
         BH    XIT                                                              
         STH   R1,RAVLNOP2                                                      
         MVC   RAVLNOP2(1),RAVLNCAT                                             
         B     WRAP                                                             
*                                                                               
PSBOOKS  BAS   RE,WHAT             LIST OF BOOKS                                
         CLI   TYPE,C'K'                                                        
         BNE   PSBOOKS2                                                         
         CLC   VAL,=H'6'                                                        
         BNE   XIT                                                              
         OI    RAVLNTYP,X'20'      INDICATE INVENTORY SOURCE                    
         BAS   RE,WHAT                                                          
*                                                                               
PSBOOKS2 CLI   TYPE,C'Y'           (MUST BE AT LEAST 2)                         
         BNE   XIT                 (BUT NO MORE THAN 4)                         
         MVC   RAVLNOP1,VAL                                                     
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         BAS   RE,WHAT                                                          
         MVC   RAVLNOP2,VAL                                                     
         CLI   END,C'Y'                                                         
         BE    FINISH                                                           
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP3,VAL                                                     
         CLI   END,C'Y'                                                         
         BE    FINISH                                                           
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP4,VAL                                                     
         CLI   END,C'Y'                                                         
         BNE   XIT                                                              
         B     WRAP                                                             
         SPACE 1                                                                
RAT      MVI   RAVLNTYP,X'02'      RATING,N1(,N2)                               
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         MVC   PRECSWP,PRECSW                                                   
         STH   R1,RAVLNOP1                                                      
         CLI   END,C'Y'                                                         
         BE    FINISH                                                           
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   HOM2                                                             
         CLC   PRECSWP,PRECSW                                                   
         BNE   XIT                                                              
         STH   R1,RAVLNOP2                                                      
         B     WRAP                                                             
         EJECT                                                                  
*              SPECIFIC ROUTINES - HUT AND SHARE                                
         SPACE 3                                                                
HUT      MVI   RAVLNTYP,X'03'      HUT,N1(,N2)                                  
         BAS   RE,WHAT             HUT,N1(,BOOK)                                
         MVC   TYPEP,TYPE                                                       
         MVC   PRECSWP,PRECSW                                                   
         CLI   TYPE,C'N'                                                        
         BNE   HUT2                HUT,BOOK,BOOK                                
         STH   R1,RAVLNOP1                                                      
         B     HUT4                                                             
         SPACE 1                                                                
HUT2     CLI   TYPE,C'Y'           BOOK1                                        
         BNE   XIT                                                              
         MVC   RAVLNOP1(2),VAL                                                  
         MVC   RAVLNBT,BOOKTYPE    SET BOOK TYPE FOR HUT SOURCE                 
         SPACE 1                                                                
HUT4     CLI   END,C'Y'                                                         
         BE    FINISH                                                           
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'           N2                                           
         BNE   HUT6                                                             
         CLI   TYPEP,C'N'                                                       
         BNE   *+14                                                             
         CLC   PRECSWP,PRECSW                                                   
         BNE   XIT                                                              
         STH   R1,RAVLNOP2                                                      
         B     WRAP                                                             
         SPACE 1                                                                
HUT6     CLI   TYPE,C'Y'           BOOK2                                        
         BNE   HOM2                                                             
         MVC   RAVLNOP2,VAL                                                     
         B     WRAP                                                             
         SPACE 1                                                                
SHR      MVI   RAVLNTYP,X'03'      SHARE,N1                                     
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         STH   R1,RAVLNOP2                                                      
         B     WRAP                                                             
         EJECT                                                                  
*              SPECIFIC ROUTINES - PUT INDEX SVI & DEMOS                        
         SPACE 3                                                                
PUT      MVI   RAVLNTYP,X'03'      PUT,BOOK1(,BOOK2)                            
         MVI   RAVLNCAT,C'P'                                                    
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'           BOOK1                                        
         BNE   XIT                                                              
         MVC   RAVLNOP1,VAL                                                     
         MVC   RAVLNBT,BOOKTYPE    SET BOOK TYPE FOR PUT SOURCE                 
         MVC   PRECSWP,PRECSW                                                   
         CLI   END,C'Y'                                                         
         BE    FINISH                                                           
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'           BOOK2                                        
         BNE   PUT2                                                             
         MVC   RAVLNOP2,VAL                                                     
         B     WRAP                                                             
PUT2     CLI   TYPE,C'N'           NUMERIC SHARE                                
         BNE   HOM2                                                             
         TM    PRECSW,X'40'        DECIMAL INPUT                                
         BO    *+8                                                              
         MH    R1,=H'10'           NO - MULTIPLY BY 10                          
         STH   R1,RAVLNOP2                                                      
         B     WRAP                                                             
         SPACE 1                                                                
INX      MVI   RAVLNTYP,X'04'      INDEX,N1                                     
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         TM    PRECSW,X'40'        DECIMAL PRECISION NOT ALLOWED                
         BO    XIT                                                              
         STH   R1,RAVLNOP1                                                      
         B     WRAP                                                             
         SPACE 1                                                                
MIN      MVI   RAVLNTYP,X'07'      MIN,N1                                       
         MVI   RAVLNCAT,C'R'       ALWAYS RATING                                
         CLI   0(R2),0             NO INPUT?                                    
         BE    XIT                                                              
         TM    2(R2),X'80'         SEE IF NUMERIC '0'                           
         BNO   XIT                 NO                                           
         SR    R1,R1                                                            
         MVI   TYPE,C'N'                                                        
         L     R1,4(R2)            GET THE VALUE                                
         STH   R1,VAL                                                           
         LA    R2,32(R2)                                                        
         BCTR  R4,0                                                             
         MVI   END,C'N'                                                         
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         MVI   END,C'Y'                                                         
         CH    R1,=H'100'                                                       
         BH    XIT                 FOR RTG MAX IS 100                           
         MH    R1,=H'10'                                                        
         STH   R1,RAVLNOP1                                                      
         B     WRAP                                                             
         SPACE 1                                                                
SVI      MVI   RAVLNTYP,X'05'      SVI,MONTH                                    
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'                                                        
         BNE   SVI2                                                             
         CH    R1,=H'12'                                                        
         BH    XIT                                                              
         STH   R1,RAVLNOP1                                                      
         B     WRAP                                                             
         SPACE 1                                                                
SVI2     CLI   TYPE,C'M'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP1,VAL                                                     
         B     WRAP                                                             
         SPACE 1                                                                
HPT      MVI   RAVLNTYP,X'06'      HPT                                          
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP1,VAL                                                     
         MVC   RAVLNBT,BOOKTYPE    BOOK TYPE FOR HPT SOURCE                     
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   HPT2                                                             
         MVC   RAVLNOP2,VAL                                                     
         BAS   RE,WHAT                                                          
         SPACE 1                                                                
HPT2     CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         TM    PRECSW,X'40'        DECIMAL PRECISION NOT ALLOWED                
         BO    XIT                                                              
         STH   R1,RAVLNOP3                                                      
         CLI   END,C'Y'                                                         
         BE    FINISH                                                           
         BAS   RE,WHAT                                                          
         SPACE 1                                                                
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         TM    PRECSW,X'40'        DECIMAL PRECISION NOT ALLOWED                
         BO    XIT                                                              
         CH    R1,=H'200'                                                       
         BH    XIT                                                              
         STH   R1,RAVLNOP4                                                      
         B     WRAP                                                             
         SPACE 1                                                                
HPV      MVI   RAVLNTYP,X'06'      HPV                                          
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVI   RAVLNCAT,C'V'       SET FOR VALUE                                
         MVC   RAVLNOP1,VAL                                                     
         MVC   RAVLNBT,BOOKTYPE    BOOK TYPE FOR HPT SOURCE                     
         BAS   RE,WHAT                                                          
         SPACE 1                                                                
HPV2     CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         TM    PRECSW,X'40'        DECIMAL PRECISION OK                         
         BO    *+8                                                              
         MH    R1,=H'10'           ELSE ADJUST IT                               
         STH   R1,RAVLNOP3                                                      
         CLI   END,C'Y'                                                         
         BE    XIT                                                              
         BAS   RE,WHAT                                                          
         SPACE 1                                                                
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         TM    PRECSW,X'40'        DECIMAL PRECISION OK                         
         BO    *+8                                                              
         MH    R1,=H'10'           ELSE ADJUST IT                               
         STH   R1,RAVLNOP4                                                      
         B     WRAP                                                             
         SPACE 1                                                                
LPS      DS    0X                                                               
         MVI   RAVLNTYP,X'09'      LPS                                          
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP1,VAL                                                     
         MVC   RAVLNBT,BOOKTYPE                                                 
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP2,VAL                                                     
         MVC   RAVLNOP3,=X'9999'   LPS IDENTIFIER                               
         B     WRAP                                                             
*                                                                               
MMU      MVI   RAVLNTYP,X'09'      MMU                                          
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP1,VAL                                                     
         MVC   RAVLNBT,BOOKTYPE    BOOK TYPE FOR MMU SOURCE                     
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP2,VAL                                                     
         B     WRAP                                                             
         SPACE 1                                                                
DEM      MVC   RAVLNTYP(2),VAL     DEMO.....                                    
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'N'           TEST FOR NUMBER                              
         BNE   XIT                                                              
         MVC   PRECSWP,PRECSW                                                   
         STH   R1,RAVLNOP1                                                      
         CLI   RAVLNTYP,C'P'       TEST FOR PUT                                 
         BNE   WRAP                NO                                           
         BAS   RE,WHAT             PUT IS DEMO, LOOK FOR SHARE                  
         CLI   TYPE,C'N'                                                        
         BNE   XIT                                                              
         CLC   PRECSWP,PRECSW                                                   
         BNE   XIT                                                              
         STH   R1,RAVLNOP2                                                      
         B     WRAP                                                             
*                                                                               
Y4S      MVI   RAVLNTYP,X'08'                                                   
         B     WRAP                                                             
*                                                                               
LPM      MVI   RAVLNTYP,X'09'      LPM                                          
         BAS   RE,WHAT                                                          
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP1,VAL                                                     
         MVC   RAVLNBT,BOOKTYPE    BOOK TYPE FOR MMU SOURCE                     
*                                                                               
         BAS   RE,WHAT             STATION                                      
         CLI   TYPE,C'S'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP2(4),VAL                                                  
*                                                                               
         CLI   END,C'Y'                                                         
         BE    FINISH                                                           
         BAS   RE,WHAT             ALPHA  MKT                                   
         CLI   TYPE,C'A'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP4,VAL                                                     
         OC    RAVLNOP4,RAVLNOP4   MUST HAVE A NUMERIC #                        
         BZ    XIT                                                              
*&&DO                                                                           
         BAS   RE,WHAT             SHARE BOOK                                   
         CLI   TYPE,C'Y'                                                        
         BNE   XIT                                                              
         MVC   RAVLNOP2,VAL                                                     
         B     WRAP                                                             
*&&                                                                             
         B     WRAP                                                             
         EJECT                                                                  
*              ROUTINE TO INTERPRET A LINE IN BLOCK                             
         SPACE 1                                                                
WHAT     ST    RE,SAVERE                                                        
         CLI   END,C'Y'                                                         
         BE    XIT                                                              
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         L     R1,4(R2)                                                         
*                                                                               
         NI    PRECSW,B'11111110'                                               
         CLI   PRECOK,C'Y'         PRECISION VALID                              
         BNE   WHATNP              NO - FAIL IF IT'S THERE                      
         LTR   R1,R1               HAVE VALUE - ISN'T PRECISION                 
         BNZ   WHATNP                                                           
         LA    R5,12(R2)                                                        
WHATPR   CLI   0(R5),0             LOOP TILL FIELD PROCESSED                    
         BE    WHATNP                                                           
         CLI   0(R5),C' '                                                       
         BE    WHATNP                                                           
         CLI   0(R5),C'.'          DECIMAL POINT OK                             
         BE    WHATPR6                                                          
         CLI   0(R5),C'0'          AND NUMERIC OK                               
         BL    WHATPR4                                                          
         CLI   0(R5),C'9'                                                       
         BH    WHATPR4                                                          
         MH    R1,=H'10'           CONVERT TO VALUE                             
         ZIC   RE,0(R5)                                                         
         N     RE,=F'15'                                                        
         AR    R1,RE                                                            
WHATPR2  LA    R5,1(R5)                                                         
         OI    PRECSW,X'40'                                                     
         B     WHATPR                                                           
WHATPR4  SR    R1,R1               NOT NUMERIC                                  
         B     WHATNP                                                           
*                                                                               
WHATPR6  TM    PRECSW,X'01'        HAD DEC POINT PREVIOUSLY                     
         BNZ   WHATPR4             THEN ITS AN ERROR                            
         OI    PRECSW,X'01'        OTHERWISE FLAG FOR POTENTIAL NEXT            
         B     WHATPR2             DECIMAL POINT AND CONT. EDIT                 
*                                                                               
WHATNP   LTR   R1,R1                                                            
         BZ    WHAT4                                                            
         MVI   TYPE,C'N'           NUMERIC                                      
         STH   R1,VAL                                                           
         SPACE 1                                                                
WHAT2    LA    R2,32(R2)                                                        
         BCTR  R4,0                                                             
         MVI   END,C'N'                                                         
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         MVI   END,C'Y'                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
WHAT4    CLI   12(R2),C'B'         IS IT A B EXPRESSION                         
         BNE   WHAT10                                                           
         CLI   RAVLNTYP,X'09'      NO B EXPRESSION IN LPM UPGRADE               
         BE    WHAT10                                                           
         CLI   0(R2),2                                                          
         BNE   XIT                                                              
         LA    R5,BLIST                                                         
         SPACE 1                                                                
WHAT6    CLC   0(1,R5),13(R2)                                                   
         BE    WHAT8                                                            
         LA    R5,2(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   WHAT6                                                            
         B     XIT                                                              
         SPACE 1                                                                
WHAT8    MVI   TYPE,C'B'                                                        
         MVI   VAL,0                                                            
         MVC   VAL+1(1),1(R5)                                                   
         B     WHAT2                                                            
         SPACE 1                                                                
BLIST    DC    X'F180F240F320F410F508F604FF'                                    
         SPACE 1                                                                
WHAT10   LA    R5,KEYLIST          IS IT A KEY WORD                             
         CLI   TYPE,C'S'           IF WE PROCESSED A STATION BEFORE             
         BE    WHAT26              MUST BE LPM UPGRADE-GO VAL AMKT              
         SPACE 1                                                                
WHAT12   CLC   0(6,R5),12(R2)                                                   
         BE    WHAT14                                                           
         LA    R5,8(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BE    WHAT16                                                           
         B     WHAT12                                                           
         SPACE 1                                                                
WHAT14   MVI   TYPE,C'K'                                                        
         MVC   VAL,6(R5)                                                        
         B     WHAT2                                                            
         SPACE 1                                                                
KEYLIST  DS    0H                                                               
         DC    C'HOMES ',H'1'                                                   
         DC    C'HOM   ',H'1'                                                   
         DC    C'RATING',H'2'                                                   
         DC    C'RTG   ',H'2'                                                   
         DC    C'R     ',H'2'                                                   
         DC    C'HUT   ',H'3'                                                   
         DC    C'SHARE ',H'4'                                                   
         DC    C'SH    ',H'4'                                                   
         DC    C'S     ',H'4'                                                   
         DC    C'PUT   ',H'5'                                                   
         DC    C'INDEX ',H'6'                                                   
         DC    C'IX    ',H'6'                                                   
         DC    C'I     ',H'6'                                                   
         DC    C'SVI   ',H'7'                                                   
         DC    C'HPT   ',H'8'                                                   
         DC    C'MIN   ',H'9'                                                   
         DC    C'Y4S   ',H'10'                                                  
         DC    C'PINDEX',H'11'     PINDEX(AVERAGE)                              
         DC    C'PIN   ',H'11'     PINDEX(AVERAGE)                              
         DC    C'PIM   ',H'11'     PINDEX(MULTIPLY)                             
         DC    C'PIY   ',H'11'     PINDEX(YEARS)                                
         DC    C'PIQ   ',H'11'     PINDEX(QUARTERS)                             
         DC    C'PIB   ',H'11'     PINDEX(BOOKS                                 
         DC    C'SINDEX',H'12'     SINDEX(AVERAGE)                              
         DC    C'SIN   ',H'12'     SINDEX(AVERAGE)                              
         DC    C'SIM   ',H'12'     SINDEX(MULTIPLY)                             
         DC    C'SIY   ',H'12'     SINDEX(YEAR)                                 
         DC    C'SIQ   ',H'12'     SINDEX(QUARTER)                              
         DC    C'SIB   ',H'12'     SINDEX(BOOK)                                 
         DC    C'PAVG  ',H'13'                                                  
         DC    C'PAY   ',H'13'                                                  
         DC    C'PAQ   ',H'13'                                                  
         DC    C'PAB   ',H'13'                                                  
         DC    C'SAVG  ',H'14'                                                  
         DC    C'SAY   ',H'14'                                                  
         DC    C'SAQ   ',H'14'                                                  
         DC    C'SAB   ',H'14'                                                  
         DC    C'MMU   ',H'15'     METERED MARKET HUTBOOK/MMU BASE              
         DC    C'HPV   ',H'16'     HUT BOOK/SHARE BOOK/HUT/SHARE                
         DC    C'LPM   ',H'17'     HUT BOOK/STATION/MKT/SHARE BOOK              
         DC    C'LPS   ',H'18'     LPS HUTBOOK                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
WHAT16   CLI   0(R2),3             IS IT A MONTH                                
         BNE   WHAT22                                                           
         LA    R5,MONTAB                                                        
         LA    R6,1                                                             
         LA    R0,12                                                            
         SPACE 1                                                                
WHAT18   CLC   0(3,R5),12(R2)                                                   
         BE    WHAT20                                                           
         LA    R5,3(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R0,WHAT18                                                        
         B     WHAT22                                                           
         SPACE 1                                                                
WHAT20   MVI   TYPE,C'M'                                                        
         STH   R6,VAL                                                           
         B     WHAT2                                                            
         SPACE 1                                                                
MONTAB   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         SPACE 1                                                                
WHAT22   XC    WORK,WORK           IS IT A BOOK                                 
         MVC   WORK+5(1),0(R2)     BUILD A DUMMY STRING                         
         ZIC   R1,WORK+5                                                        
         MVI   WEEKNUM,0                                                        
         LA    R1,12+6(R1,R2)      MAY END IN -1 -2 -3 -4                       
         CLI   0(R1),C'-'          (THIS IS THE WEEK NUMBER)                    
         BNE   WHAT23                                                           
         CLI   1(R1),C'1'                                                       
         BL    WHAT23                                                           
         CLI   1(R1),C'4'                                                       
         BH    WHAT23                                                           
         ZIC   R0,1(R1)                                                         
         SLL   R0,4                                                             
         STC   R0,WEEKNUM          SAVE WEEK NO IN HIGH NIBBLE                  
         ZIC   R1,WORK+5                                                        
         SH    R1,=H'2'            REDUCE INPUT LENGTH                          
         STC   R1,WORK+5                                                        
         SPACE 1                                                                
WHAT23   ZIC   R1,WORK+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),12(R2)                                                 
         MVI   BOOKTYPE,0                                                       
         GOTO1 BOOKVAL,PARAS,(C'N',WORK),(1,WORK+20),(C'B',CSCANNER),  X        
               BOOKTYPE,(C'C',ACOMFACS)                                         
         CLI   PARAS+4,1                                                        
         BNE   WHAT24                                                           
         MVI   TYPE,C'Y'                                                        
         MVC   VAL,WORK+21                                                      
         OC    VAL+1(1),WEEKNUM    PASS BACK WEEK NUMBER IN HIGH NIBBLE         
         B     WHAT2                                                            
         SPACE 1                                                                
WHAT24   XC    WORK,WORK           IS IT A DEMO EXPRESSION                      
         MVC   WORK+5(1),0(R2)                                                  
         CLI   0(R2),3             BUT BE AT LEAST 3 TO AVOID SYNTAX            
***      BL    XIT                 COLLISIONS - M/99 AND MALE,ETC.              
         BL    WHAT30              COLLISIONS - M/99 AND MALE,ETC.              
*                                                                               
WHAT26   CLI   TYPE,C'S'           IF WE PROCESSED A STATION BEFORE             
         BE    WHAT30              WE ARE NOT PROCESSING DEMO NOW               
         CLI   RAVLNTYP,X'09'      NO DEMO EXPRESSION IN LPM UPGRADE            
         BE    WHAT30                                                           
*                                                                               
         ZIC   R1,0(R2)                                                         
         LA    RF,8(R1)            ADD L'FLDHR TO DATA LEN                      
         STC   RF,WORK                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),12(R2)                                                 
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'U'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 CDEMOVAL,PARAS,WORK,(1,WORK+20),DBLOCK                           
         CLI   4(R1),1             TEST FOR ERROR                               
***      BNE   XIT                 YES                                          
         BNE   WHAT30                                                           
         MVI   TYPE,C'D'                                                        
         MVC   VAL,WORK+21                                                      
         B     WHAT2                                                            
*                                                                               
WHAT30   CLI   0(R2),L'DBSELALF   ALPHA MARKET?                                 
         BH    WHAT34                                                           
***      CLI   2(R2),X'40'        ALPHA CHARACTER?                              
***      BNE   WHAT34                                                           
         TM    2(R2),X'40'        ALPHA CHARACTER? OR HEX                       
         BZ    WHAT34                                                           
         CLI   TYPE,C'S'           IF WE PROCESSED A STATION BEFORE             
         BNE   WHAT34             THEN PROCESS ALPHA MKT                        
*                                 GET MKT NUMBER                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBCNVA2N                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELBK,RAVLNOP1                                                 
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELAGY,=C'SJ'                                                  
         MVC   DBSELALF,12(R2)                                                  
         GOTO1 CDEMAND,PARAS,DBLOCK,0,0                                         
         CLI   DBERROR,0           USE IT IF NO ERROR                           
         BNE   WHAT34                                                           
         MVC   VAL(2),DBSELRMK                                                  
         MVI   TYPE,C'A'                                                        
         B     WHAT2                                                            
*                                                                               
WHAT34   DS    0H                                                               
         CLI   0(R2),5            A STATION?                                    
         BH    XIT                                                              
         MVC   VAL(4),12(R2)                                                    
         OC    VAL(4),=X'40404040'                                              
         MVI   TYPE,C'S'                                                        
         B     WHAT2                                                            
         EJECT                                                                  
*              ROUTINE TO FINISH OFF AN ELEMENT                                 
         SPACE 3                                                                
FINISH   CLI   RAVLNBKS,0                                                       
         BNE   *+8                                                              
         MVI   RAVLNBKS,X'80'                                                   
*                                                                               
         TM    PRECSW,X'40'        SET PRECISION BITS                           
         BZ    FINISH1                                                          
         TM    RAVLNTYP,X'80'      ALPHA MAKE MIXED                             
         BZ    *+12                                                             
         XI    RAVLNTYP,B'01000000'                                             
         B     *+8                                                              
         OI    RAVLNTYP,X'40'      NORMAL MAKE MIXED                            
*                                                                               
FINISH1  LA    R3,14(R3)                                                        
         ZIC   R1,SOFAR                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SOFAR                                                         
         CLC   SOFAR,MAX                                                        
         BE    FINISH2                                                          
         LTR   R4,R4                                                            
         BNZ   START                                                            
         SPACE 1                                                                
FINISH2  MVC   0(1,R9),SOFAR                                                    
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
START    XC    0(14,R3),0(R3)                                                   
         MVC   RAVLNCOD(2),=X'050E'                                             
         BR    RE                                                               
         SPACE 1                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER UPVAL WORKING STORAGE                                          
*                                                                               
UPD      DSECT                                                                  
PARMS    DS    0CL12                                                            
VFLDH    DS    V                                                                
AUPEL    DS    A                                                                
ACOMFACS DS    A                                                                
*                                                                               
PARAS    DS    6F                                                               
WORK     DS    CL32                                                             
BOOKVAL  DS    V                                                                
SAVERE   DS    F                                                                
MAX      DS    CL1                                                              
SOFAR    DS    CL1                                                              
TYPE     DS    CL1                                                              
TYPEP    DS    CL1                                                              
END      DS    CL1                                                              
USERCHAR DS    CL1                                                              
WEEKNUM  DS    CL1                                                              
PRECOK   DS    CL1                                                              
PRECSW   DS    CL1                                                              
PRECSWP  DS    CL1                                                              
VAL      DS    H                                                                
BOOKTYPE DS    C                                                                
         DS    0D                                                               
BLOCK    DS    CL640                                                            
         SPACE 2                                                                
       ++INCLUDE DEDBLOCK                                                       
         SPACE 1                                                                
UPDX     EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044DDUPVAL   07/29/09'                                      
         END                                                                    
