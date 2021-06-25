*          DATA SET SPTRA26    AT LEVEL 057 AS OF 03/06/07                      
*PHASE T21626B                                                                  
         TITLE 'T21626 MARKET/STATION TBUY BUY'                                 
***********************************************************************         
*                                                                     *         
*  THIS PROGRAM READS EXISTING TBUY RECS WITHIN THE GIVEN PERIOD      *         
*  AND USES AS SOURCE FOR POTENTIAL MARKETS OF BUYS:                  *         
*  1. GOAL RECORDS                  (GOAL PROFILE)                    *         
*  2. MARKET GROUPS                 (MGRP PROFILE)                    *         
*  3. MARKET GROUPS THAT HAVE GOALS (BOTH PROFILE)                    *         
*  4. MARKET LIST OF STATIONS       (MKTLIST PROFILE)                 *         
*                                                                     *         
*  FOR ANY OF THESE OPTIONS MARKET LIST OF STATIONS IN A MARKET MUST  *         
*  EXIST TO DEFINE STATIONS IN A MARKET. THESE RECORDS CAN EXIST      *         
*  ON AN ALL MEDIA LEVEL, OR BE CLIENT OR CLIENT AND PRODUCT SPECIFIC *         
*                                                                     *         
*  LISTS EXISTING AND POTENTIAL BUYS BY STATION WITHIN MARKET.        *         
*  THEY CAN BE B(OUGHT) OR D(ELETED) FROM THIS LIST.                  *         
*  RECORDS ARE UPDATED AS THEY ARE SELECTED                           *         
*  A TABLE IS BUILT FOR THE NUMBER OF ENTRIES ON THE SCREEN ONLY,     *         
*  WITH POINTERS TO THE LAST RECORD USED FOR BUILDING THE NEXT        *         
*  SCREEN.                                                            *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 - STATION LISTS                                    *         
*             AIO3 - TBUYS                                            *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - WORK REG                                                *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - POINTER TO STATION TABLE                                *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - WORK                                                    *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  LEV 11    FEB11/87 COMBINE SVT1PR12/13 WITH SVPROF11               *         
*  LEV 12    FEB12/87 FIX GOALS BUG                                   *         
*  LEV 13-14 FEB18/87 ADD MSG FOR NO MARKET LIST                      *         
*  LEV 15    FEB23/87 FIX PARTNER PROD BUG                            *         
*  LEV 16    MAR23/87 FIX BUG SHOWED MKT MORE THAN ONCE WITH COPY     *         
*                     CODE = EST AND SOURCE = GOALS                   *         
*  LEV 17-19 APR09/87 ALLOW BLANKS = BUY AND X TO EXCLUDE SVTBPR2/ALL *         
*                                                                VOPT *         
*  LEV 20    MAY04/87 ADD PROFILE TO ONLY READ CLIENT MKTLIST         *         
*  LEV 21    JUN01/87 ADD PASSIVE KEY FOR ADDED BUYS                  *         
*  LEV 22    JUN17/87 ADD OPTION TO USE ALL CLIENT MKTLIST            *         
*  LEV 23-24 JUL07/87 WHEN NEW STARTING MKT ENTERED, DO NOT BUY       *         
*                      CONTENTS OF SCREEN IF ALL OPTION               *         
*  LEV 25    JUL24/87 ADD OPTION BUY                                  *         
*  LEV 26-27 NOV16/87 ADD =MKT FOR STARTING MARKET                    *         
*  LEV 28-29 NOV23/87 ADD ERR MSG IF =MKT NOT FOUND                   *         
*  LEV 30    DEC09/87 FIX BUG IN =MKT                                 *         
*  LEV 31-32 JAN13/88 FIX BUG DELETE                                  *         
*  LEV 33    FEB01/88 ONLY SHOW MGROUP MKTS FOR BUYS FOR MGR OR BOTH  *         
*  LEV 33    FEB03/88 FIX GOALS                                       *         
*  LEV 34    MAR16/88 FIX =MKT SHOWING MISSING MKT NEXT SCREEN        *         
*  LEV 35-36 APR25/88 READ ALL CLIENT MGRP RECS, FIX VCD ERR MSGS     *         
*  LEV 37    DEC14/88 FIX VALIDATE OPTIONS HELP/ERR MSG FOR BUY       *         
*  LEV 38    MAR14/89 ALLOW ALL PRODS                                 *         
*  LEV 39-40 MAR22/89 FORCE PRODS TO BE EITHER BASE OR EQUV (EQV CLT) *         
*  LEV 41    JUN06/89 CHANGE CLTPEQ TO SVPROF13                       *         
*  LEV 42    JUN23/89 CHECK 2 PRODS EQUIV TO EACH OTHER               *         
*  LEV 43    JAN23/92 ADD EST CHECKING WITH TB PROFILE                *         
*  LEV 44    APR16/92 CHECK FOR DELETED RECORDS                       *         
*  LEV 45    MAY14/92 FIX READ SEQ TO NO RDUPDATE                     *         
*  LEV 46    DEC02/92 CHANGE FOR MSUNPK AND NEW STATION FILE          *         
*  LEV 47    MAR02/93 ADD CABLE HEAD CODE                             *         
*  LEV 48    APR12/93 ADD NEW SPOT TRAFFIC SYSTEM-CHGE SPTDIR TO SYSDI*         
*  LEV 49    MAR22/94 FIX STRAFFIC BUG                                *         
*  LEV 50    JUL21/94 CHANGE TO FILENAME                              *         
*  LEV 51    AUG11/94 FIX LOOP WITH =MKTNO NOT FOUND                  *         
*  LEV 52    NOV22/95 FIX ERROR - = SIGN ONLY ENTERED IN START MKT    *         
*  LEV 53 SMUR NOV10/99 USE RECUP FROM FACPAK                         *         
*  LEV 54 SMUR APR11/01 USE TRAFFIC OFFICE                            *         
* LEV 55 BGRI JAN15/04 CHGE SVSPARE TO TO SVSPAREX                    *         
* LEV 56 BGRI AUG02/04 SOX                                            *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
T21626   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21626**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR26RR                                                      
         CLC   LSYSD+2(2),=AL2(ENDSYSD-SYSD)                                    
         BNL   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
         LA    R0,STACT                                                         
         AH    R0,=H'6144'                                                      
         SR    R0,R9                                                            
         C     R0,LSYSD                                                         
         BNH   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* CHECK ANY CHANGED KEY FIELDS *                                                
         SPACE                                                                  
VK       TM    TRAMEDH+4,X'20'     ANY CHANGE                                   
         BZ    VK10                YES                                          
         TM    TRACLTH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRAPRDH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRAPTRH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRACODEH+4,X'20'                                                 
         BZ    VK10                                                             
         TM    TRAPERH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRAOPTH+4,X'20'                                                  
         BO    VLST                VAL ANY SELECTED BUYS/DELETES                
         EJECT                                                                  
*     VALIDATE KEY ROUTINE *                                                    
         SPACE                                                                  
VK10     BAS   RE,CLR              CLEAR SCREEN, WORK AREAS                     
         BAS   RE,SVTWA                                                         
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK20                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK20                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK20     DS    0H                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
         SPACE                                                                  
         LA    R2,TRACLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         BAS   RE,FPRO             GET PROFILE REC(S)                           
         OI    4(R2),X'20'                                                      
         SPACE                                                                  
         LA    R2,TRAPRDH          PRODUCT                                      
         SPACE                                                                  
         BAS   RE,VPR                                                           
         SPACE                                                                  
         LA    R2,TRAPTRH          PARTNER PRODUCT                              
         SPACE                                                                  
         BRAS  RE,VPP                                                           
         SPACE                                                                  
         LA    R2,TRACODEH         CODE                                         
         SPACE                                                                  
         BRAS  RE,VCD                                                           
         SPACE                                                                  
         LA    R2,TRAPERH          PERIOD                                       
         BRAS  RE,VPER                                                          
         SPACE                                                                  
         LA    R2,TRAOPTH          OPTIONS                                      
         BAS   RE,VOPT                                                          
         OI    4(R2),X'20'                                                      
         SPACE                                                                  
* NOW GO DISPLAY LIST OF POTENTIAL/ACTUAL BUYS                                  
         SPACE                                                                  
         BAS   RE,CTB              GO COUNT EXISTING BUYS                       
         SPACE                                                                  
         BAS   RE,SVTWA                                                         
         NI    TRASMKTH+4,X'FF'-X'20' FORCE REVALIDATION                        
         B     DLST                                                             
         EJECT                                                                  
* SEE IF ANY SELECTS OF B(UY) OR D(ELETE) *                                     
         SPACE                                                                  
VLST     BAS   RE,RDTWA                                                         
         SPACE                                                                  
         CLI   SVTBPR2,C'Y'        BUY ON BLANK SEL                             
         BNE   *+12                                                             
         TM    TRASMKTH+4,X'20'    IF NEW START MARKET, SKIP LIST               
         BZ    DLST                                                             
         SPACE                                                                  
         LA    R2,TRASEL1H                                                      
         LA    R5,STATABL                                                       
         USING DLINE,R2                                                         
         USING STATABLD,R5                                                      
         CLI   STALIN,0                                                         
         BE    VLST60                                                           
         SPACE                                                                  
VLST10   CLC   STABSTA,=X'FFFFFF'  NO MKT LIST LINE                             
         BNE   VLST14                                                           
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VLST40                                                           
         CLI   TRASEL1-TRASEL1H(R2),C'X' IF EXCLUDE, OK                         
         BE    VLST40                                                           
         B     NOMKTERR                                                         
         SPACE                                                                  
VLST14   CLC   LSTA+(TRASTAH-TRASEL1H),STASTA                                   
         BNE   VLSTBUG                                                          
         CLC   LPERFTD+(TRASTAH-TRASEL1H),STAFTD                                
         BNE   VLSTBUG                                                          
         CLC   LPERLTD+(TRASTAH-TRASEL1H),STALTD                                
         BNE   VLSTBUG                                                          
         SPACE                                                                  
         CLI   SVTBPR2,C'Y'        BUY ON BLANK SEL                             
         BNE   VLST24                                                           
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VLST20                                                           
         OC    STADSKA,STADSKA     THIS STATION ALREADY BOUGHT                  
         BNZ   VLST40               ALREADY A BUY, BYPASS                       
         B     VLST26                                                           
VLST20   CLI   TRASEL1-TRASEL1H(R2),C'X'                                        
         BE    VLST40                                                           
         CLI   TRASEL1-TRASEL1H(R2),C'D'                                        
         BE    VLST30                                                           
         B     SELERR                                                           
         SPACE                                                                  
VLST24   CLI   5(R2),0             ANY ENTRY                                    
         BE    VLST40                                                           
         CLI   TRASEL1-TRASEL1H(R2),C'D'  DELETE BUY                            
         BE    VLST30                                                           
         CLI   TRASEL1-TRASEL1H(R2),C'B'                                        
         BNE   SELERR                                                           
         SPACE                                                                  
VLST26   BAS   RE,AST              GO ADD BUY                                   
         SPACE                                                                  
         B     VLST36                                                           
         SPACE                                                                  
VLST30   OC    STADSKA,STADSKA     THIS A BUY                                   
         BZ    DELSELER             NOT YET                                     
         BAS   RE,DST              GO DELETE STATION BUY                        
         SPACE                                                                  
VLST36   XC    TRASEL1-TRASEL1H(,R2),TRASEL1-TRASEL1H(R2)                       
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
VLST40   LA    R2,TRASEL2H-TRASEL1H(,R2)                                        
         LA    R5,STANEXT                                                       
         LA    RF,TRATAGH                                                       
         CR    R2,RF                                                            
         BNL   VSLT50                                                           
         CLI   STALIN,0                                                         
         BNE   VLST10                                                           
         B     VLST60                                                           
         SPACE                                                                  
VSLT50   CLI   STALIN,0            END OF SCREEN, MUST BE END OF TABL           
         BE    VLST60                                                           
VLSTBUG  DC    H'0'                                                             
         SPACE                                                                  
VLST60   CLC   BUYMKST(9),SMKTSTA  IF EQUAL KEYS                                
         BNE   DLST                                                             
         CLI   BUYMKST,X'FF'          AND HEXFF                                 
         BNE   DLST                                                             
         XC    CURRKEYS,CURRKEYS   RESTART LIST                                 
         XC    CKMGRMKT,CKMGRMKT                                                
         NI    PRGSW,X'FF'-X'04'                                                
         B     DLST                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
* DISPLAY LIST OF TBUYS AND POSSIBLE BUYS (FROM GOAL OR STATION LISTS)          
         SPACE                                                                  
* FIRST SEE IF END OF LIST *                                                    
         SPACE                                                                  
DLST     BRAS  RE,VMG              CK FOR NEW STARTING MARKET                   
         SPACE                                                                  
         OI    TRATAGH+1,X'01'     SET MODIFIED BIT                             
         OI    TRATAGH+6,X'80'      AND TRANSMIT BIT                            
         SPACE                                                                  
         NI    PRGSW,X'FF'-X'80'-X'40'-X'04' SET OFF RECS IN AIO                
         MVC   TRATOTS,SPACES                                                   
         MVC   TRATOTS(35),STACTMSG                                             
         EDIT  (B2,STACT),(3,TRATOTS)                                           
         LA    R2,TRATOTS+21                                                    
         EDIT  (B2,BUYCT),(3,(R2))                                              
         SPACE                                                                  
         MVC   TRATOTS+37(12),=C'MARKETS FROM'                                  
         MVC   TRATOTS+50(5),=C'GOALS'                                          
         CLI   SVTBPR1,C'G'                                                     
         BE    DLST14                                                           
         MVC   TRATOTS+50(5),=C'M GRP'                                          
         MVC   TRATOTS+56(5),SVDMGRP                                            
         CLI   SVTBPR1,C'M'                                                     
         BE    DLST14                                                           
         CLI   SVTBPR1,C'B'                                                     
         BNE   DLST10                                                           
         MVC   TRATOTS+62(7),=C'&& GOALS'                                       
         B     DLST14                                                           
DLST10   CLI   SVTBPR1,C'L'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRATOTS+50(11),=C'MARKET LIST'                                   
DLST14   CLI   SVTBPR3,C'Y'        CLIENT SPECIFIC                              
         BNE   DLST16                                                           
         MVC   TRATOTS+69(5),=C'(CLT)'                                          
         SPACE                                                                  
DLST16   OI    TRATOTSH+6,X'80'                                                 
         MVI   LINCT,0                                                          
         LA    R2,TRASEL1H         FIRST DISPLAY FIELD                          
         LA    R5,STATABL                                                       
         USING DLINE,R2                                                         
         USING STATABLD,R5                                                      
         XC    STATABL(200),STATABL                                             
         XC    STATABL+200(200),STATABL+200                                     
         XC    STATABL+400(240),STATABL+400                                     
         SPACE                                                                  
DLST20   BAS   RE,NXB              GET NEXT BUY OR POSSIBLE BUY                 
         BNE   DLST40               END OF LIST                                 
         SPACE                                                                  
         OI    PRGSW,X'04'         SET ON DISPLAYED AT LEAST ONE                
         EJECT                                                                  
* CLEAR SELECT FIELD *                                                          
         SPACE                                                                  
         OC    TRASEL1-TRASEL1H(L'TRASEL1,R2),TRASEL1-TRASEL1H(R2)              
         BZ    DLST24                                                           
         CLC   TRASEL1-TRASEL1H(L'TRASEL1,R2),SPACES                            
         BE    DLST24                                                           
         XC    TRASEL1-TRASEL1H(L'TRASEL1,R2),TRASEL1-TRASEL1H(R2)              
         OI    6(R2),X'80'         SET ON TRANSMIT                              
         SPACE                                                                  
DLST24   TM    1(R2),X'20'         PROTECTED FIELD                              
         BZ    DLST26                                                           
         NI    1(R2),X'FF'-X'20'   SET OFF PROTECT                              
         OI    6(R2),X'80'         SET ON TRANSMIT                              
         SPACE                                                                  
DLST26   LA    R2,TRASTAH-TRASEL1H(,R2)                                         
         MVC   TRASTA-TRASTAH(L'TRASTA,R2),SPACES                               
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,(X'80',STABMKT),QMKT,DUB                             
         SPACE                                                                  
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES                                                  
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
         SPACE                                                                  
         MVC   QSTA,DUB                                                         
         CLC   SVMKT,STABMKT                                                    
         BE    DLST30                                                           
         MVC   SVMKT,STABMKT                                                    
         BAS   RE,FMKT                                                          
         SPACE                                                                  
DLST30   NI    1(R2),X'FF'-X'08'-X'04' SET OFF INTENSITY                        
         SPACE                                                                  
         OC    STADSKA,STADSKA     IS THIS BOUGHT                               
         BZ    DLST32                                                           
         OI    1(R2),X'08'                                                      
         MVI   LBUY1,C'*'                                                       
         MVI   LBUY2,C'*'                                                       
         MVI   LBUY3,C'*'                                                       
         SPACE                                                                  
DLST32   MVC   LMKT,QMKT                                                        
         MVC   LMKTNM,MKTNM                                                     
         CLC   STABSTA,=X'FFFFFF'                                               
         BNE   DLST34                                                           
         MVC   LBUY2(30),=CL30'NO MARKET LIST FOR THIS MARKET'                  
         B     DLST38                                                           
         SPACE                                                                  
DLST34   MVC   LSTA,STASTA                                                      
         MVC   LPERFTD,STAFTD                                                   
         MVI   LPERDASH,C'-'                                                    
         MVC   LPERLTD,STALTD                                                   
         SPACE                                                                  
DLST38   LA    R5,STANEXT                                                       
         OI    6(R2),X'80'                                                      
         LA    R2,TRASEL2H-TRASTAH(,R2)                                         
         LA    RF,TRATAGH                                                       
         CR    R2,RF                                                            
         BL    DLST20                                                           
         SPACE                                                                  
         B     DOSEL                                                            
         EJECT                                                                  
* END OF LIST, CLEAR SCREEN *                                                   
         SPACE                                                                  
DLST40   OC    TRASEL1-TRASEL1H(L'TRASEL1,R2),TRASEL1-TRASEL1H(R2)              
         BZ    DLST44                                                           
         CLC   TRASEL1-TRASEL1H(L'TRASEL1,R2),SPACES                            
         BE    DLST44                                                           
         XC    TRASEL1-TRASEL1H(L'TRASEL1,R2),TRASEL1-TRASEL1H(R2)              
DLST44   OI    1(R2),X'20'         SET ON PROTECT                               
         OI    6(R2),X'80'             & TRANSMIT                               
         SPACE                                                                  
         LA    R2,TRASTAH-TRASEL1H(,R2)                                         
         OC    TRASTA-TRASTAH(L'TRASTA,R2),TRASTA-TRASTAH(R2)                   
         BZ    DLST46                                                           
         CLC   TRASTA-TRASTAH(L'TRASTA,R2),SPACES                               
         BE    DLST46                                                           
         XC    TRASTA-TRASTAH(L'TRASTA,R2),TRASTA-TRASTAH(R2)                   
         OI    6(R2),X'80'                                                      
DLST46   LA    R2,TRASEL2H-TRASTAH(,R2)                                         
         LA    RF,TRATAGH                                                       
         CR    R2,RF                                                            
         BL    DLST40                                                           
         SPACE                                                                  
* NOW CHECK IF ANY LINES DISPLAYED *                                            
         SPACE                                                                  
         LA    RF,STATABL                                                       
         CR    R5,RF                                                            
         BE    DLST50              NOTHING DISPLAYED, RESTART                   
         SPACE                                                                  
         CLI   STMKTSW,C'='                                                     
         BNE   DLST48                                                           
         OC    SVSTMKT,SVSTMKT     MUST BE SAVED STARTING MARKET                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   STMKT,SVSTMKT                                                    
         SPACE                                                                  
DLST48   CLC   BKEY(13),SKEY       IF EQUAL KEYS                                
         BNE   DOSEL                                                            
         CLI   BKEY,X'FF'          AND HEXFF                                    
         BNE   DOSEL                                                            
         B     ENDSEL              AT END OF ALL                                
         DROP  R2,R5                                                            
         SPACE                                                                  
* RESTART LIST *                                                                
         SPACE                                                                  
DLST50   TM    PRGSW,X'04'         DISPLAY AT LEAST ONE BUY                     
         BZ    DLST60                                                           
         PACK  WORK(3),SVPMGRP+1(5)                                             
         MVC   SVMGRP,WORK                                                      
         XC    CURRKEYS,CURRKEYS                                                
         XC    CKMGRMKT,CKMGRMKT                                                
         NI    PRGSW,X'03'                                                      
         B     DLST                                                             
DLST60   CLI   STMKTSW,C'='                                                     
         BNE   ENDSEL                                                           
         OC    SVSTMKT,SVSTMKT     MUST BE SAVED STARTING MARKET                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   STMKT,SVSTMKT                                                    
         B     DLST                                                             
         EJECT                                                                  
* VALIDATE PRODUCT *                                                            
         SPACE                                                                  
VPR      NTR1                                                                   
         GOTO1 VALIPRD                                                          
         CLC   WORK(3),=C'POL'     INVALID                                      
         BE    PRDERR                                                           
         MVC   QPRD,WORK                                                        
         CLI   WORK+4,0            VALID SPOT LENGTH                            
         BNE   VPR10                                                            
         MVI   WORK+4,30                                                        
         LA    R1,TRAPRD+3                                                      
         CLI   TRAPRD+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVC   0(3,R1),=C'-30'                                                  
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
VPR10    MVC   BPRD(2),WORK+3                                                   
         SPACE                                                                  
VPR20    OI    4(R2),X'20'         VALIDATED                                    
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
         SPACE                                                                  
VOPT     NTR1                                                                   
         SPACE                                                                  
         XC    OPTIONS,OPTIONS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT96              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,4                                                             
         B     VOPT04                                                           
VOPT02   ZIC   R1,5(R2)                                                         
VOPT04   EX    R1,VOPTCLCH         HELP                                         
         BE    VOPTHLP                                                          
         SPACE                                                                  
         GOTO1 SCANNER,DMCB,TRAOPTH,(7,BLOCK+64)                                
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VOPT12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VOPT14              NO, NETHER                                   
VOPT12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
         SPACE                                                                  
VOPT14   EX    R1,VOPTCLCA         ALL                                          
         BNE   VOPT20                                                           
         MVI   SVTBPR2,C'Y'                                                     
         B     VOPT90                                                           
         SPACE                                                                  
VOPT20   EX    R1,VOPTCLCB         GOALS                                        
         BNE   VOPT30                                                           
         MVI   SVTBPR1,C'G'                                                     
         B     VOPT90                                                           
VOPT30   EX    R1,VOPTCLCC         MGRP                                         
         BNE   VOPT40                                                           
         MVI   SVTBPR1,C'M'                                                     
         B     VOPT90                                                           
VOPT40   EX    R1,VOPTCLCD         BOTH                                         
         BNE   VOPT50                                                           
         MVI   SVTBPR1,C'B'                                                     
         B     VOPT90                                                           
         SPACE                                                                  
VOPT50   EX    R1,VOPTCLCE         USE MARKET LIST RECS                         
         BNE   VOPT60                                                           
         MVI   SVTBPR1,C'L'                                                     
         CLI   1(R4),0             SECOND ENTRY?                                
         BE    VOPT90               NO                                          
         CLC   =C'ALL',22(R4)                                                   
         BNE   VOPT54                                                           
         MVI   SVTBPR3,C'N'                                                     
         B     VOPT90                                                           
VOPT54   CLC   =C'CLT',22(R4)                                                   
         BNE   OPTMKTER                                                         
         MVI   SVTBPR3,C'Y'                                                     
         B     VOPT90                                                           
         SPACE                                                                  
VOPT60   EX    R1,VOPTCLCF         BUY (OPPOSITE OF ALL)                        
         BNE   VOPTHLP                                                          
         MVI   SVTBPR2,C'N'                                                     
         SPACE                                                                  
VOPT90   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
         SPACE                                                                  
VOPT96   CLI   SVTBPR1,0           IS SVTBPR1 OK                                
         BE    TBPROFER                                                         
         B     EXIT                                                             
         SPACE 3                                                                
VOPTCLCA CLC   12(0,R4),=CL4'ALL '                                              
VOPTCLCB CLC   12(0,R4),=CL6'GOALS'                                             
VOPTCLCC CLC   12(0,R4),=CL4'MGRP'                                              
VOPTCLCD CLC   12(0,R4),=CL5'BOTH '                                             
VOPTCLCE CLC   12(0,R4),=CL8'MKTLIST '                                          
VOPTCLCF CLC   12(0,R4),=CL4'BUY '                                              
VOPTCLCH CLC   8(0,R2),=CL4'HELP'                                               
         SPACE 2                                                                
VOPTHLP  L     R1,=A(OPTHLPMS)                                                  
         A     R1,SPTR26RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         CLI   SVTBPR2,C'N'                                                     
         BE    *+10                                                             
         MVC   CONHEAD+38(3),=C'BUY'                                            
         B     ERREXITC                                                         
         EJECT                                                                  
* COUNT EXISTING BUYS *                                                         
         SPACE                                                                  
CTB      NTR1                                                                   
         XC    STACT,STACT                                                      
         XC    BUYCT,BUYCT                                                      
         XC    CKMGRMKT,CKMGRMKT   FOR MGRP'S                                   
         SR    R3,R3               BUY CT                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING TBYKEY,R4                                                        
         MVC   TBYKID,=XL2'0A32'                                                
         MVC   TBYKAM(4),BAGYMD BCLT AND BPRD                                   
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     CTB12                                                            
         SPACE                                                                  
CTB10    MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         SPACE                                                                  
CTB12    SR    R5,R5               STATION BOUGHT CT                            
         CLC   KEY(6),KEYSAVE                                                   
         BNE   CTB30                                                            
         CLI   SVTBPR1,C'M'        USE MARKET GROUP FOR SOURCE                  
         BE    CTB14                                                            
         CLI   SVTBPR1,C'B'        USE MARKET GROUP AND GOALS                   
         BNE   CTB18                                                            
CTB14    CLC   CKMGRMKT,KEY+6      THIS MKT CKD YET                             
         BE    CTB18                YES                                         
         BH    CTB10                BYPASS                                      
         SPACE                                                                  
* IF MGROUP, ONLY COUNT BUYS FOR MARKETS IN MARKET GROUP *                      
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         USING MKGRECD,R4                                                       
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,BAGYMD                                                  
         MVC   MKGPCLT,SVMGRCLT    ZERO IF USING ALL CLTS                       
         MVC   MKGPMID(3),SVMGRP                                                
         MVC   MKGPMKT,SVKEY+6                                                  
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         MVC   CKMGRMKT,MKGPMKT                                                 
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+10                                                             
         MVC   CKMGRMKT,=X'FFFF'                                                
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+6(2),CKMGRMKT   PART OF MGRP                                 
         BL    CTB10                NO, BYPASS                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TBYKEY,R4                                                        
CTB18    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         USING TBYDTAEL,R6                                                      
CTB20    BAS   RE,NEXTEL                                                        
         BNE   CTB10                                                            
         CLC   BSLN(3),TBYSLN      SAME SPOT LEN/PARTNER/SPOT LEN               
         BNE   CTB20                NO, BYPASS                                  
         CLC   CODE,TBYCODE        SAME CODE                                    
         BNE   CTB20                NO, BYPASS                                  
         CLC   TBYSTART,PEREND     CK IF IN PERIOD                              
         BH    CTB20                                                            
         CLC   TBYEND,PERST                                                     
         BL    CTB20                                                            
         LTR   R5,R5                                                            
         BNZ   CTB24                                                            
         LA    R5,1(,R5)                                                        
         LH    R1,STACT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,STACT                                                         
         SPACE                                                                  
CTB24    BCT   R3,CTB20                                                         
         SPACE                                                                  
CTB30    LCR   R3,R3                                                            
         STH   R3,BUYCT                                                         
         XC    CKMGRMKT,CKMGRMKT                                                
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* FIND POSSIBLE BUY FROM STATION LIST *                                         
         SPACE                                                                  
         USING STATABLD,R5                                                      
NXB      NTR1                                                                   
         SPACE                                                                  
         TM    PRGSW,X'02'         MISSING STATION LIST FOR MKT                 
         BO    NXB10                                                            
         OC    STMKT,STMKT         NEW STARTING MARKET                          
         BNZ   NXB10                                                            
         OC    MKEY,MKEY           NEED MARKET                                  
         BNZ   NXB30                NO                                          
         SPACE                                                                  
NXB10    NI    PRGSW,X'FF'-X'02'   SET OFF MISSING STATION LIST FOR MKT         
         CLI   SVTBPR1,C'L'        USE ENTIRE MARKET LIST                       
         BE    NXB12                                                            
         CLI   SVTBPR1,C'G'        USE GOALS FOR MARKET SOURCE                  
         BE    NXB14                                                            
         CLI   SVTBPR1,C'M'        USE MARKET GROUP FOR SOURCE                  
         BE    NXB16                                                            
         CLI   SVTBPR1,C'B'        USE MARKET GROUP AND GOALS                   
         BE    NXB16                                                            
         B     SRCERR                                                           
         SPACE                                                                  
NXB12    BAS   RE,RML              USE MARKET LIST AS SOURCE                    
         B     NXB20                                                            
         SPACE                                                                  
NXB14    BAS   RE,RGL              USE GOAL AS MARKET (AND DATE) SOURCE         
         B     NXB20                                                            
         SPACE                                                                  
NXB16    BAS   RE,RMG              USE MARKET GROUP AS MARKET SOURCE            
         SPACE                                                                  
NXB20    CLI   MKEY,X'FF'                                                       
         BNE   NXB30                                                            
         MVI   SKEY,X'FF'                                                       
         MVC   SKEY+1(12),SKEY                                                  
         MVC   SMKTSTA(9),SKEY                                                  
         MVC   MMKT,SKEY                                                        
         NI    PRGSW,X'FF'-X'80'   SET OFF REC IN AIO2                          
         B     NXB50                                                            
         SPACE                                                                  
* GET NEXT STATION IN STATION LIST *                                            
         SPACE                                                                  
NXB30    OC    SKEY,SKEY           STARTING IN LIST                             
         BZ    NXB32                                                            
         SPACE                                                                  
         CLI   SKEY,X'FF'                                                       
         BE    NXB50                                                            
         TM    PRGSW,X'20'         WAS THIS STATION USED                        
         BZ    NXB50                NO                                          
         MVC   KEY,SKEY                                                         
         TM    PRGSW,X'80'         REC IN AIO2                                  
         BZ    NXB40                NO                                          
         B     NXB42                                                            
         SPACE                                                                  
NXB32    LA    R4,KEY                                                           
         SPACE                                                                  
         XC    KEY,KEY                                                          
         USING STLKEY,R4                                                        
         MVC   STLKID,=XL2'0A31'                                                
         MVC   STLKAM(3),BAGYMD BCLT                                            
         MVC   STLKPRD,QPRD                                                     
         MVC   STLKMKT,MMKT                                                     
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NXB36                                                            
         MVC   KEY(13),KEYSAVE                                                  
         XC    STLKPRD,STLKPRD                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NXB36                                                            
         SPACE                                                                  
         CLI   SVTBPR3,C'Y'        ONLY CLIENT SPECIFIC MARKET LIST             
         BE    NXB34                                                            
         MVC   KEY(13),KEYSAVE                                                  
         XC    STLKCLT,STLKCLT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NXB36                                                            
         SPACE                                                                  
* BUILD FAKE MARKET LIST FOR MISSING MARKET *                                   
         SPACE                                                                  
NXB34    MVC   SKEY,KEYSAVE                                                     
         MVC   SMKTSTA(2),MMKT                                                  
         MVC   SMKTSTA+2(3),=X'FFFFFF'                                          
         MVC   SDATES,GOLPER                                                    
         OI    PRGSW,X'02'         SET MISSING STATION LIST FOR MKT             
         NI    PRGSW,X'FF'-X'20'   SET OFF STATION USED BIT                     
         CLI   STMKTSW,C'='        ONLY 1 MARKET                                
         BNE   NXB50                NO                                          
         SPACE                                                                  
* BUILD MISSING MARKET ENTRY FOR DISPLAY *                                      
         SPACE                                                                  
         XC    STAENT,STAENT                                                    
         ZIC   R1,LINCT                                                         
         LA    R1,1(,R1)                                                        
         STC   R1,LINCT                                                         
         STC   R1,STALIN                                                        
         MVC   STABMKT,MMKT                                                     
         MVC   STABSTA,=X'FFFFFF'                                               
         SR    RF,RF                                                            
         ICM   RF,3,MMKT                                                        
         LA    RF,1(,RF)                                                        
         STCM  RF,3,MMKT                                                        
         STCM  RF,3,STMKT          RESET STARTING MKT TO NEXT                   
         B     NXBEQX                                                           
         SPACE                                                                  
NXB36    MVC   SKEY,KEY                                                         
         DROP  R4                                                               
         SPACE                                                                  
NXB40    MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         OI    PRGSW,X'80'         SET ON REC IN AIO2                           
         SPACE                                                                  
NXB42    L     R6,AIO2                                                          
         MVI   ELCODE,X'10'                                                     
         OC    SELPTR,SELPTR                                                    
         BZ    NXB43                                                            
         SR    RF,RF                                                            
         ICM   RF,3,SELPTR                                                      
         AR    R6,RF                                                            
         B     NXB46                                                            
         SPACE                                                                  
NXB43    BAS   RE,GETEL                                                         
         USING STLDTAEL,R6                                                      
         BNE   NXB48                                                            
         SPACE                                                                  
NXB44    MVC   SMKTSTA(2),MMKT                                                  
         MVC   SMKTSTA+2(3),STLSTA                                              
         MVC   SDATES,GOLPER                                                    
         S     R6,AIO2                                                          
         STCM  R6,3,SELPTR                                                      
         NI    PRGSW,X'FF'-X'20'   SET OFF STATION USED BIT                     
         B     NXB50                                                            
         SPACE                                                                  
NXB46    BAS   RE,NEXTEL                                                        
         BE    NXB44                                                            
         SPACE                                                                  
NXB48    XC    SKEY,SKEY                                                        
         XC    SELPTR,SELPTR                                                    
         NI    PRGSW,X'FF'-X'80'   SET OFF REC IN AIO2                          
         CLI   STMKTSW,C'='        ONLY 1 MARKET                                
         BNE   NXB10                NO, GET NEXT MARKET                         
         MVI   MKEY,X'FF'                                                       
         B     NXB20                                                            
         EJECT                                                                  
* GET BUY IN TBUY LIST *                                                        
         SPACE                                                                  
NXB50    LA    R4,KEY                                                           
         OC    BKEY,BKEY           STARTING IN LIST                             
         BZ    NXB52                                                            
         SPACE                                                                  
         OC    STMKT,STMKT         NEW STARTING MARKET                          
         BNZ   NXB52                                                            
         SPACE                                                                  
         CLI   BKEY,X'FF'                                                       
         BE    NXB80                                                            
         TM    PRGSW,X'10'         LAST TBUY USED                               
         BZ    NXB80                                                            
         MVC   KEY,BKEY                                                         
         CLI   BELEM,X'FF'         RAN OUT OF REC ON LAST                       
         BE    NXB78                                                            
         SPACE                                                                  
         TM    PRGSW,X'40'         REC ALREADY IN AIO3                          
         BO    NXB64                                                            
         B     NXB62                                                            
         SPACE                                                                  
NXB52    XC    BELEM,BELEM                                                      
         XC    BELPTR,BELPTR                                                    
         XC    BUYMKST,BUYMKST                                                  
         XC    KEY,KEY                                                          
         USING TBYKEY,R4                                                        
         MVC   TBYKID,=XL2'0A32'                                                
         MVC   TBYKAM(4),BAGYMD BCLT AND BPRD                                   
         MVC   TBYKMKT,STMKT                                                    
         SPACE                                                                  
         OC    STMKT,STMKT                                                      
         BNZ   NXB53                                                            
         CLI   SVTBPR1,C'M'        ONLY FOR MGROUP OR BOTH                      
         BE    *+12                                                             
         CLI   SVTBPR1,C'B'        ONLY FOR MGROUP OR BOTH                      
         BNE   NXB53                                                            
         MVC   TBYKMKT,MMKT        ONLY SHOW THIS MKT GROUP                     
         SPACE                                                                  
NXB53    XC    STMKT,STMKT                                                      
         SPACE                                                                  
NXB54    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE                                                                  
NXB56    CLI   STMKTSW,C'='        ONLY 1 MKT                                   
         BNE   NXB58                                                            
         CLC   KEY(8),KEYSAVE      CK TYP/AM/CLT/PRD/MKT                        
         BE    NXB60                                                            
         B     *+14                FORCE END                                    
NXB58    CLC   KEY(6),KEYSAVE                                                   
         BE    NXB60                                                            
         MVI   BKEY,X'FF'                                                       
         MVC   BKEY+1(12),BKEY                                                  
         MVC   BUYMKST(9),BKEY                                                  
         NI    PRGSW,X'FF'-X'40'-X'10'   SET OFF TBUY USED/REC IN AIO3          
         B     NXB80                                                            
         EJECT                                                                  
NXB60    MVC   BKEY,KEY                                                         
         CLI   SVTBPR1,C'M'        ONLY FOR MGROUP OR BOTH                      
         BE    *+12                                                             
         CLI   SVTBPR1,C'B'        ONLY FOR MGROUP OR BOTH                      
         BNE   NXB62                                                            
         CLC   TBYKMKT,MMKT        ONLY SHOW THIS MKT GROUP                     
         BL    NXB78                                                            
         BE    NXB62                                                            
         SPACE                                                                  
* ONLY IF MMKT HIGHER DO WE CK FOR MGRP *                                       
         SPACE                                                                  
         CLC   TBYKMKT,CKMGRMKT    THIS MKT CHECKED YET                         
         BE    NXB62                                                            
         BL    NXB78                                                            
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         USING MKGRECD,R4                                                       
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,BAGYMD                                                  
         MVC   MKGPCLT,SVMGRCLT    ZERO IF USING ALL CLTS                       
         MVC   MKGPMID(3),SVMGRP                                                
         MVC   MKGPMKT,SVKEY+TBYKMKT-TBYKEY                                     
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   CKMGRMKT,MKGPMKT                                                 
         CLC   KEY(11),KEYSAVE     IF PAST MGRP                                 
         BE    *+10                                                             
         MVC   CKMGRMKT,=X'FFFF'   FORCE MGRP HIGH                              
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TBYKEY,R4                                                        
         CLC   TBYKMKT,CKMGRMKT    THIS MKT PART OF MGROUP                      
         BNE   NXB78                NO, BYPASS                                  
         SPACE                                                                  
NXB62    MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         OI    PRGSW,X'40'         REC ALREADY IN AIO3                          
         SPACE                                                                  
NXB64    MVI   ELCODE,X'10'                                                     
         L     R6,AIO3                                                          
         OC    BELEM,BELEM                                                      
         BZ    NXB68                                                            
         CLI   BELEM,X'FF'                                                      
         BE    NXB78                                                            
         SPACE                                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
NXB66    CLC   BELEM,0(R6)                                                      
         BE    NXB70                                                            
         BAS   RE,NEXTEL                                                        
         BE    NXB66                                                            
         DC    H'0'                                                             
         SPACE                                                                  
NXB68    BAS   RE,GETEL                                                         
         BNE   NXB78                                                            
         USING TBYDTAEL,R6                                                      
NXB70    CLC   TBYSLN(3),BSLN                                                   
         BNE   NXB74                                                            
         CLC   TBYCODE,CODE                                                     
         BNE   NXB74                                                            
         CLC   TBYSTART,PEREND                                                  
         BH    NXB74                                                            
         CLC   TBYEND,PERST                                                     
         BL    NXB74                                                            
         SPACE                                                                  
         MVC   BUYMKST,TBYKMKT                                                  
         GOTO1 DATCON,DMCB,(3,TBYSTART),(2,BDATES)                              
         GOTO1 (RF),(R1),(3,TBYEND),(2,BDATES+2)                                
         MVC   BELEM,TBYDTAEL                                                   
         S     R6,AIO3                                                          
         STCM  R6,3,BELPTR                                                      
         NI    PRGSW,X'FF'-X'10'   SET OFF TBUY USED BIT                        
         B     NXB80                                                            
         DROP  R6                                                               
         SPACE                                                                  
NXB74    BAS   RE,NEXTEL                                                        
         BE    NXB70                                                            
         SPACE                                                                  
NXB78    XC    BELEM,BELEM                                                      
         MVC   KEY,BKEY                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     NXB56                                                            
         EJECT                                                                  
NXB80    CLC   BUYMKST(9),SMKTSTA  COMPARE BUY TO POSSSIBLE BUY                 
         BH    NXB90               USE POSSIBLE                                 
         BL    NXB84                                                            
         SPACE                                                                  
         CLI   SKEY,X'FF'          AT END OF STATION LIST                       
         BE    NXB84                                                            
         OI    PRGSW,X'20'         IF EQ, READ NEXT STA IN STA LIST             
         SPACE                                                                  
NXB84    CLI   BKEY,X'FF'          AT END                                       
         BE    NXBNEX                                                           
         SPACE                                                                  
* BUILD STATABL ENTRY WITH BUY *                                                
         SPACE                                                                  
         OI    PRGSW,X'10'         SET THIS TBUY USED SW                        
         XC    STAENT,STAENT                                                    
         LA    R6,BELEM                                                         
         USING TBYDTAEL,R6                                                      
         MVC   STABMKT(5),BUYMKST                                               
         MVC   STADSKA,BKEY+14                                                  
         MVC   STAFTDP(6),BELEM+TBYSTART-TBYDTAEL                               
         BAS   RE,STAB                                                          
         SR    R6,R6                                                            
         ICM   R6,3,BELPTR                                                      
         A     R6,AIO3                                                          
         TM    PRGSW,X'40'         REC ALREADY IN AIO3                          
         BO    NXB86                YES                                         
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         MVC   KEY+14(4),BKEY+14                                                
         GOTO1 GETREC                                                           
         OI    PRGSW,X'40'         REC ALREADY IN AIO3                          
NXB86    MVC   BELEM,TBYDTAX                                                    
         CLI   TBYDTAX,X'10'                                                    
         BE    *+8                                                              
         MVI   BELEM,X'FF'                                                      
         B     NXBEQX                                                           
         SPACE                                                                  
* BUILD STATABL ENTRY FROM STATION LIST (DUMMY ENTRY FOR MISS MKT) *            
         SPACE                                                                  
NXB90    CLI   SKEY,X'FF'                                                       
         BE    NXBNEX                                                           
         OI    PRGSW,X'20'         SET ON STATION USED                          
         XC    STAENT,STAENT                                                    
         MVC   STABMKT(5),SMKTSTA                                               
         GOTO1 DATCON,DMCB,(2,SDATES),(3,STAFTDP)                               
         GOTO1 (RF),(R1),(2,SDATES+2),(3,STALTDP)                               
         BAS   RE,STAB                                                          
         NOPR  RF                                                               
         SPACE                                                                  
NXBEQX   CR    RB,RB                                                            
         B     EXIT                                                             
NXBNEX   LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
* BUILD STANDARD PARTS OF STAENT *                                              
         SPACE                                                                  
STAB     NTR1                                                                   
         ZIC   RF,LINCT                                                         
         LA    RF,1(,RF)                                                        
         STC   RF,LINCT                                                         
         STC   RF,STALIN                                                        
         GOTO1 MSUNPK,DMCB,(X'80',STABMKT),DUB,WORK                             
         MVC   STASTA,SPACES                                                    
         MVC   STASTA(4),WORK                                                   
         SPACE                                                                  
         CLC   WORK+5(3),SPACES    THIS A CABLE HEAD STATION                    
         BE    STAB06               NO                                          
         MVC   STASTA+5(3),WORK+5                                               
         MVI   STASTA+4,C'/'                                                    
         B     STAB14                                                           
         SPACE                                                                  
STAB06   CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
         LA    RE,STASTA+4                                                      
         CLI   STASTA+3,C' '                                                    
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),WORK+4                                                   
         CLI   1(RE),C'T'                                                       
         BNE   *+12                                                             
         MVI   2(RE),C'V'                                                       
         B     STAB14                                                           
         CLI   1(RE),C'F'                                                       
         BE    STAB10                                                           
         CLI   1(RE),C'A'                                                       
         BNE   *+8                                                              
STAB10   MVI   2(RE),C'M'                                                       
         SPACE                                                                  
STAB14   GOTO1 DATCON,DMCB,(3,STAFTDP),(5,STAFTD)                               
         GOTO1 (RF),(R1),(3,STALTDP),(5,STALTD)                                 
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* READ MARKET LIST RECORDS ONLY *                                               
         SPACE                                                                  
RML      NTR1                                                                   
         SPACE                                                                  
         SR    RF,RF                                                            
         ICM   RF,3,MMKT                                                        
         LA    RF,1(,RF)                                                        
         SPACE                                                                  
         OC    STMKT,STMKT         NEW STARTING MARKET                          
         BZ    RML10                                                            
         ICM   RF,3,STMKT                                                       
         SPACE                                                                  
RML10    XC    KEY,KEY                                                          
         STCM  RF,3,MMKT                                                        
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STLKEY,R4                                                        
         MVC   STLKID,=XL2'0AB1'                                                
         MVC   STLKAM,BAGYMD                                                    
         MVC   STLPMKT,MMKT                                                     
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BNE   RML30                                                            
         CLI   STMKTSW,C'='        ONLY 1 MKT                                   
         BNE   RML12                                                            
         CLC   KEY+3(2),SVSTMKT    SAME MKT                                     
         BNE   MISMKTER                                                         
RML12    MVC   MMKT,KEY+3                                                       
         MVC   STLPCLT,BCLT                                                     
         MVC   STLPPRD,QPRD                                                     
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RML20                                                            
         MVC   KEY(13),KEYSAVE                                                  
         XC    STLPPRD,STLPPRD                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RML20                                                            
         SPACE                                                                  
         CLI   SVTBPR3,C'Y'        ONLY CLIENT SPECIFIC MARKET LIST             
         BE    RML16                                                            
         SPACE                                                                  
         MVC   KEY(13),KEYSAVE                                                  
         XC    STLPCLT,STLPCLT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RML20                                                            
RML16    CLI   STMKTSW,C'='        ONLY 1 MKT                                   
         BE    MISMKTER                                                         
         SR    RF,RF                                                            
         ICM   RF,3,MMKT                                                        
         CH    RF,=H'9999'                                                      
         BE    RML30                                                            
         SPACE                                                                  
         LA    RF,1(,RF)                                                        
         B     RML10                                                            
         SPACE                                                                  
RML20    XC    SELPTR,SELPTR                                                    
         XC    SMKTSTA,SMKTSTA                                                  
         XC    SKEY,SKEY                                                        
         MVC   GOLPER,PERSTB                                                    
         MVC   MKEY(2),MMKT                                                     
         B     EXIT                                                             
         SPACE                                                                  
RML30    MVI   MKEY,X'FF'                                                       
         CLI   STMKTSW,C'='        ONLY 1 MKT                                   
         BE    MISMKTER                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* READ GOAL RECORDS TO FIND MARKETS FOR BUYS *                                  
         SPACE                                                                  
RGL      NTR1                                                                   
         SPACE                                                                  
         LA    R4,KEY                                                           
         USING GOALRECD,R4                                                      
         SPACE                                                                  
         OC    STMKT,STMKT         NEW STARTING MARKET                          
         BNZ   RGL06                                                            
         OC    MMKT,MMKT           NEXT MARKET                                  
         BNZ   RGL46                YES                                         
RGL06    XC    SKEY,SKEY                                                        
         XC    SELPTR,SELPTR                                                    
         XC    SMKTSTA,SMKTSTA                                                  
         XC    MMKT,MMKT                                                        
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   GKEYTYPE,02                                                      
         MVC   GKEYAM(4),BAGYMD BCLT, AND BPRD                                  
         MVC   GKEYMKT,STMKT                                                    
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         SPACE                                                                  
RGL10    MVC   MKEY,KEY                                                         
         CLI   STMKTSW,C'='        ONLY 1 MKT                                   
         BNE   RGL12                                                            
         CLC   KEY(7),KEYSAVE    TYP/AM/CLT/PRD/MKT                             
         BE    RGL16                                                            
         B     MISGOLER                                                         
RGL12    CLC   KEY(5),KEYSAVE    TYP/AM/CLT/PRD                                 
         BE    RGL16                                                            
         SPACE                                                                  
RGL14    MVI   MKEY,X'FF'                                                       
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         SPACE                                                                  
RGL16    XC    SKEY,SKEY                                                        
         MVC   GOLPER,=X'FFFF0000'                                              
         SPACE                                                                  
RGL18    MVC   MKEY,KEY                                                         
         CLI   SVPROF11,C'E'       COPY CODE BY EST                             
         BNE   *+14                                                             
         CLC   GKEYEST,CODE        THIS ESTIMATE                                
         BNE   RGL40                                                            
         SPACE                                                                  
         CLI   BSLN2,0                                                          
         BNE   RGL20                                                            
         CLC   GKEYSLN,BSLN                                                     
         BE    RGL24                                                            
         B     RGL40                                                            
         EJECT                                                                  
RGL20    ZIC   RE,BSLN                                                          
         ZIC   RF,BSLN2                                                         
         AR    RE,RF                                                            
         CLM   RE,1,GKEYSEC                                                     
         BNE   RGL40                                                            
         CLC   GKEYPRD2,BPRD2                                                   
         BNE   RGL40                                                            
         SPACE                                                                  
RGL24    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   RGL40                                                            
         USING GLEMENT,R6                                                       
RGL30    CLC   GLWEEK,PERMOSTB                                                  
         BL    RGL32                                                            
         CLC   GLWEEK,PERENDB                                                   
         BH    RGL32                                                            
         SPACE                                                                  
         CLC   GLWEEK,GOLSTB                                                    
         BNL   *+10                                                             
         MVC   GOLSTB,GLWEEK                                                    
         GOTO1 DATCON,DMCB,(2,GLWEEK),(0,WORK)                                  
         GOTO1 ADDAY,(R1),WORK,WORK+6,6                                         
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WORK)                                  
         CLC   WORK(2),GOLENDB                                                  
         BNH   *+10                                                             
         MVC   GOLENDB,WORK                                                     
         SPACE                                                                  
RGL32    BAS   RE,NEXTEL                                                        
         BE    RGL30                                                            
         SPACE                                                                  
RGL40    MVC   KEYSAVE(13),KEY                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 SEQ                                                              
         CLC   KEY(7),KEYSAVE      SAME A/M, CLT, PRD, MKT                      
         BE    RGL18                                                            
         SPACE                                                                  
RGL44    CLI   GOLPER,X'FF'        ANY DATES IN PERIOD FOUND                    
         BE    RGL46                NO                                          
         SPACE                                                                  
         MVC   MMKT,KEYSAVE+GKEYMKT-GKEY                                        
         SPACE                                                                  
         CLC   GOLSTB,PERSTB                                                    
         BNL   *+10                                                             
         MVC   GOLSTB,PERSTB                                                    
         CLC   GOLENDB,PERENDB                                                  
         BNH   *+10                                                             
         MVC   GOLENDB,PERENDB                                                  
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         B     EXIT                                                             
         SPACE                                                                  
RGL46    MVC   KEY,MKEY                                                         
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         XC    MMKT,MMKT                                                        
         B     RGL10                                                            
         DROP  R4,R6                                                            
         EJECT                                                                  
* READ MARKET GROUP RECORDS TO FIND MARKETS FOR BUYS *                          
         SPACE                                                                  
RMG      NTR1                                                                   
         SPACE                                                                  
         LA    R4,KEY                                                           
         XC    SKEY,SKEY                                                        
         XC    SELPTR,SELPTR                                                    
         XC    SMKTSTA,SMKTSTA                                                  
         SPACE                                                                  
         OC    STMKT,STMKT         NEW STARTING MARKET                          
         BNZ   RMG06                                                            
         OC    MMKT,MMKT           NEXT MARKET                                  
         BNZ   RMG26                YES                                         
         SPACE                                                                  
RMG06    XC    KEY,KEY                                                          
         USING MKGRECD,R4                                                       
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,BAGYMD                                                  
         MVC   MKGPCLT,SVMGRCLT    ZERO IF USING ALL CLTS                       
         MVC   MKGPMID(3),SVMGRP                                                
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         SPACE                                                                  
RMG10    MVC   MKEY,KEY                                                         
         CLI   STMKTSW,C'='        ONLY 1 MKT                                   
         BNE   RMG14                                                            
         CLC   KEY(7),KEYSAVE    TYP/AM/CLT/PRD/MKT                             
         BE    RMG20                                                            
         B     MISMGRER                                                         
RMG14    CLC   KEY(11),KEYSAVE                                                  
         BE    RMG20                                                            
         SPACE                                                                  
RMG16    MVI   MKEY,X'FF'                                                       
         SPACE                                                                  
RMGX     XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         SPACE                                                                  
RMG20    XC    SKEY,SKEY                                                        
         MVC   GOLPER,PERSTB                                                    
         SPACE                                                                  
         OC    STMKT,STMKT         STARTING MARKET ENTERED                      
         BZ    RMG24                                                            
         CLC   STMKT,MKGPMKT                                                    
         BNE   RMG26                                                            
         SPACE                                                                  
RMG24    MVC   MMKT,MKGPMKT                                                     
         B     RMG30                                                            
         SPACE                                                                  
         DROP  R4                                                               
         SPACE                                                                  
RMG26    MVC   KEY,MKEY                                                         
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     RMG10                                                            
         SPACE                                                                  
RMG30    CLI   SVTBPR1,C'B'        CK GOALS AS WELL                             
         BNE   RMGX                                                             
         SPACE                                                                  
         USING GOALRECD,R4                                                      
         SPACE                                                                  
         XC    SKEY,SKEY                                                        
         XC    SELPTR,SELPTR                                                    
         XC    SMKTSTA,SMKTSTA                                                  
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   GKEYTYPE,02                                                      
         MVC   GKEYAM(4),BAGYMD BCLT, AND BPRD                                  
         MVC   GKEYMKT,MMKT                                                     
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE                                                                  
         MVC   GOLPER,=X'FFFF0000'                                              
         SPACE                                                                  
RMG40    CLC   KEY(7),KEYSAVE      SAME A/M, CLT, PRD, MKT                      
         BNE   RMG26                                                            
         SPACE                                                                  
         CLI   SVPROF11,C'E'       COPY CODE BY EST                             
         BNE   *+14                                                             
         CLC   GKEYEST,CODE        THIS ESTIMATE                                
         BNE   RMG60                                                            
         CLI   BSLN2,0                                                          
         BNE   RMG46                                                            
         CLC   GKEYSLN,BSLN                                                     
         BE    RMG50                                                            
         B     RMG60                                                            
         SPACE                                                                  
RMG46    ZIC   RE,BSLN                                                          
         ZIC   RF,BSLN2                                                         
         AR    RE,RF                                                            
         CLM   RE,1,GKEYSEC                                                     
         BNE   RMG60                                                            
         CLC   GKEYPRD2,BPRD2                                                   
         BNE   RMG60                                                            
         DROP  R4                                                               
         SPACE                                                                  
RMG50    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   RMG60                                                            
         USING GLEMENT,R6                                                       
RMG54    CLC   GLWEEK,PERMOSTB                                                  
         BL    RMG56                                                            
         CLC   GLWEEK,PERENDB                                                   
         BH    RMG56                                                            
         SPACE                                                                  
         CLC   GLWEEK,GOLSTB                                                    
         BNL   *+10                                                             
         MVC   GOLSTB,GLWEEK                                                    
         GOTO1 DATCON,DMCB,(2,GLWEEK),(0,WORK)                                  
         GOTO1 ADDAY,(R1),WORK,WORK+6,6                                         
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WORK)                                  
         CLC   WORK(2),GOLENDB                                                  
         BNH   *+10                                                             
         MVC   GOLENDB,WORK                                                     
         SPACE                                                                  
RMG56    BAS   RE,NEXTEL                                                        
         BE    RMG54                                                            
         SPACE                                                                  
RMG60    MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 SEQ                                                              
         CLC   KEY(7),KEYSAVE      SAME A/M, CLT, PRD, MKT                      
         BE    RMG40                                                            
         SPACE                                                                  
         CLI   GOLPER,X'FF'                                                     
         BE    RMG26                                                            
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         EJECT                                                                  
* ADD THIS STATION BUY TO TBUY REC *                                            
         SPACE                                                                  
         USING STATABLD,R5                                                      
AST      NTR1                                                                   
         SPACE                                                                  
         OC    STADSKA,STADSKA     THIS STATION ALREADY BOUGHT                  
         BNZ   BUYSELER            ALREADY A BUY                                
         SPACE                                                                  
* BUILD NEW ELEM *                                                              
         SPACE                                                                  
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING TBYDTAEL,R6                                                      
         MVI   TBYDTAEL,X'10'                                                   
         MVI   TBYDTALN,TBYDTAX-TBYDTAEL                                        
         MVC   TBYSLN(3),BSLN                                                   
         MVC   TBYCODE,CODE                                                     
         MVC   TBYSTART,STAFTDP                                                 
         CLC   STAFTDP,PERST                                                    
         BNL   AST04                                                            
         MVC   TBYSTART,PERST                                                   
         SPACE                                                                  
AST04    MVC   TBYEND,STALTDP                                                   
         CLC   STALTDP,PEREND                                                   
         BNH   AST06                                                            
         MVC   TBYEND,PEREND                                                    
         SPACE                                                                  
AST06    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING TBYKEY,R4                                                        
         MVC   TBYKID,=XL2'0A32'                                                
         MVC   TBYKAM(4),BAGYMD BCLT AND BPRD                                   
         MVC   TBYKMKT(5),STABMKT                                               
         SPACE                                                                  
         GOTO1 HIGH                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         CLC   KEY(13),KEYSAVE                                                  
         BE    AST14                                                            
         SPACE                                                                  
* SEE IF THIS RECORD IS DELETED *                                               
         SPACE                                                                  
         MVC   KEY(13),KEYSAVE                                                  
         OI    DMINBTS,X'08'       SET TO READ DELETED RECORDS                  
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE     IS THIS A DELETED RECORD                     
         BNE   AST10                NO, REAL ADD                                
         SPACE                                                                  
         NI    KEY+13,X'FF'-X'80'  RESTORE KEY                                  
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSDIR,KEY,KEY                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TBYKID+1,X'80'                                                   
         MVC   TBYPPRD,TBYKPRD                                                  
         MVI   TBYKPRD,X'FF'                                                    
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IS THIS A DELETED RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    KEY+13,X'FF'-X'80'  RESTORE KEY                                  
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSDIR,KEY,KEY                            
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVC   WORK(TBYDTAX-TBYDTAEL),ELEM                                      
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         NI    DMINBTS,X'F7'                                                    
         NI    15(R6),X'FF'-X'80'  RESTORE REC                                  
         MVC   ELEM(TBYDTAX-TBYDTAEL),WORK                                      
         B     AST30                                                            
         SPACE                                                                  
AST10    XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVC   13(2,R6),=H'24'                                                  
         MVC   TBYAGYA-TBYRECD(2,R6),AGENCY                                     
         B     AST30                                                            
         SPACE                                                                  
AST14    GOTO1 GETREC                                                           
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   AST30                                                            
         SPACE                                                                  
         USING TBYDTAEL,R6                                                      
AST20    CLC   TBYSLN(3),BSLN      MATCH ON SLN, PRD2, SLN2                     
         BNE   AST24                                                            
         CLC   TBYCODE,CODE        CODE TOO                                     
         BNE   AST24                                                            
         CLC   TBYSTART,STALTDP    ANY DATE OVERLAP                             
         BH    AST24                                                            
         CLC   TBYEND,STAFTDP                                                   
         BNL   DATOVLER                                                         
AST24    BAS   RE,NEXTEL                                                        
         BE    AST20                                                            
         SPACE                                                                  
AST30    DS    0H                                                               
         GOTO1 ADDELEM                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    AST34                                                            
         MVC   KEY(13),KEYSAVE                                                  
         SPACE                                                                  
         GOTO1 ADDREC                                                           
         SPACE                                                                  
* CREATE PASSIVE KEY                                                            
         SPACE                                                                  
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                GET DISK ADDR OF ADDED REC                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,KEY                                                           
         USING TBYKEY,R4                                                        
         OI    TBYKID+1,X'80'                                                   
         MVC   TBYPPRD,TBYKPRD                                                  
         MVI   TBYKPRD,X'FF'                                                    
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMADD',SYSDIR,KEY,KEY                            
         CLI   DMCB+8,0                                                         
         BE    AST36                                                            
         DC    H'0'                                                             
         SPACE                                                                  
AST34    GOTO1 PUTREC                                                           
         SPACE                                                                  
AST36    MVC   STADSKA,KEY+14      SET TO BUY                                   
         LH    RF,BUYCT                                                         
         LA    RF,1(,RF)                                                        
         STH   RF,BUYCT                                                         
         LH    RF,STACT                                                         
         LA    RF,1(,RF)                                                        
         STH   RF,STACT                                                         
         SPACE                                                                  
         OI    (TRASTAH-TRASEL1H)+1(R2),X'08' SET HIGH INTENSITY                
         MVI   LBUY1-DLINE+(TRASTAH-TRASEL1H)(R2),C'*'                          
         MVI   LBUY2-DLINE+(TRASTAH-TRASEL1H)(R2),C'*'                          
         MVI   LBUY3-DLINE+(TRASTAH-TRASEL1H)(R2),C'*'                          
         OI    (TRASTAH-TRASEL1H)+6(R2),X'80' SET TRANSMIT                      
         SPACE                                                                  
         NI    PRGSW,X'FF'-X'40'   SET OFF REC IN AIO3                          
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
* DELETE THIS STATION BUY FROM TBUY REC *                                       
         SPACE                                                                  
         USING STATABLD,R5                                                      
DST      NTR1                                                                   
         SPACE                                                                  
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING TBYKEY,R4                                                        
         MVC   TBYKID,=XL2'0A32'                                                
         MVC   TBYKAM(4),BAGYMD BCLT AND BPRD                                   
         MVC   TBYKMKT(5),STABMKT                                               
         MVC   KEY+14(4),STADSKA   DISK ADDRESS                                 
         SPACE                                                                  
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING TBYDTAEL,R6                                                      
         MVI   TBYDTAEL,X'10'                                                   
         MVI   TBYDTALN,TBYDTAX-TBYDTAEL                                        
         MVC   TBYSLN(3),BSLN                                                   
         MVC   TBYCODE,CODE                                                     
         MVC   TBYSTART(6),STAFTDP                                              
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         CLC   KEY(13),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSTBUG                                                           
         USING TBYDTAEL,R6                                                      
DST10    CLC   TBYDTAEL(TBYDTAX-TBYDTAEL),ELEM                                  
         BE    DST20                                                            
         BAS   RE,NEXTEL                                                        
         BE    DST10                                                            
DSTBUG   DC    H'0'                                                             
         SPACE                                                                  
DST20    GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         GOTO1 PUTREC                                                           
         XC    STADSKA,STADSKA     SET TO NO BUY                                
         LH    RF,BUYCT                                                         
         BCTR  RF,0                                                             
         STH   RF,BUYCT                                                         
         LH    RF,STACT                                                         
         BCTR  RF,0                                                             
         STH   RF,STACT                                                         
         NI    (TRASTAH-TRASEL1H)+1(R2),X'F7' SET OFF HIGH INTENSITY            
         MVI   LBUY1-DLINE+(TRASTAH-TRASEL1H)(R2),C' '                          
         MVI   LBUY2-DLINE+(TRASTAH-TRASEL1H)(R2),C' '                          
         MVI   LBUY3-DLINE+(TRASTAH-TRASEL1H)(R2),C' '                          
         OI    TRASTAH-TRASEL1H+6(R2),X'80'                                     
         SPACE                                                                  
         NI    PRGSW,X'FF'-X'40'   SET OFF REC IN AIO3                          
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
* FORMAT MARKET/STATION FOR PRINTING *                                          
         SPACE                                                                  
FMTSTA   NTR1                                                                   
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),QMKT,WORK                            
         SPACE                                                                  
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES                                                 
         BE    *+14                                                             
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
         SPACE                                                                  
         CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         CLI   QMED,C'T'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C' '                                                       
*                                                                               
FMTSTAX  B     EXIT                                                             
         EJECT                                                                  
* GET PROFILE REC(S)                                                            
         SPACE                                                                  
FPRO     NTR1                                                                   
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVC   ORGPRF13,SVPROF13                                                
         MVI   SVPROF13,C'N'                                                    
         SPACE                                                                  
* READ T1 PROFILE *                                                             
         SPACE                                                                  
         MVI   WORK+3,C'1'                                                      
         GOTO1 GETPROF,(R1),WORK,SVT1PROF                                       
         SPACE                                                                  
* READ TB PROFILE *                                                             
         SPACE                                                                  
         MVI   WORK+3,C'B'                                                      
         GOTO1 GETPROF,(R1),WORK,SVTBPROF                                       
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* READ MARKET RECORD FOR NAME *                                                 
         SPACE                                                                  
FMKT     NTR1                                                                   
*        MVC   SVKEY,KEY                                                        
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         LH    RF,SVMKT                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO1                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R5)                     
*                                                                               
         LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME-MKTRECD(R5)                                           
         MVC   MKTNM,0(R1)                                                      
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* PRINT PRODUCT CODE AND SPOT LEN - R3 MUST POINT TO BINARY PROD                
         SPACE                                                                  
PPRD     NTR1                                                                   
         LA    R5,WORK+4           ADDRESS OF OUTPUT AREA                       
         XC    WORK+4(10),WORK+4                                                
         CLI   0(R3),0             ANY PRODUCT CODE                             
         BE    EXIT                NO,DONE                                      
         L     R1,ASVCLIST         ADDRESS OF SAVED C LIST (VALICLT)            
PPRD10   CLI   0(R1),C' '                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),3(R1)                                                    
         BE    PPRD12                                                           
         LA    R1,4(R1)                                                         
         B     PPRD10                                                           
PPRD12   MVC   0(3,R5),0(R1)                                                    
         CLI   1(R3),0             ANY SPOT LEN                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R5,2(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BNH   PPRD14                                                           
         LA    R5,1(,R5)                                                        
PPRD14   MVI   0(R5),C'-'                                                       
         ZIC   RF,1(R3)                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+5(3)                                                  
         CLI   DUB,C'0'                                                         
         BE    PPRD20                                                           
         MVC   1(3,R5),DUB                                                      
         B     EXIT                                                             
PPRD20   MVC   1(2,R5),DUB+1                                                    
         B     EXIT                                                             
         EJECT                                                                  
* SAVE KEYS AND STATABL *                                                       
         SPACE                                                                  
CLR      NTR1                                                                   
         XC    STATABL(200),STATABL                                             
         XC    STATABL+200(200),STATABL+200                                     
         XC    STATABL+400(240),STATABL+400                                     
         SPACE                                                                  
         XC    CURRKEYS,CURRKEYS                                                
         XC    STACT(10),STACT                                                  
         SPACE                                                                  
         NI    TRASMKTH+4,X'FF'-X'20' FORCE RE-ENTRY OF MKT                     
         NI    PRGSW,X'03'                                                      
         XC    TRATOTS,TRATOTS                                                  
         OI    TRATOTSH+6,X'80'                                                 
         SPACE                                                                  
         LA    R2,TRASEL1H                                                      
         SPACE                                                                  
CLR10    OC    TRASEL1-TRASEL1H(L'TRASEL1,R2),TRASEL1-TRASEL1H(R2)              
         BZ    CLR14                                                            
         CLC   TRASEL1-TRASEL1H(L'TRASEL1,R2),SPACES                            
         BE    CLR14                                                            
         XC    TRASEL1-TRASEL1H(L'TRASEL1,R2),TRASEL1-TRASEL1H(R2)              
         SPACE                                                                  
CLR14    OI    1(R2),X'20'         SET ON PROTECT                               
         OI    6(R2),X'80'             & TRANSMIT                               
         SPACE                                                                  
         LA    R2,TRASTAH-TRASEL1H(,R2)                                         
         OC    TRASTA-TRASTAH(L'TRASTA,R2),TRASTA-TRASTAH(R2)                   
         BZ    CLR16                                                            
         CLC   TRASTA-TRASTAH(L'TRASTA,R2),SPACES                               
         BE    CLR16                                                            
         XC    TRASTA-TRASTAH(L'TRASTA,R2),TRASTA-TRASTAH(R2)                   
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
CLR16    LA    R2,TRASEL2H-TRASTAH(,R2)                                         
         LA    RF,TRATAGH                                                       
         CR    R2,RF                                                            
         BL    CLR10                                                            
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* SAVE KEYS AND STATABL *                                                       
         SPACE                                                                  
SVTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMWRT'                                                     
         B     COMTWA                                                           
         SPACE                                                                  
* RESTORE KEYS AND STATABL *                                                    
         SPACE                                                                  
RDTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMREAD'                                                    
COMTWA   ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,STACT                                 
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         SPACE                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
COMMGER  LA    R2,TRASMKTH                                                      
         NI    TRAMEDH+4,X'FF'-X'20'                                            
         B     ERREXIT                                                          
OPTMKTER L     R1,=A(OPTMKTMS)                                                  
         B     ERREXIT                                                          
MISMGRER L     R1,=A(MISMGRMS)                                                  
         B     MISMKTX                                                          
MISGOLER L     R1,=A(MISGOLMS)                                                  
         B     MISMKTX                                                          
MISMKTER L     R1,=A(MISMKTMG)                                                  
MISMKTX  LA    R2,TRASMKTH                                                      
         B     ERREXIT                                                          
DELSELER L     R1,=A(DELSELMS)                                                  
         B     CVLSTERR                                                         
BUYSELER L     R1,=A(BUYSELMS)                                                  
         B     CVLSTERR                                                         
SELERR   L     R1,=A(SELERRMS)                                                  
         CLI   SVTBPR2,C'Y'        BUY ON BLANK SEL                             
         BNE   CVLSTERR                                                         
         L     R1,=A(SELERRMA)                                                  
         B     CVLSTERR                                                         
NOMKTERR L     R1,=A(NOMKTMS)                                                   
CVLSTERR EDIT  (B2,STACT),(3,TRATOTS)                                           
         LA    R3,TRATOTS+21                                                    
         EDIT  (B2,BUYCT),(3,(R3))                                              
         OI    TRATOTSH+6,X'80'                                                 
         BAS   RE,SVTWA                                                         
         B     ERREXIT                                                          
SRCERR   L     R1,=A(SRCEMS)                                                    
         B     ERREXIT                                                          
ESTNUMER L     R1,=A(ESTNUMMS)                                                  
         B     ERREXIT                                                          
NOSMKTER L     R1,=A(NOSMKTMS)                                                  
         B     ERREXIT                                                          
DOSEL    L     R1,=A(DOSELMS)                                                   
         CLI   SVTBPR2,C'Y'        BUY ON BLANK SEL                             
         BNE   ENDSELX                                                          
         L     R1,=A(DOSELMSX)                                                  
         B     ENDSELX                                                          
ENDSEL   L     R1,=A(ENDSELMS)                                                  
         CLI   SVTBPR2,C'Y'        BUY ON BLANK SEL                             
         BNE   ENDSELX                                                          
         L     R1,=A(ENDSELMX)                                                  
ENDSELX  A     R1,SPTR26RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         LA    R2,TRASEL1H                                                      
         BAS   RE,SVTWA                                                         
         B     ERREXITC                                                         
         USING TBYDTAEL,R6                                                      
DATOVLER L     R1,=A(DATOVLMS)                                                  
         A     R1,SPTR26RR                                                      
         MVC   CONHEAD+10(50),0(R1)                                             
         GOTO1 DATCON,DMCB,(3,TBYSTART),(5,CONHEAD+37)                          
         MVI   WORK+8,C'-'                                                      
         GOTO1 (RF),(R1),(3,TBYEND),(5,CONHEAD+46)                              
         B     ERREXITB                                                         
         DROP  R6                                                               
TBPROFER L     R1,=A(TBPROFMS)                                                  
         B     ERREXIT                                                          
MGPMKTER L     R1,=A(MGPMKTMS)                                                  
         A     R1,SPTR26RR                                                      
         SR    R0,R0                                                            
         ICM   R0,3,KEYSAVE+STLKMKT-STLKEY                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  41(4,R1),DUB                                                     
         B     ERREXITA                                                         
ERREXIT  A     R1,SPTR26RR                                                      
ERREXITA MVC   CONHEAD+10(50),0(R1)                                             
ERREXITB MVC   CONHEAD(10),=C'* ERROR * '                                       
ERREXITC GOTO1 ERREX2                                                           
         EJECT                                                                  
FLTDTER  MVI   ERROR,NOTFLTDT      PER END NO MATCH TO FLIGHT END DATE          
         B     TRAPERR                                                          
PRDERR   MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
PRDSEQER MVI   ERROR,INVPRDSQ                                                   
         B     TRAPERR                                                          
EQPRDER  MVI   ERROR,INVEQPRD                                                   
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
STACTMSG DC    CL35'000 STATIONS BOUGHT, 000 TOTAL BUYS'                        
OPTHLPMS DC    CL60'VALID OPTIONS=GOALS/MGRP/BOTH/MKTLIST/ALL *'                
DATOVLMS DC    CL50'THIS BUY OVERLAPS EXISTING MONDA/YR-MONDA/YR*'              
DOSELMS  DC    CL60'SELECT B(UYS)/D(ELETES) OR ENTER FOR MORE LIST'             
DOSELMSX DC    CL60'SELECT X(EXCLUDE)/D(ELETES) OR ENTER FOR MORE LIST'         
ENDSELMS DC    CL60'SELECT B(UYS)/D(ELETES)-END LIST, ENTER TO RESTART C        
               LIST'                                                            
ENDSELMX DC    CL60'SELECT X(EXCLUDE)/D(ELETES)-END LIST, ENTER TO RESTC        
               ART LIST'                                                        
OPTMKTMS DC    CL50'MKTLIST=ALL OR CLT *'                                       
NOMKTMS  DC    CL50'CAN''T SELECT MISSING MARKET *'                             
MISMGRMS DC    CL50'NO MARKET GROUP FOR THIS MARKET *'                          
MISGOLMS DC    CL50'NO GOALS FOR THIS MARKET *'                                 
MISMKTMG DC    CL50'NO MKTLIST FOR THIS MARKET *'                               
SRCEMS   DC    CL50'TB PROFILE OPT 1 MUST BE G(OAL) OR M(ARKET GRP) *'          
TBPROFMS DC    CL50'NO TB PROFILE, MUST ENTER SOURCE *'                         
ESTNUMMS DC    CL50'ESTIMATE MUST BE NUMERIC AND 1-3 DIGITS *'                  
BUYSELMS DC    CL50'SELECTED STATION ALREADY BOUGHT *'                          
DELSELMS DC    CL50'SELECTED STATION TO DELETE NOT BOUGHT *'                    
SELERRMS DC    CL50'ONLY B(UY) OR D(ELETE) SELECT CODES *'                      
SELERRMA DC    CL50'ONLY X(EXCLUDE) OR D(ELETE) SELECT CODES *'                 
NOSMKTMS DC    CL50'NO MARKET HIGHER/EQUAL TO STARTING MARKET *'                
MGPMKTMS DC    CL50'NO MARKET/STATION LIST RECORD FOR MARKET 0000 *'            
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'PRODUCT'                                                  
         SSPEC H1,30,C'S T A T I O N  B U Y  L I S T'                           
         SSPEC H2,30,C'-----------------------------'                           
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,93,PAGE                                                       
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,13,C'MARKET NAME'                                             
         SSPEC H9,13,C'------------------------'                                
         SSPEC H8,39,C'STATION'                                                 
         SSPEC H9,39,C'-------'                                                 
         SSPEC H8,49,C'PROD/SLN'                                                
         SSPEC H9,49,C'--------'                                                
         SSPEC H8,59,C'PTNR/SLN'                                                
         SSPEC H9,59,C'--------'                                                
         SSPEC H8,72,C'COPY'                                                    
         SSPEC H9,72,C'CODE'                                                    
         SSPEC H8,81,C'PERIOD START/END DATES'                                  
         SSPEC H9,81,C'----------------------'                                  
         DC    X'00'               END MARKER FOR SSPEC                         
         DROP  RB,RC,R7                                                         
         EJECT                                                                  
* VALIDATE PARTNER PRODUCT *                                                    
         SPACE                                                                  
VPP      NMOD1 0,**+VPP**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         XC    BPRD2(2),BPRD2                                                   
         XC    QPRD2,QPRD2                                                      
         CLI   5(R2),0                                                          
         BE    VPP20                                                            
         GOTO1 VALIPRD                                                          
         CLC   WORK(3),=C'POL'     INVALID                                      
         BE    PRDERRA                                                          
         MVC   QPRD2,WORK                                                       
         CLI   WORK+4,0            VALID SPOT LENGTH                            
         BNE   VPP10                                                            
         MVI   WORK+4,30                                                        
         LA    R1,TRAPTR+3                                                      
         CLI   TRAPTR+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVC   0(3,R1),=C'-30'                                                  
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
VPP10    MVC   BPRD2(2),WORK+3                                                  
         SPACE                                                                  
         CLC   QPRD,QPRD2          ARE PRODUCTS IN SEQ                          
         BE    EQPRDERA                                                         
         BH    PRDSQERA                                                         
         SPACE                                                                  
VPP20    OI    4(R2),X'20'         VALIDATED                                    
         CLI   ORGPRF13,C'Y'       THIS A PROD EQUIV CLT                        
         BNE   VPPX                                                             
         SPACE                                                                  
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING PEQKEY,R4                                                        
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM(3),BAGYMD & BCLT                                          
         MVC   PEQPEPRD,QPRD                                                    
         MVC   WORK(3),QPRD                                                     
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A EQUIV PRODUCT                         
         BNE   VPP30                NO, CK PTR                                  
         MVC   WORK(3),PEQPBPRD                                                 
         SPACE                                                                  
VPP30    XC    KEY,KEY                                                          
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM(3),BAGYMD & BCLT                                          
         MVC   PEQPEPRD,QPRD2                                                   
         MVC   WORK+(3),QPRD2                                                   
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A EQUIV PRODUCT                         
         BNE   VPP40                NO, CK PTR                                  
         MVC   WORK+3(3),PEQPBPRD                                               
VPP40    CLC   WORK(3),WORK+3      ARE PRODUCTS SAME BASE?                      
         BE    EQVPRDER             YES                                         
         SPACE                                                                  
VPPX     XIT1                                                                   
PRDERRA  MVI   ERROR,INVPROD                                                    
         B     VPPEREX                                                          
PRDSQERA MVI   ERROR,INVPRDSQ                                                   
         B     VPPEREX                                                          
EQPRDERA MVI   ERROR,INVEQPRD                                                   
         B     VPPEREX                                                          
VPPEREX  GOTO1 ERREX                                                            
EQVPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EQVPRDMS),EQVPRDMS                                     
         MVC   35(3,RF),WORK                                                    
         GOTO1 ERREX2                                                           
         SPACE                                                                  
EQVPRDMS DC    C'* ERROR * PROD && PTR EQUIVALENT TO BASE PROD XXX *'           
         DROP  R4,RB,RC                                                         
         EJECT                                                                  
* VALIDATE COPY CODE (ESTIMATE IF PROFILE COPY CODE = EST ON) *                 
         SPACE                                                                  
VCD      DS    0D                                                               
         NMOD1 0,**+VCD**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVI   CODE,0                                                           
         MVI   BEST,0                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VCD20                                                            
         SPACE                                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VCD10                                                            
         CLI   5(R2),1             COPY CODE ONLY 1 CHAR                        
         BH    CPYCDER                                                          
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         MVC   CODE,WORK                                                        
VCD10    OI    4(R2),X'20'         VALIDATED                                    
         XIT1                                                                   
         SPACE                                                                  
* VALIDATE ESTIMATE AND STORE EST DATES *                                       
         SPACE                                                                  
VCD20    CLI   5(R2),0                                                          
         BE    NOESTER                                                          
         SPACE                                                                  
         CLC   =C'NO',8(R2)        NO ESTIMATE                                  
         BE    VCD10                                                            
         SPACE                                                                  
         GOTO1 VALINUM                                                          
         SPACE                                                                  
         MVC   CODE,ACTUAL                                                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM(3),BAGYMD                                                 
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,ACTUAL                                                   
         MVC   BEST,ACTUAL                                                      
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BDESTER                                                          
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,(0,ESTART),(3,ESTSTR)                                
         GOTO1 (RF),(R1),(0,EEND),(3,ESTEND)                                    
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         B     VCD10                                                            
         SPACE                                                                  
BDESTER  MVI   ERROR,BADESTS       BAD ESTIMATE NUMBER                          
         GOTO1 ERREX                                                            
NOESTER  MVC   CONHEAD,NOESTMS                                                  
         B     VCDERX                                                           
CPYCDER  MVC   CONHEAD,CPYCDMS                                                  
VCDERX   GOTO1 ERREX2                                                           
NOESTMS  DC    CL60'* ERROR * ESTIMATE MUST BE ENTERED IN COPY CODE *'          
CPYCDMS  DC    CL60'* ERROR * COPY CODE CAN ONLY BE 1 CHARACTER *'              
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD *                             
         SPACE                                                                  
VPER     DS    0D                                                               
         NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
VPER02   CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER26                                                           
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER04              NO                                           
         GOTO1 DATVAL,DMCB,9(R2),WORK                                           
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PERST)                                   
         B     VPER06                                                           
VPER04   GOTO1 DATCON,DMCB,(5,0),(3,PERST) TODAY'S DATE                         
         SPACE                                                                  
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD BCLT AND BPRD                                    
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
*                                                                               
VPER10   CLC   PERST,KEY+6         FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER14                                                           
         MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         MVC   PERST(1),KEY+6      CHANGE YEAR                                  
*                                                                               
VPER14   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R3,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         B     *+8                                                              
VPER16   BAS   RE,NEXTEL1                                                       
         BNE   VPER20                                                           
         USING FLTDTAEL,R6                                                      
         CLC   PERST,FLTEND                                                     
         BNH   VPER18                                                           
         CLC   PERST,FLTSTART                                                   
         BH    VPER16                                                           
         SPACE                                                                  
VPER18   GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(,R5)                                                       
         BCT   R3,VPER16                                                        
VPER20   MVI   0(R5),C'*'                                                       
         B     ERREXIT3                                                         
         SPACE                                                                  
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(13),=C'ESTIMATE RUNS'                                    
         GOTO1 DATCON,DMCB,(3,ESTSTR),(5,CONHEAD+14)                            
         MVI   CONHEAD+23,C'-'                                                  
         GOTO1 (RF),(R1),(3,ESTEND),(5,CONHEAD+25)                              
         B     ERREXIT3                                                         
         EJECT                                                                  
VPER30   XC    PERDTS,PERDTS                                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERRA                                                         
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER34                                                           
         CLC   =C'ES',8(R2)        USE ESTIMATE DATES                           
         BNE   VPER34                                                           
         CLI   BEST,0                                                           
         BE    BLKESTER                                                         
         MVC   PERDTS,ESTDTS                                                    
         GOTO1 DATCON,DMCB,(3,PERST),(5,TRAPER)                                 
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,PEREND),(5,TRAPER+9)                                
         OI    TRAPERH+6,X'80'                                                  
         B     VPER50                                                           
         SPACE                                                                  
VPER34   LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),WORK                                            
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PERST)                                   
         SPACE                                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40              YES                                          
         SPACE                                                                  
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),WORK                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PEREND)                                  
         CLC   PERST,PEREND                                                     
         BH    DATERRA                                                          
         EJECT                                                                  
VPER40   CLI   SVPROF11,C'E'       COPY CODE BY ESTIMATE                        
         BNE   VPER44                                                           
         BAS   RE,FEST             GO CK EST DATES                              
         B     VPER50                                                           
VPER44   BAS   RE,FFLT                                                          
         SPACE                                                                  
         BAS   RE,CEST                                                          
         SPACE                                                                  
* FORCE PERIOD START TO MONDAYS *                                               
         SPACE                                                                  
VPER50   GOTO1 DATCON,DMCB,(3,PERST),(0,WORK)                                   
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
         CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),(0,WORK+6),(2,PERMOSTB)                              
         GOTO1 (RF),(R1),(0,WORK+6),(3,PERMONST)                                
         GOTO1 (RF),(R1),(3,PERST),(2,PERSTB)                                   
         GOTO1 (RF),(R1),(3,PEREND),(2,PERENDB)                                 
VPERX    OI    4(R2),X'20'                                                      
         XIT1                                                                   
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES *                 
***************************************************************                 
         SPACE                                                                  
FFLT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD BCLT AND BPRD                                    
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    FFLT10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
*                                                                               
FFLT10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   PERST,KEY+6         FIRST TLCST DATE TO RECORD END DATE          
         BNH   FFLT14                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         B     FFLT10                                                           
*                                                                               
FFLT14   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         B     *+8                                                              
*                                                                               
FFLT16   BAS   RE,NEXTEL1                                                       
         BNE   FLTELER                                                          
*                                                                               
         USING FLTDTAEL,R6                                                      
*                                                                               
         CLC   PERST,FLTEND        FIRST TLCST AFTER FLIGHT END                 
         BH    FFLT16                                                           
         SPACE                                                                  
         OC    PEREND,PEREND       ANY END DATE ENTERED                         
         BZ    FFLT20                                                           
         SPACE                                                                  
         CLC   PEREND,FLTSTART     LAST TLCST BEFORE FLIGHT START               
         BL    FFLT16                                                           
         SPACE                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
         SPACE                                                                  
         CLC   PEREND,FLTEND       LAST TLCST DATE TO FLT END                   
         BH    FLTOVLER                                                         
         CLC   PERST,FLTSTART                                                   
         BL    FLTOVLER                                                         
         B     VPERX                                                            
         SPACE                                                                  
* ONLY ONE DATE GIVEN, MUST MATCH FLIGHT START                                  
         SPACE                                                                  
FFLT20   CLC   PERST,FLTSTART      PER START MATCH FLIGHT START                 
         BNE   FFLT16                                                           
         MVC   PEREND,FLTEND                                                    
         GOTO1 DATCON,DMCB,(3,PERST),(5,TRAPER)                                 
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,PEREND),(5,TRAPER+9)                                
         OI    TRAPERH+6,X'80'                                                  
         B     VPERX                                                            
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE DETERMINES ESTIMATE DATES FOR GIVEN TELECAST DATES *               
*****************************************************************               
         SPACE                                                                  
FEST     NTR1                                                                   
         SPACE                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                    
         SPACE                                                                  
         CLC   PERST,ESTEND        FIRST TLCST AFTER EST END                    
         BH    ESTDTERR                                                         
         CLC   PERST,ESTSTR        FIRST TLCST BEFORE EST STR                   
         BL    ESTDTERR                                                         
         SPACE                                                                  
         OC    PEREND,PEREND       ANY END DATE ENTERED                         
         BZ    FEST10                                                           
         SPACE                                                                  
         CLC   PEREND,ESTSTR       LAST TLCST BEFORE EST START                  
         BL    ESTDTERR                                                         
         CLC   PEREND,ESTEND       LAST TLCST BEFORE EST END                    
         BH    ESTDTERR                                                         
         SPACE                                                                  
         B     VPERX                                                            
         SPACE                                                                  
* ONLY ONE DATE GIVEN, MUST MATCH FLIGHT START                                  
         SPACE                                                                  
FEST10   CLC   PERST,ESTSTR        PER START MATCH ESTIMATE START               
         BNE   ESTDTERR                                                         
         MVC   PEREND,ESTEND                                                    
         GOTO1 DATCON,DMCB,(3,PERST),(5,TRAPER)                                 
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,PEREND),(5,TRAPER+9)                                
         OI    TRAPERH+6,X'80'                                                  
         B     VPERX                                                            
         EJECT                                                                  
***********************************************************                     
* SUBROUTINE FINDS ANY ESTIMATES FOR GIVEN TELECAST DATES *                     
***********************************************************                     
         SPACE                                                                  
CEST     NTR1                                                                   
         CLI   SVTBPR4,C'Y'        ALLOW WITH NO ESTIMATES                      
         BE    VPERX                                                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         SPACE                                                                  
         MVC   KEY+4(3),QPRD                                                    
         SPACE                                                                  
         MVI   KEY+7,1                                                          
*                                                                               
CEST10   MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   NOESTERR                                                         
         SPACE                                                                  
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   CEST20                                                           
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(3,WORK)                                  
         SPACE                                                                  
         CLC   WORK(3),PEREND      EST START AFTER REQ END                      
         BH    CEST20                                                           
         SPACE                                                                  
         GOTO1 (RF),(R1),(0,EEND),(3,WORK)                                      
         SPACE                                                                  
         CLC   WORK(3),PERST       EST END BEFORE REQ START                     
         BL    CEST20                                                           
         SPACE                                                                  
*  FOUND AT LEAST 1 ESTIMATE COVERING PART OF TELECAST DATES                    
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         B     VPERX                                                            
         SPACE                                                                  
CEST20   MVC   KEY+8(5),=5X'FF'    FORCE TO NEXT EST                            
         B     CEST10                                                           
         DROP  R6                                                               
         SPACE 2                                                                
         EJECT                                                                  
GETEL1   AH    R6,DATADISP                                                      
FIRSTEL1 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL1  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL1                                                         
NOESTERR MVC   CONHEAD,NOESTCMS                                                 
         B     ERREXIT3                                                         
         SPACE                                                                  
ESTDTERR MVC   CONHEAD,ESTDTEMS                                                 
         B     ERREXIT3                                                         
         SPACE                                                                  
BLKESTER MVC   CONHEAD,BLKESTMS                                                 
         SPACE                                                                  
ERREXIT3 GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERRA MVI   ERROR,MISSING                                                    
         B     TRAPERRA                                                         
FLTRECER MVI   ERROR,NOFLTREC                                                   
         B     TRAPERRA                                                         
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     TRAPERRA                                                         
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     TRAPERRA                                                         
DATERRA  MVI   ERROR,INVDATE                                                    
TRAPERRA GOTO1 ERREX                                                            
NOESTCMS DC    CL60'* ERROR * NO ESTIMATES COVERING TELECAST DATES *'           
BLKESTMS DC    CL60'* ERROR * ESTIMATE MUST BE ENTERED FOR PERIOD ES *'         
ESTDTEMS DC    CL60'* ERROR * DATE(S) NOT IN ESTIMATE PERIOD *'                 
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE MARKET/MARKET GROUP *                                                
         SPACE                                                                  
VMG      NMOD1 0,**VMGR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         LA    R2,TRASMKTH                                                      
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VMGX                                                             
         MVI   STMKTSW,0                                                        
         XC    SVSTMKT,SVSTMKT                                                  
         XC    STMKT,STMKT                                                      
         SPACE                                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VMG10                                                            
         SPACE                                                                  
         CLI   8(R2),C'='          1 MARKET ONLY OPTION                         
         BE    VMK                  GO VALIDATE MARKET                          
         CLI   8(R2),C'0'          NUMERIC                                      
         BNL   VMK                  YES                                         
         SPACE                                                                  
         CLI   SVTBPR1,C'G'        USING GOAL AS SOURCE                         
         BE    MGENTER              NO MARKET GROUPS ALLOWED                    
         B     VGR                 GO VALIDATE MARKET GROUP                     
         SPACE                                                                  
VMG10    CLI   SVTBPR1,C'G'        USING GOAL AS SOURCE                         
         BE    VMG14                                                            
         CLI   SVTBPR1,C'L'        USING MARKET LIST ONLY AS SOURCE             
         BE    VMG14                                                            
         TM    PRGSW,X'08'         MARKET GROUP ENTERED AND VALIDATED           
         BZ    NOMGRPER                                                         
VMG14    OI    4(R2),X'20'         VALIDATED                                    
VMGX     XIT1                                                                   
         EJECT                                                                  
* VALIDATE STARTING MARKET *                                                    
         SPACE                                                                  
VMK      CLI   8(R2),C'='          1 MARKET ONLY OPTION                         
         BNE   VMK10                GO VALIDATE MARKET                          
         MVI   STMKTSW,C'='                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1               IF ONLY = SIGN, ERROR                        
         BZ    INVMKTER                                                         
         STC   R1,5(R2)                                                         
         MVC   8(L'TRASMKT,R2),9(R2)                                            
         MVI   TRASMKT+L'TRASMKT-1,0                                            
         MVC   WORK(L'TRASMKT),=6C'0'                                           
         BCTR  R1,0                                                             
         EX    R1,VMKMVN                                                        
         EX    R1,VMKCLC           SEE IF NUMERIC                               
         BNE   INVMKTER                                                         
         OI    4(R2),X'08'         SET ON NUMERIC                               
         SPACE                                                                  
VMK10    GOTO1 VALIMKT                                                          
         MVC   STMKT,BMKT                                                       
         OI    TRASMKTH+4,X'20'    VALIDATED                                    
         OI    TRASMKTH+6,X'80'       SET ON TRANSMIT                           
         CLI   STMKTSW,C'='                                                     
         BE    VMK20                                                            
         XC    TRASMKT,TRASMKT                                                  
         B     VMGX                                                             
VMK20    MVC   SVSTMKT,STMKT                                                    
         MVI   TRASMKT,C'='                                                     
         MVC   TRASMKT+1(4),QMKT                                                
         B     VMGX                                                             
VMKMVN   MVN   WORK(0),8(R2)                                                    
VMKCLC   CLC   WORK(0),8(R2)                                                    
         EJECT                                                                  
* VALIDATE MARKET GROUP *                                                       
         SPACE                                                                  
VGR      CLI   5(R2),2                                                          
         BL    MGRPERR                                                          
         CLI   5(R2),5                                                          
         BH    MGRPERR                                                          
         CLI   8(R2),C'A'                                                       
         BL    MGRPERR                                                          
         CLI   8(R2),C'Z'                                                       
         BH    MGRPERR                                                          
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         LA    R1,9(,R2)                                                        
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
VGR10    CLI   0(R1),C'0'                                                       
         BL    MGRPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    MGRPERR                                                          
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,VGR10                                                         
         SPACE                                                                  
         PACK  WORK(3),DUB(5)                                                   
         MVC   SVPMGRP(1),8(R2)                                                 
         MVC   SVPMGRP+1(4),DUB                                                 
         MVC   SVDMGRP(5),8(R2)                                                 
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+8(1),8(R2)                                                   
         MVC   KEY+9(2),WORK                                                    
         MVC   SVMGRCLT,BCLT                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BE    VGR20                                                            
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLTS                                 
         XC    SVMGRCLT,SVMGRCLT                                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   BDMGRPER                                                         
         SPACE                                                                  
VGR20    XC    CURRKEYS,CURRKEYS   FORCE NEW START                              
         XC    CKMGRMKT,CKMGRMKT                                                
         MVC   SVMGRP,KEY+MKGPMID-MKGPTYP                                       
         SPACE                                                                  
         OI    PRGSW,X'08'         SET ON USING MARKET GROUP                    
         XC    TRASMKT,TRASMKT                                                  
         OI    TRASMKTH+4,X'20'                                                 
         OI    TRASMKTH+6,X'80'                                                 
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         B     VMGX                                                             
         SPACE                                                                  
BDMGRPER LA    R1,BDMGRPMS                                                      
         MVC   16(1,R1),8(R2)                                                   
         UNPK  17(5,R1),WORK(3)                                                 
         LA    R2,TRASMKTH                                                      
         NI    TRAMEDH+4,X'FF'-X'20'                                            
         B     VGRER                                                            
INVMKTER MVI   ERROR,INVMKT        BAD MARKET (NOT NUMERIC)                     
         GOTO1 ERREX                                                            
MGENTER  LA    R1,MGENTMS                                                       
         B     VGRER                                                            
MGRPERR  LA    R1,MGRPERMS                                                      
         B     VGRER                                                            
NOMGRPER LA    R1,NOMGRPMS                                                      
VGRER    MVC   CONHEAD(10),=C'* ERROR * '                                       
         MVC   CONHEAD+10(50),0(R1)                                             
         GOTO1 ERREX2                                                           
BDMGRPMS DC    CL50'NO MARKET GROUP X0000 FOUND *'                              
MGENTMS  DC    CL50'MARKET GROUP NOT NEEDED WITH GOALS AS SOURCE *'             
MGRPERMS DC    CL50'MKT GROUP MUST BE LETTER AND 1-4 DIGITS *'                  
NOMGRPMS DC    CL50'NO MARKET GROUP ENTERED *'                                  
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRBUY                                                        
         EJECT                                                                  
       ++INCLUDE SPTRSTAL                                                       
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT OFF                                                              
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAE6D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR26RR DS    F                                                                
         SPACE                                                                  
* SAVED AREA STARTS HERE FOR 6144 *                                             
         SPACE                                                                  
STACT    DS    H                                                                
BUYCT    DS    H                                                                
STMKT    DS    H                   STARTING MARKET                              
SVMKT    DS    H                                                                
CKMGRMKT DS    H                                                                
         SPACE                                                                  
SVSTMKT  DS    H                   SAVED STARTING MARKET                        
STMKTSW  DS    CL1                 = MEANS ONLY DISPLAY 1 MARKET                
         SPACE                                                                  
OPTIONS  DS    0CL1                                                             
HOLDSIGN DS    CL1                                                              
         SPACE                                                                  
CURRKEYS DS    0CL89                                                            
BKEY     DS    CL18                CURR BUY KEY                                 
BUYMKST  DS    XL5                                                              
BDATES   DS    0XL4                                                             
BST      DS    XL2                                                              
BEND     DS    XL2                                                              
BELEM    DS    XL12                LAST ELEM USED FROM BUY RECD                 
*                                  OR IF USED, NEXT ELEM OF FF IF END           
BELPTR   DS    XL2                                                              
         SPACE                                                                  
SKEY     DS    CL18                CURR STATION LIST                            
SELPTR   DS    XL2                 ELEM PTR WITHIN STATION LIST RECD            
SMKTSTA  DS    XL5                                                              
SDATES   DS    XL4                                                              
         SPACE                                                                  
MKEY     DS    CL13                CURR GOAL, MGRP, OF MKT LIST KEY             
MMKT     DS    XL2                                                              
GOLPER   DS    0XL4                                                             
GOLSTB   DS    XL2                                                              
GOLENDB  DS    XL2                                                              
         SPACE                                                                  
SVMGRP   DS    XL3                                                              
SVPMGRP  DS    CL5                 MGRP WITH TRAILING ZEROS                     
SVDMGRP  DS    CL5                 DISPLAYABLE MGRP AS ENTERED                  
SVMGRCLT DS    XL2                 ZERO IF ALL CLT MGRP                         
         SPACE                                                                  
LINCT    DS    XL1                                                              
CODE     DS    CL1                                                              
         SPACE                                                                  
ESTDTS   DS    0XL6                                                             
ESTSTR   DS    XL3                                                              
ESTEND   DS    XL3                                                              
         SPACE                                                                  
PERDTS   DS   0XL6                                                              
PERST    DS    XL3                                                              
PEREND   DS    XL3                                                              
PERMONST DS    XL3                 PERIOD START (PRECEDING MONDAY)              
PERMOSTB DS    XL2                 STARTING DATE (BACKED UP TO MONDAY)          
PERSTB   DS    XL2                                                              
PERENDB  DS    XL2                 END DATE                                     
LASTKEY  DS    XL13                                                             
ORGPRF13 DS    CL1                                                              
EQVPRD   DS    CL3                                                              
EQVPRD2  DS    CL3                                                              
SVTBPROF DS    CL16                                                             
SVTBPR1  EQU   SVTBPROF+0          G=USE GOALS, M=MARKET GROUP                  
*                                  B=USE MARKET GROUP, THEN GOALS               
*                                  L=USE MARKET LIST ONLY                       
SVTBPR2  EQU   SVTBPROF+1          Y = BLANK SEL = BUY                          
*                                              X = EXCLUDE                      
*                                  N = B = BUY                                  
SVTBPR3  EQU   SVTBPROF+2          Y = ONLY READ CLT SPECIFIC MKTLIST           
SVTBPR4  EQU   SVTBPROF+3          Y = NO ESTIMATES REUIRED FOR TBUYS           
         SPACE                                                                  
PRGSW    DS    XL1                                                              
*                        80 - STATION LIST REC IN AIO2                          
*                        40 - TBUY RECD IN AIO3                                 
*                        20 - CURR STA ENTRY USED                               
*                        10 - CURR TBUY USED                                    
*                        08 - MARKET GROUP ENTERED                              
*                        04 - AT LEAST 1 BUY DISPLAYED                          
*                        02 - MISSING STATION LIST FOR MARKET                   
         SPACE                                                                  
         DS    0D                                                               
STATABL  DS    16XL40                                                           
         SPACE                                                                  
ENDSYSD  EQU   *      IF THIS ADDRESS EXCEEDS 1D2C, PAST END OF                 
*                     SAVED TWA (6144 = X'1800')+X'52C' (ADDR OF STACT)         
         EJECT                                                                  
* DSECT FOR STATION TABLE OF BUYS AND POSSIBLE BUYS *                           
         SPACE                                                                  
STATABLD DSECT                                                                  
STAENT   DS   0XL40                                                             
STALIN   DS    XL1                                                              
STABMKT  DS    XL2                                                              
STABSTA  DS    XL3                                                              
STADSKA  DS    XL4                 DISKADDR (ZERO IF NO BUY YET)                
STAFTDP  DS    XL3                                                              
STALTDP  DS    XL3                                                              
STAFTD   DS    CL8                                                              
STALTD   DS    CL8                                                              
STASTA   DS    CL8                                                              
STANEXT  EQU   *                                                                
         SPACE                                                                  
* SCREEN DISPLAY LINE *                                                         
         SPACE                                                                  
TRASTALN EQU   TRASTA-TRASTAH                                                   
DLINE    DSECT                                                                  
         DS    CL(TRASTALN)                                                     
LBUY1    DS    CL1                                                              
         DS    CL1                                                              
LMKT     DS    CL4                                                              
         DS    CL1                                                              
LMKTNM   DS    CL24                                                             
         DS    CL2                                                              
LBUY2    DS    CL1                                                              
         DS    CL1                                                              
LSTA     DS    CL8                                                              
         DS    CL1                                                              
LBUY3    DS    CL1                                                              
         DS    CL1                                                              
LPERFTD  DS    CL8                                                              
LPERDASH DS    CL1                                                              
LPERLTD  DS    CL8                                                              
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPTRA26   03/06/07'                                      
         END                                                                    
