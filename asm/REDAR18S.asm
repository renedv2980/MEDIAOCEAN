*          DATA SET REDAR18S   AT LEVEL 155 AS OF 07/20/03                      
*PHASE T80F18C                                                                  
         TITLE 'T80F18 - REDAR18 - MAKEGOOD LIST'                               
***********************************************************************         
*                                                                     *         
*  REDAR18 (T80F18) --- MAKEGOOD LIST                                 *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 14MAY03 (SKU) SELF APPLY                                            *         
* 30JAN03 (SKU) FIX STATION CREATION BUG                              *         
* 20AUG02 (SKU) MORE FILTER BUG FIX                                   *         
* 18JUL02 (SKU) TEAM FILTER BUG FIX                                   *         
* 26JUN02 (SKU) DIRECT PROCESSING FILTER                              *         
* 31JAN02 (SKU) SHOW MAKEGOOD IF ERROR OR DDS                         *         
* 26SEP01 (SKU) SHOW ERROR NUMBER ON LIST                             *         
* 04SEP01 (SKU) SKIP MAKEGOOD OFFERS WITH MISSING CONTRACTS           *         
* 29AUG01 (SKU) FIX LISTING BUG                                       *         
* 18AUG00 (SKU) TEMP CODE TO ALLOW BLRSA ACCESS TO SE OFFICE          *         
* 19APR00 (SKU) FIX OFFICE RESTRICTION                                *         
* 24MAR00 (SKU) OPTIMIZE FILTERS                                      *         
* 02JAN00 (SKU) ZERO PAD ORDER NUMBER IN LIST DISPLAY                 *         
* 11FEB99 (BU ) FIX 'DELETED RECORD/NON-DELETED PASSIVE' PROBLEM      *         
* 10FEB99 (BU ) FIX SKIP-READ BUG                                     *         
* 19NOV98 (BU ) FILTER S/P-TEAM + STATION                             *         
* 10NOV98 (SKU) SPECIAL GROUP FILTER SUPPORT                          *         
* 19MAR98 (SKU) TEMP CODE TO ALLOW BLRSA ACCESS TO PO OFFICE          *         
* 04MAR98 (SKU) LIST UNSELECTED CHOICE OFFERS                         *         
* 14AUG97 (SKU) ERROR SUPPORT                                         *         
* 07MAR97 (SKU) SUPPORT VARIOUS/BRAND ORDERS                          *         
* 12DEC96 (SKU) SUPPORT OWNERSHIP CONCEPT BETWEEN REP AND STATION     *         
* 24SEP96 (SKU) ADD OFFICE LIMIT ACCESS                               *         
* 18SEP96 (SKU) FIX OFFICE AND SALESPERSON FILTERS                    *         
* 03JUL96 (SKU) DON'T LIST KATZ/SELTEL EDI ORDERS                     *         
* 27JUN96 (SKU) APPLY BUG FIX                                         *         
* 07MAR95 (SKU) INITIAL RELEASE                                       *         
*                                                                     *         
***********************************************************************         
T80F18   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80F18*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         NI    GLSTSTAT,X'FF'-APPLCDSP  SET GENCON DISPLAY LIST DATA            
         XC    LLIST,LLIST         USE DEFAULT LIST LENGTH                      
         MVI   MYSCRNUM,X'F2'                                                   
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
*                                                                               
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                  VALIDATE STATUS                              
         LA    R2,MKGSTATH                                                      
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK10                                                             
         CLC   =C'NEW',8(R2)       NEW?                                         
         BE    VK10                                                             
         CLC   =C'SEN',8(R2)       SENT?                                        
         BE    VK10                                                             
         CLC   =C'APR',8(R2)       APPROVED?                                    
         BE    VK10                                                             
         CLC   =C'APL',8(R2)       APPLIED?                                     
         BE    VK10                                                             
         CLC   =C'REJ',8(R2)       REJECTED?                                    
         BE    VK10                                                             
         CLC   =C'RES',8(R2)       RESENT?                                      
         BE    VK10                                                             
         CLC   =C'CAN',8(R2)       CANCELLED?                                   
         BE    VK10                                                             
         CLC   =C'ERR',8(R2)       ERROR?                                       
         BE    VK10                                                             
         CLC   =C'REC',8(R2)       RECALLED?                                    
         BE    VK10                                                             
         CLC   =C'SEL',8(R2)       SELF APPLY?                                  
         BE    VK10                                                             
         CLC   =C'MGX',8(R2)       MGXSEL?                                      
         BNE   INVLFLD                                                          
*                                                                               
VK10     DS    0H                  VALIDATE CONTRACT/ORDER NUMBER               
         XC    CONNUM,CONNUM                                                    
         LA    R2,MKGKNUMH                                                      
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK30                                                             
*                                                                               
         GOTO1 SCANNER,DMCB,MKGKNUMH,BLOCK                                      
         CLI   DMCB+4,1            FILTERING ON CONTRACT NUMBER                 
         BNE   INVLFLD             USER TYPED: XXXXXXXX                         
         CLI   BLOCK+1,0                                                        
         BNE   VK20                                                             
         TM    BLOCK+2,X'80'       VALID NUMERIC                                
         BZ    INVLFLD                                                          
         GOTO1 VALICON,DMCB,(R2)                                                
         MVC   CONNUM,CCONNUM                                                   
         B     VK30                                                             
*                                                                               
VK20     DS    0H                  FILTERING ON ORDER NUMBER                    
         CLI   BLOCK,1                                                          
         BNE   INVLFLD                                                          
         CLI   BLOCK+12,C'O'                                                    
         BNE   INVLFLD                                                          
         TM    BLOCK+3,X'80'       VALID NUMERIC                                
         BZ    INVLFLD                                                          
*                                                                               
VK30     DS    0H                  VALIDATE CONTRACT MG STATUS                  
         XC    GRPCODE,GRPCODE                                                  
         LA    R2,MKGKSTAH                                                      
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK40                                                             
         CLI   8(R2),C'#'          SPECIAL IF PRECEED BY A # SIGN               
         BNE   VK35                                                             
         MVC   GRPCODE,9(R2)       NEXT 2 LETTERS ARE THE GROUP CODE            
         XC    MKGKSTA,MKGKSTA                                                  
         MVI   MKGKSTAH+5,0                                                     
         B     VK40                                                             
*                                                                               
VK35     DS    0H                                                               
         CLC   =C'NEW',8(R2)       NEW?                                         
         BE    VK40                                                             
         CLC   =C'SEN',8(R2)       SENT?                                        
         BE    VK40                                                             
         CLC   =C'APL',8(R2)       APPLIED?                                     
         BE    VK40                                                             
         CLC   =C'REJ',8(R2)       REJECTED?                                    
         BE    VK40                                                             
         CLC   =C'RES',8(R2)       RESENT?                                      
         BE    VK40                                                             
         CLC   =C'CAN',8(R2)       CANCELLED?                                   
         BE    VK40                                                             
         CLC   =C'REC',8(R2)       RECALLED?                                    
         BE    VK40                                                             
         CLC   =C'SEL',8(R2)       SELF APPLY?                                  
         BE    VK40                                                             
         CLC   =C'REV',8(R2)       REVISED?                                     
         BNE   INVLFLD                                                          
*                                                                               
VK40     DS    0H                  VALIDATE OFFICE                              
         LA    R2,MKGOFFH                                                       
*                                                                               
* CHECK IF DIRECT PROCESSING. IF SO, MAKEGOOD INBOX WILL USE TWAACCS            
* FOR FILTERING ON OFFICE IF TWAACCS STARTS WITH "O="                           
*                                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    VK45                                                             
         DROP  R4                                                               
*                                                                               
         CLC   =C'O=',TWAACCS                                                   
         BNE   VK45                                                             
         XC    MKGOFF,MKGOFF                                                    
         MVC   MKGOFF,TWAACCS+2                                                 
         MVI   MKGOFFH+5,2                                                      
         NI    MKGOFFH+4,X'FF'-X'20'                                            
         OI    MKGOFFH+6,X'80'                                                  
         B     VK70                                                             
*                                                                               
VK45     DS    0H                                                               
         OC    MKGOFF,MKGOFF                                                    
         BNZ   VK48                                                             
         XC    OFFFILT,OFFFILT                                                  
*                                                                               
VK48     DS    0H                                                               
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK80                                                             
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VK60                                                             
         CLC   =X'800F',TWAAUTH    CHECK ACCESS                                 
         BE    VK60                                                             
         CLC   =C'O=',TWAACCS                                                   
         BNE   VK60                                                             
         CLI   5(R2),0             DEFAULT OFFICE RESTRICTION                   
         BNE   VK50                                                             
         MVC   MKGOFF,TWAACCS+2                                                 
         MVI   MKGOFFH+5,2                                                      
         B     VK70                                                             
*                                                                               
VK50     DS    0H                                                               
*********                                                                       
         CLC   =C'BL',AGENCY                                                    
         BNE   VK55                                                             
         CLC   =C'SA',TWAACCS+2                                                 
         BNE   VK55                                                             
         CLC   =C'PO',MKGOFF                                                    
         BE    VK70                                                             
         CLC   =C'SE',MKGOFF                                                    
         BE    VK70                                                             
VK55     DS    0H                                                               
*********                                                                       
         CLC   MKGOFF,TWAACCS+2                                                 
         BNE   NOACCESS                                                         
*                                                                               
VK60     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VK80                                                             
*                                                                               
VK70     DS    0H                                                               
         GOTO1 VALIOFF                                                          
         BNE   INVLOFF                                                          
         MVC   OFFFILT,8(R2)                                                    
*                                                                               
VK80     DS    0H                  VALIDATE STATION                             
         LA    R2,MKGSTAH                                                       
*                                                                               
* CHECK IF DIRECT PROCESSING. IF SO, MAKEGOOD INBOX WILL USE SIGNON ID          
* FOR FILTERING ON STATION. SIGONID WILL BE USED ONLY IF IT IS 4                
* CHARACTERS OR LESS (STATION CALL LETTER)                                      
*                                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    VK83                                                             
         DROP  R4                                                               
*                                                                               
         CLI   SIGNONID+4,C' '     MUST BE 4-CHARS OR LESS                      
         BNE   VK83                                                             
         XC    MKGSTA,MKGSTA                                                    
         MVC   MKGSTA(4),SIGNONID                                               
         MVI   MKGSTAH+5,4                                                      
         NI    MKGSTAH+4,X'FF'-X'20'                                            
         OI    MKGSTAH+6,X'80'                                                  
         B     VK85                                                             
*                                                                               
VK83     DS    0H                                                               
         CLI   5(R2),0             STATION FILTER IS OPTIONAL                   
         BNE   VK85                                                             
         XC    STANFILT,STANFILT                                                
         B     VK90                                                             
VK85     TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK90                                                             
         GOTO1 VALISTA                                                          
         MVC   STANFILT,WORK                                                    
         OC    STANFILT,SPACES                                                  
*                                                                               
VK90     DS    0H                                                               
         XC    SALFILT,SALFILT     CLEAR SALESPERSON FILTER                     
         LA    R2,MKGSALH                                                       
         CLI   5(R2),0             SALESPERSON FILTER IS OPTIONAL               
         BE    VK120                                                            
*        TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
*        BO    VK120                                                            
*                                                                               
*   DUE TO NEW PASSIVE KEY STRUCTURE, SALESPERSON NO LONGER                     
*        REQUIRES OFFICE FILTER TO LIMIT I/O                                    
*                                                                               
***      CLI   MKGOFFH+5,0         SALESPERSON FILTER NEEDS OFFICE              
***      BNE   VK100                                                            
***      LA    R2,MKGOFFH                                                       
***      B     NEEDOFF                                                          
*                                                                               
VK100    DS    0H                                                               
         GOTO1 VALISAL             REVALIDATE EVERYTIME                         
         BNE   INVLSAL                                                          
***      CLC   OFFFILT,WORK+20     SALESPERSON OFFICE MUST MATCH OFFICE         
***      BNE   INVLSAL             FILTER                                       
         XC    SALFILT,SALFILT                                                  
         MVC   SALFILT,8(R2)                                                    
         OC    SALFILT,SPACES                                                   
*                                                                               
*                                                                               
VK120    DS    0H                                                               
         XC    TEAMFILT,TEAMFILT   CLEAR S/P TEAM    FILTER                     
         LA    R2,MKGTEMH                                                       
         CLI   5(R2),0             S/P TEAM FILTER IS OPTIONAL                  
         BE    VKX                                                              
*        TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
*        BO    VKX                                                              
*                                                                               
VK140    DS    0H                                                               
         GOTO1 VALITEAM            REVALIDATE EVERYTIME                         
         BNE   INVLTEAM                                                         
         XC    TEAMFILT,TEAMFILT                                                
         MVC   TEAMFILT,8(R2)                                                   
         OC    TEAMFILT,SPACES                                                  
*                                                                               
VKX      DS    0H                  SET PREVAL BITS AND EXIT                     
         OI    MKGSTATH+4,X'20'                                                 
         OI    MKGKNUMH+4,X'20'                                                 
         OI    MKGOFFH+4,X'20'                                                  
         OI    MKGSTAH+4,X'20'                                                  
         OI    MKGSALH+4,X'20'                                                  
         OI    MKGTEMH+4,X'20'                                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
*                                                                               
* FOR FILTERS ON STATUS, NEED TO KEEP THE CON#/AGENCY# OFF ETC                  
*  FOR DISPLAY                                                                  
*                                                                               
*                                                                               
***********************************************************************         
LR       DS    0H                                                               
         XC    FILTRKEY,FILTRKEY                                                
         TM    MGBITFLG,BFFORCEC                                                
         BZ    LIRE0020                                                         
         LH    R2,SVCRDISP                                                      
         AR    R2,RA                                                            
         OI    6(R2),X'40'         FORCE CURSOR HERE                            
         NI    MGBITFLG,X'FF'-BFFORCEC                                          
*                                                                               
LIRE0020 DS    0H                                                               
         OI    MGBITFLG,BFGETFKY   SET TO GET FIRST LIST KEY                    
         TM    MGBITFLG,BFFRSTKY   USE FIRST KEY TO START LIST?                 
         BZ    LIRE0040                                                         
         MVC   KEY(L'MGFRTKEY),MGFRTKEY                                         
         NI    MGBITFLG,X'FF'-BFFRSTKY                                          
*                                                                               
*   TEST                                                                        
         LTR   RF,RF                                                            
*   TEST END                                                                    
*                                                                               
LIRE0040 DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
         MVI   SPEMFILT,C'N'       SET 'S/P-TEAM KEYS' TO NO                    
         MVI   A0ORA1,0            CLEAR KEY INDICATOR                          
         OC    SALFILT(5),SALFILT  S/P-TEAM FILTERS ENTERED?                    
         BZ    LIRE0120            NO                                           
         MVI   SPEMFILT,C'Y'       SET 'S/P-TEAM KEYS' TO YES                   
         OC    KEY(L'RMKGKEY),KEY  FIRST TIME THRU?                             
         BZ    LIRE0060            YES                                          
         TM    MGBITFLG,BFPASSKY   USE FIRST KEY TO START LIST?                 
         BO    LIRE0055            YES - USE FIRST KEY                          
         MVC   KEY(27),SALTMKEY    NO  - RESET START KEY                        
         LA    RF,KEY              BUMP TO NEXT KEY                             
         USING RMKG2TYP,RF                                                      
         CLI   RMKG2GR2,C'Z'                                                    
         BL    LIRE0050                                                         
*                                  LETTER Z REACHED IN SECOND GROUP             
         ZIC   RE,RMKG2GR1         CODE. BUMP FIRST GROUP LETTER CODE           
         LA    RE,1(RE)                                                         
         STC   RE,RMKG2GR1                                                      
         MVI   RMKG2GR2,C'A'                                                    
         B     LIRE0055                                                         
*                                                                               
LIRE0050 DS    0H                                                               
         ZIC   RE,RMKG2GR2         GET NEXT GROUP                               
         LA    RE,1(RE)                                                         
         STC   RE,RMKG2GR2                                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
LIRE0055 DS    0H                                                               
         NI    MGBITFLG,X'FF'-BFPASSKY                                          
         MVC   A0ORA1,KEY          RESET A0/A1 FLAG                             
         B     LIRE0105                                                         
SPMGKEYD USING RMKG2TYP,KEY                                                     
***>>>                                                                          
LIRE0060 EQU   *                                                                
         XC    AXSPTMKY,AXSPTMKY   CLEAR SETUP KEY                              
         MVI   A0ORA1,X'A0'        SET TO S/P KEY                               
         MVC   AXSPTMKY(5),SALFILT                                              
*                                  PRELOAD S/P+TEAM FILTERS                     
*                                     (TEAM MAY NOT BE PRESENT)                 
         OC    SALFILT(3),SALFILT  S/P FILTER ENTERED?                          
         BNZ   LIRE0080            YES                                          
         MVI   A0ORA1,X'A1'        NO  - ONLY TEAM ENTERED                      
         MVC   AXTEMKEY,TEAMFILT                                                
*                                  PRELOAD TEAM FILTER ONLY                     
LIRE0080 EQU   *                                                                
**       OC    KEY(L'RMKGKEY),KEY  FIRST TIME THRU?                             
**       BZ    LIRE0100            NO, BUT CLEAR TO GET HEADER                  
LIRE0100 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   SPMGKEYD.RMKG2TYP(1),A0ORA1                                      
*                                  INSERT KEY TYPE: A0 OR A1                    
         MVI   SPMGKEYD.RMKG2TYP+1,X'11'                                        
*                                  INSERT SECOND PART OF KEY                    
         MVC   SPMGKEYD.RMKG2REP,AGENCY                                         
*                                  INSERT REP CODE INTO KEY                     
         MVC   SPMGKEYD.RMKG2SP(5),AXSPTMKY                                     
*                                  INSERT PRELOADED KEY FOR A0 OR A1            
LIRE0105 DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   SPMGKEYD.RMKG2TYP(1),A0ORA1                                      
         BNE   LIRE1000                                                         
         CLC   SPMGKEYD.RMKG2REP,AGENCY                                         
         BNE   LIRE1000                                                         
         CLI   A0ORA1,X'A0'        S/P-TEAM SEQUENCE?                           
         BNE   LIRE0110            NO  - CHECK TEAM SEQUENCE                    
*                                  YES - CHECK S/P, POSSIBLE TEAM               
         CLC   SPMGKEYD.RMKG2SP,AXSPTMKY                                        
*                                  SAME S/P FOUND?                              
         BNE   LIRE1000            NO  - FINISHED                               
*                                  SAVE KEY FOR RESTART                         
         OC    AXSPTMKY+3(2),AXSPTMKY+3                                         
*                                  YES - ANY TEAM ENTERED?                      
         BZ    LIRE0107            NO  - ACCEPTED                               
         CLC   SPMGKEYD.RMKG2TEM,AXSPTMKY+3                                     
*                                  YES - SAME TEAM?                             
         BNE   LIRE1000            NO  - FINISHED                               
LIRE0107 EQU   *                                                                
         CLI   MKGSTAH+5,0         STATION FILTER ENTERED?                      
         BE    LIRE0108            NO                                           
         CLC   STANFILT,SPMGKEYD.RMKG2STA                                       
*                                  YES - FILTER = KEY STATION?                  
         BNE   LIRE0780            NO  - SKIP ORDER                             
*                                                                               
LIRE0108 EQU   *                                                                
         MVC   SALTMKEY,SPMGKEYD.RMKG2TYP                                       
         B     LIRE0180                                                         
LIRE0110 EQU   *                                                                
*                                  TEAM IS IN HIGH POSITION IN KEY              
*                                                                               
         CLC   SPMGKEYD.RMKG2SP(2),AXTEMKEY                                     
*                                  SAME TEAM?                                   
         BNE   LIRE1000            NO  - FINISHED                               
         CLI   MKGSTAH+5,0         STATION FILTER ENTERED?                      
         BE    LIRE0115            NO                                           
         CLC   STANFILT,SPMGKEYD.RMKG2STA                                       
*                                  YES - FILTER = KEY STATION?                  
         BNE   LIRE0780            NO  - SKIP ORDER                             
*                                                                               
LIRE0115 EQU   *                                                                
         MVC   SALTMKEY,SPMGKEYD.RMKG2TYP                                       
         B     LIRE0180                                                         
*                                                                               
***>>>                                                                          
                                                                                
*                                                                               
LIRE0120 EQU   *                                                                
         OC    KEY(L'RMKGKEY),KEY  FIRST TIME THRU?                             
         BZ    LIRE0140            NO, BUT CLEAR TO GET HEADER                  
MGKEYD   USING RMKGKEY,KEY                                                      
         XC    MGKEYD.RMKGKPLN(6),MGKEYD.RMKGKPLN                               
         B     LIRE0160                                                         
*                                                                               
LIRE0140 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   MGKEYD.RMKGKTYP,X'11'                                            
         MVC   MGKEYD.RMKGKREP,AGENCY                                           
         MVC   MGKEYD.RMKGKOFF,OFFFILT                                          
         MVC   MGKEYD.RMKGKSTA,STANFILT                                         
*                                                                               
         OC    CONNUM,CONNUM                                                    
         BZ    LIRE0160                                                         
*                                                                               
         MVC   OFFFILT,CCONKOFF                                                 
         MVC   STANFILT,CCONKSTA                                                
         MVC   MGKEYD.RMKGKOFF,CCONKOFF                                         
         MVC   MGKEYD.RMKGKSTA,CCONKSTA                                         
         PACK  WORK(1),CONNUM+3(1) REVERSE THE COMPLIMENT                       
         PACK  WORK+1(1),CONNUM+2(1)                                            
         PACK  WORK+2(1),CONNUM+1(1)                                            
         PACK  WORK+3(1),CONNUM(1)                                              
         MVC   MGKEYD.RMKGKCON,WORK                                             
*                                                                               
         OC    GRPCODE,GRPCODE                                                  
         BZ    LIRE0160                                                         
         MVC   MGKEYD.RMKGKGRP,GRPCODE                                          
*                                                                               
LIRE0160 DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LIRE0165 DS    0H                                                               
         CLI   MGKEYD.RMKGKTYP,X'11'                                            
         BNE   LIRE1000                                                         
         CLC   MGKEYD.RMKGKREP,AGENCY                                           
         BNE   LIRE1000                                                         
         OC    OFFFILT,OFFFILT                                                  
         BZ    LIRE0170                                                         
         CLC   MGKEYD.RMKGKOFF,OFFFILT                                          
         BNE   LIRE1000                                                         
*                                                                               
LIRE0170 DS    0H                                                               
         OC    STANFILT,STANFILT                                                
         BZ    LIRE0175                                                         
         CLC   MGKEYD.RMKGKSTA,STANFILT                                         
         BE    LIRE0175                                                         
*                                                                               
         OC    OFFFILT,OFFFILT                                                  
         BNZ   LIRE1000                                                         
         B     LIRE0174                                                         
*                                                                               
LIRE0173 DS    0H                                                               
         MVC   MGKEYD.RMKGKSTA,STANFILT                                         
         XC    MGKEYD.RMKGKCON(12),MGKEYD.RMKGKCON                              
*                                                                               
         GOTO1 HIGH                                                             
         CLI   MGKEYD.RMKGKTYP,X'11'                                            
         BNE   LIRE1000                                                         
         CLC   MGKEYD.RMKGKREP,AGENCY                                           
         BNE   LIRE1000                                                         
         CLC   MGKEYD.RMKGKSTA,STANFILT                                         
         BE    LIRE0175            SKIP TO NEXT OFFICE                          
*                                                                               
LIRE0174 DS    0H                                                               
         CLC   MGKEYD.RMKGKOFF,KEYSAVE+RMKGKOFF-RMKGKEY                         
         BNE   LIRE0173                                                         
         ZIC   RE,MGKEYD.RMKGKOFF+1                                             
         AHI   RE,1                                                             
         STC   RE,MGKEYD.RMKGKOFF+1                                             
         B     LIRE0173            SKIP TO NEXT OFFICE                          
*                                                                               
LIRE0175 DS    0H                                                               
         OC    MGKEYD.RMKGKPLN(6),MGKEYD.RMKGKPLN                               
         BNZ   LIRE0740                                                         
*                                                                               
LIRE0180 DS    0H                                                               
         TM    MGBITFLG,BFGETFKY   SAVE IN CASE OF SELECTION                    
         BZ    LIRE0200                                                         
         MVC   MGFRTKEY,KEY                                                     
         NI    MGBITFLG,X'FF'-BFGETFKY                                          
         XC    KEYSAVE,KEYSAVE                                                  
*                                                                               
LIRE0200 DS    0H                                                               
         MVC   MGKEY,KEY                                                        
*                                                                               
*        BAS   RE,CHKCHOIZ         IF CHOICE, CHECK IF AN OFFER WAS             
*        BNZ   LIRE0740            SELECTED                                     
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08' RESET BITS                                   
*                                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
*                                                                               
         TM    RMKGCNTL,X'80'      RECORD DELETED?                              
         BO    LIRE0740            YES - SKIP IT                                
*                                                                               
         OC    RMKGDARN,RMKGDARN   FOR DARE ORDERS ONLY                         
         BZ    LIRE0740                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY DARE STATUS                                                           
***********************************************************************         
         LA    R3,STATTAB                                                       
*                                                                               
LIRE0220 DS    0H                                                               
         CLC   RMKGSFG1,0(R3)      CHECK STATUS BIT                             
         BE    LIRE0240                                                         
         LA    R3,L'STATTAB(R3)                                                 
         CLI   0(R3),X'FF'                                                      
         BE    LIRE0280            IF STATUS NOT FOUND, DON'T DIE ON            
         B     LIRE0220            LIST. JUST DISPLAY ORDER!                    
*                                                                               
LIRE0240 DS    0H                  CHECK STATUS AGAINST FILTER REQUEST          
         CLI   MKGSTATH+5,0                                                     
         BE    LIRE0260                                                         
         CLC   =C'SEL',MKGSTAT                                                  
         BE    LIRE0245                                                         
         CLC   =C'MGX',MKGSTAT                                                  
         BNE   LIRE0250                                                         
*                                                                               
LIRE0245 DS    0H                                                               
         TM    RMKGSFG3,RMGF3SAQ   CHECK IF SELF APPLIED                        
         BZ    LIRE0740            YES, BUT                                     
         TM    RMKGSFG3,RMGF3ARQ   SKIP IF APPROVAL RECEIVED                    
         BO    LIRE0740                                                         
         AHI   R3,L'STATTAB                                                     
         CLC   MKGSTAT(3),1(R3)                                                 
         BE    LIRE0265                                                         
         B     LIRE0740            NO, GET NEXT GROUP                           
*                                                                               
LIRE0250 DS    0H                                                               
         CLC   MKGSTAT(3),1(R3)                                                 
         BNE   LIRE0740            NO, GET NEXT GROUP                           
*                                                                               
LIRE0260 DS    0H                                                               
         TM    RMKGSFG3,RMGF3SAQ   CHECK IF SELF APPLIED                        
         BZ    LIRE0265            YES, BUT                                     
         TM    RMKGSFG3,RMGF3ARQ   SKIP IF APPROVAL RECEIVED                    
         BO    LIRE0265                                                         
         AHI   R3,L'STATTAB                                                     
*                                                                               
         CLC   =C'MGXSEL',4(R3)    DON'T SHOW MGXSEL UNLESS FILTERED ON         
         BNE   LIRE0265                                                         
         CLI   MKGSTATH+5,0                                                     
         BE    LIRE0740                                                         
*                                                                               
LIRE0265 DS    0H                                                               
         MVC   LSTSTAT,4(R3)       YES, DISPLAY STATUS                          
*                                                                               
         CLC   =C'ERR',LSTSTAT                                                  
         BNE   LIRE0270                                                         
         BAS   RE,SHOWERR#                                                      
*                                                                               
LIRE0270 DS    0H                                                               
         MVC   LSTBYWHO,13(R3)     BY WHOM                                      
*                                                                               
         CLC   =C'NEW',LSTSTAT     UPDATED BY REP OR STATION?                   
         BNE   LIRE0280                                                         
         TM    RMKGSFG2,RMGF2RPQ                                                
         BO    LIRE0280                                                         
         MVC   LSTBYWHO,=C'STA'                                                 
         EJECT                                                                  
***********************************************************************         
* DISPLAY CONTRACT STATUS                                                       
***********************************************************************         
LIRE0280 DS    0H                                                               
         TM    RMKGSCST,RMKGSAPQ   APPLIED?                                     
         BO    LIRE0300                                                         
         TM    RMKGSCST,RMKGSCNQ   CANCELLED?                                   
         BO    LIRE0320                                                         
         TM    RMKGSCST,RMKGSRCQ   RECALLED?                                    
         BO    LIRE0340                                                         
         TM    RMKGSCST,RMKGSRJQ   REJECTED?                                    
         BO    LIRE0360                                                         
         TM    RMKGSCST,RMKGSRVQ   REVISED?                                     
         BO    LIRE0380                                                         
         B     LIRE0400            MUST BE NEW                                  
***********************************************************************         
* STATUS: APPLIED BY REP                                                        
***********************************************************************         
LIRE0300 DS    0H                                                               
         MVC   LSTCSTAT,=C'APPLIED  '                                           
         MVC   LSTCWHO,=C'REP'     BY REP ALWAYS                                
         CLI   MKGKSTAH+5,0                                                     
         BE    LIRE0440                                                         
         CLC   =C'APL',MKGKSTA     FILTER ON APPLIED SHOULD CHECK FOR           
         BE    LIRE0440            'APL'                                        
         CLC   =C'SEL',MKGKSTA     SELF APPLY FILTERING?                        
         BNE   LIRE0740                                                         
         TM    RMKGSFG3,RMGF3SAQ                                                
         BZ    LIRE0740                                                         
         TM    RMKGSFG3,RMGF3ARQ                                                
         BO    LIRE0740                                                         
         B     LIRE0440                                                         
***********************************************************************         
* STATUS: CANCELLED BY REP/STA                                                  
***********************************************************************         
LIRE0320 DS    0H                                                               
         MVC   LSTCSTAT,=C'CANCELLED'                                           
         MVC   LSTCWHO,=C'REP'    BY REP OR STA                                 
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    LIRE0420                                                         
         MVC   LSTCWHO,=C'STA'                                                  
         B     LIRE0420                                                         
***********************************************************************         
* STATUS: RECALLED BY REP/STA                                                   
***********************************************************************         
LIRE0340 DS    0H                                                               
         MVC   LSTCSTAT,=C'RECALLED '                                           
         MVC   LSTCWHO,=C'REP'    BY REP OR STA                                 
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    LIRE0420                                                         
         MVC   LSTCWHO,=C'STA'                                                  
         B     LIRE0420                                                         
***********************************************************************         
* STATUS: REJECTED BY REP/STA                                                   
***********************************************************************         
LIRE0360 DS    0H                                                               
         MVC   LSTCSTAT,=C'REJECTED '                                           
         MVC   LSTCWHO,=C'STA'    BY REP OR STA                                 
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    LIRE0420                                                         
         MVC   LSTCWHO,=C'REP'                                                  
         B     LIRE0420                                                         
***********************************************************************         
* STATUS: REVISED BY REP/STA                                                    
***********************************************************************         
LIRE0380 DS    0H                                                               
         MVC   LSTCSTAT,=C'REVISED  '                                           
         MVC   LSTCWHO,=C'REP'     BY REP OR STA                                
         TM    RMKGSFG2,RMGF2RPQ                                                
         BO    *+10                                                             
         MVC   LSTCWHO,=C'STA'                                                  
         B     LIRE0420                                                         
***********************************************************************         
* STATUS: NEW BY REP/STA                                                        
***********************************************************************         
LIRE0400 MVC   LSTCSTAT,=C'NEW      '                                           
         MVC   LSTCWHO,=C'REP'     BY REP OR STA                                
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    *+10                                                             
         MVC   LSTCWHO,=C'STA'                                                  
*                                                                               
* FILTER: ON CONTRACT MG STATUS                                                 
*                                                                               
LIRE0420 DS    0H                                                               
         CLI   MKGKSTAH+5,0                                                     
         BE    LIRE0440                                                         
         CLC   MKGKSTA,LSTCSTAT                                                 
         BNE   LIRE0740                                                         
*                                                                               
* FILTER: GROUP CODE                                                            
*                                                                               
LIRE0440 DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
*                                                                               
         MVC   LSTGPCD,RMKGKGRP                                                 
*                                                                               
* FILTER: CONTRACT NUMBER                                                       
*                                                                               
LIRE0460 DS    0H                  IF SAME CONTRACT/ORDER, SKIP READS           
         CLI   SPEMFILT,C'Y'       S/P+TEAM FILTERING?                          
         BNE   LIRE0462            NO                                           
         CLC   KEY+RMKG2CON-RMKG2TYP(4),FILTRKEY+RMKG2CON-RMKG2TYP              
*:::SAME CONTRACT NUMBER?                                                       
         BNE   LIRE0470            NO                                           
         B     LIRE0465                                                         
FLKEYD   USING RMKGKEY,FILTRKEY                                                 
*                                                                               
LIRE0462 DS    0H                  IF SAME CONTRACT/ORDER, SKIP READS           
         CLC   MGKEYD.RMKGKCON,FLKEYD.RMKGKCON                                  
         BNE   LIRE0470                                                         
*                                                                               
         DROP  FLKEYD                                                           
*                                                                               
LIRE0465 EQU   *                                                                
**       MVC   LSTCON#,SVLSTCON    INSERT LAST CONTRACT #                       
****>>>  MVC   LSTCON#,=C'@@@@'    INSERT LAST CONTRACT #                       
         B     LIRE0620                                                         
LIRE0470 EQU   *                                                                
         MVC   FILTRKEY,KEY                                                     
*                                                                               
         ZAP   MYWORK(5),=P'0'                                                  
         PACK  MYWORK(1),RMKGKCON+3(1)                                          
         PACK  MYWORK+1(1),RMKGKCON+2(1)                                        
         PACK  MYWORK+2(1),RMKGKCON+1(1)                                        
         PACK  MYWORK+3(1),RMKGKCON(1)                                          
         MVC   CCONNUM,MYWORK                                                   
         ZAP   MYWORK+5(5),=P'0'                                                
         MVO   MYWORK+5(5),MYWORK(4)                                            
         ZAP   MYWORK(5),=P'99999999'                                           
         SP    MYWORK(5),MYWORK+5(5)                                            
         EDIT  (P5,MYWORK),(8,LSTCON#),ALIGN=LEFT                               
         MVC   SVLSTCON,LSTCON#                                                 
                                                                                
         CLI   MKGKNUMH+5,0                                                     
         BE    LIRE0480                                                         
         CLC   =C'O=',MKGKNUM      FILTER ON ORDER OR CONTRACT?                 
         BE    LIRE0480                                                         
         ZIC   R1,MKGKNUMH+5       FILTER BY CONTRACT!                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   MKGKNUM(0),LSTCON#  MATCH ON VARIABLE LENGTH                     
         BNE   LIRE0780                                                         
*                                                                               
* FILTER: ORDER NUMBER                                                          
*                                                                               
LIRE0480 DS    0H                                                               
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),RMKGDARN                                               
         EDIT  (P5,MYWORK),(8,LSTAGY#),FILL=0                                   
         CLI   MKGKNUMH+5,0                                                     
         BE    LIRE0500                                                         
         CLC   =C'O=',MKGKNUM      FILTER ON ORDER?                             
         BNE   LIRE0500                                                         
         ZIC   R1,MKGKNUMH+5                                                    
         SH    R1,=H'3'                                                         
         BM    LIRE0500                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   MKGKNUM+2(0),LSTAGY#  MATCH ON VARIABLE LENGTH                   
         BNE   LIRE0780                                                         
*                                                                               
* FILTER: OFFICE                                                                
*                                                                               
LIRE0500 DS    0H                                                               
         MVC   LSTOFF,RMKGKOFF                                                  
         CLI   MKGOFFH+5,0                                                      
         BE    LIRE0520                                                         
         CLC   OFFFILT,RMKGKOFF                                                 
         BNE   LIRE0780                                                         
* FILTER: STATION                                                               
*                                                                               
LIRE0520 DS    0H                                                               
         MVC   LSTSTA(4),RMKGKSTA                                               
         CLI   RMKGKSTA+4,C' '                                                  
         BE    LIRE0540                                                         
         MVI   LSTSTA+4,C'-'                                                    
         MVC   LSTSTA+5(1),RMKGKSTA+4 BAND                                      
*                                                                               
LIRE0540 DS    0H                                                               
         CLI   MKGSTAH+5,0                                                      
         BE    LIRE0560                                                         
         CLC   STANFILT,RMKGKSTA                                                
         BNE   LIRE0780                                                         
*                                                                               
LIRE0560 DS    0H                                                               
         MVC   MGKEY,RMKGKEY       RELOAD X'11' KEY SEQUENCE                    
         CLI   SPEMFILT,C'Y'       S/P+TM FILTERING?                            
         BNE   LIRE0570            NO                                           
         MVC   MGKEY,SALTMKEY      RELOAD X'A011'/X'A111' SEQUENCING            
LIRE0570 DS    0H                                                               
         DROP  R6                                                               
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    LIRE0580                                                         
         MVC   KEY,MGKEY           SKIP IF MISSING CONTRACT                     
         B     LIRE0780                                                         
*        MVC   LSTCON#,=C'*MISSING'                                             
*        B     LIRE0660                                                         
*                                                                               
LIRE0580 DS    0H                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2                                                          
         USING RCONREC,R6                                                       
         MVC   LSTSAL,RCONSAL                                                   
*                                                                               
* SALESPERSON                                                                   
*                                                                               
         CLI   MKGSALH+5,0                                                      
         BE    LIRE0600                                                         
         CLC   SALFILT,RCONSAL                                                  
         BE    LIRE0600                                                         
         MVC   KEY,MGKEY                                                        
         B     LIRE0780                                                         
         DROP  R6                                                               
*                                                                               
LIRE0600 DS    0H                                                               
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   LIRE0620            SHOULDN'T HAPPEN BUT DON'T DIE               
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'04'+X'02'                                             
         BZ    LIRE0620            SKIP LIST OF EDI ORDERS                      
         MVC   KEY,MGKEY                                                        
         B     LIRE0780                                                         
         DROP  R6                                                               
*                                                                               
* APPLIED ORDERS SHOULD BE HIDDEN IF THEY HAVE DIFFERENT MOD NUMBER             
* THAN THE CONTRACTS                                                            
*                                                                               
LIRE0620 DS    0H                                                               
         L     R4,AIO                                                           
         USING RMKGREC,R4                                                       
         TM    RMKGSCST,RMKGSAPQ   APPLIED ORDERS ONLY                          
         BZ    LIRE0640                                                         
         TM    RMKGSFG3,RMGF3SAQ   SELF-APPLY FLAG ON?                          
         BZ    LIRE0630                                                         
         TM    RMKGSFG3,RMGF3ARQ   APPROVAL RECEIVED?                           
         BZ    LIRE0640                                                         
*                                                                               
* APPLIED ORDERS WILL NOT SHOW IF APPLIED OUTSIDE OF DARE                       
*                                                                               
LIRE0630 DS    0H                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    LIRE0660                                                         
*                                                                               
* SHOW ORDER IF THERE IS AN ERROR WITH APPLY                                    
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    LIRE0640            SHOW EVERYTHING ALL THE TIME                 
         L     R6,AIO2                                                          
         USING RCONREC,R6                                                       
         CLC   RCONMOD,RMKGAPMN                                                 
         BH    LIRE0660                                                         
         DROP  R6                                                               
*                                                                               
LIRE0640 DS    0H                                                               
         TM    RMKGSCST,RMKGSAPQ   APPLIED ORDERS ONLY                          
         BZ    LIRE0645                                                         
         TM    RMKGSFG3,RMGF3SAQ                                                
         BZ    LIRE0645                                                         
         TM    RMKGSFG3,RMGF3ARQ                                                
         BO    LIRE0645                                                         
         MVC   LSTCSTAT,=C'SELFAPP  '                                           
         L     R6,AIO2                                                          
         USING RCONREC,R6                                                       
         CLC   RMKGAPMN,RCONMOD                                                 
         BE    LIRE0645                                                         
         MVI   LSTCSTAT+7,C'*'                                                  
         DROP  R6                                                               
*                                                                               
*                                                                               
* FOR ANY STATUS, IF STATION IS WORKING ON OFFER, DON'T SHOW IN LIST            
* IF STATION HASN'T SENT(OR MGS) IT BACK TO THE REP                             
*                                                                               
LIRE0645 DS    0H                  STATION IN WIP?                              
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    LIRE0660                                                         
*                                                                               
         L     R6,AIO2             IF STATION CREATED, CHECK IF                 
         MVI   ELCODE,X'21'        STATION MGS THE OFFER TO REP                 
         BAS   RE,GETEL                                                         
         BNE   LIRE0680                                                         
         USING RCONMGEL,R6                                                      
*                                                                               
         TM    RMKGSCST,RMKGSCRQ                                                
         BNZ   LIRE0680            REP IS OFFERER                               
*                                                                               
         CLC   RMKGSCRD,RCONMGDT   IF CREATION DATE/TIME IS LATER               
         BH    LIRE0650            THAN LAST STATION MGS DATE/TIME              
         BL    LIRE0680            DON'T SHOW IN LIST                           
         CLC   RMKGSCRT,RCONMGTM                                                
         BL    LIRE0680                                                         
         DROP  R6                                                               
*                                                                               
LIRE0650 DS    0H                                                               
         L     R6,AIO2             IF STATION CREATED, CHECK IF                 
         MVI   ELCODE,X'20'        STATION SENT THE CONTRACT TO REP             
         BAS   RE,GETEL                                                         
         BNE   LIRE0660                                                         
         USING RCONSEND,R6                                                      
*                                                                               
         CLC   RMKGSCRD,RCONSSDT   IF CREATION DATE/TIME IS LATER               
         BH    LIRE0660            THAN LAST STATION SEND DATE/TIME             
         BL    LIRE0680            DON'T SHOW IN LIST                           
         GOTO1 HEXIN,DMCB,RCONSSTI,WORK,6                                       
         CLC   RMKGSCRT,WORK                                                    
         BL    LIRE0680                                                         
         DROP  R6,R4                                                            
*                                                                               
LIRE0660 DS    0H                  RESTORE MAKEGOOD KEY                         
         MVC   KEY,MGKEY                                                        
         B     LIRE0740                                                         
*                                                                               
* FILTER: SALESPERSON                                                           
*                                                                               
LIRE0680 DS    0H                                                               
FLKEYD   USING RMKGKEY,FILTRKEY                                                 
         CLC   MGKEYD.RMKGKCON,FLKEYD.RMKGKCON                                  
         BE    LIRE0720                                                         
*                                                                               
         DROP  FLKEYD                                                           
*                                                                               
LIRE0700 DS    0H                  RE-ESTABLISH SEQ ORDER                       
         MVC   KEY,MGKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
LIRE0720 DS    0H                                                               
         GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
                                                                                
LIRE0740 DS    0H                  GET NEXT GROUP                               
         CLI   SPEMFILT,C'Y'       S/P OR TEAM FILTERING?                       
         BE    LIRE0770            YES                                          
         LA    RF,KEY                                                           
         USING RMKGKEY,RF                                                       
*                                                                               
         XC    RMKGKPLN(6),RMKGKPLN CLEAR TO GET GROUP KEY NEXT                 
*                                                                               
         CLI   RMKGKGR2,C'Z'                                                    
         BL    LIRE0760                                                         
*                                  LETTER Z REACHED IN SECOND GROUP             
         ZIC   RE,RMKGKGR1         CODE. BUMP FIRST GROUP LETTER CODE           
         LA    RE,1(RE)                                                         
         STC   RE,RMKGKGR1                                                      
         MVI   RMKGKGR2,C'A'                                                    
         B     LIRE0160                                                         
*                                                                               
LIRE0760 DS    0H                                                               
         ZIC   RE,RMKGKGR2         GET NEXT GROUP                               
         LA    RE,1(RE)                                                         
         STC   RE,RMKGKGR2                                                      
         B     LIRE0160                                                         
         DROP  RF                                                               
*                                                                               
***>>>                                                                          
LIRE0770 EQU   *                                                                
         MVC   KEY(27),SALTMKEY    RESET KEY IN USE                             
         LA    RF,KEY                                                           
         USING RMKG2TYP,RF                                                      
         CLI   RMKG2GR2,C'Z'                                                    
         BL    LIRE0775                                                         
*                                  LETTER Z REACHED IN SECOND GROUP             
         ZIC   RE,RMKG2GR1         CODE. BUMP FIRST GROUP LETTER CODE           
         LA    RE,1(RE)                                                         
         STC   RE,RMKG2GR1                                                      
         MVI   RMKG2GR2,C'A'                                                    
         B     LIRE0105                                                         
*                                                                               
LIRE0775 DS    0H                                                               
         ZIC   RE,RMKG2GR2         GET NEXT GROUP                               
         LA    RE,1(RE)                                                         
         STC   RE,RMKG2GR2                                                      
         B     LIRE0105                                                         
         DROP  RF                                                               
***>>>                                                                          
                                                                                
LIRE0780 DS    0H                  GET NEXT MKGD OFFER REC                      
         CLI   SPEMFILT,C'N'       S/P OR TEAM FILTERING?                       
         BE    LIRE0800            NO  - PROCESS X'11' KEY FORM                 
         MVC   SPMGKEYD.RMKG2GRP(2),=C'ZZ'                                      
*                                  FORCE TO NEXT GROUP                          
         B     LIRE0105            NO  - GO BACK FOR NEXT KEY                   
         DROP  SPMGKEYD                                                         
LIRE0800 EQU   *                                                                
         MVC   MGKEYD.RMKGKGRP,=C'ZZ'                                           
         B     LIRE0160            GO BACK FOR NEXT KEY                         
         DROP  MGKEYD                                                           
                                                                                
LIRE1000 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SHOW ERROR NUMBER                                                             
***********************************************************************         
SHOWERR# NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SERRX                                                            
*                                                                               
SERR10   DS    0H                                                               
         LR    R3,R6                                                            
         BRAS  RE,NEXTEL                                                        
         BE    SERR10                                                           
*                                                                               
         LR    R6,R3                                                            
         USING RMKGATEM,R6                                                      
         CLI   RMKGATAT,X'80'      MUST BE ERROR                                
         BNE   SERRX                                                            
         EDIT  RMKGATEN,(3,LSTSTAT+6)                                           
*                                                                               
SERRX    DS    0H                                                               
         B     EXIT                                                             
*&&DO                                                                           
***********************************************************************         
* CHECK IF CHOICE GROUP CONTAINS A SELECTED OFFER                               
***********************************************************************         
CHKCHOIZ NTR1                                                                   
         MVC   MGKEY,KEY                                                        
         MVC   AIO,AIO3                                                         
         MVI   ISCHOICE,C'N'                                                    
         B     CHKCH20                                                          
*                                                                               
CHKCH10  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CHKCH20  DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   CHKCHXIT            ALL DONE                                     
         OC    RMKGKPLN(6),RMKGKPLN SKIP COMMENT RECORDS                        
         BZ    CHKCH10                                                          
         TM    RMKGKRTY,X'10'      FIND 'CHOICE' RECORDS                        
         BZ    CHKCH10                                                          
         DROP  R6                                                               
                                                                                
         MVI   ISCHOICE,C'Y'                                                    
                                                                                
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        GET STATUS ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      SELECTED?                                    
         BZ    CHKCH10                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ISCHOICE,C'N'       AT LEAST ONE WAS SELECTED                    
*                                                                               
* RESTORE KEY SEQUENCE AND IO AREA BEFORE EXITING                               
*                                                                               
CHKCHXIT DS    0H                                                               
         MVC   KEY(L'MGKEY),MGKEY                                               
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CLI   ISCHOICE,C'N'                                                    
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
STPFKL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   STPFINIT                                                         
                                                                                
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    STPFKL10                                                         
*                                                                               
         CLI   PFKEY,9             IF SELECTION                                 
         BNE   STPFKL10                                                         
*                                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         LA    R0,MKGSELH          CURSOR SHOULD BE WITHIN LIST                 
         CR    R2,R0                                                            
         BL    RECNTFND                                                         
*        LA    R0,MKGPFLNH                                                      
         CR    R2,R0                                                            
         BNL   RECNTFND                                                         
*                                                                               
STPFKL10 LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFINIT                                                         
         LA    R2,BLPFTAB                                                       
*                                                                               
STPFINIT DS    0H                                                               
*        OI    CTLRFLG1,CF1SVDAQ   DON'T SAVE D/A OF SELECTED                   
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
STPFLL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   STPFX                                                            
                                                                                
         CLI   PFKEY,0                                                          
         BE    STPFX                                                            
                                                                                
         CLI   PFKEY,2             ORDER LIST?                                  
         BE    STPFLL10                                                         
         CLI   PFKEY,9             SELECTION?                                   
         BNE   STPFX                                                            
                                                                                
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
STPFLL10 DS    0H                                                               
         ZIC   R0,PFKEY                                                         
         AH    R0,=H'12'                                                        
         STC   R0,PFKEY                                                         
         LA    R2,LPFTABLE         YES, USE SELECT PFKEY TABLE                  
*                                  TEST THE SEL CODES IN TESTSEL                
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFLL20                                                         
         LA    R2,BLPFTAB                                                       
*                                                                               
STPFLL20 DS    0H                                                               
         OI    CTLRFLG1,CF1TSELQ                                                
* USE FIRST KEY ON NEXT LIST AND FORCE CURSOR                                   
         OI    MGBITFLG,BFFRSTKY+BFFORCEC+BFPASSKY                              
         MVC   SVCRDISP,CURDISP                                                 
         XC    DISPFLAG,DISPFLAG   SET UP STATUS FLAG FOR SELECT                
         GOTO1 INITIAL,DMCB,(R2)   2ND PASS                                     
                                                                                
STPFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
LPFTABLE DS    0C                                                               
*                                                                               
* GO TO ORDER/LIST                                                              
         DC    AL1(LPF2X-*,2,0,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF2X    EQU   *                                                                
*                                                                               
* SELECT                                                                        
         DC    AL1(LPF09X-*,9,0,0,0,PFTRETRN)                                   
         DC    CL3'S',CL8' ',CL8' '                                             
LPF09X   EQU   *                                                                
*                                                                               
* ACTUAL SELECT                                                                 
         DC    AL1(LPF21X-*,21,PFTCPROG,0,0,0)                                  
         DC    CL3' ',CL8'MGGROUP',CL8'SELECT '                                 
LPF21X   EQU   *                                                                
*                                                                               
* ACTUAL RETURN TO CALLER                                                       
         DC    AL1(LPF14X-*,14,0,0,0,0)                                         
         DC    CL3' ',CL8'ORDER',CL8'LIST'                                      
LPF14X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
***********************************************************************         
*  BRAND LIST PFKEY TABLE DEFINITIONS                                           
***********************************************************************         
BLPFTAB  DS    0C                                                               
*                                                                               
* GO TO ORDER/LIST                                                              
         DC    AL1(BLPF2X-*,2,0,0,0,PFTRETRN)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
BLPF2X   EQU   *                                                                
*                                                                               
* SELECT                                                                        
         DC    AL1(BLPF09X-*,9,0,0,0,PFTRETRN)                                  
         DC    CL3'S',CL8' ',CL8' '                                             
BLPF09X  EQU   *                                                                
*                                                                               
* ACTUAL SELECT                                                                 
         DC    AL1(BLPF21X-*,21,PFTCPROG,0,0,0)                                 
         DC    CL3' ',CL8'MGGROUP',CL8'SELECT '                                 
BLPF21X  EQU   *                                                                
*                                                                               
* ACTUAL RETURN TO CALLER                                                       
         DC    AL1(BLPF14X-*,14,0,0,0,0)                                        
         DC    CL3' ',CL8'BRAND',CL8'LIST'                                      
BLPF14X  EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
NOACCESS MVI   GERROR1,55          SECURITY LOCKOUT                             
         B     ERREND                                                           
*                                                                               
INVPFERR MVI   GERROR1,ERINVPFK    INVALID PFKEY                                
         LA    R2,CONRECH                                                       
         B     ERREND                                                           
*                                                                               
INVSTDT  MVC   RERROR,=AL2(79)     INVALID START DATE                           
         B     ERREND                                                           
*                                                                               
INVENDT  MVC   RERROR,=AL2(80)     INVALID END DATE                             
         B     ERREND                                                           
*                                                                               
ERSTENDT MVC   RERROR,=AL2(64)     END DATE BEFORE START DATE                   
         B     ERREND                                                           
*                                                                               
ERDAREAG MVC   RERROR,=AL2(439)    NO REP AGENCIES FOUND                        
         B     ERREND                                                           
*                                                                               
INVLOFF  MVC   RERROR,=AL2(447)    INVALID OFFICE                               
         B     ERREND                                                           
*                                                                               
INVLSAL  MVC   RERROR,=AL2(448)    INVALID SALESPERSON                          
         B     ERREND                                                           
*                                                                               
NEEDOFF  MVC   RERROR,=AL2(449)    SALESPERSON FILTER NEED OFF FILTER           
         B     ERREND                                                           
*                                                                               
INVLTEAM MVC   RERROR,=AL2(827)    INVALID TEAM                                 
         B     ERREND                                                           
*                                                                               
NEXTREQ  MVC   RERROR,=AL2(3)      ENTER NEXT REQUEST                           
         B     INFEND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
*                                                                               
INFEND   DS    0H                                                               
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* BYTE 1     = STATUS BYTE RMKGSFG1 IN X'01' ELEM OF MKGOFF HEADER REC          
* BYTE 2-4   = USER INPUT FILTER                                                
* BYTE 5-13  = DISPLAY STATUS                                                   
* BYTE 14-16 = PARTY (STATION, REP OR AGENCY) THAT INVOKED THE STATUS           
*                                                                               
STATTAB  DS    0CL16                                                            
         DC    AL1(RMGF1MER),C'ERR',C'ERROR    ',C'AGY'                         
         DC    AL1(RMGF1MSN),C'SEN',C'SENT     ',C'REP'                         
         DC    AL1(RMGF1MAR),C'APR',C'APPROVED ',C'AGY'                         
         DC    AL1(RMGF1MRR),C'REJ',C'REJECTED ',C'AGY'                         
         DC    AL1(RMGF1MRR),C'SEL',C'REJSEL   ',C'REP'                         
         DC    AL1(RMGF1MCF),C'APL',C'APPLIED  ',C'REP'                         
         DC    AL1(RMGF1MCF),C'SEL',C'SELFAPP  ',C'REP'                         
         DC    AL1(RMGF1MCN),C'CAN',C'CANCELLED',C'REP'                         
         DC    AL1(RMGF1MCN),C'MGX',C'MGXSEL   ',C'REP'                         
         DC    AL1(RMGF1MCR),C'RES',C'RESENT   ',C'REP'                         
         DC    AL1(RMGF1MCM),C'REC',C'RECALLED ',C'REP'                         
         DC    AL1(0),C'NEW',C'NEW      ',C'REP'                                
         DC    X'FF'                                                            
         EJECT                                                                  
***>>>   PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REDARFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
*                                                                               
*      PRODUCTION SCREEN IS REDARF2D - REORDER BEFORE GOING LIVE                
*                                                                               
       ++INCLUDE REDARF2D                                                       
       ++INCLUDE REDARTWA                                                       
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
       ++INCLUDE RECNTPROF                                                      
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
DISPFLAG DS    X                   STATUS BYTE USED BY SELECT SCRN/OVLY         
MGBITFLG DS    X                                                                
BFFRSTKY EQU   X'80'               USE MGFRTKEY FOR NEXT LIST SCREEN            
BFGETFKY EQU   X'40'               GET MGFRTKEY FOR NEXT LIST SCREEN            
BFFORCEC EQU   X'20'               FORCE CURSOR                                 
BFPASSKY EQU   X'10'               USE MGFRTKEY FOR PASSIVES                    
MGFRTKEY DS    CL(L'RMKGKEY)       FIRST KEY                                    
MGKEY    DS    CL(L'RMKGKEY)                                                    
FILTRKEY DS    CL(L'RMKGKEY)                                                    
SALTMKEY DS    CL(L'RMKGKEY)                                                    
MYWORK   DS    CL64                                                             
STAGRP   DS    CL2                 STATION GROUP/SUBGROUP                       
STANFILT DS    CL5                                                              
CONNUM   DS    XL4                                                              
CONADV   DS    CL4                                                              
CONPRD   DS    CL3                                                              
CONPRDX  DS    CL20                                                             
SVCRDISP DS    H                   SAVE OFF CURSOR POSITION                     
ISCHOICE DS    C                   Y/N, CHOICE OFFER                            
GRPCODE  DS    CL2                 MAKEGOOD GROUP CODE                          
SPEMFILT DS    CL1                 Y =  S/P OR TEAM A0/A1 KEYS                  
A0ORA1   DS    CL1                 A0 = USE A011 KEY                            
*                                  A1 = USE A111 KEY                            
SVLSTCON DS    CL8                 SAVE AREA: LIST CONTRACT                     
AXSPTMKY DS    CL5                 S/P+TEAM PRELOAD AREA                        
         ORG   AXSPTMKY                                                         
AXTEMKEY DS    CL2                 TEAM (NO S/P) PRELOAD AREA                   
AXSPKEY  DS    CL3                 TEAM (NO S/P) PRELOAD AREA                   
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTSTAT  DS    CL9                                                              
         DS    CL2                                                              
LSTBYWHO DS    CL3                                                              
         DS    CL2                                                              
LSTGPCD  DS    CL2                                                              
         DS    CL2                                                              
LSTCON#  DS    CL8                                                              
         DS    CL2                                                              
LSTAGY#  DS    CL8                                                              
         DS    CL2                                                              
LSTOFF   DS    CL2                                                              
         DS    CL2                                                              
LSTSTA   DS    CL6                                                              
         DS    CL2                                                              
LSTSAL   DS    CL3                                                              
         DS    CL2                                                              
LSTCSTAT DS    CL9                                                              
         DS    CL2                                                              
LSTCWHO  DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'155REDAR18S  07/20/03'                                      
         END                                                                    
