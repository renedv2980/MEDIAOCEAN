*          DATA SET ACPRO55    AT LEVEL 024 AS OF 07/19/10                      
*PHASE T60B55A                                                                  
*                                                                               
***********************************************************************         
* LEVEL CHANGE DOCUMENTATION                                          *         
* --------------------------                                          *         
* TKLU 07APR03 001 - NEW ARTICLE LIST PROGRAM TO REPLACE PL/LIST, SEE *         
*                    PROJECT (DU01-0584), CONTAINS ACPROA3AD          *         
* TKLU 19OCT03 002 - LIVE DATE (INCLUDE LIVE SCREEN)                  *         
* TKLU 17DEC05 003 - <DU01-5029> - MCS ARTICLE ENHANCEMENTS           *         
***********************************************************************         
* INFORMATION                                                         *         
* -----------                                                         *         
* THIS BOOK WILL LIST NEW STYLE ARTICLE RECORDS (REPLACEMENT OF OLD   *         
* PRICELIST RECORDS). ANY FILTERS WILL BE APPLIED EXCLUSIVELY, I.E    *         
* ONLY RECORDS WILL BE LISTED THAT MATCH ALL FILTER CRITERIA IF SET.  *         
* THERE IS A PROFILE TO SET FILTERING TO 'INCLUSIVE'.                 *         
* ALL PRICES SHOWN ARE IN CURRENCY AND FOR TODAY'S DATE.              *         
***********************************************************************         
         TITLE 'T60B55 - ARTICLE FILE - LIST'                                   
T60B55   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B55**,RA,RR=R8                                              
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE                                                                  
         GOTO1 DICTATE,DMCB,C'LL  ',DDINL,DDOUTL                                
         GOTO1 DICTATE,(R1),C'LU  ',DDINU,DDOUTU                                
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)   TODAY'S DATE                       
         SPACE 1                                                                
*&&DO                                                                           
         LA    R3,PROHED                                                        
         CLI   MODE,NEWSCR                                                      
         BNE   MODE4                                                            
         SPACE 1                                                                
         XC    OVWORK,OVWORK                                                    
         CLI   AGYCNTRY,CTRYGBR                                                 
         BE    MODE2                                                            
         MVC   PRONIL,MYSPACES                                                  
         OI    PRONILH+FLDOIND-FLDHDRD,FOUTTRN                                  
         MVC   PRONII,MYSPACES                                                  
         OI    PRONIIH+FLDATB-FLDHDRD,FATBPROT                                  
         MVC   PROIAL,MYSPACES                                                  
         OI    PROIALH+FLDOIND-FLDHDRD,FOUTTRN                                  
         MVC   PROIAI,MYSPACES                                                  
         OI    PROIAIH+FLDATB-FLDHDRD,FATBPROT                                  
MODE2    MVC   PROHED,AC@PRA3C     SET CURRENCY IN HEADING ONLY IF FO-          
         TM    COMPSTA6,CPYSFBIL   REIGN CURRENCY ON BILLING OR                 
         BNZ   XIT                                                              
         TM    COMPSTAA,CPYSFCES   ON ESTIMATES ALLOWED                         
         BNZ   XIT                                                              
         MVC   PROHED,AC@PRA3                                                   
         B     XIT                                                              
*&&                                                                             
         SPACE 1                                                                
MODE4    CLI   MODE,LISTRECS                                                    
         BE    LR00                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MAIN ROUTINE TO LIST ARTICLE RECORDS                                *         
***********************************************************************         
         SPACE 1                                                                
LR00     DS    0H                                                               
         XC    WORK,WORK                                                        
*&&UK                                                                           
         LA    R0,AS$PFADL                                                      
         CLI   CALLSP,0            GET CORRECT INFO LINE                        
         BE    *+8                                                              
         LA    R0,AS$PFADL                                                      
         GOTO1 GETTXT,WORK,(R0),('PFLMAX',PROPFAH),(C'S',0)                     
         OI    PROPFAH+6,X'80'                                                  
*&&                                                                             
         BAS   RE,VALFLT           VALIDATE FILTER INPUT FIRST                  
*                                                                               
         USING ARTRECD,R4                                                       
         L     R4,AIO3             READ ARTICLE RECORDS SEQUENTIALLY            
         OC    OVWORK,OVWORK       FIRST TIME?                                  
         BZ    LR10                                                             
         CLC   OVWFLT,FILTERS      BUT ENSURE FILTERS NOT CHANGED               
         BNE   LR10                                                             
         MVC   ARTKEY,OVWKEY                                                    
         MVC   MYKEY,ARTKEY                                                     
         B     LR14                                                             
         SPACE 1                                                                
LR10     XC    ARTKEY,ARTKEY       BUILD START KEY FOR PRICELISTS               
         MVI   ARTKTYP,ARTKTYPQ                                                 
         MVI   ARTKSUB,ARTKAQ                                                   
         MVC   ARTKCPY,CUL                                                      
         MVC   ARTKART,SPACES                                                   
         XR    R1,R1               START CODE                                   
         ICM   R1,1,PROSTAH+5                                                   
         BZ    LR12                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ARTKART(0),PROSTA                                                
         SPACE                                                                  
LR12     MVC   MYKEY,ARTRECD                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,ARTRECD,ARTRECD,0                     
         BE    LR16                                                             
         DC    H'0'                I/O ERROR                                    
         SPACE                                                                  
LR14     L     R4,AIO3             REREAD PREVIOUS RECORD FIRST                 
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,ARTRECD,ARTRECD,0                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,ARTRECD,ARTRECD,0                     
         BE    LR16                                                             
         DC    H'0'                I/O ERROR                                    
         SPACE                                                                  
LR16     L     R4,AIO3             END OF ARTICLE RECORDS REACHED?              
         CLC   MYKEY,ARTKEY        (MYKEY IS 3 BYTES: TYPE/SUB/CPY)             
         BE    LR16A                                                            
         XC    OVWORK,OVWORK                                                    
         B     XIT                                                              
         SPACE 1                                                                
LR16A    BAS   RE,APPFLT           APPLY FILTERS                                
         BNE   LR14                                                             
         SPACE 1                                                                
         MVC   IODA,ARTKDA         READ FULL RECORD                             
         L     R4,AIO3                                                          
         GOTO1 DATAMGR,DMCB,GETRECQ,ACCMST,IODA,ARTRECD,IOWORK                  
         BE    LR18                                                             
         DC    H'0'                I/O ERROR                                    
         SPACE 1                                                                
         USING LINED,R5            DISPLAY RECORD ON LINE                       
LR18     MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         L     R4,AIO3             R4 POINTS TO RECORD                          
         MVC   AIO,AIO3                                                         
         SPACE 1                                                                
         MVC   LINART,ARTKART      SET KEY DATA (OR SPACES)                     
         MVC   LINOFF,ARTKOFF                                                   
         MVC   LINCLI,ARTKCLI                                                   
         MVC   LINSUP,ARTKSUP                                                   
         SPACE 1                                                                
         USING NAMELD,R3                                                        
         LA    R3,ARTRFST          READ ELEMENT DATA                            
         SPACE 1                                                                
LR20     CLI   NAMEL,0                                                          
         BE    LR30                                                             
         CLI   NAMEL,NAMELQ                                                     
         BE    LR24                                                             
         CLI   NAMEL,AFDELQ                                                     
         BE    LR26                                                             
         CLI   NAMEL,PRIELQ                                                     
*&&UK*&& BE    LR28                                                             
*&&US*&& BE    LR28B                                                            
         SPACE 1                                                                
LR22     XR    R0,R0                                                            
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     LR20                                                             
         SPACE 1                                                                
LR24     XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q                                                       
         CHI   RE,L'LINDESC                                                     
         BH    LR24A                                                            
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LINDESC(0),NAMEREC                                               
         B     LR22                                                             
         SPACE 1                                                                
LR24A    MVC   LINDESC(L'LINDESC-3),NAMEREC                                     
         MVC   LINDESC+L'LINDESC-3(3),MYDOTS                                    
         B     LR22                                                             
         SPACE 1                                                                
         USING AFDELD,R3                                                        
LR26     MVC   LINWOC,AFDWC                                                     
         MVC   LINFLT,AFDFILT                                                   
         B     LR22                                                             
         SPACE 1                                                                
         USING PRIELD,R3                                                        
*&&UK                                                                           
LR28     TM    COMPSTA6,CPYSFBIL   FC ON BILLING?                               
         BNZ   BBB8A                                                            
         TM    COMPSTAA,CPYSFCES   FC ON ESTIMATES?                             
         BZ    LR28B                                                            
LR28A    MVC   LINCUR,PRICURR                                                   
*&&                                                                             
         SPACE 1                                                                
LR28B    DS    0H                  GET LATEST PRICE ENTRY                       
         XR    RE,RE                                                            
         IC    RE,PRICNTR          SET COUNTER                                  
         LA    R6,PRINTRY          AND SEE SORT NOTE ON ELEMENT DSECT           
         USING PRINTRY,R6                                                       
         SPACE 1                                                                
LR28C    CLC   PRIDAT,TODAY        COMPARE EFF. DATE                            
         BNH   LR28D                                                            
         AHI   R6,PRINTRQ                                                       
         BCT   RE,LR28C                                                         
         MVC   LINPRI,ASTERS       NO CURRENT PRICE FOUND THEN MARK             
         SHI   R6,PRINTRQ          BUT PUT OUT DATE                             
         GOTO1 DATCON,DMCB,(1,PRIDAT),(13,LINPRI+1)                             
         B     LR22                                                             
*                                                                               
LR28D    DS    0H                  ASSUME CURRENCY HAS 2DP|                     
         CURED (P6,PRIAMT),(L'LINPRI,LINPRI),2,ZERO=YES                         
         B     LR22                                                             
*                                                                               
LR30     L     R1,AIO3             READ FOR GEGENCON LIST/SELECT LOGIC          
         MVC   KEY,0(R1)                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         MVC   OVWKEY,KEY                                                       
         MVC   OVWFLT,FILTERS                                                   
         GOTO1 LISTMON             ALL DONE THEN SET LINE                       
         B     LR14                AND READ FOR NEXT RECORD                     
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AND SET FILTERS                                 *         
* (NOTE THAT THERE IS LITTLE CROSS VALIDATION, I.E. NO TESTS WHETHER  *         
*  E.G. OFFICE AND CLIENT FILTER VALUES MATCH EACH OTHER.           ) *         
***********************************************************************         
         SPACE 1                                                                
VALFLT   DS    0H                                                               
         ST    RE,SAVERE                                                        
         SPACE 1                                                                
         XC    FILTERS(FILTERQ),FILTERS     CLEAR ALL FILTERS                   
         SPACE 1                                                                
         MVC   FILSTRT,PROSTA               SET START VALUE                     
*&&UK                                                                           
         CLI   PRODPROF+3,FILINQ   PROFILE TO ALLOW INCLUSIVE FILTERS?          
         BNE   VALF00                                                           
         MVI   FILINEX,FILINQ                                                   
*&&                                                                             
VALF00   CLI   TWAACCS,0           LIMIT ACCESS?                                
         BE    VALF10                                                           
         MVI   FILLAOF,FILLIMQ                                                  
         SPACE 1                                                                
VALF10   CLI   PROOFFH+5,0         OFFICE FILTER SET?                           
         BE    VALF20                                                           
         LA    R2,PROOFFH                                                       
         USING OGRRECD,R4                                                       
         LA    R4,KEY              FIRST TEST OFFICE EXISTS                     
         MVC   AIO,AIO2                                                         
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY(3),CUL                                                   
         MVC   OGRKOFC,PROOFF                                                   
         OC    OGRKOFC,SPACES                                                   
         MVI   ERROR,OFFNOXIS                                                   
         GOTO1 HIGH                                                             
         CLC   OGRKEY,KEYSAVE                                                   
         BNE   ERREND                                                           
         SPACE 1                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,OGRKOFC                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 OFFAL                                                            
         BE    VALF12                                                           
         MVI   ERROR,SECLKOUT                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF12   MVC   FILOFF,OGRKOFC      SET OFFICE                                   
         DROP  R1,R4                                                            
         SPACE 1                                                                
VALF20   CLI   PROCLIH+5,0         CLIENT FILTER SET?                           
         BE    VALF30                                                           
         LA    R2,PROCLIH                                                       
         USING ACTRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   AIO,AIO2                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(3),PROCLI                                                
         OC    ACTKACT,SPACES                                                   
         MVI   ERROR,BADCLI                                                     
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   ERREND                                                           
         SPACE 1                                                                
         USING PPRELD,R3                                                        
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         SPACE 1                                                                
VALF22   CLI   PPREL,PPRELQ                                                     
         BE    VALF24                                                           
         CLI   PPREL,0             NO OFFICE                                    
         BE    VALF26                                                           
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         B     VALF22                                                           
         SPACE 1                                                                
VALF24   MVC   TEMP(2),PPRGAOFF    SAVE CLIENT'S OFFICE                         
         OC    TEMP(2),TEMP        NO OFFICE                                    
         BZ    VALF26                                                           
         CLC   TEMP(2),SPACES      NO OFFICE                                    
         BE    VALF26                                                           
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,TEMP                                                    
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 OFFAL                                                            
         BE    VALF26                                                           
         MVI   ERROR,SECLKOUT                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF26   MVC   FILCLI,ACTKACT                                                   
         DROP  R1,R4,R3                                                         
         SPACE 1                                                                
VALF30   CLI   PROSUPH+5,0         SUPPLIER FILTER SET?                         
         BE    VALF40                                                           
         LA    R2,PROSUPH                                                       
         USING LDGRECD,R4                                                       
         LA    R4,KEY              READ LEDGER FIRST                            
         MVC   AIO,AIO2                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUL                                                      
         MVC   LDGKUNT(2),PROSUP                                                
         OC    LDGKUNT(2),SPACES                                                
         MVI   ERROR,INVLDGR                                                    
         CLC   LDGKUNT(2),LDGSV                                                 
         BE    VALF31                                                           
         CLC   LDGKUNT(2),LDGSX                                                 
         BE    VALF31                                                           
         B     ERREND                                                           
         SPACE 1                                                                
VALF31   GOTO1 HIGH                                                             
         CLC   LDGKEY,KEYSAVE                                                   
         BNE   ERREND                                                           
         SPACE 1                                                                
         USING LDGELD,R3                                                        
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         MVI   ERROR,INVLDGR                                                    
         SPACE 1                                                                
VALF32   CLI   LDGEL,LDGELQ                                                     
         BE    VALF33                                                           
         CLI   LDGEL,0                                                          
         BE    ERREND                                                           
         IC    R0,LDGLN                                                         
         AR    R3,R0                                                            
         B     VALF32                                                           
         SPACE 1                                                                
VALF33   MVC   TEMP(1),LDGOPOS     SAVE LEDGER OFFICE SETTING                   
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
         LA    R4,KEY              READ ACCOUNT                                 
         MVC   AIO,AIO2                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKULA(14),PROSUP                                               
         OC    ACTKULA,SPACES                                                   
         MVI   ERROR,INVALID                                                    
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   ERREND                                                           
         SPACE 1                                                                
         USING ABLELD,R3                                                        
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         MVI   ERROR,INVPOST                                                    
         SPACE 1                                                                
VALF34   CLI   ABLEL,ABLELQ                                                     
         BE    VALF35                                                           
         CLI   ABLEL,0                                                          
         BE    ERREND                                                           
         IC    R0,ABLLN                                                         
         AR    R3,R0                                                            
         B     VALF34                                                           
         SPACE 1                                                                
VALF35   CLI   TEMP,LDGONONE       OFFICE MATCHING?                             
         BE    VALF37                                                           
         CLI   TEMP,LDGONKHI                                                    
         BH    VALF37                                                           
         LA    RE,ACTKULA+1                                                     
         LA    RF,0                                                             
         TM    TEMP,LDGOKEY2       2CO?                                         
         BZ    VALF36                                                           
         LA    RF,1                                                             
         NI    TEMP,X'FF'-LDGOKEY2                                              
         SPACE 1                                                                
VALF36   IC    R0,TEMP                                                          
         AR    RE,R0                                                            
         MVC   TEMP+2(2),SPACES                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),0(RE)                                                  
         SPACE 1                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,TEMP+2                                                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 OFFAL                                                            
         BE    VALF37                                                           
         MVI   ERROR,SECLKOUT                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF37   MVC   FILSUP,ACTKULA                                                   
         DROP  R1,R4,R3                                                         
         SPACE 1                                                                
VALF40   CLI   PROSTTH+5,0         STATUS FILTER SET?                           
         BE    VALF50                                                           
         LA    R2,PROSTTH                                                       
         OC    PROSTT,SPACES                                                    
         CLI   PROSTT,FILINTQ      INTERNAL?                                    
         BNE   VALF42                                                           
         MVI   FILSTT,FILINTQ                                                   
         B     VALF50                                                           
         SPACE 1                                                                
VALF42   CLI   PROSTT,FILEXTQ      EXTERNAL?                                    
         BNE   VALF44                                                           
         MVI   FILSTT,FILEXTQ                                                   
         B     VALF50                                                           
         SPACE 1                                                                
VALF44   MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF50   CLI   PROWOCH+5,0         WORK CODE FILTER SET?                        
         BE    VALF60                                                           
         LA    R2,PROWOCH                                                       
         USING WCORECD,R4                                                       
         LA    R4,KEY              TEST W/C EXISTS                              
         MVC   AIO,AIO2                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY(3),CUL                                                   
         MVC   WCOKWRK,PROWOC                                                   
         MVI   ERROR,BADWORK                                                    
         GOTO1 HIGH                                                             
         CLC   WCOKEY,KEYSAVE                                                   
         BNE   ERREND                                                           
         SPACE 1                                                                
         MVC   FILWOC,PROWOC                                                    
         DROP  R4                                                               
         SPACE 1                                                                
VALF60   CLI   PROFLTH+5,0         'FILTER' FILTER SET?                         
         BE    VALF70                                                           
         LA    R2,PROFLTH                                                       
         OC    PROFLT,SPACES                                                    
         LA    RE,FLTCHRS                                                       
VALF62   CLI   0(RE),X'FF'          VALID FILTER CHARACTER?                     
         BE    VALF64                                                           
         CLC   0(1,RE),PROFLT                                                   
         BE    VALF66                                                           
         AHI   RE,1                                                             
         B     VALF62                                                           
         SPACE 1                                                                
VALF64   MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF66   MVC   FILFLT,PROFLT                                                    
         SPACE 1                                                                
VALF70   CLI   PROLOCH+5,0         LOCK FILTER SET?                             
         BE    VALF76                                                           
         LA    R2,PROLOCH                                                       
         CLC   PROLOC,AC3YES                                                    
         BNE   VALF72                                                           
         MVI   FILLOC,FILYESQ                                                   
         B     VALF76                                                           
         SPACE 1                                                                
VALF72   CLC   PROLOC,AC3ONLY                                                   
         BNE   VALF74                                                           
         MVI   FILLOC,FILONLYQ                                                  
         B     VALF76                                                           
         SPACE 1                                                                
VALF74   MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF76   MVC   FILPFL,AC3YES                                                    
         MVC   FILNOP,AC3YES                                                    
         MVC   FILDOR,AC3YES                                                    
         MVC   FILNIC,AC3YES                                                    
         MVC   FILART,AC3YES                                                    
         CLI   PROPFLH+5,0         FLEXIBLE PRICE?                              
         BE    VALF82                                                           
         LA    R2,PROPFLH                                                       
         CLC   PROPFL,AC3NO                                                     
         BNE   VALF78                                                           
         MVI   FILPFL,FILNOQ                                                    
         B     VALF82                                                           
         SPACE 1                                                                
VALF78   CLC   PROPFL,AC3ONLY                                                   
         BNE   VALF80                                                           
         MVI   FILPFL,FILONLYQ                                                  
         B     VALF82                                                           
         SPACE 1                                                                
VALF80   MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF82   CLI   PRONOPH+5,0         NO PRICE?                                    
         BE    VALF88                                                           
         LA    R2,PRONOPH                                                       
         CLC   PRONOP,AC3NO                                                     
         BNE   VALF84                                                           
         MVI   FILNOP,FILNOQ                                                    
         B     VALF88                                                           
         SPACE 1                                                                
VALF84   CLC   PRONOP,AC3ONLY                                                   
         BNE   VALF86                                                           
         MVI   FILNOP,FILONLYQ                                                  
         B     VALF88                                                           
         SPACE 1                                                                
VALF86   MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF88   CLI   PRODORH+5,0         DESCRIPTION OVERRIDE?                        
         BE    VALF94                                                           
         LA    R2,PRODORH                                                       
         CLC   PRODOR,AC3NO                                                     
         BNE   VALF90                                                           
         MVI   FILDOR,FILNOQ                                                    
         B     VALF94                                                           
         SPACE 1                                                                
VALF90   CLC   PRODOR,AC3ONLY                                                   
         BNE   VALF92                                                           
         MVI   FILDOR,FILONLYQ                                                  
         B     VALF94                                                           
         SPACE 1                                                                
VALF92   MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF94   DS    0H                                                               
*&&UK                                                                           
         CLI   AGYCNTRY,CTRYGER                                                 
         BE    VALFX                                                            
         SPACE 1                                                                
         CLI   PRONIIH+5,0         APPLY NIC?                                   
         BE    VALF100                                                          
         LA    R2,PRONIIH                                                       
         CLC   PRONII,AC3NO                                                     
         BNE   VALF96                                                           
         MVI   FILNIC,FILNOQ                                                    
         B     VALF100                                                          
         SPACE 1                                                                
VALF96   CLC   PRONII,AC3ONLY                                                   
         BNE   VALF98                                                           
         MVI   FILNIC,FILONLYQ                                                  
         B     VALF100                                                          
         SPACE 1                                                                
VALF98   MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
         SPACE 1                                                                
VALF100  CLI   PROIAIH+5,0         ARTIST?                                      
         BE    VALFX                                                            
         LA    R2,PROIAIH                                                       
         CLC   PROIAI,AC3NO                                                     
         BNE   VALF102                                                          
         MVI   FILART,FILNOQ                                                    
         B     VALFX                                                            
         SPACE 1                                                                
VALF102  CLC   PROIAI,AC3ONLY                                                   
         BNE   VALF104                                                          
         MVI   FILART,FILONLYQ                                                  
         B     VALFX                                                            
         SPACE 1                                                                
VALF104  MVI   ERROR,NOFILTRQ                                                   
         B     ERREND                                                           
*&&                                                                             
VALFX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AND SET FILTERS (R4 POINTS TO RECORD)           *         
***********************************************************************         
         SPACE 1                                                                
         USING ARTRECD,R4                                                       
APPFLT   NTR1                                                                   
         SPACE 1                                                                
         CLI   FILLAOF,FILLIMQ     TEST LIMIT ACCESS FILTER                     
         BNE   APPF02                                                           
         CLC   ARTKOFF,SPACES      BUT ONLY IF RECORD'S OFFICE SET              
         BE    APPF02                                                           
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,ARTKOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 OFFAL                                                            
         BNE   APPFNO                                                           
         DROP  R1                                                               
         SPACE 1                                                                
APPF02   OC    FILOFF,FILOFF       APPLY OFFICE FILTER?                         
         BZ    APPF06                                                           
         CLI   FILINEX,FILINQ      INCLUSIVE FILTERING?                         
         BNE   APPF04                                                           
         CLC   ARTKOFF,SPACES      OFFICE SET ON RECORD?                        
         BE    APPF06                                                           
         SPACE 1                                                                
APPF04   CLC   ARTKOFF,FILOFF      MATCH ON OFFICE?                             
         BNE   APPFNO                                                           
         SPACE 1                                                                
APPF06   CLC   ARTKCLI,SPACES      CHECK CLIENT'S OFFICE VS LIMIT ACC           
         BE    APPF06C                                                          
         USING ACTRECD,R3                                                       
         LA    R3,KEY                                                           
         MVC   AIO,AIO2                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(5),ARTKCLI                                               
         MVI   ERROR,BADCLI                                                     
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   APPFNO              CLI NOT EXISTING -> SKIP                     
         SPACE 1                                                                
         USING PPRELD,R3                                                        
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         SPACE 1                                                                
APPF06A  CLI   PPREL,PPRELQ                                                     
         BE    APPF06B                                                          
         CLI   PPREL,0             NO OFFICE                                    
         BE    APPF06C                                                          
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         B     APPF06A                                                          
         SPACE 1                                                                
APPF06B  MVC   TEMP(2),PPRGAOFF    SAVE CLIENT'S OFFICE                         
         OC    TEMP(2),TEMP        NO OFFICE                                    
         BZ    APPF06C                                                          
         CLC   TEMP(2),SPACES      NO OFFICE                                    
         BE    APPF06C                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,TEMP                                                    
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 OFFAL                                                            
         BNE   APPFNO              EXCLUD EIF DIFFERENT OFFICE                  
         DROP  R1,R3                                                            
         SPACE 1                                                                
APPF06C  OC    FILCLI,FILCLI       APPLY CLIENT FILTER?                         
         BZ    APPF10                                                           
         CLI   FILINEX,FILINQ      INCLUSIVE FILTERING?                         
         BNE   APPF08                                                           
         CLC   ARTKCLI,SPACES      CLIENT SET ON RECORD?                        
         BE    APPF10                                                           
         SPACE 1                                                                
APPF08   CLC   ARTKCLI,FILCLI      MATCH ON CLIENT?                             
         BNE   APPFNO                                                           
         SPACE 1                                                                
         USING LDGRECD,R3                                                       
APPF10   CLC   ARTKSUP,SPACES      CHECK SUPPLIER'S OFFICE                      
         BE    APPF10E                                                          
         LA    R3,KEY              READ LEDGER FIRST                            
         MVC   AIO,AIO2                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUL                                                      
         MVC   LDGKUNT(2),ARTKSUP                                               
         GOTO1 HIGH                                                             
         CLC   LDGKEY,KEYSAVE                                                   
         BNE   APPFNO              IF LEDGER DOESN'T EXIST -> SKIP              
         SPACE 1                                                                
         USING LDGELD,R3                                                        
         L     R3,AIO2                                                          
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         MVI   TEMP,LDGONONE                                                    
         SPACE 1                                                                
APPF10A  CLI   LDGEL,LDGELQ                                                     
         BE    APPF10B                                                          
         CLI   LDGEL,0                                                          
         BE    APPF10C                                                          
         IC    R0,LDGLN                                                         
         AR    R3,R0                                                            
         B     APPF10A                                                          
         SPACE 1                                                                
APPF10B  MVC   TEMP(1),LDGOPOS     SAVE LEDGER OFFICE SETTING                   
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
APPF10C  LA    R3,KEY              READ ACCOUNT                                 
         MVC   AIO,AIO2                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKULA,ARTKSUP                                                  
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   APPFNO              ACCOUNT DOESN'T EXIST -> SKIP                
         SPACE 1                                                                
         CLI   TEMP,LDGONONE                                                    
         BE    APPF10E                                                          
         CLI   TEMP,LDGONKHI                                                    
         BH    APPF10E                                                          
         SPACE 1                                                                
         LA    RE,ACTKULA+1                                                     
         LA    RF,0                                                             
         TM    TEMP,LDGOKEY2       2CO?                                         
         BZ    APPF10D                                                          
         LA    RF,1                                                             
         NI    TEMP,X'FF'-LDGOKEY2                                              
         SPACE 1                                                                
APPF10D  IC    R0,TEMP                                                          
         AR    RE,R0                                                            
         MVC   TEMP+2(2),SPACES                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),0(RE)                                                  
         SPACE 1                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,TEMP                                                    
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 OFFAL                                                            
         BNE   APPFNO              EXCLUD EIF DIFFERENT OFFICE                  
         DROP  R1,R3                                                            
         SPACE 1                                                                
APPF10E  OC    FILSUP,FILSUP       APPLY SUPPLIER FILTER?                       
         BZ    APPF14                                                           
         CLI   FILINEX,FILINQ      INCLUSIVE FILTERING?                         
         BNE   APPF12                                                           
         CLC   ARTKSUP,SPACES      SUPPLIER SET ON RECORD?                      
         BE    APPF14                                                           
         SPACE 1                                                                
APPF12   CLC   ARTKSUP,FILSUP      MATCH ON SUPPLIER?                           
         BNE   APPFNO                                                           
         SPACE 1                                                                
APPF14   OC    FILSTT,FILSTT       APPLY STATUS FILTER?                         
         BZ    APPF18                                                           
         CLI   FILSTT,FILINTQ                                                   
         BNE   APPF16                                                           
         TM    ARTKSTAT,ARTKTINT   INTERNAL?                                    
         BZ    APPFNO                                                           
         B     APPF18                                                           
         SPACE 1                                                                
APPF16   TM    ARTKSTAT,ARTKTINT   EXTERNAL?                                    
         BNZ   APPFNO                                                           
         SPACE 1                                                                
APPF18   OC    FILWOC,FILWOC       APPLY W/C FILTER?                            
         BZ    APPF22                                                           
         CLI   FILINEX,FILINQ      INCLUSIVE FILTERING?                         
         BNE   APPF20                                                           
         CLC   ARTKSWC,SPACES      W/C SET ON RECORD?                           
         BE    APPF22                                                           
         SPACE 1                                                                
APPF20   CLC   ARTKSWC,FILWOC      MATCH ON W/C?                                
         BNE   APPFNO                                                           
         SPACE 1                                                                
APPF22   OC    FILFLT,FILFLT       APPLY 'FILTER' FILTER?                       
         BZ    APPF26                                                           
         CLI   FILINEX,FILINQ      INCLUSIVE FILTERING?                         
         BNE   APPF24                                                           
         CLC   ARTKSFLT,SPACES     'FILTER' SET ON RECORD?                      
         BE    APPF26                                                           
         SPACE 1                                                                
APPF24   CLC   ARTKSFLT,FILFLT     MATCH ON 'FILTER'?                           
         BNE   APPFNO                                                           
         SPACE 1                                                                
APPF26   OC    FILLOC,FILLOC       APPLY LOCK FILTER?                           
         BZ    APPF28                                                           
         CLI   FILLOC,FILYESQ                                                   
         BE    APPF30                                                           
         TM    ARTKSTAT,ARTKLOCK   LOCKED?                                      
         BZ    APPFNO                                                           
         B     APPF30                                                           
         SPACE 1                                                                
APPF28   TM    ARTKSTAT,ARTKLOCK   NONE-LOCKED?                                 
         BNZ   APPFNO                                                           
         SPACE 1                                                                
APPF30   CLI   FILPFL,FILYESQ      FLEXIBLE PRICE FILTER?                       
         BE    APPF34                                                           
         CLI   FILPFL,FILNOQ                                                    
         BNE   APPF32                                                           
         TM    ARTKSTAT,ARTKFLXQ                                                
         BNZ   APPFNO                                                           
         B     APPF34                                                           
APPF32   CLI   FILPFL,FILONLYQ                                                  
         BNE   APPF34                                                           
         TM    ARTKSTAT,ARTKFLXQ                                                
         BZ    APPFNO                                                           
         SPACE 1                                                                
APPF34   CLI   FILNOP,FILYESQ      NO PRICE FILTER?                             
         BE    APPF38                                                           
         CLI   FILNOP,FILNOQ                                                    
         BNE   APPF36                                                           
         TM    ARTKSTAT,ARTKNOPQ                                                
         BNZ   APPFNO                                                           
         B     APPF38                                                           
APPF36   CLI   FILNOP,FILONLYQ                                                  
         BNE   APPF38                                                           
         TM    ARTKSTAT,ARTKNOPQ                                                
         BZ    APPFNO                                                           
         SPACE 1                                                                
APPF38   CLI   FILDOR,FILYESQ      DESCRIPTION OVERRIDE FILTER?                 
         BE    APPF42                                                           
         CLI   FILDOR,FILNOQ                                                    
         BNE   APPF40                                                           
         TM    ARTKSTAT,ARTKDAMQ                                                
         BNZ   APPFNO                                                           
         B     APPF42                                                           
APPF40   CLI   FILDOR,FILONLYQ                                                  
         BNE   APPF42                                                           
         TM    ARTKSTAT,ARTKDAMQ                                                
         BZ    APPFNO                                                           
         SPACE 1                                                                
APPF42   CLI   FILNIC,FILYESQ      APPLY NIC FILTER?                            
         BE    APPF46                                                           
         CLI   FILNIC,FILNOQ                                                    
         BNE   APPF44                                                           
         TM    ARTKSTAT,ARTKNICQ                                                
         BNZ   APPFNO                                                           
         B     APPF46                                                           
APPF44   CLI   FILNIC,FILONLYQ                                                  
         BNE   APPF46                                                           
         TM    ARTKSTAT,ARTKNICQ                                                
         BZ    APPFNO                                                           
         SPACE 1                                                                
APPF46   CLI   FILART,FILYESQ      ARTIST FILTER?                               
         BE    APPF50                                                           
         CLI   FILART,FILNOQ                                                    
         BNE   APPF48                                                           
         TM    ARTKSTA2,ARTKSART                                                
         BNZ   APPFNO                                                           
         B     APPF50                                                           
APPF48   CLI   FILART,FILONLYQ                                                  
         BNE   APPF50                                                           
         TM    ARTKSTA2,ARTKSART                                                
         BZ    APPFNO                                                           
         SPACE 1                                                                
APPF50   DS    0H                                                               
         SPACE 1                                                                
APPFYES  CR    RC,RC               INCLUDE FROM LIST                            
         B     XIT                                                              
         SPACE 1                                                                
APPFNO   LTR   RB,RB               EXCLUDE FROM LIST                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT AND ERROR HANDLING                                     *         
***********************************************************************         
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CONSTANTS, LTORG, EQUATES AND TABLES                                *         
***********************************************************************         
         SPACE 1                                                                
PZERO    DC    P'0'                                                             
ASTERS   DC    12C'*'                                                           
MYDOTS   DC    3C'.'                                                            
LDGSV    DC    CL2'SV'                                                          
LDGSX    DC    CL2'SX'                                                          
FLTCHRS  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
         DC    X'FF'                                                            
         SPACE 1                                                                
GETRECQ  DC    CL8'GETREC'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
         SPACE 1                                                                
MYSPACES DC    64C' '                                                           
         EJECT                                                                  
DDINL    DS    0C                                                               
*&&UK*&& DCDDL AC#PRA3C,77                                                      
         DCDDL AC#PRA3,77                                                       
         DC    X'00'                                                            
DDINU    DS    0C                                                               
         DCDDL AC#YES,3,LABEL=AC3YES                                            
         DCDDL AC#ONLY,2,LABEL=AC2ONLY                                          
         DCDDL AC#NO,2,LABEL=AC2NO                                              
         DC    X'00'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
FILTERS  DS    0X                                                               
FILSTRT  DS    CL4                                                              
FILINEX  DS    XL1                                                              
FILINQ   EQU   C'I'                                                             
FILLAOF  DS    XL1                                                              
FILLIMQ  EQU   X'01'                                                            
FILOFF   DS    CL2                                                              
FILCLI   DS    CL5                                                              
FILSUP   DS    CL14                                                             
FILSTT   DS    XL1                                                              
FILINTQ  EQU   C'I'                                                             
FILEXTQ  EQU   C'E'                                                             
FILWOC   DS    CL2                                                              
FILFLT   DS    CL1                                                              
FILLOC   DS    XL1                                                              
FILYESQ  EQU   C'Y'                                                             
FILNOQ   EQU   C'N'                                                             
FILONLYQ EQU   C'O'                                                             
FILPFL   DS    XL1                                                              
FILNOP   DS    XL1                                                              
FILDOR   DS    XL1                                                              
FILNIC   DS    XL1                                                              
FILART   DS    XL1                                                              
FILTERQ  EQU   *-FILTERS                                                        
         SPACE 1                                                                
RELO     DS    F                                                                
TODAY    DS    XL3                                                              
SAVERE   DS    A                                                                
MYKEY    DS    XL3                                                              
TEMP     DS    CL30                                                             
IOWORK   DS    XL64                                                             
IODA     DS    XL4                                                              
         SPACE 1                                                                
DDOUTL   DS    0C                                                               
*&&UK                                                                           
AC@PRA3C DS    CL77                                                             
*&&                                                                             
AC@PRA3  DS    CL77                                                             
         SPACE 1                                                                
DDOUTU   DS    0C                                                               
AC3YES   DS    CL3                                                              
AC3ONLY  DS    CL3                                                              
AC3NO    DS    CL3                                                              
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE, SCREEN AND DSECTS                            *         
***********************************************************************         
         SPACE 1                                                                
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
LINED    DSECT                                                                  
LINART   DS    CL4                 PRICE LIST CODE                              
         DS    XL1                                                              
LINOFF   DS    CL2                 OFFICE CODE                                  
         DS    XL1                                                              
LINCLI   DS    CL3                 CLIENT CODE                                  
         DS    XL1                                                              
LINSUP   DS    CL14                SUPPLIER CODE                                
         DS    XL1                                                              
LINDESC  DS    CL23                DESCRIPTION                                  
         DS    XL1                                                              
LINPRI   DS    CL10                PRICE                                        
         DS    XL1                                                              
LINWOC   DS    CL2                 WORK CODE                                    
         DS    XL1                                                              
LINFLT   DS    CL1                 FILTER                                       
LINELNQ  EQU   *-LINED                                                          
         SPACE 1                                                                
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROA3D                                                       
OVWORK   DS    0X                                                               
OVWKEY   DS    XL42                                                             
OVWFLT   DS    XL(FILTERQ)                                                      
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*DDEBLOCK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
*DDLANGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
*ACMSGEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*DDCTRYEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACPRO55   07/19/10'                                      
         END                                                                    
