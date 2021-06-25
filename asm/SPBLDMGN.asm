*          DATA SET SPBLDMGN   AT LEVEL 042 AS OF 11/19/19                      
*PHASE T00AC2T  <=============                                                  
*===================================================================*           
* 13JUN17 HWON ONLY SET MGAKEY WHEN KEY IS A MATCH                  *           
* 27APR15 HWON CLEAR MGAERR WHEN REBUILDING AND SKIPPING NO CHARGE  *           
* 31MAR15 HWON IF MGAOPT2_MINB1 IS ON, USE TAR MINIO BUFFER         *           
* 31MAR15 HWON CHANGED TO SUPPORT BUILDING MGTABLE IN MINIO BUFFER  *           
* 24APR14 MHER 2 BYTE MAKEGOOD CODES                                *           
*  IN THIS VRSN CODE HOLDS ALPHA VALUE/BCODE(2) HAS BINARY VALUE                
* 20FEB14 AKAT RELINK TO FIX DISPLACEMENT OF MGLLINE AND ON         *           
* 24OCT13 MHER REMOVE OTHER STA CODE, BUMP MAKEGOOD LIMIT TO 240    *           
* 25JAN05 MHER PRINT 2 DECIMAL RATINGS AS WELL !                    *           
* 08DEC04 EJOR FIX 2 DEC BUG                                        *           
* 24MAR04 MHER 2 DECIMAL RATINGS                                    *           
* 12JAN04 PWES/19 COSTS FOR CANADIAN NETWORK                        *           
* 06AUG02 MHER STOP BLOWING UP IF TABLE GETS TOO BIG - JUST STOP !  *           
* 31OCT00 EJOR LISA LISA LISA STRIKES AGAIN                         *           
*              CORRECT EOF TESTS ON BLINE CALLS                     *           
* 22APR98 ABEA PRE-EMPT NOW OVERRIDES NO CHARGES                    *           
* 03MAR98 MHER SUPPORT WIM OPTION TO PRINT -OTOS ONLY               *           
* 18NOV97 MHER SUPPORT OPTIONS TO SUPPRESS COSTS AND DEMOS          *           
* 02OCT97 MHER EXTEND NUMBER OF MG CODES TO 99                      *           
*              CHANGE TO SPGENBUY/REGEL ALSO                        *           
*===================================================================*           
*                                                                               
* THIS ROUTINE DEALS WITH NEW MAKEGOODS                                         
*                                                                               
* P1 - PARAMETER BLOCK                                                          
*                                                                               
SPBLDMGN TITLE 'BLDMGAN - T00AC2 - BUILD TABLE OF MAKEGOODS'                    
         PRINT NOGEN                                                            
BLDMGA   CSECT                                                                  
         NMOD1 WORKX-WORKD,**BMGA**,R7                                          
         USING WORKD,RC                                                         
*==================================================================*            
MAXCODES EQU   815                 MAX MAKEGOOD CODES                           
MAXCAN   EQU   240                                                              
MAXENTS  EQU   8000                MAX ENTRIES IN TABLE                         
* THIS VALUE IS LIMITED BY TEMPSTR USAGE                                        
* 18K PAGES, 4 PAGES AVAILABLE GIVES US 73728 BYTES OF STORAGE                  
* WITH 208BYTE OVERHEAD, 68BYTE ENTRIES AND 4BYTE POINTER = 1021 MAX            
* IF YOU NEED MORE, HAVE TO SWITCH TO TEMPEST                                   
*==================================================================*            
*                                                                               
         LR    R6,R1                                                            
         USING MGABLKD,R6                                                       
         L     R9,MGAACOM                                                       
         USING COMFACSD,R9                                                      
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
         USING TSARD,R8                                                         
         LA    R8,TSARBLK                                                       
         XC    TSARBLK,TSARBLK                                                  
*                                                                               
         MVI   MGAERR,0                                                         
         MVC   DATADISP,=H'24'                                                  
         XC    AD50EL,AD50EL                                                    
*                                                                               
         CLI   MGAACT,MGAQTRNS     TRANSLATE BINARY CODE TO ALPHA               
         BNE   M02                                                              
         BAS   RE,TRANSLAT                                                      
         B     MGAX                                                             
*                                                                               
M02      CLI   MGAACT,MGAQBIN      TRANSLATE ALPHA CODE TO BINARY               
         BNE   M05                                                              
         MVC   CODE,MGQCODE                                                     
         BAS   RE,TRANSBIN                                                      
         MVC   MGBCODE,BCODE       RETURN 2-BYTE VALUE                          
         B     MGAX                                                             
*                                                                               
M05      CLI   MGAACT,MGAQBLD      BUILD THE TABLE                              
         BNE   M10                                                              
         BAS   RE,MGABLDT                                                       
         B     MGAX                                                             
*                                                                               
M10      CLI   MGAACT,MGAQADD      ADD AN ENTRY                                 
         BNE   M20                                                              
         BAS   RE,ADDENTRY                                                      
         B     MGAX                                                             
*                                                                               
M20      CLI   MGAACT,MGAQDEL      DELETE AN ENTRY                              
         BNE   M30                                                              
         BAS   RE,DELENTRY                                                      
         B     MGAX                                                             
*                                                                               
M30      CLI   MGAACT,MGAQCOD      GET NEXT AVAILABLE CODE                      
         BNE   M40                                                              
         BRAS  RE,GETOLD                                                        
         B     MGAX                                                             
*                                                                               
M40      CLI   MGAACT,MGAQFND      FIND THIS ENTRY                              
         BNE   M50                                                              
         BAS   RE,FNDENTRY                                                      
         B     MGAX                                                             
*                                                                               
M50      CLI   MGAACT,MGAQNXT      FIND NEXT ENTRY                              
         BNE   M60                                                              
         BAS   RE,NXTENTRY                                                      
         B     MGAX                                                             
*                                                                               
M60      CLI   MGAACT,MGAQRDH      READ HIGH                                    
         BNE   M70                                                              
         BAS   RE,RDHI                                                          
         B     MGAX                                                             
*                                                                               
M70      CLI   MGAACT,MGAQBLN      BUILD A TABLE FOR A SINGLE BUY LINE          
         BNE   M80                                                              
         BAS   RE,BLINE                                                         
         B     MGAX                                                             
*                                                                               
M80      CLI   MGAACT,MGAQGET      GET AN ENTRY - MGATSNUM SET                  
         BNE   M90                                                              
         BAS   RE,GETENTRY                                                      
         B     MGAX                                                             
*                                                                               
M90      CLI   MGAACT,MGAQTOT      DO TOTALS FOR TABLE ENTRY                    
         BNE   M100                                                             
         BAS   RE,TOTAL                                                         
         B     MGAX                                                             
*                                                                               
M100     CLI   MGAACT,MGAQPRNT     SET UP A PRINT LINE                          
         BNE   M120                                                             
         BAS   RE,SETPRNT                                                       
*                                                                               
M120     DS    0H                                                               
*                                                                               
MGAX     B     XIT                                                              
         EJECT                                                                  
*=================================================================              
* BUILD TABLE OF MAKEGOODS/MISSED                                               
* FOR CABLE, READ ALL NETWORKS FOR THIS HEADEND                                 
*=================================================================              
                                                                                
MGABLDT  NTR1                                                                   
         OC    MGATAB,MGATAB       IS THERE A TABLE                             
         BZ    MGA05                                                            
         L     R1,MGATAB           A(BEGINING OF TABLE)                         
         LH    R2,MGATABLN                                                      
         TM    MGAOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGATABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX                                                       
         B     MGA10                                                            
*                                                                               
MGA05    XC    TSARBLK,TSARBLK                                                  
*                                                                               
         XC    GMISSTOT,GMISSTOT                                                
         XC    GMGTOT,GMGTOT                                                    
         XC    GMISSRTG,GMISSRTG                                                
         XC    GMGRTG,GMGRTG                                                    
* CODE MOVED TO CALLTSAR, SO THAT IT'S ALWAYS SET -HWON 3/31/2015               
*&&DO                                                                           
         TM    MGAOPT2,MGAOPT2_NODSK   TEST NO DISK TO BE USED                  
         BZ    MGA07                                                            
         OI    TSINDS,TSINODSK     PASS PARAM THROUGH                           
         MVC   TSPAGN,MGATSRPGS                                                 
         NI    TSPAGN,X'7F'        TURN OFF 2 BUFFER FLAG                       
         TM    MGATSRPGS,X'80'     USER WANT BOTH BUFFERS                       
         BZ    *+8                                                              
         OI    TSIND2,TSI2BIGN     SET FLAG FOR TSAR                            
         B     MGA09                                                            
*                                                                               
MGA07    TM    MGAOPT2,MGAOPT2_MINB1 TEST USE MINIO BUFFER 1                    
         BZ    MGA09                                                            
         OI    TSRECI,TSRXTN+TSRMINB1                                           
*&&                                                                             
MGA09    MVI   BYTE,TSAINI         INITIALIZE TSAR                              
         BRAS  RE,CALLTSAR                                                      
         TM    TSINDS,TSIINIOK                                                  
         BNZ   *+6                                                              
         DC    H'0'                NO MORE SPACE ON TEMPEST FILE                
*                                                                               
MGA10    OC    MGAKEY,MGAKEY       ALREADY READ A REC                           
         BNZ   MGA20                                                            
         LA    R3,MGAKEY                                                        
         USING BUYRECD,R3                                                       
         MVC   BUYKAM,MGAAGMD      AGY/MEDIA                                    
         MVC   BUYKCLT,MGACLT      CLIENT                                       
         MVC   BUYKPRD,MGAPRD      PRODUCT                                      
         MVC   BUYKMSTA,MGASTA     MARKET/STATION                               
         MVC   BUYKEST,MGAEST                                                   
*                                                                               
         TM    MGAOPT2,MGAOPT2_CANAD   TEST CANADIAN                            
         BO    MGA20                                                            
         CLI   BUYKSTA,X'E8'       TEST CABLE                                   
         BL    MGA20                                                            
         NI    BUYKSTA+2,X'80'     DROP NETWORK BITS                            
*                                                                               
MGA20    OC    BUYKMSTA(2),BUYKMSTA  TEST MARKET 0 (CANADA)                     
         BNZ   *+8                                                              
         OI    MGAOPT,MGONONC      SKIP NO CHARGE                               
*                                                                               
         MVC   KEY,MGAKEY                                                       
*                                                                               
MGA22    BAS   RE,HIGH                                                          
         B     MGA40                                                            
*                                                                               
MGA30    BAS   RE,SEQ                                                           
*                                                                               
MGA40    CLC   KEY(BUYKBUY-BUYREC),KEYSAVE                                      
         BNE   MGA50                                                            
*                                                                               
MGA42    MVC   MGAKEY,KEY          SET LAST KEY READ                            
         BAS   RE,GETREC                                                        
*                                                                               
         BRAS  RE,PROCBUY          PROCESS THE BUY                              
         BE    MGA30                                                            
*                                                                               
MGA44    TM    MGAOPT,MGONONC      DID WE ALREADY TRY SKIPPIN NO CHARGE         
         BO    MGABX               JUST GIVE UP !!!                             
         MVI   MGAERR,0            DON'T FORGET THIS!                           
         XC    MGAKEY,MGAKEY                                                    
         OI    MGAOPT,MGONONC      SKIP NO CHARGE                               
         B     MGA05                                                            
*                                                                               
MGA50    TM    MGAOPT2,MGAOPT2_CANAD                                            
         BO    MGA60                                                            
         CLI   MGASTA+2,X'E8'      TEST CABLE                                   
         BL    MGA60                                                            
                                                                                
* AT LEAST CHANGE OF ESTIMATE                                                   
                                                                                
MGA52    DS    0H                                                               
                                                                                
* RELEASE FILE LOCKS BUT NOT DIRECTORY TO AFFORD SOME PROTECTION                
                                                                                
         GOTO1 CDATAMGR,DMCB,=C'DMUNLK',=C'SPTFILE'  RLSE FILE LOCKS            
*                                                                               
         CLC   KEY(6),KEYSAVE      SAME A-M/CLT/PRD/MKT                         
         BNE   MGA60                                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,KEY+6          GET STATION/NTWK                             
         SR    RE,RE                                                            
         ICM   RE,7,MGAKEY+6       GET PREVOUS STATION/NTWK                     
         SRL   R0,7                DROP NETWORK                                 
         SRL   RE,7                                                             
         CR    R0,RE               TEST STILL SAME STA (W/O NTWK)               
         BNE   MGA60                                                            
*                                                                               
         CLC   KEY+9(1),MGAEST     COMPARE ESTIMATE                             
         BE    MGA42               RIGHT EST --> PROCESS                        
         BL    MGA54               LOW - READ FOR EST                           
*                                                                               
         MVC   KEY+9(4),=4X'FF'    EST HIGH - READ FOR NEXT STA                 
         BAS   RE,HIGH                                                          
         B     MGA52                                                            
*                                                                               
MGA54    MVC   KEY+9(1),MGAEST                                                  
         XC    KEY+10(3),KEY+10                                                 
         BAS   RE,HIGH             READ FOR ESTIMATE                            
         B     MGA52                                                            
*                                                                               
MGA60    BAS   RE,WRAPUP                                                        
         BNE   MGA44                                                            
*                                                                               
MGABX    J     XIT                                                              
         EJECT                                                                  
*====================================================================           
* BUILD A TABLE FOR A SINGLE BUY LINE                                           
*====================================================================           
                                                                                
BLINE    NTR1                                                                   
         MVI   MGAERR,MGAQRMIS                                                  
         OC    MGABUY,MGABUY       MAKE SURE A(BUY) IS PASSED                   
         BZ    BLNX                                                             
         OC    MGATAB,MGATAB       AND A(TABLE)                                 
         BZ    BLNX                                                             
*                                                                               
         L     R1,MGATAB           A(TABLE)                                     
         LH    R2,MGATABLN                                                      
         TM    MGAOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGATABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX          SET A(END OF TABLE)                          
*                                                                               
         L     R4,MGATAB           A(TABLE)                                     
         TM    MGAOPT,MGOPENTB     PUT AT END OF TABLE                          
         BNO   BL20                                                             
*                                                                               
BL10     OC    0(MGERECL,R4),0(R4) YES - FIND END OF TABLE                      
         BZ    BL30                                                             
         LA    R4,MGERECL(R4)      BUMP TO NEXT ENTRY                           
         CR    R4,R1               R1 POINTS TO END OF TABLE                    
         BNL   TABFULER                                                         
         B     BL10                                                             
*                                                                               
BL20     XC    0(MGERECL,R4),0(R4)                                              
*                                                                               
BL30     ST    R4,ANENTRY          A(NEXT ENTRY)                                
*                                                                               
         MVI   MGAERR,0                                                         
         MVC   MGAIO,MGABUY        SET A(IO AREA)                               
         BRAS  RE,PROCBUY                                                       
         TM    MGAOPT,MGOPENTB     DON'T DO TOTALS YET                          
         BO    BLNX                                                             
         BAS   RE,WRAPUP                                                        
*                                                                               
BLNX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        JUST DO TOTALS                                                         
*                                                                               
TOTAL    NTR1                                                                   
         L     R1,MGATAB           A(TABLE)                                     
         LH    R2,MGATABLN                                                      
         TM    MGAOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGATABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX          SET A(END OF TABLE)                          
*                                                                               
         L     R4,MGATAB           A(TABLE)                                     
         TM    MGAOPT,MGOPENTB     PUT AT END OF TABLE                          
         BNO   TO20                                                             
*                                                                               
TO10     OC    0(MGERECL,R4),0(R4) YES - FIND END OF TABLE                      
         BZ    TO30                                                             
         LA    R4,MGERECL(R4)      BUMP TO NEXT ENTRY                           
         CR    R4,R1               R1 POINTS TO END OF TABLE                    
         BNL   TABFULER                                                         
         B     TO10                                                             
*                                                                               
TO20     XC    0(MGERECL,R4),0(R4)                                              
*                                                                               
TO30     ST    R4,ANENTRY          A(NEXT ENTRY)                                
         BAS   RE,WRAPUP                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        WRAP UP TOTALS & SORT FOR OFFLINE                                      
*                                                                               
WRAPUP   NTR1                                                                   
         BAS   RE,TOTENTRY                                                      
         BNE   NO                  TSAR BUFFER FULL                             
         OC    MGATAB,MGATAB       IS THERE A TABLE                             
         BZ    WRX                                                              
         LH    R4,MGACNT                                                        
         LTR   R4,R4                                                            
         BZ    WRX                                                              
         GOTO1 CXSORT,DMCB,MGATAB,(R4),MGERECL,MGEKEYL,0                        
         BAS   RE,TABTOTAL         DO TOTALS FOR TABLE                          
         LH    R4,MGACNT                                                        
         LTR   R4,R4                                                            
         BNZ   *+6                 SORT AGAIN                                   
         DC    H'0'                                                             
         GOTO1 CXSORT,DMCB,MGATAB,(R4),MGERECL,MGEKEYL,0                        
WRX      B     YES                                                              
         EJECT                                                                  
*=============================================================                  
* TRANSLATE BINARY VALUE IN BCODE                                               
*        TO ALPHA  VALUE IN CODE                                                
*=============================================================                  
                                                                                
TRANSLAT NTR1                                                                   
         XC    ENTRY,ENTRY         CLEAR ENTRY                                  
         XC    BCODE,BCODE                                                      
         XC    CODE,CODE                                                        
         MVI   BYTE2,0             ASSUME ENTRY TYPE=MISSED                     
*                                                                               
         MVC   MGAIO,MGABUY        SET A(IO AREA)                               
         L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
*                                                                               
         OC    MGAELEM,MGAELEM     IS A(ELEMENT PASSED)                         
         BNZ   TRNS10                                                           
*                                                                               
         CLI   BDMGDATE,X'C0'      SEE IF NEW MAKEGOOD                          
         BH    TRNS05                                                           
         OC    BDCOST,BDCOST                                                    
         BNZ   TRNS02                                                           
         MVI   BCODE,X'FD'         BONUS                                        
         MVI   BYTE2,1             SET ENTRY TYPE TO MAKEGOOD                   
         B     TRNS30                                                           
*                                                                               
TRNS02   OC    BDMGDATE,BDMGDATE   SEE IF OLD STYLE MAKEGOOD                    
         BZ    TRNSX                                                            
         MVC   CODE,=C'MG'                                                      
         B     *+10                                                             
*                                                                               
TRNS05   MVC   CODE,BDMGDATE                                                    
         MVI   BYTE2,1             SET ENTRY TYPE TO MAKEGOOD                   
         B     TRNS30                                                           
*                                                                               
         USING REGELEM,R3                                                       
TRNS10   L     R3,MGAELEM                                                       
         ST    R3,SAVER3                                                        
*                                                                               
         TM    RSTATUS,X'40'       TEST MINUSSED                                
         BNO   TRNSX               NO                                           
         CLI   1(R3),14            MAKE SURE THAT IT'S ALLOCATED                
         BL    TRNSX                                                            
*                                                                               
         MVI   BCODE,X'FF'         SET PRE-EMPTED                               
         TM    RSTATUS,X'02'       TEST MADEGOOD                                
         BZ    TRNS30              NO                                           
*                                                                               
         LLC   R0,13(R3)                                                        
         STH   R0,BCODE            SET GROUP CODE TO BE XLATED                  
*                                                                               
         CLI   13(R3),X'FF'                                                     
         BNE   TRNS30                                                           
*                                                                               
         LLC   R0,1(R3)                                                         
         AR    R3,R0               POINT TO 0C ELEM                             
*                                                                               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         CLI   0(R3),X'19'                                                      
         BNE   TRNS30                                                           
*                                                                               
TRNS20   MVC   CODE,2(R3)          CODE IN X'19' IS TRANSLATED NOW              
         B     TRNSX                                                            
*                                                                               
TRNS30   BRAS  RE,TRANSCD          TRANSLATE CODE X'01-X'7F'TO A0...            
*                                                                               
TRNSX    MVC   MGQCODE,CODE                                                     
         MVC   MGBCODE,BCODE                                                    
         MVC   MGAENTRY+MGECODE-MGENTRYD,CODE  AND HERE TOO!                    
         B     XIT                                                              
         EJECT                                                                  
*==========================================================                     
* PUT OUT TOTAL ENTRY                                                           
*==========================================================                     
                                                                                
TOTENTRY NTR1                                                                   
         OC    MGATAB,MGATAB       IF AN AREA IS PASSED TO US                   
         BNZ   TEX                    SKIP                                      
         BAS   RE,CLRACCUM         CLEAR ACCUMULATORS                           
*                                                                               
         XC    THISCODE,THISCODE                                                
         XC    THISSTA,THISSTA                                                  
*                                                                               
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
*                                                                               
         XC    TSRNUM,TSRNUM                                                    
         XC    MGECODE(MGERECL),MGECODE            CLR REC                      
         MVI   BYTE,TSARDH         READ HIGH                                    
         B     *+8                                                              
*                                                                               
TE10     MVI   BYTE,TSANXT         READ HIGH                                    
         BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BO    TE60                                                             
*                                                                               
         CLC   MGECODE,THISCODE    TEST SAME CODE                               
         BE    TE30                                                             
*                                                                               
TE12     OC    THISCODE,THISCODE   SKIP FIRST TIME                              
         BZ    TE30                                                             
         CLC   MGECODE,=X'FEFE'    IS THIS THE GRAND TOTAL RECORD               
         BE    TE70                                                             
         BAS   RE,ADDTOT           ADD TOTAL RECORD                             
         BNE   NO                  TSAR BUFFER FULL                             
         XC    MISSTOT,MISSTOT     CLEAR ACCUMULATORS                           
         XC    MGTOT,MGTOT                                                      
         XC    MISSRTG,MISSRTG                                                  
         XC    MGRTG,MGRTG                                                      
         B     TE40                                                             
*                                                                               
TE30     DS    0H                                                               
         CLI   MGETYPE,X'FE'       IS THIS THE TOTAL RECORD                     
         BNE   TE40                                                             
         MVI   BYTE3,C'Y'          THIS CODE'S TOTAL DOES EXIST                 
         BAS   RE,PUTTOT           PUT TOTAL RECORD                             
         B     TE50                                                             
*                                                                               
TE40     MVI   BYTE3,C'N'          THIS CODE'S TOTAL DOESN'T EXIST              
         BAS   RE,ACCUM            ACCUMULATE THIS ENTRY                        
*                                                                               
TE50     B     TE10                                                             
         EJECT                                                                  
TE60     OC    THISCODE,THISCODE   IF THERE WAS NOTHING - EXIT                  
         BZ    TEX                                                              
         BAS   RE,ADDTOT           ADD LAST TOTAL RECORD                        
         BNE   NO                  TSAR BUFFER FULL                             
         MVI   BYTE3,C'N'                                                       
         MVC   THISCODE,=X'FEFE'   SET FOR GRAND TOTAL                          
         XC    THISSTA,THISSTA                                                  
         MVC   MISSTOT,GMISSTOT    SET GRAND TOTAL                              
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,ADDTOT           ADD TOTAL RECORD                             
         BNE   NO                  TSAR BUFFER FULL                             
         B     TEX                                                              
*                                                                               
TE70     MVC   MISSTOT,GMISSTOT    SET GRAND TOTAL                              
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,PUTTOT           PUT GRAND TOTAL RECORD                       
*                                                                               
TEX      B     YES                                                              
         EJECT                                                                  
*                                                                               
*        PUT A TOTAL RECORD                                                     
*                                                                               
TABTOTAL NTR1                                                                   
         XC    GMISSTOT,GMISSTOT                                                
         XC    GMGTOT,GMGTOT                                                    
         XC    GMISSRTG,GMISSRTG                                                
         XC    GMGRTG,GMGRTG                                                    
*                                                                               
         BAS   RE,CLRACCUM                                                      
         XC    THISCODE,THISCODE                                                
         L     R4,MGATAB           A(TABLE)                                     
         USING MGENTRYD,R4                                                      
         OC    0(MGERECL,R4),0(R4)   IF THE TABLE IS EMPTY                      
         BZ    TTX                      EXIT                                    
*                                                                               
TT10     OC    0(MGERECL,R4),0(R4) END OF TABLE                                 
         BZ    TT30                                                             
         CLI   MGETYPE,X'FE'       DID WE REACH THE TOTALS                      
         BE    TT30                                                             
         CLC   MGECODE,THISCODE    SAME CODE                                    
         BE    TT20                                                             
*                                                                               
TT12     OC    THISCODE,THISCODE   SKIP FIRST TIME                              
         BZ    TT20                                                             
         BAS   RE,TABTOT                                                        
*                                                                               
TT20     BAS   RE,ACCUM            ACCUMULATE THIS ENTRY                        
         LA    R4,MGERECL(R4)      BUMP TO NEXT POSITION                        
         B     TT10                                                             
*                                                                               
TT30     BAS   RE,TABTOT           ADD LAST TOTAL                               
         MVC   THISCODE,=X'FEFE'   SET GRAND TOTAL                              
         XC    THISSTA,THISSTA                                                  
         MVC   MISSTOT,GMISSTOT                                                 
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,TABTOT           ADD GRAND TOTAL                              
*                                                                               
TTX      B     XIT                                                              
         EJECT                                                                  
*        ACCUMULATE AMOUNTS IN THIS ENTRY                                       
*                                                                               
ACCUM    NTR1                                                                   
         MVC   THISCODE,MGECODE                                                 
         MVC   THISSTA,MGESTA                                                   
*                                                                               
         L     R1,MGECOST          COST                                         
         SR    R2,R2                                                            
         ICM   R2,15,MGERTGB       BINARY RATING                                
         CLI   MGETYPE,0                                                        
         BNE   AC40                                                             
         L     R5,MISSTOT          MISSED TOTALS                                
         AR    R5,R1                                                            
         ST    R5,MISSTOT                                                       
         L     R5,GMISSTOT                                                      
         AR    R5,R1                                                            
         ST    R5,GMISSTOT                                                      
*                                                                               
         L     R5,MISSRTG                                                       
         AR    R5,R2                                                            
         ST    R5,MISSRTG                                                       
         L     R5,GMISSRTG                                                      
         AR    R5,R2                                                            
         ST    R5,GMISSRTG                                                      
         B     ACX                                                              
*                                                                               
AC40     L     R5,MGTOT            MAKEGOOD TOTALS                              
         AR    R5,R1                                                            
         ST    R5,MGTOT                                                         
         L     R5,GMGTOT                                                        
         AR    R5,R1                                                            
         ST    R5,GMGTOT                                                        
*                                                                               
         L     R5,MGRTG                                                         
         AR    R5,R2                                                            
         ST    R5,MGRTG                                                         
         L     R5,GMGRTG                                                        
         AR    R5,R2                                                            
         ST    R5,GMGRTG                                                        
*                                                                               
ACX      B     XIT                                                              
         EJECT                                                                  
*        ADD A TOTAL RECORD                                                     
*                                                                               
ADDTOT   NTR1                                                                   
         CLI   BYTE3,C'Y'          WAS THE TOTAL RECORD WRITTEN BACK            
         BE    ADX                                                              
         MVC   ENTRY2,ENTRY                                                     
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         XC    ENTRY,ENTRY                                                      
         MVC   MGECODE,THISCODE                                                 
         MVI   MGETYPE,X'FE'                                                    
         MVC   MGESTA,=3X'FF'      FORCE TOT REC HIGH FOR CODE                  
*                                                                               
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
         TM    MGAOPT2,MGAOPT2_NODEMS                                           
         BO    ADDTOT2                                                          
         EDIT  MISSRTG,(4,MGEMSRTE),1                                           
         EDIT  MGRTG,(4,MGEMGRTE),1                                             
*                                                                               
ADDTOT2  MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
*                                                                               
         BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSEDUP       DUPLICATE KEY                                
         BNO   *+6                                                              
         DC    H'0'                NO MORE SPACE ON TEMPEST FILE                
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNO   *+12                                                             
         MVI   MGAERR,MGAQEOF                                                   
         B     NO                                                               
*                                                                               
         MVC   ENTRY,ENTRY2                                                     
         MVI   BYTE,TSARDH         RE-READ LAST RECORD READ                     
         BRAS  RE,CALLTSAR                                                      
*                                                                               
ADX      B     YES                                                              
         EJECT                                                                  
*        ADD TOTAL ENTRY                                                        
*                                                                               
         EJECT                                                                  
*        PUT A TOTAL RECORD                                                     
*                                                                               
TABTOT   NTR1                                                                   
         CLI   MGAERR,MGAQTFUL     TOO MANY ENTRIES                             
         BE    TBX                  YES - DON'T ADD ANY MORE!                   
*                                                                               
         XC    ENTRY,ENTRY                                                      
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         MVC   MGECODE,THISCODE                                                 
         MVC   MGESTA,=3X'FF'      FORCE TOT HI FOR ENTRY                       
         MVI   MGETYPE,X'FE'                                                    
*                                                                               
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
         TM    MGAOPT2,MGAOPT2_NODEMS                                           
         BO    TABTOT10                                                         
*                                                                               
         ICM   R1,15,MGABRDEM                                                   
         JZ    TABTOT2                                                          
         OC    0(3,R1),0(R1)                                                    
         JNZ   TABTOT4                                                          
*                                                                               
TABTOT2  L     R1,MGADEM                                                        
         JZ    TABTOT10                                                         
         OC    0(3,R1),0(R1)                                                    
         JZ    TABTOT10                                                         
*                                                                               
TABTOT4  BAS   RE,ISRATING         SET RTGFLAG                                  
                                                                                
* TEST VALUES TO 2-DECIMAL                                                      
                                                                                
         TM    MGAOPT2,MGAOPT2_2DEC+MGAOPT2_2DECIMP  2-DEC RTGS OR IMPS         
         BZ    TABTOT6                               NO                         
*                                                                               
         L     R1,MISSRTG          ROUND VALUES TO 1-DEC                        
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,MISSRTG                                                       
*                                                                               
         L     R1,MGRTG                                                         
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,MGRTG                                                         
*                                                                               
TABTOT6  CLI   RTGFLAG,C'Y'        TEST RATING                                  
         JNE   TABTOT8                                                          
*                                                                               
         EDIT  MISSRTG,(4,MGEMSRTE),1                                           
         EDIT  MGRTG,(4,MGEMGRTE),1                                             
         J     TABTOT10                                                         
*                                                                               
TABTOT8  EDIT  MISSRTG,(5,MGEMSRTE),1                                           
         EDIT  MGRTG,(5,MGEMGRTE),1                                             
*                                                                               
TABTOT10 L     R1,ANENTRY          SET A(NEXT ENTRY)                            
         MVC   0(MGERECL,R1),ENTRY                                              
         LH    R4,MGACNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         LA    R4,1(R4)                                                         
         STH   R4,MGACNT                                                        
*                                                                               
         LA    R1,MGERECL(R1)      BUMP TO NEXT ENTRY                           
         L     R4,ATABLEX          CHECK IF TABLE IS FULL                       
         CR    R1,R4                                                            
         BNL   TABFULER                                                         
         XC    0(MGERECL,R1),0(R1) CLEAR NEXT POSITION IN TABLE                 
         ST    R1,ANENTRY          SET A(NEXT ENTRY)                            
*                                                                               
         XC    MISSTOT,MISSTOT     CLEAR ACCUMULATORS                           
         XC    MGTOT,MGTOT                                                      
         XC    MISSRTG,MISSRTG                                                  
         XC    MGRTG,MGRTG                                                      
*                                                                               
TBX      B     XIT                                                              
*                                                                               
PUTTOT   NTR1                                                                   
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
         MVI   BYTE,TSAPUT         PUT RECORD TO TSAR                           
         BRAS  RE,CALLTSAR                                                      
         B     XIT                                                              
         SPACE 2                                                                
*        CLEAR THE ACCUMULATORS                                                 
*                                                                               
CLRACCUM NTR1                                                                   
         XC    MISSTOT,MISSTOT     CLEAR ACCUMULATORS                           
         XC    MGTOT,MGTOT                                                      
         XC    MISSRTG,MISSRTG                                                  
         XC    MGRTG,MGRTG                                                      
         B     XIT                                                              
         EJECT                                                                  
*====================================================================           
*        ADD AN ENTRY TO TSAR                                                   
*====================================================================           
                                                                                
ADDENTRY NTR1                                                                   
         MVC   ENTRY,MGAENTRY      SET UP ENTRY PASSED BY USER                  
         MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
         BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSEDUP       DUPLICATE KEY                                
         BNO   *+8                                                              
         MVI   MGAERR,MGAQDUP                                                   
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNO   *+8                                                              
         MVI   MGAERR,MGAQEOF                                                   
         B     XIT                                                              
                                                                                
*====================================================================           
*        FIND AN ENTRY - KEY IN MGAENTRY                                        
*====================================================================           
                                                                                
FNDENTRY NTR1                                                                   
         MVC   ENTRY,MGAENTRY                                                   
         XC    TSRNUM,TSRNUM                                                    
         MVI   BYTE,TSARDH         READ HI                                      
         BRAS  RE,CALLTSAR                                                      
         CLI   TSERRS,0            IS THERE AN ERROR                            
         BE    *+8                                                              
         MVI   MGAERR,MGAQNF                                                    
         B     XIT                                                              
         EJECT                                                                  
*==================================================================             
*        DELETE AN ENTRY FROM TSAR                                              
*==================================================================             
                                                                                
DELENTRY NTR1                                                                   
         MVC   ENTRY,MGAENTRY      SET UP ENTRY PASSED BY USER                  
         MVI   BYTE,TSADEL         DELETE A RECORD FROM TSAR                    
         BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSERNF       RECORD NOT FOUND                             
         BO    DE10                                                             
         LH    R1,MGACNT           DECREMENT NUMBER OF ENTRIES IN TABLE         
         BCTR  R1,0                                                             
         STH   R1,MGACNT                                                        
         B     *+8                                                              
*                                                                               
DE10     MVI   MGAERR,MGAQNF                                                    
         B     XIT                                                              
                                                                                
*==================================================================             
*        FIND NEXT ENTRY - TSRNUM IN MGATSNUM                                   
*==================================================================             
                                                                                
NXTENTRY NTR1                                                                   
         MVC   TSRNUM,MGATSNUM                                                  
         MVI   BYTE,TSANXT         READ NEXT                                    
         BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BE    *+8                                                              
         MVI   MGAERR,MGAQEOF                                                   
         B     XIT                                                              
                                                                                
*==================================================================             
*        GET AN ENTRY - TSRNUM IN MGATSNUM                                      
*==================================================================             
                                                                                
GETENTRY NTR1                                                                   
         MVC   TSRNUM,MGATSNUM                                                  
         MVI   BYTE,TSAGET         READ NEXT                                    
         XC    MGAENTRY,MGAENTRY                                                
         BRAS  RE,CALLTSAR                                                      
         CLI   TSERRS,0                                                         
         BE    GEX                                                              
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNE   GE10                                                             
         MVI   MGAERR,MGAQEOF                                                   
         B     GEX                                                              
*                                                                               
GE10     TM    TSERRS,TSERNF       RECORD NOT FOUND                             
         BNO   GEX                                                              
         MVI   MGAERR,MGAQNF                                                    
*                                                                               
GEX      B     XIT                                                              
         EJECT                                                                  
*==================================================================             
*        SET UP A PRINT LINE                                                    
*==================================================================             
                                                                                
SETPRNT  NTR1                                                                   
         USING MGLINED,R2                                                       
         L     R2,MGALINE                                                       
         USING MGENTRYD,R4                                                      
         LA    R4,MGAENTRY                                                      
         MVC   MGLCODE,MGECODE                                                  
         CLI   MGETYPE,X'FE'       TOTAL ENTRY                                  
         BNE   PR10                                                             
         BAS   RE,PRTTOT                                                        
         B     PRX                                                              
*                                                                               
PR10     CLI   MGETYPE,0                                                        
         BNE   PR20                                                             
         MVI   MGLTYPE,C'-'                                                     
         TM    MGAOPT2,MGAOPT2_CANAD                                            
         BO    PR12                                                             
         CLI   MGESTA,X'E8'                                                     
         BNL   *+10                                                             
PR12     MVC   MGLTYPE(3),=C'MS-'                                               
         LA    R5,MGLMSRTG-1         MISSED RATING TOTAL                        
         B     PR30                                                             
*                                                                               
PR20     DS    0H                                                               
         MVI   MGLTYPE,C'+'                                                     
         TM    MGAOPT2,MGAOPT2_CANAD                                            
         BO    PR22                                                             
         CLI   MGESTA,X'E8'                                                     
         BNL   *+10                                                             
PR22     MVC   MGLTYPE(3),=C'MG+'                                               
         LA    R5,MGLMGRTG-1         MISSED RATING TOTAL                        
*                                                                               
PR30     ICM   R0,15,MGERTGB       BINARY RATING                                
         BAS   RE,PRTRTG                                                        
*                                                                               
         EDIT  MGELINE,(3,MGLLINE),FILL=0                                       
         CLC   MGEDATE,=X'FFFF'                                                 
         BNE   PR40                                                             
         MVC   MGLDATE,=C'*MISSED*'                                             
         B     PR70                                                             
*                                                                               
PR40     GOTO1 CDATCON,DMCB,(2,MGEDATE),(4,MGLDATE)                             
*                                                                               
PR50     CLI   MGESPNUM,1                                                       
         BE    PR60                                                             
         MVI   MGLDATE+5,C'-'                                                   
         LA    R5,MGLDATE+6                                                     
         EDIT  MGESPNUM,(2,(R5)),FILL=0                                         
*                                                                               
PR60     MVC   MGLDAYPT,MGEDAYPT                                                
         EDIT  MGESLN,(3,MGLSLN)                                                
         GOTO1 MGUNTIME,DMCB,MGETIME,MGLTIME,=C'N'                              
*                                                                               
         ICM   R0,15,MGECOST                                                    
         LA    R5,MGLMSCST                                                      
         CLI   MGETYPE,0                                                        
         BE    *+8                                                              
         LA    R5,MGLMGCST                                                      
*                                                                               
         TM    MGESTAT,MGESTAT_NTP TEST DF/NTP BUY                              
         BO    *+8                 YES - DO NOT SHOW COST                       
         BAS   RE,PRTAMT                                                        
*                                                                               
PR70     MVC   MGLPGMNM,MGEPGMNM     PROGRAM                                    
         TM    MGAOPT2,MGAOPT2_CANAD                                            
         BO    PR80                                                             
         CLI   MGESTA,X'E8'          TEST CABLE                                 
         BL    PR80                                                             
         OC    MGESTA,MGESTA                                                    
         BZ    PR80                                                             
         CLI   MGETYPE,X'FE'       TEST TOTAL RECORD                            
         BE    PR80                YES - NO STATION                             
* DO MSUNPK CALL                                                                
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MGESTA                                                 
         ICM   RF,15,MGMSUNPK                                                   
         BZ    PR72                                                             
         GOTO1 (RF),DMCB,(X'80',WORK),WORK+10,WORK+15                           
         MVC   MGLNET,WORK+20      MOVE NETWORK                                 
         B     PR80                                                             
*                                                                               
PR72     ICM   RF,15,MGSTAPAK      MUST PASS MSUNPK OR STAPACK                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
* DO STAPACK CALL                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         ST    R9,STAPACOM                                                      
         MVC   STAPAGY,MGAQAGY                                                  
         MVI   STAPMED,C'T'                                                     
         MVI   STAPCTRY,C'U'                                                    
         MVC   STAPSTA,MGESTA                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 (RF),(R1)                                                        
         CLI   (STAPERR-STAPACKD)+WORK,0                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MGLNET,(STAPQNET-STAPACKD)+WORK                                  
*                                                                               
PR80     DS    0H                                                               
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
*==================================================================             
*        PRINT OUT A TOTAL LINE                                                 
*==================================================================             
                                                                                
PRTTOT   NTR1                                                                   
         USING MGLINED,R2                                                       
         USING MGENTRYD,R4                                                      
         CLC   =X'FEFE',MGECODE    GRAND TOTAL                                  
         BNE   *+10                                                             
         MVC   MGLCODE-1(4),=C'GRND'                                            
         MVC   MGLTYPE(4),=C'TOTL'                                              
*                                                                               
         L     R0,MGETMISS         MISSED TOTAL                                 
         LA    R5,MGLMSCST                                                      
         BAS   RE,PRTAMT                                                        
*                                                                               
         LA    R5,MGLMSRTG-1       MISSED RATING TOTAL                          
         L     R0,MGEMSRTG                                                      
         BAS   RE,PRTRTG                                                        
*                                                                               
PT10     LA    R5,MGLMGCST        MAKEGOOD TOTAL                                
         L     R0,MGETMG                                                        
         BAS   RE,PRTAMT                                                        
*                                                                               
         LA    R5,MGLMGRTG-1       MAKEGOOD RATING TOTAL                        
         L     R0,MGEMGRTG                                                      
         BAS   RE,PRTRTG                                                        
*                                                                               
PT20     B     XIT                                                              
                                                                                
*===============================================================                
*        EDIT OUT TO TOTAL LINE                                                 
*===============================================================                
                                                                                
PRTAMT   NTR1                                                                   
         TM    MGAOPT2,MGAOPT2_NOCOST                                           
         BO    XIT                                                              
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(8,0(R5)),FLOAT=$,ZERO=NOBLANK                              
         B     XIT                                                              
                                                                                
*=============================================================                  
*        EDIT OUT RATINGS TO TOTAL LINE                                         
*=============================================================                  
                                                                                
PRTRTG   NTR1                                                                   
         TM    MGAOPT2,MGAOPT2_NODEMS                                           
         BO    XIT                                                              
         TM    MGAOPT2,MGAOPT2_2DEC                                             
         BZ    PRTRTG10                                                         
         CLI   MGETYPE,X'FE'       TEST TOTAL                                   
         BE    PRTRTG2                                                          
         EDIT  (R0),(8,0(R5)),2                                                 
         B     XIT                                                              
                                                                                
* IF TOTALS TO 2-DEC, ROUND AND PRINT TO 1 DECIMAL ONLY                         
                                                                                
PRTRTG2  LR    R1,R0                                                            
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    R0,R1                                                            
*                                                                               
PRTRTG10 EDIT  (R0),(8,0(R5)),1                                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        READ HI - MGAENTRY SET                                                 
*                                                                               
RDHI     NTR1                                                                   
         XC    TSRNUM,TSRNUM                                                    
         MVC   ENTRY,MGAENTRY                                                   
         MVI   BYTE,TSARDH         READ HIGH                                    
         BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BE    *+8                                                              
         MVI   MGAERR,MGAQEOF                                                   
         B     XIT                                                              
         EJECT                                                                  
*===============================================================                
* THESE CALLS INTERFACE TO GETBUY!                                              
*===============================================================                
                                                                                
HIGH     NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVI   GBYACT,C'H'         DMRDHI                                       
         B     DIR                                                              
*                                                                               
SEQ      NTR1                                                                   
         MVI   GBYACT,C'S'         DMRSEQ                                       
*                                                                               
DIR      XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
         LA    RE,KEY                                                           
         ST    RE,GBYKEYIN                                                      
         ST    RE,GBYKEYOT                                                      
         MVC   GBYCOMF,MGAACOM                                                  
         MVC   GBY1OR2,MG1OR2                                                   
*                                                                               
         GOTO1 MGGETBUY,GETBLK                                                  
         B     DIRX                                                             
*                                                                               
GETREC   NTR1                                                                   
         MVI   GBYACT,C'G'         GETREC                                       
*                                                                               
         XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
         LA    RE,KEY+14                                                        
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,MGAIO                                                     
         LA    RE,DMWORK                                                        
         ST    RE,GBYDMWRK                                                      
         MVC   GBYCOMF,MGAACOM                                                  
         MVC   GBY1OR2,MG1OR2                                                   
*                                                                               
         GOTO1 MGGETBUY,GETBLK                                                  
*                                                                               
DIRX     CLI   DMCB+8,0            TEST DATAMGR ERROR                           
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        CALL TSAR - USE 2 BUFFERS/48 PAGES/NO DISK                             
*                                                                               
CALLTSAR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R8,TSARBLK                                                       
         USING TSARD,R8                                                         
*                                                                               
         TM    MGAOPT2,MGAOPT2_NODSK   TEST NO DISK TO BE USED                  
         JZ    CLTS010                                                          
         OI    TSINDS,TSINODSK     PASS PARAM THROUGH                           
         MVC   TSPAGN,MGATSRPGS                                                 
         NI    TSPAGN,X'7F'        TURN OFF 2 BUFFER FLAG                       
         TM    MGATSRPGS,X'80'     USER WANT BOTH BUFFERS                       
         JZ    *+8                                                              
         OI    TSIND2,TSI2BIGN     SET FLAG FOR TSAR                            
         J     CLTS020                                                          
*                                                                               
CLTS010  TM    MGAOPT2,MGAOPT2_MINB1 TEST USE MINIO BUFFER 1                    
         BZ    CLTS020                                                          
         OI    TSRECI,TSRXTN+TSRMINB1                                           
*                                                                               
CLTS020  MVC   TSACOM,MGAACOM      A(COMFACS)                                   
         MVI   TSPAGL,1            USE TEMPSTR PAGE 1                           
         MVI   TSKEYL,MGEKEYL      KEY LENGTH                                   
         LA    R1,MGERECL          RECORD LENGTH                                
         STH   R1,TSRECL                                                        
         OI    TSINDS,TSIXTTWA     18K RECORDS                                  
         MVC   TSACTN,BYTE                                                      
         LA    R1,ENTRY            A(RECORD)                                    
         ST    R1,TSAREC                                                        
         GOTO1 MGATSAR,TSARBLK                                                  
         MVC   MGATSNUM,TSRNUM     RETURN TSAR RECORD NUMBER                    
         MVC   MGAENTRY,ENTRY                                                   
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* PROCESS A BUY RECORD                                                          
* CREATE TSAR ENTRIES FOR ALL MISSED SPOTS                                      
* AND FOR ALL SPOTS ON A MAKEGOOD LINE                                          
*==============================================================                 
                                                                                
PROCBUY  NTR1  BASE=*,LABEL=*                                                   
         MVI   BUYCIND,0                                                        
         MVI   BUYSTAT,0                                                        
*                                                                               
         L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
*                                                                               
         TM    15(R3),X'80'        RECORD IS REALLY DELETED                     
         JO    YES                 JUST GET OUT                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
         TM    BDCIND2,X'20'       TEST CANADIAN                                
         BO    *+16                                                             
         TM    BDCIND2,X'10'       TEST COST IN DOLLARS                         
         BZ    *+8                                                              
         MHI   R0,100                                                           
*                                                                               
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    PB5A                                                             
* BDCIND IS CHARACTER                                                           
         CLI   BDCIND,C'P'         TEST NTP RATE                                
         BNE   *+8                                                              
         OI    BUYSTAT,MGESTAT_NTP                                              
*                                                                               
         TM    BDCIND2,BDC2NEG     TEST MINUS RATE                              
         BZ    PB5C                                                             
         OI    BUYCIND,X'01'       (ONLY SAVES THAT IT WAS NEGATIVE)            
         B     PB5B                                                             
* BDCIND IS A BITMASK                                                           
PB5A     MVC   BUYCIND,BDCIND      SAVE COST IND                                
         CLI   BUYCIND,0           TEST NTP RATE                                
         BNE   *+8                                                              
         OI    BUYSTAT,MGESTAT_NTP                                              
*                                                                               
         TM    BDCIND,X'01'        TEST MINUS RATE                              
         BZ    PB5C                                                             
*                                                                               
PB5B     LNR   R0,R0                                                            
*                                                                               
PB5C     ST    R0,BUYCOST          SAVE BUYLINE COST                            
*                                                                               
         CLC   =C'DF',BUYALPHA     ONLY DF HAS NTP TRADE RATE                   
         BE    *+8                                                              
         NI    BUYSTAT,X'FF'-MGESTAT_NTP                                        
*                                                                               
         XC    ELEMDT,ELEMDT       CLEAR LAST DATE                              
         MVI   ELEMNO,0            RESET SPOT NUMBER                            
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
PB10     L     R3,MGAIO                                                         
         LA    R3,24(R3)                                                        
         USING REGELEM,R3                                                       
         BRAS  RE,NEXTEL           FIND 0B/0C ELEMENT                           
         BNE   PB90                                                             
*                                                                               
PB20     ST    R3,SAVER3                                                        
         SR    R0,R0                                                            
         STH   R0,BCODE            CLEAR BCODE                                  
         XC    BCODESTA,BCODESTA   AND 'OTHER' STATION                          
*                                                                               
         XC    PRD1(4),PRD1                                                     
         TM    MGAOPT,MGOFLTDT     FILTER BY DATE                               
         BNO   PB30                                                             
         CLC   RDATE,MGSFLTDT      START DATE                                   
         BL    PB80                                                             
         CLC   RDATE,MGEFLTDT      END DATE                                     
         BH    PB80                                                             
*                                                                               
PB30     TM    RSTATUS,X'80'       IF MINUS                                     
         BO    PB80                SKIP                                         
*                                                                               
         CLC   RDATE,ELEMDT        SAME DATE                                    
         BE    *+8                                                              
         MVI   ELEMNO,0            NO - RESET SPOT NUMBER                       
         MVC   ELEMDT,RDATE                                                     
*                                                                               
         IC    R1,ELEMNO           INC SPOT NUMBER                              
         LA    R1,1(R1)                                                         
         STC   R1,ELEMNO                                                        
*                                                                               
         MVI   BYTE2,0             SET ENTRY TYPE TO MISSED                     
         CLI   1(R3),14            MAKE SURE THAT IT'S ALLOCATED                
         BL    PB80                                                             
*                                                                               
         MVC   PRD1,10(R3)                                                      
         MVC   SLN1,11(R3)                                                      
         CLI   1(R3),X'0E'         IS THERE A PRD2                              
         BE    PB40                                                             
         MVC   PRD2,14(R3)                                                      
         MVC   SLN2,15(R3)                                                      
*                                                                               
PB40     DS    0H                                                               
         CLI   MGAPRD,X'FF'                                                     
         BE    PB42                                                             
         CLC   PRD1,MGAPRD         REQUEST PRD MATCH ANY ALLOC PRD              
         BE    PB42                                                             
         CLC   PRD2,MGAPRD                                                      
         BNE   PB80                                                             
*                                                                               
PB42     MVC   LASTCOST,BUYCOST                                                 
         TM    RSTATUS,X'20'       TEST RATE OVERRIDE                           
         BZ    PB44                                                             
         L     RE,MGAIO            POINT TO BUYREC                              
         LA    RE,BUYMSTA-BUYRECD(RE)                                           
         OC    0(2,RE),0(RE)       TEST MARKET 0 (CANADA)                       
         BZ    PB44                YEP - TAKE ORIG INPUT NETWORK COST           
         SR    R0,R0                                                            
         ICM   R0,7,RPCOST                                                      
         TM    BUYCIND,X'01'       TEST MINUS RATE                              
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         ST    R0,LASTCOST                                                      
*                                                                               
PB44     TM    BUYSTAT,MGESTAT_NTP TRADE BUY = 0 COST BUT NOT A BONUS           
         BZ    *+14                                                             
         XC    LASTCOST,LASTCOST   COST=$0                                      
         B     PB50                                                             
*                                                                               
         OC    LASTCOST,LASTCOST   COST=$0                                      
         BNZ   PB50                                                             
         TM    RSTATUS,X'40'       TEST MINUSSED AND                            
         BNO   *+12                                                             
         TM    RSTATUS,X'02'       MADE GOOD ON ANOTHER LINE                    
         BO    PB50                YES - IGNORE BONUS                           
*                                                                               
         L     RE,MGAIO                                                         
         LA    RE,BDMGDATE-BUYRECD(RE)                                          
         CLI   0(RE),X'C0'         TEST THIS IS A NEW MAKEGOOD LINE             
         BH    PB50                YES - IGNORE BONUS                           
         TM    RSTATUS,X'40'       TEST MINUSSED                                
         BO    PB50                YES - PRE-EMP OVERRIDES NO CHARGE            
*                                                                               
         TM    MGAOPT,MGONONC      SKIP NO CHARGE                               
         BNO   PB45                                                             
         CLC   NUMNC,=H'200'       OVER 200 BONUS WITH SKIP NO CHARGE           
         BL    PB80                                                             
         MVI   BCODE,X'FC'         THEN ADD ONE TO INDICATE THAT                
         MVI   BYTE2,2             NO CHARGES WERE SKIPPED                      
         XC    NUMNC,NUMNC         ONLY ADD ONCE                                
         B     PB64                                                             
PB45     MVI   BCODE,X'FD'         BONUS                                        
         MVI   BYTE2,2             MAKEGOOD                                     
         B     PB64                                                             
*                                                                               
PB50     TM    RSTATUS,X'40'       TEST MINUSSED                                
         BZ    PB60                NO                                           
         MVI   BCODE,X'FF'         SET PRE-EMPTED                               
         TM    RSTATUS,X'02'       IS IT MADE GOOD ON ANOTHER LINE              
         BO    PB52                YES - PROCESS MISSED                         
*                                                                               
         TM    MGAOPT2,MGAOPT2_CANAD                                            
         BO    PB70                                                             
         L     RE,MGAIO            POINT TO BUYREC                              
         CLI   BUYKSTA-BUYRECD(RE),X'E8'      TEST CABLE                        
         BL    *+10                           NO                                
         MVC   BCODESTA,BUYKSTA-BUYRECD(RE)   NEED TO DISPLAY STATION           
*                                                                               
         CLI   BDMGDATE-BUYRECD(RE),X'C0'  IS THIS A MAKEGOOD LINE              
         BH    PB80                NO - IGNORE                                  
         B     PB70                                                             
                                                                                
* SPOT HAS BEEN MADE GOOD                                                       
* USE MGCODE FROM REGEL+13 UNLESS X'19' ELEM FOLLOWS -OTO                       
                                                                                
PB52     TM    MGAOPT,MGOPRONL     TEST PREEMPTS ONLY REQUEST                   
         BO    PB80                YES - SKIP                                   
*                                                                               
         MVI   BCODE,0                                                          
         MVC   BCODE+1(1),13(R3)   MOVE MG CODE FROM REGEL                      
*                                                                               
         CLC   BCODE,=H'240'                                                    
         BH    PB54                                                             
         BRAS  RE,TRANSCD          GET ALPHA VALUE IN CODE                      
         B     PB64                                                             
*                                                                               
PB54     LLC   R0,RLEN                                                          
         AR    R3,R0               POINT TO NEXT ELEM                           
         CLI   0(R3),X'0C'         NEXT ELEM SHOULD BE MINUS OTO                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'19'         TEST MGCODE ELEM                             
         BNE   PB64                                                             
         MVC   CODE,2(R3)          MOVE ALPHA MGCODE                            
         B     PB62                AND GO GET BINARY EQUIV                      
*                                                                               
PB60     L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
*                                                                               
         CLI   BDMGDATE,0          TEST LINE IS A MAKEGOOD                      
         BZ    PB80                NO - IGNORE                                  
         MVI   BYTE2,1             SET ENTRY TYPE TO MAKEGOOD                   
*                                                                               
         MVC   CODE,BDMGDATE         ED TO TRANSLATE TO BINARY                  
*                                                                               
PB62     BRAS  RE,TRANSBIN         GET BINARY EQUIV IN BCODE                    
*                                                                               
PB64     CLI   BYTE2,1             TEST MAKEGOOD                                
         BNE   PB66                                                             
         TM    MGAOPT,MGOPRONL     TEST PREEMPTS ONLY REQUEST                   
         BO    PB80                YES - IGNORE                                 
                                                                                
*==========================================================                     
* CREATE ENTRY FOR THIS SPOT                                                    
*==========================================================                     
                                                                                
PB66     CLI   MGAPRD,X'FF'                                                     
         BE    PB70                                                             
         CLC   PRD1,MGAPRD                                                      
         BE    PB70                                                             
         CLC   PRD2,MGAPRD                                                      
         BNE   PB80                                                             
*                                                                               
PB70     TM    MGAOPT,MGOFLTPR     FILTER BY PRODUCT                            
         BNO   PB72                                                             
         BRAS  RE,CHKPRFLT         GO CHECK PRODUCT FILTERS                     
         BNE   PB80                                                             
*                                                                               
PB72     TM    MGAOPT,MGONOPR      SKIP PRE-EMPTS                               
         BNO   PB74                                                             
         CLI   BCODE,X'FF'         PRE- EMPTED                                  
         BE    PB80                                                             
*                                                                               
PB74     CLI   BYTE2,1             TEST MAKEGOOD                                
         BE    PB76                YES - CODE IS ALREADY TRANSLATED             
         BL    *+8                                                              
         MVI   BYTE2,1                                                          
         BRAS  RE,TRANSCD          TRANSLATE CODE X'01-X'7F'TO A0-...           
*                                                                               
PB76     BRAS  RE,BLDENTRY         YES - BUILD ENTRY TO TABLE                   
         JNE   NO                                                               
         BRAS  RE,BLDTABLE         PUT ENTRY TO TABLE/TSAR/GO TO HOOK           
         JNE   NO                                                               
*                                                                               
         CLI   MGAERR,MGAQTFUL     TABLE FULL?                                  
         JE    NO                   YES - GET OUT                               
*                                                                               
         CLI   MGAERR,MGAQEOF      TOO MANY ENTRIES                             
         BNE   PB80                                                             
         CLC   NUMNC,=H'200'       OVER 200 NO CHARGES                          
         BL    PB80                                                             
         XC    NUMNC,NUMNC                                                      
         J     NO                  START OVER WITHOUT NO CHARGE BUYS            
*                                                                               
PB80     L     R3,SAVER3                                                        
         BRAS  RE,NEXTEL           GO FIND NEXT ELEMENT                         
         BE    PB20                                                             
*                                                                               
PB90     BRAS  RE,TSTSPCL          TEST FOR SPECIAL CASE                        
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* TRANSLATE BINARY VALUES 1-240 TO A0-A9,B0-B9,Z0-Z9                            
* HIGHER VALUES GO AA-AZ,BA-BZ,ZA-ZZ                                            
*=============================================================                  
                                                                                
TRANSCD  NTR1  BASE=*,LABEL=*                                                   
         CLI   BCODE,X'FD'         BONUS                                        
         BNE   TC01                                                             
         MVC   CODE,=C'*N'                                                      
         B     TCX                                                              
*                                                                               
TC01     CLI   BCODE,X'FC'         BONUS LINE INDICATING BONUS' HAVE            
         BNE   TC02                BEEN SKIPPED - TOO MANY OF THEM              
         MVC   CODE,=C'*X'                                                      
         B     TCX                                                              
*                                                                               
TC02     CLI   BYTE2,1             IS THIS MAKEGOOD                             
         BE    TC05                                                             
         CLI   BCODE,X'FF'         MISSED BUT NOT MADEGOOD                      
         BNE   TC05                                                             
         MVC   CODE,=C'*P'                                                      
         B     TCX                                                              
*                                                                               
TC05     MVC   CODE,BCODE                                                       
         CLI   CODE,C'A'           TEST ALREADY TRANSLATED                      
         BNL   TCX                                                              
         SR    R0,R0                                                            
         LH    R1,BCODE                                                         
         CLC   BCODE,=H'240'       A0-A9,B0-B9,...Z0-Z9 <= 240                  
         BH    TC10                                                             
         BCTR  R1,0                MAKE IT ZERO-BASED                           
         D     R0,=F'10'                                                        
         IC    R1,ALPHATAB(R1)     GET ALPHA CHAR FROM TABLE                    
         STC   R1,CODE                                                          
         STC   R0,CODE+1                                                        
         OI    CODE+1,X'F0'                                                     
         J     XIT                                                              
*                                                                               
TC10     AHI   R1,-240                                                          
         BCTR  R1,0                                                             
         D     R0,=F'24'           24 CHARS FOR EACH LETTER                     
         IC    R1,ALPHATAB(R1)                                                  
         STC   R1,CODE                                                          
         LR    R1,R0                                                            
         IC    R1,ALPHATAB(R1)                                                  
         STC   R1,CODE+1                                                        
*                                                                               
TCX      DS    0H                                                               
         J     XIT                                                              
ALPHATAB DC    C'ABCDEFGHJKLMNPQRSTUVWXYZ'                                      
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* TRANSLATE ALPHA VALUE IN CODE TO BINARY IN BCODE                              
* INPUT = ALPHA  CODE IN CODE                                                   
* OUTPUT= BINARY CODE IN BCODE(2)                                               
*=============================================================                  
                                                                                
TRANSBIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,24                                                            
         LARL  R1,ALPHATAB                                                      
         CLC   0(1,R1),CODE        FIND FIRST CHAR IN TABLE                     
         BE    *+14                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         LA    R1,24                                                            
         SR    R1,R0               GIVES DSPL INTO TABLE                        
         CLI   CODE+1,C'0'         IS SECOND CHAR NUMERIC                       
         BL    ALPHA2ND                                                         
*                                                                               
         MHI   R1,10               EACH CHAR HAS 10 DIGITS                      
         LLC   RE,CODE+1                                                        
         N     RE,=X'0000000F'                                                  
         B     TRANSBI2                                                         
         J     XIT                                                              
*                                                                               
ALPHA2ND MHI   R1,24               EACH LETTER HAS 24 CODES                     
         AHI   R1,240              AND AA IS 241                                
*                                                                               
         LA    RF,24               FIND SECOND CHAR IN TABLE                    
         LARL  RE,ALPHATAB                                                      
         CLC   0(1,RE),CODE+1                                                   
         BE    *+14                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         LA    RE,24                                                            
         SR    RE,RF                                                            
*                                                                               
TRANSBI2 LA    R1,1(RE,R1)                                                      
         STH   R1,BCODE                                                         
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* BUILD AN ENTRY TO THE TABLE                                                   
*=========================================================                      
                                                                                
BLDENTRY NTR1  BASE=*,LABEL=*                                                   
         MVC   SVELCDS,ELCDLO      SAVE PREVIOUS ELCODES                        
*                                                                               
         XC    ENTRY,ENTRY         CLEAR ENTRY                                  
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
*                                                                               
         CLI   MGAACT,MGAQTRNS     IF TRANSLATE CODES                           
         BE    BE02                DON'T COUNT                                  
         LH    R1,MGACNT           BUMP NUMBER OF ENTRIES IN TABLE              
         LA    R1,1(R1)                                                         
         STH   R1,MGACNT                                                        
*                                                                               
BE02     L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
         MVC   MGELINE,BUYKBUY       LINE NUMBER                                
*                                                                               
         MVC   MGESLN,BDSEC          SPOT LENGTH                                
         MVC   MGETIME,BDTIMST       TIME                                       
         TM    MGAOPT2,MGAOPT2_NOCOST                                           
         BO    *+10                                                             
         MVC   MGECOST,LASTCOST                                                 
         MVC   MGEDAYPT,BDDAYPT      DAYPART                                    
         MVC   MGEPGMNM,BDPROGRM     PROGRAM                                    
         MVC   MGESTAT,BUYSTAT       SAVE STATUS FLAGS                          
*                                                                               
         BRAS  RE,GETRTG                                                        
         MVC   MGERTG,RATING       RATING                                       
         MVC   MGERTGB,RATINGB     BINARY RATING                                
                                                                                
* FOR CABLE, ALWAYS NEED STATION                                                
                                                                                
         TM    MGAOPT2,MGAOPT2_CANAD   TEST CANADIAN                            
         BO    BE10                                                             
         CLI   BUYKSTA,X'E8'                                                    
         BL    *+10                                                             
         MVC   MGESTA,BUYKSTA      JUST SET ACTUAL STATION                      
*                                                                               
BE10     MVC   MGETYPE,BYTE2                                                    
         MVC   MGEDATE,ELEMDT      SET DATE                                     
         MVC   MGESPNUM,ELEMNO     SPOT NUMBER                                  
         MVC   MGECODE,CODE        ALPHA MAKEGOOD CODE                          
         MVC   MGEBCODE,BCODE+1    GET BINARY CODE                              
*                                                                               
         CLC   BCODE,=H'240'                                                    
         BNH   *+8                                                              
         MVI   MGEBCODE,X'FF'                                                   
*                                                                               
         MVC   MGEPRD1,PRD1        PRODUCT 1                                    
         MVC   MGESLN1,SLN1        SPOT LEN 1                                   
         MVC   MGEPRD2,PRD2        PRODUCT 2                                    
         MVC   MGESLN2,SLN2        SPOT LEN 2                                   
         MVC   ELCDLO(2),SVELCDS   RESTORE ELCODES                              
*                                                                               
BEX      J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
*  BUILD THE TABLE OF ENTRIES                                                   
*=================================================================              
                                                                                
BLDTABLE NTR1  BASE=*,LABEL=*                                                   
         OC    MGATAB,MGATAB       IS AN AREA PASSED TO US                      
         BZ    BT10                                                             
         L     R4,MGATAB           A(TABLE)                                     
         OC    0(MGERECL,R4),0(R4) FIRST TIME IN - TABLE EMPTY                  
         BNZ   *+8                                                              
         ST    R4,ANENTRY          A(NEXT ENTRY)                                
         L     R4,ANENTRY                                                       
         L     R1,ATABLEX                                                       
         AHI   R1,-MGERECL                                                      
         CR    R4,R1                                                            
         BNL   TABFULER                                                         
*                                                                               
         MVC   0(MGERECL,R4),ENTRY PUT ENTRY IN TABLE                           
         LA    R4,MGERECL(R4)      BUMP TO NEXT POSITION                        
         XC    0(MGERECL,R4),0(R4)                                              
         ST    R4,ANENTRY          SET A(NEXT ENTRY)                            
         B     BT20                                                             
*                                                                               
BT10     CLI   BCODE,X'FD'         COUNT NUMBER OF NO CHRGE ADDED               
         BNE   BT15                                                             
         LH    R1,NUMNC                                                         
         LA    R1,1(R1)                                                         
         STH   R1,NUMNC                                                         
*                                                                               
****     XC    TSARBLK,TSARBLK                                                  
BT15     MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
         BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNO   BT20                                                             
         MVI   MGAERR,MGAQEOF                                                   
         J     NO                                                               
*                                                                               
BT20     OC    MGAHOOK,MGAHOOK                                                  
         BZ    BTX                                                              
         L     RF,MGAHOOK          HOOK TO USER                                 
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF               MAY RETURN CC                                
*                                                                               
BTX      J     YES                                                              
*                                                                               
TABFULER MVI   MGAERR,MGAQTFUL                                                  
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        CHECK PRODUCT FILTERS                                                  
*                                                                               
CHKPRFLT NTR1  BASE=*,LABEL=*                                                   
         CLI   MGFLTPRD,X'FF'      ALL PRODUCTS                                 
         BE    CF10                                                             
         CLI   MGFLTPRD,0          ALL PRODUCTS                                 
         BE    CF10                                                             
         CLC   MGFLTPRD,PRD1       DOES PRODUCT MATCH                           
         JNE   NO                                                               
*                                                                               
CF10     CLI   MGFLTPG,X'FE'       DON'T FILTER ON PIGGYBACKS                   
         JE    YES                                                              
         CLI   MGFLTPG,0           EXCLUDE PIGGYBACKS                           
         BNE   CF20                                                             
         CLI   PRD2,0              IS THIS A PIGGYBACK                          
         JE    YES                 NO - KEEP IT                                 
         J     NO                                                               
*                                                                               
CF20     CLI   MGFLTPG,X'FF'       ONLY PIGGYBACKS                              
         BNE   CF30                                                             
         CLI   PRD2,0              IS THIS A PIGGYBACK                          
         JNE   YES                 YES- KEEP IT                                 
         J     NO                                                               
*                                                                               
CF30     CLC   MGFLTPG,PRD2       DOES PIGGYBACK MATCH                          
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* GET MAKEGOOD CODE FROM TSAR BUFFER                                            
*=========================================================                      
                                                                                
GETOLD   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
*                                                                               
         XC    HIGHCODE,HIGHCODE                                                
         MVI   TSERRS,0                                                         
         XC    ENTRY,ENTRY                                                      
*                                                                               
         TM    MGCODTAB,X'80'      TEST TABLE PREVIOUSLY BUILT                  
         BO    GETOLD20            YES - DON'T REBUILD                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,MGCODTBL       USER PASSED LENGTH/256?                      
         BNZ   *+8                 YES                                          
         LA    R0,1                IF 0, SET TO 1                               
         ICM   R1,15,MGCODTAB      USER PASSED TABLE?                           
         BNZ   GETOLD05             YES                                         
         LA    R0,4                 NO, USE MY OWN                              
         LA    R1,CODETAB                                                       
GETOLD05 CHI   R0,4                MAKE SURE WE HAVE 1024 BYTES                 
         JNE   *+2                                                              
*                                                                               
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
* MARK ALL ENTRIES IN THE N SERIES AS USED SO THEY ARE NEVER ASSIGNED!          
*                                                                               
         ICM   R1,15,MGCODTAB      USER PASSED TABLE?                           
         BNZ   *+8                                                              
         LA    R1,CODETAB                                                       
*                                                                               
         LA    RF,10                                                            
         LA    RE,X'79'-1(R1)      POINT TO ENTRY FOR N0                        
*                                                                               
         MVI   0(RE),C'N'                                                       
         LA    RE,1(RE)                                                         
         JCT   RF,*-8                                                           
*                                                                               
* PER CC ON 07AUG14, NA-NZ CAN IN FACT BE USED. I HOPE SHE'S RIGHT              
*                                                                               
**NOP**  LA    RE,529(R1)          AA=241, BA=265,... NA=529                    
**NOP**  LA    RF,24                                                            
**NOP**                                                                         
**NOP**  MVI   0(RE),C'N'                                                       
**NOP**  LA    RE,1(RE)                                                         
**NOP**  JCT   RF,*-8                                                           
*                                                                               
         MVI   BYTE,TSARDH         READ HI                                      
         B     GETOLD12                                                         
*                                                                               
GETOLD10 MVI   BYTE,TSANXT                                                      
*                                                                               
GETOLD12 BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BZ    GETOLD14                                                         
         CLC   TSPRECN,=Y(MAXENTS) BUT IF MORE THAN MAX RECS IN TABLE           
         BNH   GETOLD20                                                         
         MVI   MGAERR,MGAQTFUL                                                  
         J     XIT                                                              
*                                                                               
GETOLD14 DS    0H                                                               
         CLI   MGETYPE,X'FE'       TEST TOTAL                                   
         BE    GETOLD10            YES- SKIP                                    
         CLI   MGECODE,C'*'        NO-CHARGE, PREEMPT?                          
         BE    GETOLD10            YES-SKIP THOSE AS WELL!!!                    
*                                                                               
         LLC   RE,MGEBCODE         GET MAKEGOOD CODE FROM ENTRY                 
         CLI   MGEBCODE,X'FF'                                                   
         JNE   GETOLD16                                                         
                                                                                
* ONLY HAVE ALPHA IN MGECODE, GET BINARY NOW                                    
                                                                                
         MVC   CODE,MGECODE                                                     
         BRAS  RE,TRANSBIN                                                      
         LH    RE,BCODE                                                         
                                                                                
GETOLD16 ICM   R1,15,MGCODTAB      USER PASSED TABLE?                           
         BNZ   *+8                  YES                                         
         LA    R1,CODETAB                                                       
         BCTR  R1,0                                                             
         LA    RF,0(RE,R1)         POINT TO POSITION                            
         STC   RE,0(RF)            SET VALUE IN TABLE                           
         CHI   RE,240                                                           
         BNH   *+8                                                              
         MVI   0(RF),X'FF'         DO NOT SET LOW-ORD BYTE YOU SFI              
         B     GETOLD10                                                         
*                                                                               
GETOLD20 LA    R0,MAXCODES                                                      
         ICM   R1,15,MGCODTAB                                                   
         BNZ   *+8                                                              
         LA    R1,CODETAB                                                       
*                                                                               
GETOLD22 CLI   0(R1),0             TEST SLOT AVAILABLE                          
         BE    GETOLD24                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,GETOLD22                                                      
         MVI   MGAERR,MGAQFULL     NO MORE CODES AVAILABLE                      
         J     XIT                                                              
*                                                                               
GETOLD24 LA    RE,MAXCODES+1                                                    
         SR    RE,R0               GET NEW VALUE                                
         STH   RE,BCODE            NOTE 2 BYTES!                                
         MVI   0(R1),X'FF'         AND SET FLAG IN TABLE ENTRY                  
*                                                                               
         ICM   RE,15,MGCODTAB      DOES USER WANT TABLE RETURNED                
         BZ    GETOLD30                                                         
         TM    MGCODTAB,X'80'      ALREADY SET?                                 
         BO    GETOLD30                                                         
*                                                                               
         MVC   BYTE,MGCODTAB                                                    
         NI    BYTE,X'7F'                                                       
         CLI   BYTE,4              MAKE SURE 1024 BYTES PASSED                  
         JNE   *+2                                                              
         OI    MGCODTAB,X'80'      SET FLAG FOR NO TABLE REBUILD                
*                                                                               
GETOLD30 MVI   BYTE2,0                                                          
         BAS   RE,TRANSCD                                                       
*                                                                               
         MVC   MGECODE,CODE                                                     
         MVC   MGEBCODE,BCODE+1    ASSUME 1-BYTE CODE                           
         CLC   BCODE,=H'240'                                                    
         BNH   *+8                                                              
         MVI   MGEBCODE,X'FF'      SET FLAG FOR 2-BYTE CODE                     
         MVC   MGAENTRY,ENTRY                                                   
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*====================================================================*          
* TEST IF ALL SPOTS ON A MAKEGOOD LINE HAVE BEEN MISSED              *          
* IF SO, PRINT 'MISSED' FOR THE MAKEGOOD LINE                        *          
*====================================================================*          
         SPACE 1                                                                
TSTSPCL  NTR1  BASE=*,LABEL=*                                                   
         L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
         CLI   BDMGDATE,X'C1'      SEE IF NEW MAKEGOOD                          
         BL    TSX                                                              
         MVC   CODE,BDMGDATE                                                    
         MVI   BYTE2,1             SET ENTRY TYPE TO MAKEGOOD                   
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
         LA    R3,24(R3)                                                        
         USING REGELEM,R3                                                       
*                                                                               
TS10     BRAS  RE,NEXTEL           FIND 0B/OC ELEMENT                           
         BNE   TS20                NO - DIDN'T FIND ANY THAT RAN                
         TM    RSTATUS,X'C0'       TEST MINUS OR MINUSSED                       
         BZ    TSX                 NO - DONE                                    
         B     TS10                ELSE TRY AGAIN                               
*                                                                               
TS20     MVC   ELEMDT,=X'FFFF'     SET HIGH DATE                                
         MVI   ELEMNO,1                                                         
*                                                                               
         BAS   RE,BLDENTRY         YES - BUILD ENTRY TO TABLE                   
* NEXT INSTRUCTION MUST BE CAREFUL NOT TO CLEAR BCODE                           
         XC    ENTRY+MGEKEYL+1(L'ENTRY-MGEKEYL-1),ENTRY+MGEKEYL+1               
         BRAS  RE,BLDTABLE         PUT ENTRY TO TABLE/TSAR/GO TO HOOK           
*                                                                               
TSX      J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
*        GET THE RATING                                                         
*==============================================================                 
                                                                                
GETRTG   NTR1  BASE=*,LABEL=*                                                   
         XC    RATING,RATING                                                    
         XC    RATINGB,RATINGB                                                  
         TM    MGAOPT2,MGAOPT2_NODEMS   TEST SUPPRESS DEMOS                     
         BO    GRX                                                              
         MVI   MGAERR,MGAQDMIS     A(DEMOS) MISSING                             
         OC    MGABRDEM,MGABRDEM   NEED A(SVBRDEM)                              
         BZ    GRX                                                              
         OC    MGADEM,MGADEM       AND A(SVDEMS)                                
         BZ    GRX                                                              
         MVI   MGAERR,0                                                         
*                                                                               
         MVI   ELCDLO,2            FIND DEMO ELEM IN THE BUYLINE                
         MVI   ELCDHI,2                                                         
         L     R3,MGAIO                                                         
         LA    R3,24(R3)                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   GRX                                                              
*                                                                               
         L     R4,MGABRDEM                                                      
         OC    0(3,R4),0(R4)                                                    
         BNZ   *+8                                                              
         L     R4,MGADEM                                                        
*                                                                               
GR10     CLI   1(R4),0             TEST E-O-L                                   
         BE    GRX                                                              
         XC    RATING,RATING                                                    
         XC    RATINGB,RATINGB                                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R3)            FIND DEMO IN BUYREC                          
         AHI   R0,-24                                                           
         BNP   GRX                                                              
         SRL   R0,3                                                             
         LA    R5,24(R3)                                                        
*                                                                               
GR20     CLC   0(3,R4),0(R5)                                                    
         BE    GR30                                                             
         LA    R5,8(R5)                                                         
         BCT   R0,GR20                                                          
         B     GRX                                                              
*                                                                               
GR30     L     R0,4(R5)                                                         
         N     R0,=X'3FFFFFFF'     TAKE OFF OVERRIDE AND 2 DEC BITS             
         BAS   RE,EDITDEM                                                       
         TM    4(R5),X'80'                                                      
         BZ    *+8                                                              
         MVI   RATING+7,C'*'                                                    
*                                                                               
GRX      J     XIT                                                              
         EJECT                                                                  
*===============================================================                
* SUBROUTINE TO FORMAT DEMO AND HUT VALUES                                      
* ON ENTRY R0 HAS DEMO VALUE                                                    
*===============================================================                
                                                                                
EDITDEM  NTR1                                                                   
         TM    MGAOPT2,MGAOPT2_NODEMS TEST SUPPRESS DEMOS                       
         JO    XIT                                                              
*                                                                               
         LLC   R1,3(R5)            GET HUT VALUE                                
         AR    R1,R1               X 2                                          
         MR    R0,R0                                                            
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,RATINGB          SAVE THE VALUE                               
*                                                                               
         LR    R1,R4                                                            
         BAS   RE,ISRATING         IS THE DEMO A RATING                         
         JNE   ED02                NO                                           
*                                                                               
         TM    MGAOPT2,MGAOPT2_2DEC   CALLER WANT 2-DEC RTGS                    
         JO    ED10                   YES                                       
         J     ED04                   NO                                        
*                                                                               
ED02     TM    MGAOPT2,MGAOPT2_2DECIMP  CALLER WANT 2-DEC IMPS                  
         JO    ED10                                                             
                                                                                
* USER DOES NOT WANT 2-DEC VALUES                                               
                                                                                
ED04     L     R1,RATINGB                                                       
         TM    4(R5),X'40'         DEMO TO 2-DEC                                
         JZ    ED06                NO                                           
         M     R0,=F'2'            ADJUST TO 1-DEC                              
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,RATINGB                                                       
*                                                                               
ED06     TM    MGAOPT,MGONEDIT     TEST NO EDIT REQUIRED                        
         JO    XIT                                                              
         EDIT  (R1),(4,RATING),1                                                
         J     XIT                                                              
                                                                                
* RETURN 2 DECIMAL VALUE                                                        
                                                                                
ED10     TM    4(R5),X'40'         DEMO TO 2-DEC ?                              
         JO    ED20                YES                                          
*                                                                               
         L     R1,RATINGB          ADJUST 1-DEC VALUE TO 2-DEC                  
         M     R0,=F'10'                                                        
         ST    R1,RATINGB                                                       
*                                                                               
ED12     TM    MGAOPT,MGONEDIT     TEST NO EDIT REQUIRED                        
         JO    XIT                                                              
         EDIT  (R1),(5,RATING),2                                                
         J     XIT                                                              
                                                                                
* USER WANTS 2 DEC                                                              
                                                                                
ED20     L     R1,RATINGB                                                       
         TM    4(R5),X'40'         TEST RTG HAS 2 DEC                           
         BO    ED22                YES                                          
         MHI   R1,10               SCALE UP TO 2 DEC                            
         ST    R1,RATINGB                                                       
*                                                                               
         CHI   R1,9999             WILL 2 DEC FIT                               
         BH    ED24                NO                                           
         EDIT  (R1),(5,RATING),2                                                
         B     EDX                                                              
*                                                                               
ED22     M     R0,=F'2'            WON'T FIT - ROUND TO 1 DEC                   
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
ED24     EDIT  (R1),(5,RATING),1                                                
*                                                                               
EDX      J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R3),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R3)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
NEXTEL2  CLI   0(R3),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R3)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R3)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
                                                                                
*==============================================================                 
* SUBROUTINE RETURNS WITH CC EQ IF DEMO AT 0(R1) IS A RATING                    
* ON ENTRY R1 POINTS TO 3-BYTE DEMO CODE                                        
*==============================================================                 
                                                                                
ISRATING NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   0(R1),0             TEST NORMALIZED NON-T DEMO                   
         JNE   ISRTG2              NO                                           
*                                                                               
         CLI   0(R1),C'R'                                                       
         JE    ISRTGYES                                                         
         CLI   0(R1),C'E'                                                       
         JE    ISRTGYES                                                         
         J     ISRTGNO                                                          
*                                                                               
ISRTG2   CLI   2(R1),0             OR JUST A NON-T DEMO                         
         JE    ISRTG10             YES                                          
*                                                                               
         CLI   1(R1),C'R'          SEE IF NORMAL DEMO IS A RATING               
         JE    ISRTGYES                                                         
         CLI   1(R1),C'E'                                                       
         JE    ISRTGYES                                                         
         J     ISRTGNO                                                          
*                                                                               
ISRTG10  OC    AD50EL,AD50EL                                                    
         JNZ   ISRTG12                                                          
         MVI   ELCDLO,X'50'                                                     
         MVI   ELCDHI,X'50'                                                     
         L     R3,MGAIO                                                         
         LA    R3,24(R3)                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         ST    R3,AD50EL                                                        
*                                                                               
ISRTG12  LLC   RF,1(R1)            YES, GET THE NON-TRAD INDEX                  
         BCTR  RF,0                                                             
         MHI   RF,9                9 CHARS/DEMO IN 50EL                         
         A     RF,AD50EL                                                        
         LA    RF,2(RF)                                                         
         CLI   0(RF),C'R'          SEE IF NON-TRAD CTGY IS RATING               
         JE    ISRTGYES              OR EXTENDED RATING                         
         CLI   0(RF),C'E'                                                       
         JNE   ISRTGNO                                                          
*                                                                               
ISRTGYES MVI   RTGFLAG,C'Y'                                                     
         J     YES                                                              
*                                                                               
ISRTGNO  MVI   RTGFLAG,C'N'                                                     
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
BYTE     DS    XL1                                                              
BYTE2    DS    XL1                                                              
BYTE3    DS    XL1                                                              
BYTE4    DS    XL1                                                              
WORK     DS    CL48                                                             
*                                                                               
DMCB     DS    6F                                                               
DMWORK   DS    12D                                                              
         DS    0D                                                               
TSARBLK  DS    CL48                                                             
         DS    0D                                                               
KEY      DS    CL20                                                             
KEYSAVE  DS    CL20                                                             
BCODE    DS    H                   BINARY CODE                                  
CODE     DS    H                   ALPHA CODE                                   
BCODESTA DS    XL3                 OTHER STATION (IF ANY)                       
         DS    CL1                                                              
*                                                                               
         DS    0D                                                               
GETBLK   DS    XL64                                                             
         ORG   GETBLK                                                           
       ++INCLUDE SPGETBUYD                                                      
         ORG                                                                    
*                                                                               
USERRD   DS    A                                                                
ANENTRY  DS    A                                                                
SAVER3   DS    F                                                                
MISSTOT  DS    F                   TOTAL ACCUMULATORS                           
MGTOT    DS    F                                                                
GMISSTOT DS    F                                                                
GMGTOT   DS    F                                                                
MISSRTG  DS    F                                                                
MGRTG    DS    F                                                                
GMISSRTG DS    F                                                                
GMGRTG   DS    F                                                                
AD50EL   DS    A                                                                
*                                                                               
BUYCOST  DS    F                                                                
LASTCOST DS    F                                                                
ATABLEX  DS    F                                                                
RATINGB  DS    F                                                                
DATADISP DS    H                                                                
HIGHCODE DS    H                                                                
NUMNC    DS    XL2                 COUNT OF NUMBER OF NO CHARGE ENTRIES         
RTGFLAG  DS    CL1                                                              
ELCODE   DS    XL1                                                              
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
SVELCDS  DS    XL2                                                              
BUYCIND  DS    XL1                 X'01'=MINUS                                  
BUYSTAT  DS    XL1                 SEE MGESTAT                                  
COMMAND  DS    CL8                                                              
TOTFND   DS    XL1                                                              
ELEMNO   DS    XL1                                                              
ELEMDT DS      XL2                                                              
THISCODE DS    CL2                                                              
THISSTA  DS    XL3                                                              
PRD1     DS    XL1                                                              
SLN1     DS    XL1                                                              
PRD2     DS    XL1                                                              
SLN2     DS    XL1                                                              
RATING   DS    CL10                                                             
ENTRY    DS    CL(MGERECL)                                                      
ENTRY2   DS    CL(MGERECL)                                                      
CODETAB  DS    XL1024                                                           
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPMGADN                                                        
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
*SPGENBUY                                                                       
*DDCOMFACSD                                                                     
*DDCOREQUS                                                                      
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENMGCD                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042SPBLDMGN  11/19/19'                                      
         END                                                                    
