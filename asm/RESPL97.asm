*          DATA SET RESPL97    AT LEVEL 065 AS OF 05/10/05                      
*PHASE T80897A                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE DAYUNPK                                                                
*INCLUDE MILEDIT                                                                
*INCLUDE REGETIUN                                                               
         TITLE 'RESPL97 - T80897 - OVERNIGHT TRANSFERS'                         
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESPL97 --- REP INVENTORY OVERNIGHT TRANSFER FUNCTION    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* OCT26/90 (MRR) --- MODIFY DBLOCK AND PRINT ROUTINE FOR 1 DECIMAL*             
*                                                                 *             
* OCT23/95 (BU ) --- CORRECT 'UT' DEFICIENCY                      *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80897   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 00,**ONTR**,R9,R7                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T808FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         STM   R8,RC,OVERR8                                                     
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         MVI   RCSUBPRG,0                                                       
         LA    R4,KEY              GET PARENT REP FROM REP RECORD               
         USING RREPKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REP                                                     
         DROP  R4                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RREPELEM,R6                                                      
         MVC   REP,RREPPAR                                                      
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,BTODAY)                                
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         L     R2,=A(STACK)                                                     
         A     R2,RELO                                                          
         ST    R2,ASTACK                                                        
         L     R2,=V(RECUP)                                                     
         A     R2,RELO                                                          
         ST    R2,RECUP                                                         
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2                                                      
         MVC   DEMOMATH,CDEMOMTH                                                
         MVC   DEMAINT,CDEMAINT                                                 
         MVC   DEFINE,CDEFINE                                                   
         DROP  R2                                                               
         L     R2,=V(DAYUNPK)                                                   
         A     R2,RELO                                                          
         ST    R2,DAYUNPK                                                       
         L     R2,=V(MILEDIT)                                                   
         A     R2,RELO                                                          
         ST    R2,MILEDIT                                                       
         EJECT                                                                  
*              CONTROL THE IO ROUTINES                                          
         SPACE 3                                                                
         LA    R2,DPLIST                                                        
         LA    R3,NDPT                                                          
         SPACE 1                                                                
MAB      CLI   0(R2),0             CONTROL FOR EACH DP                          
         BE    XIT                                                              
         ST    R2,SAVER2                                                        
         BAS   RE,NEWDPT                                                        
         BAS   RE,MAD                                                           
         LA    R2,1(R2)                                                         
         BCT   R3,MAB                                                           
         B     XIT                                                              
         SPACE 1                                                                
MAD      NTR1                      BUILD A STACK OF D/A                         
         L     R5,ASTACK                                                        
         SR    R6,R6                                                            
         LA    R4,KEY                                                           
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RIDPKTYP,X'92'                                                   
         MVC   RIDPKREP,REP                                                     
         MVC   RIDPKSTA,ACTSTAT                                                 
         MVC   RIDPKDPT,0(R2)                                                   
         GOTO1 HIGH                                                             
         B     MAD4                                                             
         SPACE 1                                                                
MAD2     GOTO1 SEQ                                                              
         SPACE 1                                                                
MAD4     CLC   KEYSAVE(11),KEY     STATION D/P C/B                              
         BNE   MAD6                                                             
         CLC   RIDPKSTD,STRTOPT                                                 
         BL    MAD2                                                             
         CLC   RIDPKSTD,ENDOPT                                                  
         BH    MAD2                                                             
         MVC   0(4,R5),KEY+28                                                   
         LA    R5,4(R5)                                                         
         LA    R6,1(R6)                                                         
         B     MAD2                                                             
         SPACE 1                                                                
MAD6     LTR   R6,R6                                                            
         BZ    XIT                                                              
         L     R5,ASTACK                                                        
         B     MA3                                                              
         SPACE 1                                                                
MA2      LM    R5,R6,SAVESTAK                                                   
         LA    R5,4(R5)                                                         
         BCT   R6,MA3                                                           
         B     XIT                                                              
         SPACE 1                                                                
MA3      MVC   KEY+28(4),0(R5)                                                  
         STM   R5,R6,SAVESTAK                                                   
         GOTO1 GETREC                                                           
         MVC   KEY(27),IO                                                       
         USING RINVKEY,R4                                                       
         SPACE 2                                                                
MA4      MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RINVPEL,R6                                                       
         LA    R1,RINVPFLT         FILTER FILTER                                
         LA    R5,TITFILT                                                       
         LA    R0,6                                                             
         SPACE 2                                                                
MA6      CLI   0(R5),C'A'          IF ANYTHING IS SPECIFIED                     
         BL    MA8                                                              
         CLC   0(1,R5),0(R1)       IT MUST MATCH                                
         BNE   MA2                                                              
         SPACE 2                                                                
MA8      LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,MA6                                                           
*                                  SEE IF IT ENDS BEFORE TODAY                  
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MA12                                                             
         CLC   RINVPEFF+2(2),BTODAY                                             
         BL    MA2                                                              
         SPACE 1                                                                
MA12     BAS   RE,HEADER           FORMAT INVENTORY DETAILS                     
         BAS   RE,PURE             LOOK FOR PURE DETAILS                        
         MVI   SPACING,2                                                        
         BAS   RE,SPLAT                                                         
         CLI   TRACOPT,C'Y'                                                     
         BNE   MA2                                                              
         BAS   RE,TRACE                                                         
         B     MA2                                                              
         SPACE 1                                                                
TRACOPT  DC    C'N'                                                             
         EJECT                                                                  
*              ROUTINE TO FORMAT HEADER DETAILS                                 
         SPACE 3                                                                
HEADER   NTR1                                                                   
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         MVC   P5,SPACES                                                        
         MVC   P6,SPACES                                                        
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         EDIT  (1,RINVKQTR),(2,P+1),FILL=0                                      
         MVC   P+3(2),RINVKDAY                                                  
         CLI   P+4,C'0'                                                         
         BNE   *+8                                                              
         MVI   P+4,C' '                                                         
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,P+6)                                 
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HEADER2                                                          
         CLC   RINVPEFF(2),RINVPEFF+2                                           
         BE    HEADER2                                                          
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(8,P2+6)                              
         SPACE 1                                                                
HEADER2  GOTO1 UNDAY,DMCB,RINVPDAY,P+16                                         
         GOTO1 UNTIME,DMCB,RINVPTIM,P+26                                        
         ZIC   R5,RINVPLEN                                                      
         SH    R5,=H'40'                                                        
         GOTO1 CHOPPER,DMCB,((R5),RINVPROG),(27,P+38),(C'P',3)                  
         MVC   P+67(1),RINVPAUT                                                 
         LA    R5,CEEL             BUILD A CE ELEMENT                           
         USING RINVZEL,R5                                                       
         MVC   RINVZEL(2),=X'CE0A'                                              
         MVC   RINVZDAY(5),RINVPDAY                                             
         MVC   RINVZBK,BOOK                                                     
         NI    RINVZBK,X'FF'-X'08' TURN OFF TIME PERIOD BIT                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO FIND PURE DETAILS                                    
         SPACE 3                                                                
PURE     NTR1                                                                   
         CLI   RINVPAUT,C'N'       OPTION NOT TO TRANSFER AUTO.                 
         BE    XIT                                                              
         L     R4,=A(DATA)         BUILD NEW DATA RECORD                        
         A     R4,RELO                                                          
         ST    R4,ADATAREC         SET POINTER TO DATA RECORD                   
         LA    R1,1000(R4)         THEN SET ONE TO INTERIM RECORD AREA          
         ST    R1,AINTEREC                                                      
         XCEF  (R4),2000                                                        
         MVC   0(24,R4),IO         KEY AS FOR HEADER                            
         MVC   RINVKSRC,TITSRCE    TACK ON SOURCE                               
         TM    BOOK,X'08'          ADJUST FOR T/P                               
         BNO   *+8                                                              
         OI    RINVKSRC,X'02'                                                   
         MVC   RINVKBK,BOOK+1      ADD BOOK                                     
         MVC   RINVLEN,=H'35'                                                   
         SPACE 1                                                                
         MVI   TPCNT,0             SET COUNTERS                                 
         MVI   HITCNT,0                                                         
         MVI   PCNT,0                                                           
         MVI   VARSW,C'N'          SET MORE THAN 2 PROGRAM SWITCH               
         XC    PROGEL,PROGEL                                                    
         XC    DEMODUB,DEMODUB     CLEAR EXTRA STORAGE FOR DEMUP                
         XC    TOTSHR(12),TOTSHR   CLEAR SHARE ACCUMULATORS                     
         SPACE 1                                                                
         SPACE 1                                                                
PURE1    EQU   *                                                                
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'      RTG = 1 DECIMAL                               
         MVI   DBXTTSP,X'01'      SHR = 1 DECIMAL                               
         MVI   DBXTTIP,X'02'      IMP = 00'S                                    
         DROP  R1                                                               
*                                                                               
         MVC   DBSELAGY,REP                                                     
         MVC   DBFILE,=C'PAV'                                                   
         TM    BOOK,X'08'                                                       
         BNO   *+10                                                             
         MVC   DBFILE,=C'TP '                                                   
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELSRC,TITSRCE    SELECT OPTIONS                               
         MVC   DBSELBK,BOOK+1                                                   
         MVC   DBSELSTA,RINVKSTA                                                
         CLI   DBFILE,C'T'         TEST FOR TIME PERIOD                         
         BNE   PURE2                                                            
         MVI   DBTPTT,C'P'         SET 4 WK AVERAGE OPTION                      
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    PURE2                                                            
         CLI   DBSELSTA+4,C'2'     FOR A PS/1 STATION                           
         BE    PURE2                                                            
         MVI   DBSELSTA+4,C'T'                                                  
         SPACE 1                                                                
PURE2    LR    R0,R4                                                            
         LA    R4,IO                                                            
         MVC   DBSELDAY,RINVPDAY                                                
         MVC   DBSELTIM,RINVPTIM                                                
         MVI   DBBEST,C'B'                                                      
         SPACE 1                                                                
         SR    R1,R1                                                            
         ICM   R1,3,DBSELTIM                                                    
         AH    R1,=H'200'          ADD 2 HOURS TO START                         
         CH    R1,=H'2400'         TEST IF PAST MIDNIGHT                        
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         CLC   RINVPTIM+2(2),=C'CC' TEST FOR TO CONCLUSION                      
         BNE   *+8                                                              
         STCM  R1,3,DBSELTIM+2     SET END TIME                                 
         SPACE 1                                                                
         LR    R4,R0                                                            
         LA    R1,DBLOCK           INITIALIZE BLOCK FOR DEMOMATH                
         ST    R1,MTHCFACS                                                      
         XC    MTHFCTR,MTHFCTR                                                  
         SPACE 1                                                                
         GOTO1 DEMAND,DMCB,DBLOCK,DHOOK                                         
         SPACE 1                                                                
         CLI   HITCNT,0            ANY LUCK                                     
         BE    XIT                 YES - NEED TO AVERAGE RECORD                 
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BE    *+6                                                              
         DC    H'0'                DEMO MODULE ERROR                            
         SPACE 1                                                                
*              UNWEIGHT THE INTERIM RECORD AND SHARES BEFORE 'FF' CALL          
*                                                                               
         MVC   MTHFCTR+2(2),DBDIVSOR                                            
         GOTO1 DEMOMATH,DMCB,=C'DIVIDE',AINTEREC,AINTEREC,MATHFAC               
         BAS   RE,DIVSHR                                                        
         SPACE 1                                                                
*              BUILD DUMMY UPGRADE ELEMENT FOR INDEX 'FF' DEMUP CALL            
*              TO GET OLD HPT VALUES                                            
*                                                                               
PURE4    XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNBKS,DBFILE     SET FILE NAME FROM DBLOCK                    
         MVI   RAVLNCAT,C'D'       DAY/TIME TRANSFER                            
         MVC   RAVLNOP1,=X'FFFF'   GET OLD HPT'S                                
         MVC   RAVLNOP2,=H'1'      UNWEIGHTED RECORD                            
         LA    R1,DBLOCK           PASS A(MY DBLOCK)                            
         STCM  R1,15,RAVLNOP3                                                   
         DROP  RE                                                               
         SPACE 1                                                                
         L     R5,AINTEREC         UPGRADE INTERIM RECORD                       
         GOTO1 DEMUP,DMCB,23(R5),WORK,ACOMFACS,DEMODUB,HOMSHR                   
         SPACE 1                                                                
         MVC   MTHIFIL,=C'PAV'     FORCE BOOK ELEMENT LOOKUP                    
         MVC   MTHOFIL,=C'INV'     CONVERT TO INVENTORY FORMAT                  
         MVC   MTHOSRC,=C'NSI'     AND MAD INTERIM TO FINAL RECORD              
         SPACE 1                                                                
         GOTO1 DEMOMATH,DMCB,=C'MAD',AINTEREC,ADATAREC,MATHFAC                  
         SPACE 1                                                                
         L     RE,AINTEREC                                                      
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         LR    R0,RE                                                            
         MVCL  RE,R0               CLEAR INTERIM RECORD AREA                    
         SPACE 1                                                                
         MVC   MTHIFIL,=C'INV'                                                  
         GOTO1 DEMOMATH,DMCB,=C'DIVIDE',(R4),(R4),MTHCFACS                      
         SPACE 1                                                                
PURE6    OC    PROGEL,PROGEL       ANY PROGRAMS TO ADD                          
         BZ    PURE8                                                            
         SPACE 1                                                                
         LA    R5,PROGEL                                                        
         USING PROGELD,R5                                                       
         MVI   PCODE,X'01'                                                      
         MVI   PELLEN,PNAME2-PROGELD    FIND ELEMENT LENGTH                     
         OC    PNAME2,PNAME2       TEST FOR ONE PROGRAM                         
         BZ    *+8                                                              
         MVI   PELLEN,PROGELX-PROGELD NO-SET LENGTH FOR TWO PROGRAMS            
         MVI   PLIN,1                                                           
         MVC   WORK(2),BOOK+1      DISPLAY FROM BOOK (MMMYY)                    
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(6,DUB)                                     
         MVC   PMON,DUB                                                         
         MVC   PYR,DUB+4                                                        
         MVC   PINVCODE,=C'TP '                                                 
         MVI   PEQS,C'='                                                        
         OC    PFBK(9),SPACES                                                   
         SPACE 1                                                                
         CLI   VARSW,C'Y'          TEST FOR MORE THAN 2 PROGRAMS                
         BNE   PURE7                                                            
         MVC   PNAME1,SPACES                                                    
         MVC   PNAME1(7),=C'VARIOUS'                                            
         MVI   PELLEN,PNAME2-PROGELD                                            
         DROP  R5                                                               
         SPACE 1                                                                
PURE7    LR    R6,R4                                                            
         MVI   ELCODE,X'CE'                                                     
         BAS   RE,GETEL                                                         
         GOTO1 RECUP,DMCB,(2,(R4)),PROGEL,(R6)                                  
         SPACE 1                                                                
PURE8    LA    R5,CDEL             BUILD AND ADD A CD ELEMENT                   
         XC    CDEL,CDEL                                                        
         USING RINVCEL,R5                                                       
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,=C'TP'                                                  
         CLI   DBFILE,C'T'         TEST FOR TIME PERIOD                         
         BE    *+10                                                             
         MVC   RINVCODE,SPACES                                                  
         LR    R6,R4               POSITION ELEMENT INSERTION                   
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         GOTO1 RECUP,DMCB,(2,(R4)),CDEL,(R6)                                    
         SPACE 1                                                                
PURE10   LR    R6,R4               ADD IN A CE ELEMENT                          
         MVI   ELCODE,X'CE'                                                     
         BAS   RE,GETEL                                                         
         GOTO1 RECUP,DMCB,(2,(R4)),CEEL,(R6)                                    
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         LA    RE,WORK             BUILD TRANSFER FROM ELEMENT                  
         USING RINVFREL,RE                                                      
         MVI   RINVFRCD,X'03'                                                   
         MVI   RINVFRLN,16                                                      
         MVC   RINVFRST,RINVKSTA   SET STATION FROM KEY                         
         LA    RF,CEEL             GET FROM SOURCE/BOOK FROM CE EL              
         MVC   RINVFRBK,RINVZBK-RINVZEL(RF)                                     
         MVI   RINVFRTY,C'P'                                                    
         MVI   RINVFRPR,C'O'       OVERNIGHT TRANSFERS                          
         DROP  RE                                                               
         SPACE 1                                                                
         LR    R6,R4                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         GOTO1 RECUP,DMCB,(2,(R4)),WORK,(R6)                                    
         SPACE 1                                                                
PURE12   XC    WORK,WORK           BUILD DUMMY UPGRADE AS PRELUDE TO            
         LA    RE,WORK             INDEX 100 DEMUP CALL FOR NEW HPTS            
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNOP1,=H'100'                                                 
         DROP  RE                                                               
         SPACE 1                                                                
         LR    R6,R4               ADD UPGRADE EL AND CALL DEMUP                
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         GOTO1 RECUP,DMCB,(2,(R4)),WORK,(R6)                                    
         NI    11(R4),X'FF'-X'40'  BECAUSE OF UT/TP PROBLEM                     
         GOTO1 DEMUP,DMCB,34(R4),WORK,ACOMFACS                                  
         OI    11(R4),X'40'                                                     
         SPACE 1                                                                
PURE14   ST    R4,DBAREC                                                        
         LA    R1,34(R4)                                                        
         ST    R1,DBAQUART                                                      
         MVC   DBFILE,=C'INV'                                                   
         ZIC   R3,PCNT                                                          
         MH    R3,=H'132'                                                       
         LA    R3,P(R3)            POSITION TO P1-P4                            
         MVC   95(9,R3),=C'(AVERAGE)'                                           
         BAS   RE,PERF                                                          
         SPACE 1                                                                
PURE15   GOTO1 DATCON,DMCB,(2,BTODAY),(3,DUB)                                   
         MVC   KEY,0(R4)           SEE IF RECORD EXISTS                         
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    PURE16                                                           
         SPACE 1                                                                
         MOVE  (IO,1000),0(R4)                                                  
         MVC   KEY,IO              NO - SO ADD A NEW ONE                        
         XC    WORK,WORK                                                        
         LA    RE,WORK             BUILD ACTIVITY ELEMENT                       
         USING RINVAEL,RE                                                       
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,DUB                                                     
         MVC   RINVALST,DUB                                                     
         MVI   RINVAWHY,C'A'                                                    
         DROP  RE                                                               
         SPACE 1                                                                
         LA    R4,IO                                                            
         LR    R6,R4                                                            
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL                                                         
         GOTO1 RECUP,DMCB,(2,(R4)),WORK,(R6)                                    
         GOTO1 ADDREC                                                           
         B     XIT                                                              
         SPACE 1                                                                
PURE16   GOTO1 GETREC              OVERWRITE WITH NEW RECORD                    
         LA    R6,IO                                                            
         MVI   ELCODE,X'EF'        SEARCH FOR OLD ACTIVITY ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   PURE18                                                           
         USING RINVAEL,R6                                                       
         MVC   RINVALST,DUB                                                     
         MVI   RINVAWHY,C'C'                                                    
         DROP  R6                                                               
         LR    R3,R6               SAVE ELEMENT ADDRESS                         
         LR    R6,R4                                                            
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL                                                         
         GOTO1 RECUP,DMCB,(2,(R4)),(R3),(R6)                                    
         SPACE 1                                                                
PURE18   MOVE  (IO,1000),0(R4)                                                  
         GOTO1 PUTREC                                                           
         B     XIT                                                              
         EJECT                                                                  
*              HOOK TO HANDLE DEMO RECORDS                                      
         SPACE 3                                                                
DHOOK    NTR1                                                                   
         L     RE,AINTEREC                                                      
         CLI   0(RE),0             TEST FOR INITIALIZED INTERIM REC             
         BNE   DH1                                                              
         MVC   0(1,RE),DBFILE                                                   
         MVC   20(2,RE),=H'24'     DUMMY RECORD LENGTH                          
         SPACE 1                                                                
DH1      MVC   MTHFCTR+2(2),DBFACTOR                                            
         SPACE 1                                                                
         AI    PCNT,1                                                           
         AI    HITCNT,1                                                         
         ZIC   R3,PCNT             1-4                                          
         BCTR  R3,0                                                             
         MH    R3,=H'132'                                                       
         LA    R3,P(R3)            POSITION TO P1-P4                            
         TM    BOOK,X'08'                                                       
         BO    DH2                                                              
         MVC   BLOCK(4),SPACES                                                  
         GOTO1 DEFINE,DMCB,=C'PURE',DBLOCK,BLOCK                                
         MVC   74(4,R3),BLOCK+3                                                 
         SPACE 1                                                                
DH2      GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,BLOCK                                 
         MVC   80(3,R3),BLOCK+2                                                 
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,BLOCK                                
         GOTO1 MILEDIT,DMCB,BLOCK+2,84(R3)                                      
         SPACE 1                                                                
DH4      MVC   BLOCK(16),SPACES                                                 
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,BLOCK                             
         MVC   92(16,R3),BLOCK                                                  
         BAS   RE,PERF                                                          
         BAS   RE,GETSHR           GET SHARES FROM INPUT REC AND WT             
         SPACE 1                                                                
*              MAD BY QUARTER HOUR WEIGHT INTO INTERIM RECORD                   
*                                                                               
         MVC   MTHIFIL,DBFILE      KEEP FILE FORMAT OF INPUT RECORD             
         MVC   MTHOFIL,DBFILE                                                   
         MVC   MTHOSRC(1),DBSELSRC                                              
*====================================================================           
         XCEFL IUNWORK,2016                                                     
         MVC   IUNWORK(1),DBFILE                                                
         MVC   IUNWORK+20,=H'24'   DUMMY RECORD LENGTH                          
         GOTO1 =V(REGETIUN),DMCB,(9,DBLOCK),IUNWORK+500,RR=RELO                 
         MVC   DBNUMVLS,=H'320'                                                 
         LA    R4,IUNWORK+500                                                   
         USING IUNREC,R4                                                        
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
         GOTO1 DEMOUT,DMCB,(C'L',SHARES),DBLOCK,HOMESHR,0                       
         L     R3,DBAQUART                                                      
         L     R4,DBAREC                                                        
         LA    RE,IUNWORK                                                       
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
         GOTO1 DEMAINT,DMCB,=C'PUT',DBLOCK,IUNWORK+500,OFORMAT                  
         DROP  R4                                                               
*                                                                               
         MVC   MTHOSRC,=C'NSI'                                                  
         GOTO1 DEMOMATH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
         ST    R4,DBAREC           RESTORE DBLOCK STUFF                         
         ST    R3,DBAQUART                                                      
         B     HOOK2T                                                           
HOOK1T   GOTO1 DEMOMATH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
*                                                                               
HOOK2T   EQU   *                                                                
*====================================================================           
*--->    GOTO1 DEMOMATH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
         SPACE 1                                                                
         CLI   PCNT,4                                                           
         BNE   DH6                                                              
         BAS   RE,SPLAT                                                         
         MVI   PCNT,0                                                           
         SPACE 1                                                                
DH6      TM    BOOK,X'08'          CAPTURE PROG NAMES FOR TP                    
         BNO   XIT                                                              
         CLC   BLOCK(4),=C'AVG.'   FILTER OUT THIS NAME                         
         BE    XIT                                                              
         LA    R2,PROGEL                                                        
         USING PROGELD,R2                                                       
         OC    PNAME2,PNAME2       TEST FOR TWO NAMES ALREADY                   
         BZ    DH7                                                              
         MVI   VARSW,C'Y'          SET MORE THAN 2 NAMES SWITCH                 
         B     XIT                                                              
         SPACE 1                                                                
DH7      MVI   VARSW,C'N'                                                       
         OC    PNAME1,PNAME1                                                    
         BNZ   DH8                                                              
         MVC   PNAME1,BLOCK                                                     
         B     XIT                 ALL DONE                                     
         SPACE 1                                                                
DH8      MVC   PNAME2(16),BLOCK                                                 
         CLC   PNAME1,PNAME2                                                    
         BE    DH10                                                             
         CLI   HITCNT,1                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
DH10     MVC   PNAME1,BLOCK                                                     
         XC    PNAME2,PNAME2       CLEAR SECOND PROGRAM NAME                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET SHARES AND UPDATE ACCUMULATORS                    
         SPACE 3                                                                
GETSHR   NTR1                                                                   
         MVC   DUB(2),DBACTBK                                                   
         GOTO1 DEMOUT,DMCB,(C'P',DEMOSHR),DBLOCK,HOMSHR                         
         MVC   DBACTBK,DUB         RESTORE BOOK VALUE                           
         SPACE 1                                                                
         LA    R1,HOMSHR           POINT TO OUTPUT AREA                         
         LA    RE,TOTSHR           POINT TO ACCUMS                              
         LA    R0,3                                                             
*                                                                               
GETSHR2  L     RF,0(R1)                                                         
         MH    RF,MTHFCTR+2        MULTIPLY SHARES BY WEIGHTING                 
         A     RF,0(RE)            UPDATE SHARE ACCUMS                          
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)            NEXT OUTPUT VALUE                            
         LA    RE,4(RE)            NEXT ACCUM                                   
         BCT   R0,GETSHR2                                                       
         B     XIT                                                              
         SPACE 2                                                                
DEMOSHR  DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO DIVIDE SHARES BY TOTAL WEIGHTING                      
         SPACE 3                                                                
DIVSHR   NTR1                                                                   
         LA    R0,3                COUNTER                                      
         LA    R1,TOTSHR           POINT TO SHARE ACCUMS                        
         LA    R2,HOMSHR           OUTPUT AREA                                  
         SPACE 1                                                                
DIVSHR2  L     RF,0(R1)                                                         
         SR    RE,RE                                                            
         SLDL  RE,1                ROUNDED DIVIDE                               
         D     RE,MTHFCTR                                                       
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
         ST    RF,0(R2)            UNWEIGHTED VALUE TO OUTPUT                   
         LA    R1,4(R1)            POINT TO NEXT SHARE                          
         LA    R2,4(R2)                                                         
         BCT   R0,DIVSHR2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              SHOW PERFORMANCE FOR EACH LINE                                   
         SPACE 3                                                                
PERF     NTR1                                                                   
         EDIT  (4,MTHFCTR),(6,108(R3)),1                                        
         LA    R2,DEMLIST                                                       
         LA    R3,112(R3)                                                       
         LA    R4,3                                                             
         XC    DEMOS,DEMOS         CLEAR AN OUTPUT AREA FOR DEMOUT              
         LA    R5,DEMOS                                                         
         SPACE 1                                                                
         GOTO1 DEMOUT,DMCB,(C'P',(R2)),DBLOCK,DEMOS                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
FORMDEM2 EQU   *                                                                
         L     R1,0(R5)                                                         
         EDIT  (R1),(6,(R3)),1                                                  
         LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R5,4(R5)            POINT TO NEXT OUTPUT VALUE                   
         BCT   R4,FORMDEM2                                                      
         B     XIT                                                              
         SPACE 1                                                                
DEMLIST  DS    0H                                                               
         DC    X'81',C'R',AL1(01)                                               
         DC    X'81',C'S',AL1(01)                                               
         DC    X'81',C'P',AL1(01)                                               
         DC    X'81',C'R',AL1(45)                                               
         DC    X'81',C'R',AL1(95)                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO OUTPUT CHUNKS                                         
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         SPACE 2                                                                
         GOTO1 SPOOL,PARAS,(R8)                                                 
         B     XIT                                                              
         SPACE 2                                                                
NEWDPT   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
TRACE    NTR1                                                                   
         L     R4,=A(DATA)                                                      
         A     R4,RELO                                                          
         USING RINVKEY,R4                                                       
         MVC   DUB,RINVLEN                                                      
         LH    R3,DUB                                                           
         SPACE 1                                                                
TRACE2   LTR   R3,R3                                                            
         BZ    XIT                                                              
         CH    R3,=H'40'                                                        
         BH    TRACE4                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R4)                                                       
         LA    R3,1(R3)                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+44,(R3),=C'TOG'                               
         MVI   SPACING,2                                                        
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
TRACE4   MVC   P(40),0(R4)                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+44,40,=C'TOG'                                 
         BAS   RE,SPLAT                                                         
         LA    R4,40(R4)                                                        
         SH    R3,=H'40'                                                        
         B     TRACE2                                                           
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RE,4(RD)                                                         
         CLC   0(4,RE),=C'SPUL'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     *-14                                                             
         LM    RE,RC,12(RE)                                                     
         DROP  RF                                                               
         L     R2,SAVER2                                                        
         MVC   H4+10(4),TITSTAT                                                 
         MVC   H4+16(24),TITMKT                                                 
         LA    R3,DPTBL            LOOK UP DAYPART                              
         MVC   DPBYTE,0(R2)                                                     
         SPACE 2                                                                
HOOK2    CLC   DPBYTE,0(R3)                                                     
         BE    HOOK4                                                            
         LA    R3,L'DPTBL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   HOOK2                                                            
         SPACE 2                                                                
HOOK4    MVC   H4+60(20),1(R3)     DAYPART                                      
         MVC   H4+101(3),TITSRCE                                                
         ZIC   R1,BOOK+2           BOOK                                         
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   H4+113(3),0(R1)                                                  
         MVI   H4+116,C'/'                                                      
         EDIT  (1,BOOK+1),(2,H4+117)                                            
         MVC   H4+120(2),=C'TP'                                                 
         CLI   TITBOOK,C'T'                                                     
         BE    *+10                                                             
         MVC   H4+120(3),=C'PAV'                                                
         OC    ABOX(4),ABOX                                                     
         BZ    HOOK6               ON-LINE, NO BOX TO SET                       
         L     R4,ABOX             INITIALIZE BOXES                             
         USING BOXD,R4                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+05,C'C'     NUM                                          
         MVI   BOXCOLS+15,C'C'     EFF DATE                                     
         MVI   BOXCOLS+25,C'C'     DAY(S)                                       
         MVI   BOXCOLS+37,C'C'     TIME(S)                                      
         MVI   BOXCOLS+66,C'C'     PROGRAMMING                                  
         MVI   BOXCOLS+70,C'R'     TR                                           
         MVI   BOXCOLS+73,C'L'                                                  
         MVI   BOXCOLS+79,C'C'     NUM                                          
         MVI   BOXCOLS+83,C'C'     DAY                                          
         MVI   BOXCOLS+91,C'C'     TIME                                         
         MVI   BOXCOLS+107,C'C'    PROGRAMMING                                  
         MVI   BOXCOLS+111,C'C'    WT                                           
         MVI   BOXCOLS+118,C'C'    HUT                                          
         MVI   BOXCOLS+125,C'C'    SHR                                          
         MVI   BOXCOLS+132,C'R'    HUT                                          
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
HOOK6    EQU   *                                                                
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
         DS    CL4                    DBLOCK 4 BYTE OVERFLOW                    
DBEXTRA1 DS    CL128                                                            
         EJECT                                                                  
*              CONSTANTS TABLES LTORG ETC                                       
         SPACE 3                                                                
TPCNT    DC    AL1(0)                                                           
HITCNT   DC    AL1(0)                                                           
PCNT     DC    AL1(0)                                                           
VARSW    DS    C                                                                
PROGEL   DS    CL255                                                            
CEEL     DS    CL10                                                             
CDEL     DS    CL10                                                             
         SPACE 1                                                                
         DS    0F                                                               
MATHFAC  DS    0CL17                                                            
MTHCFACS DS    F                                                                
MTHFCTR  DS    F                                                                
MTHIFIL  DS    CL3                                                              
MTHOFIL  DS    CL3                                                              
MTHOSRC  DS    CL3                                                              
         SPACE 1                                                                
RECUP    DS    V                                                                
DEMOMATH DS    V                                                                
DEMAINT  DS    V                                                                
DAYUNPK  DS    V                                                                
MILEDIT  DS    V                                                                
DEFINE   DS    V                                                                
ADATAREC DS    A                                                                
AINTEREC DS    A                                                                
HOMSHR   DS    3F                                                               
TOTSHR   DS    3F                                                               
DEMODUB  DS    D                                                                
         SPACE 1                                                                
         DS    0H                                                               
DPTBL    DS    0CL21                                                            
         DC    CL21'MMORNING'                                                   
         DC    CL21'DDAYTIME'                                                   
         DC    CL21'EEARLY FRINGE'                                              
         DC    CL21'REARLY NEWS'                                                
         DC    CL21'APRIME ACCESS'                                              
         DC    CL21'TLATE NEWS'                                                 
         DC    CL21'LLATE FRINGE'                                               
         DC    CL21'WWEEKEND'                                                   
         DC    CL21'KKIDS'                                                      
         DC    CL21'FFRINGE'                                                    
         DC    CL21'NNEWS'                                                      
         DC    CL21'PPRIME'                                                     
         DC    CL21'VMOVIES'                                                    
         DC    CL21'SSPECIAL'                                                   
         DC    CL21'JSPORTS'                                                    
         DC    CL21'OSOAPS'                                                     
         DC    CL21'UCOMPETITIVE'                                               
         DC    CL21'XLOCAL'                                                     
         DC    CL21'YOTHER'                                                     
         DC    X'FF'                                                            
         DC    CL20'GENERAL'                                                    
NDPT     EQU   (*-DPTBL)/L'DPTBL                                                
         SPACE 2                                                                
TYPTAB   DC    C'TTSA.'                                                         
OFORMAT  DC    C'IUNUIUN',X'530B00'                                             
INDEX    DC    C'&&',X'FF'            INDEX UPGRADE MARKER                      
SHARES   DC    X'00',C'S',AL1(1)                                                
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
IUNWORK  DS    2016C                                                            
RELOC    DC    A(*)                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
* PROGRAM NAME TEXT ELEMENT DSECT                                               
*                                                                               
PROGELD  DSECT                                                                  
PCODE    DS    X                                                                
PELLEN   DS    X                                                                
PLIN     DS    X                                                                
         DS    CL3                 SPARE                                        
PFBK     DS    0CL5                FROM BOOK                                    
PMON     DS    CL3                                                              
PYR      DS    CL2                                                              
         DS    CL1                 SPARE                                        
PINVCODE DS    CL2                                                              
PEQS     DS    CL1                                                              
PNAME1   DS    CL16                FIRST PROGRAM NAME                           
PNAME2   DS    CL16                SECOND PROGRAM NAME                          
PROGELX  EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDBEXTRAD                                                     
DUMMY    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENREP                                                       
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE RESPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE RESPLFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESPLD7D                                                       
         PRINT ON                                                               
STRTOPT  DS    CL3                                                              
ENDOPT   DS    CL3                                                              
DPLIST   DS    CL20                                                             
SAVER2   DS    F                                                                
SAVESTAK DS    2F                                                               
ASTACK   DS    A                                                                
RELO     DS    A                                                                
DPBYTE   DS    CL1                                                              
BTODAY   DS    CL2                                                              
P5       DS    CL133                                                            
P6       DS    CL133                                                            
         EJECT                                                                  
*                                                                               
*        DEMO RECORD IUN DSECT FOR USE BY FIXPAV                                
*                                                                               
IUNREC   DSECT                                                                  
UPREC    DS    0F                                                               
***********************************************************************         
*                                  ORIGINAL BOOK VALUES               *         
***********************************************************************         
OLDUNV   DS    (NUMVALS)F          UNIVERSES                          *         
OLDUNVX  EQU   *                                                      *         
***********************************************************************         
OLDRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                      *         
         ORG                                                                    
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
OLDRTGX  EQU   *                                                      *         
***********************************************************************         
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                      *         
         ORG                                                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                      *         
         ORG                                                                    
OLDHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  NEW VALUES                         *         
NEWUNV   EQU   OLDTOT              DEFINE ORIGIN FOR SPGETIUN CALL    *         
*                                                                     *         
***********************************************************************         
NEWRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                      *         
         ORG                                                                    
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
NEWRTGX  EQU   *                                                      *         
***********************************************************************         
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                      *         
         ORG                                                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
NEWHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  OTHER VALUES                       *         
***********************************************************************         
HOMESHR  DS    3F                  ORIGINAL HOMES SHARES              *         
HOMSHRX  EQU   *                                                      *         
HOMSHRLN EQU   *-HOMSHR                                               *         
***********************************************************************         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                       *         
LUNVX    EQU   *                                                      *         
***********************************************************************         
UPRECX   DS    0F                                                               
*                                                                               
NUMVALS  EQU   32                                                               
DISPHOM  EQU   20                                                               
LENVALS  EQU   NUMVALS*4                                                        
*                                                                               
         EJECT                                                                  
DATA     CSECT                                                                  
         DC    2000X'00'                                                        
         SPACE 1                                                                
STACK    CSECT                                                                  
         DC    5000X'00'                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065RESPL97   05/10/05'                                      
         END                                                                    
