*          DATA SET SPREPFXYK  AT LEVEL 056 AS OF 03/06/01                      
*PHASE SPFX02I                                                                  
         TITLE 'SPFX02 - CANADIAN NETWORK SPOT COUNT REPORT'                    
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PBUY                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* REQFRST                                                                       
REQF     DS    0H                                                               
         XC    COUNT,COUNT                                                      
         B     EXIT                                                             
                                                                                
* PROCBUY                                                                       
PBUY     DS    0H                                                               
*                                                                               
*  CODE HERE                                                                    
*                                                                               
*                                                                               
* PRINT THE HEADER                                                              
*                                                                               
         BAS   RE,PRTBUY           PRINT HEADER OF MY TABLE                     
         GOTO1 REPORT                                                           
*                                                                               
         USING BUYRECD,R6                                                       
         L     R6,ADBUY            <==  YOU HAVE NETWORK BUY ALREADY            
         LA    R3,TABLE                                                         
         USING MYTABD,R3                                                        
*                                                                               
* FILL MYTAB W/ DATES FROM 01 ELEM                                              
*                                                                               
         ZIC   R0,BDWKS            PUT #OF WKS IN R0                            
         GOTO1 DATCON,DMCB,(3,BDSTART),(2,DATE)   PUT 1ST DATE IN TAB           
         GOTO1 DATCON,DMCB,(3,BDSTART),(0,TMPDATE1)  PREP FOR ADDAY             
         B     DECR                                                             
*                                                                               
DATEPUT  GOTO1 ADDAY,DMCB,TMPDATE1,TMPDATE2,7       ADD A WEEK                  
         MVC   TMPDATE1,TMPDATE2                                                
         LA    R3,L'ROWLQ(R3)      MOVE TO NXT ROW OF MYTAB                     
         GOTO1 DATCON,DMCB,(0,TMPDATE1),(2,DATE)   PUT NEW DATE IN TAB          
DECR     BCT   R0,DATEPUT                                                       
*                                                                               
*        GOTO1 DATCON,DMCB,(0,TMPDATE1),(3,TMPDATE2)                            
*        CLC   BDEND,TMPDATE2                                                   
*        BNE   ERROR                                                            
         LA    R3,L'ROWLQ(R3)      MOVE TO LAST ROW OF MYTAB                    
         MVC   DATE,=X'FFFF'       EOT                                          
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYMSTA,WORK,P+12    PRINT NETWK STA NAME            
*                                                                               
*COUNTING DATES FOR 0B & 0C ELEMS ( NETWORK LEVEL )                             
*                                                                               
*        XC    OBCNTR,OBCNTR       RESET ELEM COUNTER                           
         LA    R6,BDELEM                                                        
         LA    R3,TABLE                                                         
*                                                                               
DATECNG  BAS   RE,COUNTOB         SUB WILL COUNT 0B & OC ELEMS                  
*                                                                               
         MVC   NTVLVL,OBCNTR      WHEN EO/REC STORE COUNTER IN MYTAB            
         XC    OBCNTR,OBCNTR      RESET FO NXT DATE                             
         LA    R3,L'ROWLQ(R3)     GET NEXT DATE                                 
         CLC   DATE,=X'FFFF'                                                    
         BE    LOCLSTA                                                          
         L     R6,ADBUY           REPOINT R6 TO 1ST ELEM                        
         LA    R6,(BDELEM-BUYREC)(R6)                                           
         B     DATECNG                                                          
*                                                                               
*COUNTING 0B & 0C ELEMS IN LOCAL STATIONS(FROM 68 ELEM)                         
*                                                                               
LOCLSTA  MVC   SAVBKEY,KEY         SAVE ORIGINAL KEY                            
*        MOVE REC TO MY IO AREA                                                 
         L     R0,=A(IOAREA)       SET 'TO' ADDRESS                             
         A     R0,RELO                                                          
         LR    R5,R0                                                            
         L     RE,ADBUY            SET 'FROM' ADDR                              
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)         SET 'TO' LEN                                 
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
*                                                                               
         XR    R1,R1                                                            
         LA    R1,STADISP          STORE DISPL TO 1ST ELEM                      
         STC   R1,DISPL                                                         
*                                                                               
         XC    STACTR,STACTR      RESET COUNTER FOR # OF LOCAL STAT             
         LA    R4,P+18                                                          
*                                                                               
         USING NTWKELEM,R5                                                      
         MVI   ELCODE,X'68'                                                     
         BAS   RE,GETEL                                                         
         BNE   STADONE                                                          
         ZIC   R1,STACTR          IF 68 ELEM FOUND INCRENET CNTR                
         LA    R1,1(R1)                                                         
         STC   R1,STACTR                                                        
*                                                                               
GETSTA   XC    KEY,KEY             GET OTHER EXPLODED RECORDS                   
         MVC   KEY(13),SAVBKEY                                                  
         MVC   KEY+4(5),NTWKMKST   MKT/STA FROM RECORD                          
         GOTO1 MSUNPK,DMCB,NTWKMKST,WORK,(R4)                                   
         LA    R4,6(R4)                                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     IS THERE REC W/ THIS KEY?                    
         BNE   NXTSTA              IF NOT, FIND NXT 68 ELEM                     
         GOTO1 GETBUY                                                           
         LA    R3,TABLE                                                         
*    CODE TO COUNT 0B ELEMS                                                     
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         LA    R6,BDELEM                                                        
*                                                                               
CHADT    BAS   RE,COUNTOB                                                       
*                                                                               
         ZIC   R1,DISPL            GET DISPL TO 1ST LOC STA IN MYTAB            
         AR    R1,R3                                                            
         MVC   0(1,R1),OBCNTR      STORE COUNTER VALUE IN LOCAL STA             
         XC    OBCNTR,OBCNTR                                                    
         LA    R3,L'ROWLQ(R3)                                                   
         CLC   DATE,=X'FFFF'                                                    
         BE    ENDSTA                                                           
         L     R6,ADBUY                                                         
         LA    R6,(BDELEM-BUYREC)(R6)  REPOINT R6 TO 1 ELEM IN BYE REC          
         B     CHADT                                                            
*                                                                               
ENDSTA   DS    0H                                                               
*                                                                               
NXTSTA   BAS   RE,NEXTEL           GET NEXT 68 ELEM                             
         BNE   STADONE                                                          
*                                                                               
         ZIC   R1,DISPL            IF FOUND RESET DISPL TO NXT                  
         LA    R1,1(R1)            AVAILABLE SPOT IN MYTAB                      
         STC   R1,DISPL                                                         
*                                                                               
         ZIC   R1,STACTR           INCREMENT STA CNTR                           
         LA    R1,1(R1)                                                         
         STC   R1,STACTR                                                        
         B     GETSTA                                                           
*                                                                               
STADONE  DS    0H                                                               
         DROP  R6,R5                                                            
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
*     PRINTING FROM MYTAB                                                       
         LA    R3,TABLE                                                         
PRTLINE  CLC   DATE,=X'FFFF'           EOT?                                     
         BE    ENDREC                                                           
         GOTO1 DATCON,DMCB,(2,DATE),(11,P)   PUT DATE IN PRINTABLE FORM         
         EDIT  NTVLVL,(1,P+16),ZERO=NOBLANK                                     
         LA    R6,P+22                                                          
         ZIC   R4,STACTR               PUT # OF 68 ELEMS FOUND                  
         LA    R2,STADISP              GET IN R2 ADDR OF 1ST LOC STA            
         AR    R2,R3                   IN MYTAB                                 
STAPRT   MVC   TMPCNTR,0(R2)                                                    
         EDIT  TMPCNTR,(1,(R6)),ZERO=NOBLANK                                    
         LA    R2,1(R2)                POINT TO NXT LOC STA                     
         LA    R6,6(R6)                MOVE TO NXT AVAIL SPACE TO PRT           
         BCT   R4,STAPRT                                                        
         GOTO1 REPORT                                                           
         LA    R3,L'ROWLQ(R3)                                                   
         B     PRTLINE                                                          
*                                                                               
ENDREC   GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*RROR    MVC   P(10),=C'DATE ERROR'                                             
*        GOTO1 REPORT                                                           
*                                                                               
*EQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
*        EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
*        GOTO1 REPORT                                                           
REQL     B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*     SUB WILL COUNT NUMBER OF 0B & 0C ELEMENTS IN RECORD             *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ** * * *          
COUNTOB  NTR1                                                                   
         USING REGELEM,R6                                                       
         USING MYTABD,R3                                                        
         XC    OBCNTR,OBCNTR                                                    
*                                                                               
CKELEM   CLI   0(R6),0             END OF RECORD?                               
         BE    BACK                                                             
         CLI   0(R6),X'0B'                                                      
         BNE   NXTCK               IF NOT 0B,CHECK FOR 0C                       
CKDATE   CLC   DATE,RDATE          CHK IF DATE IN ELEM = DATE IN MYTAB          
         BNE   NXTEL                                                            
         ZIC   R1,OBCNTR           IF YES, INCREMENT COUNTER                    
         LA    R1,1(R1)                                                         
         STC   R1,OBCNTR                                                        
         B     NXTEL                                                            
NXTCK    CLI   0(R6),X'0C'         IF 0C ELEM, DO THE SAME AS 0B                
         BNE   NXTEL                                                            
         BE    CKDATE                                                           
NXTEL    ZIC   R1,1(R6)            PREP TO LOOK FOR NXT ELEM                    
         AR    R6,R1                                                            
         B     CKELEM                                                           
*                                                                               
BACK     B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PRINT BUY                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTBUY   NTR1                                                                   
*        SR    R0,R0                                                            
*        ICM   R0,3,13(R5)                                                      
*        GOTO1 PRNTBL,DMCB,=C'BUYREC',(R5),C'DUMP',(R0),=C'1D00'                
*                                                                               
         L     R5,ADBUY                                                         
*        A     R5,RELO                                                          
         USING BUYRECD,R5                                                       
PRTB2    LA    R4,P                                                             
         USING PLINED,R4                                                        
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVC   PPRD,=C'POL'                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
*        CLI   QOPT5,C'Y'          PRINT OUT KEY FOR DEBUGGING                  
*        BNE   PRTB3                                                            
*        GOTO1 HEXOUT,DMCB,KEY,PKEY,18                                          
PRTB3    GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
         GETEL R5,24,ELCODE                                                     
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
SAVBKEY  DS    XL13                                                             
SAVEKEY2 DS    XL18                                                             
SVKEY    DS    XL10                AM(1),CLT(2),PRD(1),MKSTA(5),EST(1)          
TMPDATE1 DS    CL6                 TO HOLD DATE FOR ADDAY                       
TMPDATE2 DS    CL6                 TO HOLD DATE FOR ADDAY                       
OBCNTR   DS    X                   COUNTER FOR 0B & 0C ELEMS                    
STACTR   DS    X                   COUNTER FOR 68 ELEMS                         
TMPCNTR  DS    X                   TEMP HOLDER FOR STA COUNTER                  
DISPL    DS    X                   DISPL-T OF STATION IN MYTAB                  
ADDRIO   DS    F                   HOLD TEMP ADDR OF MY IO                      
*                                                                               
* TABLE HERE                                                                    
*                                                                               
TABLE    DS    7700X                                                            
IOAREA   DS    2000X                                                            
*                                                                               
         DS    0F                                                               
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PIND2    DS    CL1                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
MYTABD   DSECT                                                                  
ROWLQ    DS    0CL77                                                            
DATE     DS    XL2                                                              
STATSLQ  DS    0XL74                                                            
NTVLVL   DS    X                                                                
STADISP  EQU   *-MYTABD                                                         
STATN    DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SPREPFXYK 03/06/01'                                      
         END                                                                    
