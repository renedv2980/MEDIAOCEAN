*          DATA SET RERRGONLD  AT LEVEL 031 AS OF 05/01/02                      
*PHASE RERRGLDA,+0                                                              
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE LOADER                                                                 
*INCLUDE DMDADDS                                                                
*INCLUDE DMDYNDD                                                                
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'RERRGLD - RRG FILE LOAD'                                        
***********************************************************************         
*  HISTORY OF CHANGES                                                 *         
***********************************************************************         
*                                                                     *         
* JUL24/95 (BG ) 008 ADD 4 PRIOR-1 FIELDS                             *         
*                                                                     *         
* APR18/96 (BG ) 009 ADD BUDP - PRIOR YEAR BUDGET                     *         
*                                                                     *         
* APR30/96 (BG ) 010 CHA REC SIZE TO 128, ADD WEEK 1 YR AGO           *         
*                                                                     *         
* MAY24/96 (BG ) 011 ADD TO REPTAB, CK MAX SIZE REPTAB                *         
*                                                                     *         
* MAY29/96 (BG ) 012 CHA REC SIZE TO 104                              *         
*                                                                     *         
* MAY30/96 (BG ) 013 PUT DATA TYPES IN 0001 RECORD                    *         
*                                                                     *         
* JUN06/96 (BG ) 014 ADD 4G, 6G CONF DOLLARS, DIRECT DOLLARS          *         
*                                                                     *         
* JUN14/96 (BG ) 015 REPTAB FROM 300 TO 400                           *         
*                                                                     *         
* JAN03/97 (BG ) 016 ADD DEV CONTRACT TYPE                            *         
*                                                                     *         
* APR22/97 (BG ) 017 FIX OWNER DOUBLING DOLLARS, ADD TRACES           *         
*                                                                     *         
* APR23/98 (BG ) 018 NOW 19 BUCKETS, ACCEPT BG SPECS, LARGER RECS     *         
*                                                                     *         
* APR28/98 (BG ) 019 SYNC UP WITH CHANGES FOR OLD VERSION             *         
*                                                                     *         
* MAY14/98 (BG ) 020 CHNGE QLGRGRP (1 CHAR) FROM X BLANK TO X NULL    *         
*                                                                     *         
* JUN03/98 (BG ) 021 ADD LAST YEAR PACED AS OF TODAY                  *         
*                                                                     *         
* SEP11/98 (BG ) 022 ADD BF - SALESPERSON (SP) TREATED AS 3G          *         
*                                                                     *         
* APR21/99 (BG ) 024 ADD SALESPERSON CODE TO RECTYPS TABLE            *         
*                                                                     *         
* MAY20/99 (BG ) 025 REPTAB FROM 400 TO 600                           *         
*                                                                     *         
* JUL18/99 (BG ) 026 ADD 7G AND REPTAB FROM 600 TO 800                *         
*                                                                     *         
* SEP13/99 (BG ) 027 FIX EDITING RECORD TOTALS                        *         
*                                                                     *         
* OCT11/99 (BG ) 028 ADD 1G                                           *         
*                                                                     *         
* NOV06/99 (BU ) SKIP ADV WITH NO CODE                                *         
*                                                                     *         
* NOV08/99 (BG ) 028 ERR MSG                                          *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
RERRGLD  CSECT                                                                  
         NBASE 0,*RERRGLD,=V(REGSAVE),R9                                        
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         EJECT                                                                  
*                                                                               
*        INITIALIZATION                                                         
*                                                                               
*        READ PARAMETER CARDS                                                   
*                                                                               
         MVC   CARD,SPACES                                                      
         SPACE                                                                  
RGL10    DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    RGL60                                                            
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   =C'TRACE=',CARD                                                  
         BNE   RGL20                                                            
         MVC   TRACES,CARD+6     RECS RD, INTO SORT, FROM SORT, PUTS            
         B     RGL10                                                            
*                                                                               
RGL20    DS    0H                                                               
         CLC   =C'DUMP=',CARD                                                   
         BNE   RGL30                                                            
         MVC   ABNDOPT,CARD+5                                                   
         B     RGL10                                                            
*                                                                               
RGL30    DS    0H                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   RGL40                                                            
         ICM   RF,15,VDDSIO                                                     
         BZ    RGL10                                                            
         MVC   0(8,RF),CARD+6                                                   
         B     RGL10                                                            
RGL40    DS    0H                                                               
         MVC   P(8),=C'BAD CARD'                                                
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
*                                                                               
*                                                                               
*        MAIN PROCESSING                                                        
*                                                                               
RGL60    GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         LA    R1,MONLOHI          INIT GLOBAL LO AND HI MONTH                  
         LA    R0,MAXREPS          FOR EACH REP                                 
MA002    XC    0(2,R1),0(R1)                                                    
         MVC   2(2,R1),=X'FFFF'                                                 
         XC    4(2,R1),4(R1)                                                    
         LA    R1,L'MONLOHI(R1)                                                 
         BCT   R0,MA002                                                         
*                                                                               
         OPEN  (IN,INPUT)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MA010    DS    0H                                                               
         GET   IN,INREC                                                         
         CLI   TRACEREC,C'Y'       TRACE RECS READ                              
         BNE   *+8                                                              
         BAS   RE,TRCRD                                                         
         SPACE                                                                  
         AP    RECINCT,=P'1'                                                    
         TM    INCOM,X'01'         TEST THIS RECORD FOR ONLINE FILE             
         BZ    MA010                                                            
*                                                                               
         MVC   RPTCD(3),INCOM+1    REQUEST NUM AND REPORT NUM                   
         MVC   REPCD,INCOM+4       REP CODE                                     
         MVI   FOUND,C'N'                                                       
         XC    SAVER2,SAVER2                                                    
         XC    ASPECS,ASPECS                                                    
         LA    R2,REPTAB                                                        
         USING REPORTD,R2                                                       
*                                                                               
MA020    CLC   RPTCD,RPREQ         SEARCH REPORT TABLE FOR REQ/RPT              
         BE    MA030               FOUND                                        
         CLC   RPREQ,RPTCD         COMPARE REQ                                  
         BNE   *+10                                                             
         MVC   ASPECS,RPSPECS      EQ - SAVE SPECS ADDR                         
         OC    RPREQ,RPREQ         IS THIS END OF TABLE                         
         BNZ   MA040               NO - CONTINUE SEARCH                         
         CLI   FOUND,C'Y'          YES- FOUND YET ?                             
         BE    MA010                    YES- READ NEXT                          
         BAS   RE,LDSPECS               NO - LOAD THE SPECS                     
*                                                                               
MA030    BAS   RE,POST             POST RRGON RECORD                            
         BNE   MA100                                                            
         MVI   FOUND,C'Y'                                                       
*                                                                               
MA040    LA    R2,REPORTX-REPORTD(R2)                                           
         CLI   0(R2),X'FF'                                                      
         BNE   MA020               CONTINUE REPORT TABLE SEARCH                 
         CLI   FOUND,C'Y'                                                       
         BE    MA010               READ NEXT                                    
         DC    H'0'                                                             
*                                                                               
MA100    CLOSE (IN)                END OF INPUT                                 
         LA    R3,MONLOHI                                                       
         LA    R0,MAXREPS          BUILD TYPE 1 HEADER RECORDS                  
*                                  FOR EACH REP                                 
MA102    OC    0(2,R3),0(R3)                                                    
         BZ    MA104                                                            
         XC    SORTREC,SORTREC                                                  
         MVC   ROKREP,0(R3)                                                     
         MVC   ROKHD1,=X'0001'                                                  
         GOTO1 =V(DATCON),DMCB,(5,0),(3,RODHD1DT)                               
         THMS                                                                   
         STCM  R1,15,RODHD1TM      DATE AND TIME TO HEADER REC                  
         CLI   3(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),1             FORCE START MONTH TO 1                       
         SPACE                                                                  
         MVC   RODHD1ST,2(R3)      LO AND HI MONTHS TO HDR REC                  
         MVC   RODHD1EN,4(R3)                                                   
         MVC   RODHDOPT,6(R3)      MOVE IN OPTIONS - ALL/CONF/UNCON/DIR         
         SPACE                                                                  
         BAS   RE,PUTSORT          PUT HDR REC TYPE 1                           
         LA    R3,L'MONLOHI(R3)                                                 
         BCT   R0,MA102                                                         
*                                                                               
MA104    LA    R2,REPTAB           BUILD TYPE 2 HDR RECS                        
*                                                                               
MA110    LA    R4,RPTYP                                                         
         LA    RF,L'RPTYP                                                       
*                                                                               
MA112    CLI   0(R4),26            GRSUBGRP                                     
         BNE   *+8                                                              
         MVI   0(R4),7             CHANGE TO GROUP                              
*                                                                               
         CLI   0(R4),32            STAMKT                                       
         BNE   *+8                                                              
         MVI   0(R4),2             CHANGE TO STATION                            
         LA    R4,1(R4)                                                         
         BCT   RF,MA112                                                         
*                                                                               
         LA    R3,RPMNLOHI                                                      
         LA    R0,MAXREPS          FOR EACH REP --                              
*                                                                               
MA114    OC    0(2,R3),0(R3)                                                    
         BZ    MA116                                                            
         XC    SORTREC,SORTREC                                                  
         MVC   ROKREP,0(R3)                                                     
         SPACE                                                                  
         CLC   RPREQ,=C'1G'        IRNY NEW WITH ALL DOLLARS                    
         BE    MA115                                                            
         CLC   RPREQ,=C'7G'        NO SALESPERSON                               
         BE    MA115                                                            
         CLC   RPREQ,=C'BF'        TEMP TEST                                    
         BE    MA115               TEMP TEST                                    
         CLC   RPREQ,=C'BG'        TEMP TEST                                    
         BE    MA115               TEMP TEST                                    
         CLC   RPREQ,=C'3G'        THIS ALL DOLLARS                             
         BE    MA115                                                            
         SPACE                                                                  
         MVI   ROKHD2RQ,C'C'       FOR CONFIRMED DOLLARS                        
         CLC   RPREQ,=C'4G'                                                     
         BE    MA115                                                            
         SPACE                                                                  
         MVI   ROKHD2RQ,C'D'       FOR DIRECT CONFIRMED DOLLARS                 
         CLC   RPREQ,=C'6G'                                                     
         BE    MA115                                                            
         MVC   P+2(34),=C'ONLY REPORT REQUESTS SUPPORTED ARE'                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P+2(33),=C'1G - ALL DOLLARS SPECIAL FOR IRNY'                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P+2(16),=C'3G - ALL DOLLARS'                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P+2(31),=C'7G - ALL DOLLARS NO SALESPERSON'                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P+2(22),=C'4G - CONFIRMED DOLLARS'                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P+2(21),=C'6G - CONFIRMED DIRECT'                                
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                ONLY THESE REPORTS ALLOWED                   
MA115    DS    0H                                                               
         MVC   ROKHD2,=X'0002'                                                  
*                                                                               
         MVC   ROKHD2TY,RPTYP       MOVE RRGON RPT TYPE TO HDR REC              
         MVC   ROKHD2ST,2(R3)       MOVE RPT LO AND HI MONTHS TO HDR            
         MVC   ROKHD2EN,4(R3)                                                   
         OI    RODATA,X'01'        (MAKES SURE DATA IS SIGNIFICANT)             
         BAS   RE,PUTSORT           PUT THIS TYPE 2 HDR REC                     
*                                                                               
         LA    R3,L'RPMNLOHI(R3)                                                
         BCT   R0,MA114                                                         
*                                                                               
MA116    LA    R2,REPORTX-REPORTD(R2)   NEXT RPT TYPE                           
         CLI   0(R2),X'FF'                                                      
         BE    MA150                                                            
         CLI   0(R2),0                                                          
         BNE   MA110                                                            
*                                                                               
MA150    MVC   SORTREC-4(2),=AL2(L'ROREC+4)                                     
         MVC   WORKREC-4(2),SORTREC-4                                           
         OPEN  (OUT,OUTPUT)        GET SORT RECS & PUT TO OUTPUT                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RE,15,4(R1)                                                      
         BZ    MA210                                                            
         MVC   SORTREC,0(RE)                                                    
         BAS   RE,GETREC                                                        
*                                                                               
MA160    MVC   REPCD,ROKREP                                                     
         MVI   HDRDONE,C'N'                                                     
         XC    WORKREC,WORKREC                                                  
*                                                                               
MA162    CLC   ROKHD2,=X'0002'     TEST FOR TYPE 2 HDR REC                      
         BL    MA190                                                            
         BE    MA165                                                            
         GOTO1 PUTREC,WORKREC-4    PUT FINAL TYPE 2 HDR REC                     
         MVI   HDRDONE,C'Y'                                                     
         B     MA190                                                            
*                                                                               
MA165    OC    WORKREC,WORKREC     TEST FOR FIRST TYPE 2 HDR REC                
         BNZ   MA170                                                            
         MVC   WORKREC,SORTREC     YES- MOVE IT TO WORK RECORD                  
         B     MA200                                                            
*                                                                               
MA170    CLC   ROKHD2TY,WORKREC+ROKHD2TY-ROREC  COMPARE TYPE 2 HDR REC          
         BE    MA180                            KEYS                            
         GOTO1 PUTREC,WORKREC-4    NE - PUT LAST TYPE 2 HDR REC                 
         MVC   WORKREC,SORTREC                                                  
         B     MA200                                                            
*                                                                               
MA180    LA    R1,WORKREC+ROKHD2ST-ROREC  EQ-UPDATE LO AND HI MONTHS            
         CLC   ROKHD2ST,0(R1)                                                   
         BNL   *+10                                                             
         MVC   0(2,R1),ROKHD2ST                                                 
         LA    R1,WORKREC+ROKHD2EN-ROREC                                        
         CLC   ROKHD2EN,0(R1)                                                   
         BNH   *+10                                                             
         MVC   0(2,R1),ROKHD2EN                                                 
         B     MA200                                                            
*                                                                               
MA190    GOTO1 PUTREC,SORTREC-4                                                 
*                                                                               
MA200    ICM   RF,15,AREC                                                       
         BZ    MA210                                                            
         MVC   SORTREC,0(RF)                                                    
         BAS   RE,GETREC                                                        
         CLC   REPCD,ROKREP        TEST REP CODE CHANGE                         
         BNE   MA160               YES                                          
         CLI   HDRDONE,C'Y'        NO-TEST FOR ALL HDR RECS DONE                
         BE    MA190                                                            
         BNE   MA162                                                            
*                                                                               
MA210    CLOSE (OUT)                                                            
         MVC   P,SPACES                                                         
         MVC   P+1(2),SVREPCD                                                   
         EDIT  (P8,REPRECS),(8,P+9)                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         MVC   P+1(5),=C'*ALL*'                                                 
         EDIT  (P8,TOTRECS),(8,P+9)                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         MVC   P+1(6),=C'*READ*'                                                
         EDIT  (P8,RECINCT),(8,P+9)                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         MVC   P+1(7),=C'*SRTED*'                                               
         EDIT  (P8,SORTCT),(8,P+9)                                              
         GOTO1 =V(PRINTER)                                                      
         CLI   ABNDOPT,C'Y'                                                     
         BNE   MA230                                                            
         DC    H'0'                                                             
MA230    XBASE                                                                  
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO ADD UP SORTER RECORDS WITH SAME KEY                                
*                                                                               
GETREC   NTR1  ,                                                                
         SPACE                                                                  
GETREC10 CLI   TRACSRTO,C'Y'       TRACE OUT OF SORT                            
         BNE   GETREC20                                                         
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         UNPK  WORK(7),FULL(4)                                                  
         MVC   P(2),WORK                                                        
         MVI   P+2,C':'                                                         
         MVC   P+3(2),WORK+2                                                    
         MVI   P+5,C':'                                                         
         MVC   P+6(2),WORK+4                                                    
         MVC   P+10(116),SORTREC                                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),DMCB,SORTREC,WORK,116,=C'SEP'                         
         MVC   P,SPACES                                                         
         MVC   P+10(116),WORK                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(9),=C'FROM SORT'                                               
         MVC   P+10(116),WORK+116                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GETREC20 GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RF,4(R1)                                                         
         ST    RF,AREC                                                          
         LTR   RF,RF                                                            
         BZ    GETRECX                                                          
         CLC   ROKEY,0(RF)                                                      
         BNE   GETRECX                                                          
         CLI   ROKDTLTY,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R0,NFLDS                                                         
         LA    RE,RODPER                                                        
         LA    RF,RODPER-ROREC(RF)                                              
         L     R1,0(RE)                                                         
         A     R1,0(RF)                                                         
         ST    R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,*-20                                                          
         B     GETREC10                                                         
*                                                                               
GETRECX  B     EXIT                                                             
NFLDS    EQU   18               TOTAL # OF FLDS TO ADD (WAS 15, 12, 8)          
         EJECT                                                                  
* PUT A RECORD TO THE OUTPUT FILE                                               
* INPUT  : R1=A(RECORD)                                                         
*                                                                               
         SPACE 1                                                                
PUTREC   NTR1  ,                                                                
         LR    R2,R1                                                            
         SPACE                                                                  
         CLI   TRACPUT,C'Y'        TRACE OUTPUT RECORDS                         
         BNE   PUTREC1                                                          
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         UNPK  WORK(7),FULL(4)                                                  
         MVC   P(2),WORK                                                        
         MVI   P+2,C':'                                                         
         MVC   P+3(2),WORK+2                                                    
         MVI   P+5,C':'                                                         
         MVC   P+6(2),WORK+4                                                    
         MVC   P+10(80),4(R2)                                                   
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),DMCB,4(R2),WORK,80,=C'SEP'                            
         MVC   P,SPACES                                                         
         MVC   P+10(80),WORK                                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P+1(7),=C'O/P REC'                                               
         MVC   P+10(80),WORK+80                                                 
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PUTREC1  PUT   OUT,(R2)                                                         
         CP    TOTRECS,=P'0'                                                    
         BNE   PUTREC2                                                          
         MVC   SVREPCD,REPCD                                                    
         MVC   P,SPACES                                                         
         MVC   P(13),=C'REP     COUNT'                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         MVC   P(14),=C'---    -------'                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
         B     PUTREC4                                                          
*                                                                               
PUTREC2  CLC   REPCD,SVREPCD                                                    
         BE    PUTREC4                                                          
         MVC   P,SPACES                                                         
         MVC   P+1(2),SVREPCD                                                   
         EDIT  (P8,REPRECS),(8,P+9)                                             
         GOTO1 =V(PRINTER)                                                      
         ZAP   REPRECS,=P'0'                                                    
         MVC   SVREPCD,REPCD                                                    
*                                                                               
PUTREC4  AP    REPRECS,=P'1'                                                    
         AP    TOTRECS,=P'1'                                                    
*                                                                               
PUTRECX  B     EXIT                                                             
         EJECT                                                                  
LDSPECS  NTR1                                                                   
*                                                                               
*        ROUTINE TO LOAD REQUEST SPECS                                          
*                                                                               
         ICM   R4,15,ASPECS        TEST SPECS ALREADY LOADED                    
         BNZ   LS010                                                            
         MVC   WORK(8),SPACES                                                   
         MVC   WORK(4),=C'RERG'                                                 
         MVC   WORK+4(2),RPTCD                                                  
         GOTO1 =V(LOADER),DMCB,WORK     LOAD THE SPECS                          
         MVC   ASPECS,DMCB+4                                                    
         ICM   R4,15,ASPECS                                                     
         BNZ   LS010                                                            
         DC    H'0'                                                             
*                                                                               
LS010    STCM  R4,15,RPSPECS                                                    
         MVC   RPREQ(3),RPTCD                                                   
         MVI   SPEC,10             GET FIRST REPORT SPEC                        
         BAS   RE,GETSPEC                                                       
         B     *+8                                                              
*                                                                               
LS020    BAS   RE,NEXTSPEC         GET NEXT REPORT SPEC                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   RPRPT,2(R4)         DO WE HAVE THE CORRECT REPORT                
         BNE   LS020                                                            
         MVI   SPEC,18             YES- LOOK FOR ONLINE SPEC(S)                 
         BAS   RE,NEXTSPEC                                                      
         BE    LS025                                                            
         DC    H'0'                                                             
*                                                                               
LS025    ST    R4,SVONLSP          SAVE ONLINE SPEC ADDR                        
         MVC   RPTYP,2(R4)         RRGON RECORD TYPE                            
         LA    R1,RPMNLOHI                                                      
         LA    R0,MAXREPS          INIT LO AND HI MONTHS FOR EACH REP           
*                                                                               
LS026    XC    0(2,R1),0(R1)                                                    
         MVC   2(2,R1),=X'FFFF'                                                 
         XC    4(2,R1),4(R1)                                                    
         LA    R1,L'RPMNLOHI(R1)                                                
         BCT   R0,LS026                                                         
*                                                                               
         LA    R3,RPROWS           FIND THE ROW ADDRESS(ES)                     
         LA    R5,RPTYP                                                         
         LA    R6,3                                                             
         MVI   SPEC,30                                                          
         BAS   RE,GETSPEC                                                       
         ST    R4,FULL             SAVE ADDR OF FIRST ROW SPEC                  
*                                                                               
LS030    CLI   0(R5),0                                                          
         BE    LS036                                                            
         L     R4,FULL                                                          
         BAS   RE,GETSPEC          GET FIRST ROW SPEC                           
         B     *+8                                                              
*                                                                               
LS032    BAS   RE,NEXTSPEC         NEXT ROW SPEC                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R5),3(R4)                                                    
         BE    LS034                                                            
         CLI   0(R5),4             CHECK FOR OFFICE                             
         BNE   LS033                                                            
         CLI   3(R4),14            YES- OFFC CODE WILL DO                       
         BNE   LS032                                                            
         B     LS034                                                            
*                                                                               
LS033    CLI   0(R5),2             CHECK FOR STATION                            
         BNE   LS032                                                            
         CLI   3(R4),32            YES, STAMKT CODE WILL DO                     
         BNE   LS032                                                            
*                                                                               
LS034    ZIC   R1,2(R4)                                                         
         MH    R1,=Y(RGROWSZ)                                                   
         LA    R1,INREC(R1)                                                     
         ST    R1,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,LS030                                                         
*                                                                               
LS036    ICM   R1,15,SAVER2      CHECK IF ALREADY DONE ONE ONLINE SPEC          
         BZ    LS040                                                            
         MVC   RPMNTH,RPMNTH-REPORTD(R1)       YES- COPY MONTH ROW ADDR         
         SPACE                                                                  
*                                                   AND COL ADDRESSES           
         MVC   RPPCOLS(REPORTX-RPPCOLS),RPPCOLS-REPORTD(R1)                     
         B     LS080                                                            
*                                                                               
LS040    ST    R2,SAVER2                                                        
         BAS   RE,NEXTSPEC         GET MONTH ROW SPEC                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   3(R4),17            MONTH                                        
         BE    LS050                                                            
         CLI   3(R4),20            QTR                                          
         BNE   LS040                                                            
*                                                                               
LS050    ZIC   R1,2(R4)                                                         
         MH    R1,=Y(RGROWSZ)                                                   
         LA    R1,INREC(R1)                                                     
         STCM  R1,15,RPMNTH        MONTH ROW ADDDRESS                           
         MVI   SPEC,40                                                          
         BAS   RE,NEXTSPEC         FIND COL SPECS                               
         BE    LS055                                                            
         MVI   SPEC,42             NONE - FIND COLEQU                           
         BAS   RE,NEXTSPEC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,2(R4)                                                       
         ICM   R4,15,RPSPECS                                                    
         MVI   SPEC,10             FIND THE REPORT FOR COLEQU                   
         BAS   RE,GETSPEC                                                       
         B     *+8                                                              
*                                                                               
LS053    BAS   RE,NEXTSPEC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,2(R4)                                                       
         BNE   LS053                                                            
         MVI   SPEC,40                                                          
         BAS   RE,NEXTSPEC         FIND COL SPECS FOR COLEQU                    
         BE    LS055                                                            
         DC    H'0'                                                             
*                                                                               
LS055    LA    R3,RRGCOLS          FIND COLS NEEDED FOR RRGON                   
*                                                                               
LS060    CLI   0(R3),X'FF'                                                      
         BE    LS075                                                            
         CLC   0(4,R3),3(R4)                                                    
         BE    LS070                                                            
         LA    R3,6(R3)                                                         
         B     LS060                                                            
*                                                                               
LS070    ZIC   R1,2(R4)            COL NUM                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(RGCOLSZ)                                                   
         LA    R1,INCOLS(R1)                                                    
         LH    RF,4(R3)                                                         
         LA    RF,0(RF,R2)                                                      
         ST    R1,0(RF)            POST COL ADDRESS                             
*                                                                               
LS075    BAS   RE,NEXTSPEC         FIND NEXT RRGON COL                          
         BE    LS055                                                            
*                                                                               
LS080    L     R4,SVONLSP                                                       
         MVI   SPEC,18             NOW LOOK FOR NEXT ONLINE SPEC                
         BAS   RE,NEXTSPEC                                                      
         BNE   LS100                                                            
         LA    R2,REPORTX-REPORTD(R2)  FOUND -                                  
         CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         MVC   RPREQ(3),RPTCD                                                   
         MVC   RPSPECS,ASPECS                                                   
         B     LS025                   LOOP ROUND AGAIN                         
*                                                                               
LS100    L     R2,SAVER2                                                        
         B     EXIT                                                             
         SPACE                                                                  
* FIRST 4 BYTES MATCH UP TO RRG SPECS FOR TABLE SEARCH                          
* NEXT 2 BYTES GIVE RELATIVE TO ADDRESS IN RRGON RECORD                         
         SPACE                                                                  
*             0001=PER                                                          
*             0002=YTD                                                          
         SPACE                                                                  
RRGCOLS  DS    0H                                                               
         DC    X'0100',AL1(9,29),Y(RPPCOLS-REPORTD)    PER 2EST (YR-2)          
         DC    X'0100',AL1(9,22),Y(RPPCOLS+4-REPORTD)  PER -EST (YR-1)          
         DC    X'0100',AL1(9,21),Y(RPPCOLS+8-REPORTD)  PER EST  (CUR)           
         DC    X'0100',AL1(9,30),Y(RPPCOLS+12-REPORTD) PER 2ACT (YR-2)          
         DC    X'0100',AL1(9,24),Y(RPPCOLS+16-REPORTD) PER -ACT (YR-1)          
         DC    X'0100',AL1(9,83),Y(RPPCOLS+20-REPORTD) PER  BUDP PRIOR          
         DC    X'0100',AL1(9,25),Y(RPPCOLS+24-REPORTD) PER  BUD  CURR           
*                                                                               
         DC    X'0100',AL1(9,75),Y(RPYCOLS+28-REPORTD) PER THIS WK L YR         
*                                                                               
         DC    X'0200',AL1(9,29),Y(RPYCOLS-REPORTD)    YTD 2EST (YR-2)          
         DC    X'0200',AL1(9,22),Y(RPYCOLS+4-REPORTD)  YTD -EST (YR-1)          
         DC    X'0200',AL1(9,21),Y(RPYCOLS+8-REPORTD)  YTD EST  (CUR)           
         DC    X'0200',AL1(9,30),Y(RPYCOLS+12-REPORTD) YTD 2ACT (YR-2)          
         DC    X'0200',AL1(9,24),Y(RPYCOLS+16-REPORTD) YTD -ACT (YR-1)          
         DC    X'0200',AL1(9,83),Y(RPYCOLS+20-REPORTD) YTD BUDP PRIOR           
         DC    X'0200',AL1(9,25),Y(RPYCOLS+24-REPORTD) YTD BUD  CURR            
*                                                                               
         DC    X'0100',AL1(9,31),Y(RPYCOLS+36-REPORTD) PER EST (YR-1)           
*                                                         PACED TODAY'          
         DC    X'0100',AL1(9,96),Y(RPYCOLS+40-REPORTD) PER THIS WK              
*                                                          LAST YR-1            
*                                                                               
*        DC    X'0200',AL1(9,31),Y(RPYCOLS+40-REPORTD) YTD EST (YR-1)           
*                                                       PACED TODAY             
         DC    X'FF'                                                            
         EJECT                                                                  
         DS    0H                                                               
POST     NTR1                                                                   
*                                                                               
*        ROUTINE TO POST SORT RECORDS                                           
*                                                                               
         XC    SORTREC,SORTREC                                                  
         MVC   ROKREP,REPCD        BUILD SORT KEY                               
         SPACE                                                                  
         CLC   RPREQ,=C'1G'        IRNY NEW WITH ALL DOLLARS                    
         BE    PS001                                                            
         CLC   RPREQ,=C'7G'        NO SALESPERSON                               
         BE    PS001                                                            
         CLC   RPREQ,=C'BF'        TEMP TEST                                    
         BE    PS001               TEMP TEST                                    
         CLC   RPREQ,=C'BG'        TEMP TEST                                    
         BE    PS001               TEMP TEST                                    
         SPACE                                                                  
         CLC   RPREQ,=C'3G'        THIS ALL DOLLARS                             
         BE    PS001                                                            
         SPACE                                                                  
         MVI   ROKHD2RQ,C'C'                                                    
         SPACE                                                                  
         CLC   RPREQ,=C'4G'        THIS CONFIRMED DOLLARS                       
         BE    PS001                                                            
         SPACE                                                                  
         MVI   ROKHD2RQ,C'D'                                                    
         SPACE                                                                  
         CLC   RPREQ,=C'6G'        THIS DIRECT CONFIRMED DOLLARS                
         BE    PS001                                                            
         SPACE                                                                  
         MVI   ROKHD2RQ,C'R'                                                    
         SPACE                                                                  
         CLC   RPREQ,=C'??'        THIS DIRECT DOLLARS                          
         BE    PS001                                                            
         SPACE                                                                  
         MVI   ROKHD2RQ,C'U'       FOR UNCONFIRMED DOLLARS                      
         SPACE                                                                  
         CLC   RPREQ,=C'??'        THIS UNCONFIRMED DOLLARS                     
         BE    PS001                                                            
         DC    H'0'                ONLY THESE REPORTS ALLOWED                   
PS001    DS    0H                                                               
         LA    R0,3                                                             
         LA    R1,RPROWS                                                        
         LA    R3,RPTYP                                                         
         LA    R4,ROKDTLTY                                                      
         LA    R5,ROKDTLVL                                                      
*                                                                               
PS002    CLI   0(R3),0             CHECK FOR RRG ROWS COMPLETE                  
         BE    PS030                                                            
         MVC   0(1,R4),0(R3)       BUILD RGGON RECORD TYPE                      
         L     R6,0(R1)                                                         
         MVC   0(L'ROKDTLVL,R5),0(R6)  POST ROW VALUE                           
*                                                                               
         CLI   0(R3),QLOFF         CHECK FOR OFFICE                             
         BNE   PS004                                                            
         MVC   0(2,R5),1(R5)       (OFFICE WAS OFFSET BY 1)                     
         MVI   2(R5),0                                                          
         MVI   L'ROKDTLVL-1(R5),0                                               
         B     PS010                                                            
*                                                                               
PS004    CLI   0(R3),39            CHECK FOR UNKNOWN MARKET                     
         BNE   PS010                                                            
         CLC   0(4,R5),=X'FEFEFEFE'                                             
         BE    PSXEQ                                                            
*                                                                               
PS010    LA    RF,RECTYPS                                                       
*                                                                               
PS012    CLI   0(RF),X'FF'         CHECK ROW VALUE IS SIGNIFICANT               
         BE    PS014                                                            
         CLC   0(1,R3),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     PS012                                                            
         ZIC   RE,1(RF)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES                                                   
         BNH   PSXEQ                                                            
*                                                                               
PS014    CLI   0(R3),QLSTA         CHECK FOR STATION                            
         BNE   PS015                                                            
         XC    WORK(8),WORK        REFORMAT STATION TO PRINTABLE FORM           
         MVC   WORK(4),2(R5)                                                    
         CLI   6(R5),C'V'          TEST 3 CALL LETTERS TV                       
         BNE   PS0142                                                           
         MVC   WORK+3(3),=C'-TV'                                                
         B     PS0148                                                           
*                                                                               
PS0142   CLI   6(R5),C'M'          TEST 3 CALL LETTERS RADIO                    
         BNE   PS0144                                                           
         MVC   WORK+3(3),=C'- M'                                                
         MVC   WORK+4(1),5(R5)                                                  
         B     PS0148                                                           
*                                                                               
PS0144   CLI   6(R5),C'T'          TEST 4 CALL LETTERS TV                       
         BNE   PS0146                                                           
         MVC   WORK+4(3),=C'-TV'                                                
         B     PS0148                                                           
*                                                                               
PS0146   CLI   7(R5),C'M'          TEST 4 CALL LETTERS RADIO                    
         BNE   PS0147                                                           
         MVC   WORK+4(3),=C'- M'                                                
         MVC   WORK+5(1),6(R5)                                                  
         B     PS0148                                                           
*                                                                               
*                                                                               
PS0147   CLI   6(R5),C'L'          TEST 4 CALL LETTERS LOW POWER TV             
         BNE   *+14                                                             
         MVC   WORK+4(2),=C'-L'                                                 
         B     PS0148                                                           
*                                                                               
         CLI   5(R5),C'L'          TEST 4 CALL LETTERS LOW POWER TV             
         BNE   PS0148                                                           
         MVC   WORK+3(2),=C'-L'                                                 
*                                                                               
PS0148   MVC   0(8,R5),WORK        REPLACE ROW WITH PROPER STATION FMT          
         B     PS020                                                            
*                                                                               
PS015    CLI   0(R3),QLADV         CHECK FOR ADVERTISER                         
         BNE   PS016                                                            
*                                                                               
         LA    RE,8                                                             
         LA    RF,7(,R5)                                                        
         CLI   0(RF),C' '                                                       
         BH    PS020                                                            
         MVI   0(RF),0             ELIMINATE TRAILING BLANKS                    
         BCTR  RF,0                                                             
         BCT   RE,*-14                                                          
*                                                                               
         MVC   P+2(24),=C'NULLS ADVERTISER FOR REP'                             
         MVC   P+27(2),REPCD                                                    
         GOTO1 =V(PRINTER)                                                      
*        DC    H'0'                                                             
*                                                                               
PS016    CLI   0(R3),QLAGENCY      CHECK FOR AGENCY (4 CHAR)                    
         BE    *+12                                                             
         CLI   0(R3),QLAGY         CHECK FOR AGENCY                             
         BNE   PS017                                                            
*                                                                               
         LA    RE,8                                                             
         LA    RF,7(,R5)                                                        
         CLI   0(RF),C' '                                                       
         BH    PS020                                                            
         MVI   0(RF),0             ELIMINATE TRAILING BLANKS                    
         BCTR  RF,0                                                             
         BCT   RE,*-14                                                          
         DC    H'0'                                                             
*                                                                               
PS017    CLI   0(R3),QLGRGRP       CHECK FOR GRGRP (1 CHAR)                     
         BNE   PS018                                                            
         MVI   1(R5),0             FORCE BLANK TO NULL                          
         B     PS020                                                            
*                                                                               
PS018    CLI   0(R3),26            CHECK FOR GRSUBGRP                           
         BNE   *+12                                                             
         MVI   0(R4),QLGRP         YES- REPLACE WITH GROUP                      
         B     PS020                                                            
*                                                                               
         CLI   0(R3),32            CHECK FOR STAMKT                             
         BNE   PS020                                                            
         MVI   0(R4),QLSTA         YES - REPLACE WITH STATION                   
*                                                                               
PS020    LA    R1,4(R1)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,L'ROKDTLVL(R5)                                                
         BCT   R0,PS002                                                         
*                                                                               
PS030    ICM   R1,15,RPMNTH                                                     
         MVC   ROKDTLYM,0(R1)      YEAR/MONTH                                   
         LA    RE,MONLOHI                                                       
         LA    R0,MAXREPS          UPDATE GLOBAL LO AND HI MONTHS               
*                                  FOR THIS REP                                 
PS032    OC    0(2,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   0(2,RE),REPCD                                                    
         B     PS034                                                            
         CLC   REPCD,0(RE)                                                      
         BE    PS034                                                            
         LA    RE,L'MONLOHI(RE)                                                 
         BCT   R0,PS032                                                         
         DC    H'0'                                                             
*                                                                               
PS034    CLC   0(2,R1),2(RE)       UPDATE GLOBAL LO AND HI MONTHS               
         BNL   *+10                                                             
         MVC   2(2,RE),0(R1)                                                    
         CLI   2(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),4(RE)                                                    
         BNH   *+10                                                             
         MVC   4(2,RE),0(R1)                                                    
         SPACE                                                                  
         CLC   RPREQ,=C'1G'        IRNY NEW WITH ALL DOLLARS                    
         BE    PS034C                                                           
         CLC   RPREQ,=C'7G'        NO SALESPERSON                               
         BE    PS034C                                                           
         CLC   RPREQ,=C'BF'        TEMP TEST                                    
         BE    PS034C              TEMP TEST                                    
         CLC   RPREQ,=C'BG'        TEMP TEST                                    
         BE    PS034C              TEMP TEST                                    
         CLC   RPREQ,=C'3G'        THIS ALL DOLLARS                             
         BNE   *+12                                                             
PS034C   DS    0H                                                               
         MVI   6(RE),C'A'                                                       
         B     PS035                                                            
         SPACE                                                                  
         CLC   RPREQ,=C'4G'        THIS CONFIRMED DOLLARS                       
         BNE   *+12                                                             
         MVI   7(RE),C'C'                                                       
         B     PS035                                                            
         SPACE                                                                  
         CLC   RPREQ,=C'??'        THIS UNCONFIRMED DOLLARS                     
         BNE   *+12                                                             
         MVI   8(RE),C'U'                                                       
         B     PS035                                                            
         SPACE                                                                  
         CLC   RPREQ,=C'6G'        THIS DIRECT CONFIRMED DOLLARS                
         BNE   *+12                                                             
         MVI   9(RE),C'D'                                                       
         B     PS035                                                            
         SPACE                                                                  
         CLC   RPREQ,=C'??'        THIS DIRECT DOLLARS                          
         BNE   *+12                                                             
         MVI   10(RE),C'R'                                                      
         B     PS035                                                            
         SPACE                                                                  
         DC    H'0'                ONLY THESE REPORTS ALLOWED                   
*                                                                               
PS035    CLI   1(R1),0             DON'T LET PRIOR MONTH GO TO LO MONTH         
         BE    PS038                                                            
         LA    RE,RPMNLOHI         UPDATE REPORT LO AND HI MONTHS               
         LA    R0,MAXREPS          FOR THIS REP                                 
*                                                                               
PS036    OC    0(2,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   0(2,RE),REPCD                                                    
         B     PS037                                                            
         CLC   REPCD,0(RE)                                                      
         BE    PS037                                                            
         LA    RE,L'RPMNLOHI(RE)                                                
         BCT   R0,PS036                                                         
         DC    H'0'                                                             
*                                                                               
PS037    CLC   0(2,R1),2(RE)                                                    
         BNL   *+10                                                             
         MVC   2(2,RE),0(R1)                                                    
         CLC   0(2,R1),4(RE)                                                    
         BNH   PS038                                                            
         MVC   4(2,RE),0(R1)                                                    
*                                                                               
PS038    LA    R3,RODATA           POST THE COLUMNS                             
         LA    R4,RPPCOLS                                                       
         LA    R5,NFLDS            NOW 15 FIELDS, WAS 12, 8                     
*                                                                               
PS040    ICM   R1,15,0(R4)                                                      
         BZ    *+10                                                             
         MVC   0(4,R3),0(R1)                                                    
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,PS040                                                         
*                                                                               
         BAS   RE,PUTSORT                                                       
         B     PSXEQ                                                            
*                                                                               
PSXNE    LTR   RB,RB                                                            
         B     PSX                                                              
*                                                                               
PSXEQ    CR    RB,RB                                                            
*                                                                               
PSX      B     EXIT                                                             
         SPACE 2                                                                
* FIRST CHAR IS CODE, SECOND IS LENGTH-1                                        
         SPACE                                                                  
RECTYPS  DC    AL1(04),X'01'       OFFICE                                       
         DC    AL1(06),X'02'       SALESPERSON                                  
         DC    AL1(13),X'01'       CLASS                                        
         DC    AL1(15),X'01'       CATEGORY                                     
         DC    AL1(23),X'01'       TVB                                          
         DC    AL1(24),X'02'       OWNER                                        
         DC    AL1(33),X'01'       CONTRACT TYPE                                
         DC    AL1(39),X'03'       MARKET                                       
         DC    AL1(45),X'01'       DEVELOPMENTAL CONTRACT TYPE                  
         DC    X'FF'                                                            
         EJECT                                                                  
* ROUTINE TO PUT RECORD TO SORTER                                               
*                                                                               
PUTSORT  LR    R8,RE                                                            
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         AP    SORTCT,=P'1'                                                     
         CLI   TRACSRTI,C'Y'       TRACE INTO SORT                              
         BNE   PUTSORTX            *** DISABLE THE TRACE                        
         SPACE                                                                  
         AP    RECCNT,=P'1'                                                     
         CP    RECCNT,RECFREQ                                                   
         BL    PUTSORTX                                                         
         ZAP   RECCNT,=P'0'                                                     
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         UNPK  WORK(7),FULL(4)                                                  
         MVC   P(2),WORK                                                        
         MVI   P+2,C':'                                                         
         MVC   P+3(2),WORK+2                                                    
         MVI   P+5,C':'                                                         
         MVC   P+6(2),WORK+4                                                    
         MVC   P+10(116),SORTREC                                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),DMCB,SORTREC,WORK,116,=C'SEP'                         
         MVC   P,SPACES                                                         
         MVC   P+10(116),WORK                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(9),=C'INTO SORT'                                               
         MVC   P+10(116),WORK+116                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PUTSORTX LR    RE,R8                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        GETSPEC SEARCHES FOR A SPEC FROM THIS SPEC                             
*        NEXTSPEC SEARCHES FOR A SPEC FROM NEXT SPEC                            
*                                                                               
*        IN  R4 = A(ORIGINAL SPEC)                                              
*            SPEC = SOUGHT AFTER SPEC CODE (1 BYTE)                             
*        OUT CC EQ, R4 = A(FOUND SPEC)     SPEC FOUND                           
*            CC NE, R4 = A(ORIGINAL SPEC)  SPEC NOT FOUND                       
         SPACE                                                                  
GETSPEC  DS    0H                                                               
         CLC   0(1,R4),SPEC        START SEARCH AT THIS SPEC                    
         BER   RE                                                               
*                                                                               
NEXTSPEC DS    0H                  START SEARCH AT NEXT SPEC                    
         ST    R4,SAVER4                                                        
         SR    R0,R0                                                            
*                                                                               
GS005    ICM   R0,1,1(R4)            LOOK AT NEXT SPEC                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    GS020               END OF SPECS                                 
         CLC   0(1,R4),SPEC        COMPARE TO SOUGHT SPEC CODE                  
         BE    GS010               FOUND                                        
         CLI   0(R4),10            ONLY LOOK IN THIS REPORT                     
         BE    GS020               STOP AT NEXT REPORT                          
         B     GS005                                                            
*                                                                               
GS010    CR    RB,RB               CC EQ - SPEC FOUND                           
         BR    RE                                                               
*                                                                               
GS020    L     R4,SAVER4                                                        
         LTR   RB,RB               CC NE - SPEC NOT FOUND                       
         BR    RE                                                               
         EJECT                                                                  
* TRACE RECORDS READ                                                            
         SPACE                                                                  
TRCRD    NTR1                                                                   
         B     EXIT                                                             
         EJECT                                                                  
VDDSIO   DC    V(DDSIO)                                                         
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*STORGE*'                                                    
DUMPLIST DC    A(RERRGLD)                                                       
         DC    V(STXITER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
DUB      DS    D                                                                
CARD     DS    CL80                                                             
FULL     DS    F                                                                
WORK     DS    CL256                                                            
DMCB     DS    6F                                                               
SAVER2   DS    F                                                                
SAVER4   DS    F                                                                
SVONLSP  DS    F                                                                
ASPECS   DS    A                                                                
AREC     DS    A                                                                
SPEC     DS    X                                                                
FOUND    DS    C                                                                
BYTE     DS    C                                                                
RPTCD    DS    XL3                                                              
HDRDONE  DS    C                                                                
TRACES   DS    0CL4                                                             
TRACEREC DS    CL1                 RECS READ                                    
TRACSRTI DS    CL1                 INTO SORT                                    
TRACSRTO DS    CL1                 OUT OF SORT                                  
TRACPUT  DS    CL1                 PUT TO OUTPUT FILE                           
ABNDOPT  DS    CL1                                                              
         SPACE                                                                  
REPCD    DS    CL2                                                              
SVREPCD  DC    CL2' '                                                           
         SPACE                                                                  
*   EACH ENTRY = 2 BYTES REP, 2 BYTES LOW YR/MON, 2 BYTES HI YR/MON             
*                5 BYTES 0 A = ALL DOLLARS        (3G)                          
*                        1 C = CONFIRMED DOLLARS  (6G)                          
*                        2 U = UNCONFIRMED DOLLARS(??)                          
*                        3 D = DIRECT CONF DOLLARS(4G)                          
*                        4 R = DIRECT DOLLARS     (??)                          
MONLOHI  DS    (MAXREPS)XL11                                                    
RECCNT   DC    PL3'0'                                                           
RECFREQ  DC    PL3'1'                                                           
FILINCT  DC    PL8'0'                                                           
SORTCT   DC    PL8'0'                                                           
TOTRECS  DC    PL8'0'                                                           
REPRECS  DC    PL8'0'                                                           
RECINCT  DC    PL8'0'                                                           
*                                                                               
MAXREPS  EQU   20                  MAX N'REPS                                   
*                                                                               
         DC    CL8'*INREC**'                                                    
INREC    DS    0CL(RGRECLEN)                                                    
INKEY    DS    XL(RGKEYLEN)                                                     
INCOM    DS    XL(RGCMTLEN)                                                     
INCOLS   DS    XL(RGDTALEN)                                                     
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'SORTREC*'                                                    
         DS    H                   RDW                                          
         DC    H'00'                                                            
SORTREC  DS    0CL(L'ROREC)                                                     
       ++INCLUDE REGENRRGOD                                                     
*                                                                               
*                                                                               
         DS    0D                                                               
         DS    H                   RDW                                          
         DC    H'00'                                                            
WORKREC  DS    CL(L'ROREC)                                                      
*                                                                               
*                                                                               
IN       DCB   DDNAME=RRGBKUP,DSORG=PS,EODAD=MA100,LRECL=RGRECLEN,     C        
               BLKSIZE=16*RGRECLEN,MACRF=(GM,PM),RECFM=FB                       
*                                                                               
OUT      DCB   DDNAME=RRGTEMP,DSORG=PS,LRECL=120,                      C        
               BLKSIZE=12004,MACRF=(GM,PM),RECFM=VB                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,40,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=116 '                                  
*                                                                               
*                                                                               
         DS    0F                                                               
         DC    CL8'*REPTAB*'                                                    
REPTAB   DC    800XL(REPORTX-REPORTD)'00'                                       
         DC    X'FF'                                                            
         EJECT                                                                  
REPORTD  DSECT                                                                  
*                                                                               
RPREQ    DS    CL2                 2 CHAR ID OF PHASE USED BY RG02              
RPRPT    DS    X                   REPORT NUMBER                                
RPTYP    DS    XL3                 DATA TYPES (02=QLSTA)                        
RPMNLOHI DS    (MAXREPS)XL6        REP CODE, YR/MO LOW, YR/MO HIGH              
RPSPECS  DS    XL4                 ADDR OF RG PHASE                             
RPROWS   DS    3XL4                DATA ADDRESSES IN RRG RECORD                 
RPMNTH   DS    XL4                 YR/MO BINARY                                 
RPPCOLS  DS    7XL4                PER CTRS                                     
RPYCOLS  DS    7XL4                YTD CTRS                                     
         DS    XL4                 THIS WEEK LAST YEAR                          
         DS    XL4                 CURR WEEK BILLING THIS YEAR                  
         DS    XL4                 EST ORDERED LAST YR - AS AT TODAY            
         DS    XL4                 EST ORD LAST YR -1 - AS AT 1 YR AGO          
REPORTX  EQU   *                                                                
*                                                                               
* ROWS & COLS ARE ADDRESSES OF THE SOURCE FIELD                                 
*                                                                               
         EJECT                                                                  
       ++INCLUDE REREPQWKDC                                                     
       ++INCLUDE RENRGWKEQU                                                     
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031RERRGONLD 05/01/02'                                      
         END                                                                    
