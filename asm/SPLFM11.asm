*          DATA SET SPLFM11    AT LEVEL 082 AS OF 05/01/02                      
*PHASE T21911A                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
T21911   TITLE 'SPLFM11 - CLIENT HEADER'                                        
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* SEP28/98     MHER   IF TWA+12=X'04' ALLOW FRZ FOR WI                *         
* MAY21/98     RKOH   COS2 NOW A COST FACTOR IF AGY HAS AGYCOS2Q BIT  *         
*                     SET.                                            *         
* APR20/98     RKOH   XEST FOR CROSS ESTIMATE REPORTING OPTION        *         
* APR17/98     RKOH   TRD FOR TRADE, FRZ FOR FREEZE CLIENT OPTIONS    *         
* OCT28/97 32  MHER   PWB=N FOR NO PW BILLING                         *         
* JAN09/96 18  AROT - GMI OPTION                                      *         
* JUL11/95 113 SPRI - SECURITY                                        *         
* JUL11/95 112 AROT - DON'T ALLOW USER TO DEL A CLT THAT IS IN A GRP  *         
*          111 ???                                                    *         
* DEC09/94 079 MHER - SUPPORT CTA OPTION                              *         
* NOV21/94 075 GLEE - SUPPORT ZENITH CLIENT CODE IN CLIENT OPTIONS    *         
*                                                                     *         
*  ??????   ?   ??  - HISTORY UNKNOWN                                 *         
***********************************************************************         
         PRINT NOGEN                                                            
T21911   CSECT                                                                  
         NMOD1 0,T21911,R9,RR=RE                                                
         ST    RE,RELO                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING CLTHDRD,R8                                                       
         CLI   SVFMTSW,0                TEST FORMAT OR EDIT                     
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
MYEXIT   XIT1  REGS=(R2)                                                        
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         MVI   ERRCD,SCRTYERR                                                   
         CLI   SVACT,C'D'          ACTION DISPLAY?                              
         BE    *+12                                                             
         CLI   SVACT,C'C'          ACTION CHANGE?                               
         BNE   *+8                                                              
         BAS   RE,CHKAUT           CHECK IF AUTHORIZED                          
*                                                                               
FMT10    DS    0H                                                               
         XC    SCRNFLAG,SCRNFLAG   INITIALIZE THE FLAG                          
         FOUT  CLTNAMEH,CNAME,20                                                
         FOUT  CLTONUMH,COFFICE,1                                               
         FOUT  CLTAOFCH,CACCOFC,2                                               
         OC    CACCAGY,CACCAGY                                                  
         BNZ   FMT10A                                                           
         MVC   CLTAOFC+2(3),=C'   '                                             
         B     FMT10B                                                           
FMT10A   MVC   CLTAOFC+2(1),=C'/'                                               
         MVC   CLTAOFC+3(2),CACCAGY                                             
FMT10B   MVC   CLTIFCD,CCLTIFC                                                  
         FOUT  CLTIFCDH                                                         
         OI    CLTAOFCH+4,X'20'                                                 
*                                                                               
         MVC   CLTOPTS,SPACES                                                   
         LA    R1,CLTOPTS                                                       
*                                                                               
         TM    COPT1,COP1COSQ      OPTION SET?                                  
         BNO   FMT11               NO - SO CONTINUE                             
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT11               NO - SO CONTINUE                             
*                                                                               
         MVC   0(7,R1),=C'COS2=Y,'                                              
         LA    R1,7(R1)                                                         
*                                                                               
FMT11    EQU   *                                                                
*                                                                               
         TM    COPT1,COP1INFQ                                                   
         BNO   FMT12                                                            
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT12               NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'INF=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT12    EQU   *                                                                
*                                                                               
         TM    COPT1,COP1DBLQ                                                   
         BNO   FMT13                                                            
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT13               NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'DBL=N,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT13    EQU   *                                                                
*                                                                               
         TM    COPT1,COP1MGRQ                                                   
         BNO   FMT14                                                            
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT14               NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'MGR=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT14    EQU   *                                                                
*                                                                               
         TM    COPT1,COP1NMG                                                    
         BZ    FMT14A                                                           
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT14A              NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'NMG=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT14A   EQU   *                                                                
*                                                                               
         TM    COPT1,COP1UPLQ                                                   
         BZ    FMT14B                                                           
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT14B              NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'UPL=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT14B   EQU   *                                                                
*                                                                               
         TM    COPT1,COP1CTAQ      TEST CTA CLIENT                              
         BZ    FMT15                                                            
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT15               NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'CTA=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT15    EQU   *                                                                
*                                                                               
         OC    CPWPCT,CPWPCT       ANY PROFIT WITHIN                            
         BZ    FMT16                                                            
*                                                                               
         LA    R0,10               MAX LENGTH OF OUTPUT DATA                    
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT16               NO - SO CONTINUE                             
*                                                                               
         MVC   0(3,R1),=C'PW='                                                  
         LA    R4,3(R1)                                                         
         CLC   CPWPCT,=X'800000'                                                
         BNE   FMT15A                                                           
         MVI   0(R4),C'0'                                                       
         LA    R0,1                                                             
         B     FMT15B                                                           
*                                                                               
FMT15A   SR    R3,R3                                                            
         ICM   R3,7,CPWPCT                                                      
         EDIT  (R3),(6,0(R4)),2,ALIGN=LEFT                                      
*                                                                               
FMT15B   AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R1,1(R4)                                                         
*                                                                               
FMT16    DS    0H                                                               
*                                                                               
         OC    CZENCLT,CZENCLT     ANY ZENITH CLIENT TO DISPLAY?                
         BZ    FMT17                                                            
*                                                                               
         LA    R0,7                MAX LENGTH OF OUTPUT DATA                    
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT17               NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'ZEN='                                                 
         LA    RE,1                                                             
         CLI   CZENCLT+2,X'00'      TEST IF IT'S 2 OR 3 CHARACTERS              
         BE    *+8                                                              
         LA    RE,2                                                             
         EXMVC RE,4(R1),CZENCLT                                                 
         LA    R1,5(R1,RE)                                                      
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
FMT17    EQU   *                                                                
*                                                                               
         TM    COPT1,COP1GMI       TEST GMI                                     
         BZ    FMT17A                                                           
*                                                                               
         LA    R0,4                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT17A              NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'GMI,'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
FMT17A   EQU   *                                                                
*                                                                               
         TM    COPT2,COP2EXJ1      EXCLUDED FROM J1 REPORT                      
         BZ    FMT17B                                                           
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT17B              NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'J1=NO,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT17B   EQU   *                                                                
*                                                                               
         TM    COPT2,COP2EXA7      EXCLUDED FROM A7 REPORT                      
         BZ    FMT17C                                                           
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT17C              NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'A7=NO,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT17C   EQU   *                                                                
*                                                                               
         TM    COPT2,COP2NPWB      NO PW BILLING                                
         BZ    FMT18                                                            
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT18               NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'PWB=N,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT18    EQU   *                                                                
*                                                                               
         TM    COPT2,COP2TRAD      ELSE - TRADE CLIENT BIT SET?                 
         BZ    FMT18A              NO - SO CONTINUE                             
*                                                                               
         LA    R0,4                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT18A              NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'TRD,'    ELSE - MOVE OUT OPTION                       
         LA    R1,4(R1)            INC A(FIELD POSITION)                        
*                                                                               
FMT18A   EQU   *                                                                
*                                                                               
         TM    COPT2,COP2FRZ       ELSE - FREEZE BIT SET?                       
         BZ    FMT19               NO - SO CONTINUE                             
*                                                                               
         LA    R0,4                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT19               NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'FRZ,'    ELSE - MOVE OUT OPTION                       
         LA    R1,4(R1)            INC A(FIELD POSITION)                        
*                                                                               
FMT19    EQU   *                                                                
*                                                                               
         TM    COPT2,COP2XEST      ELSE - CROSS EST REPORTING?                  
         BZ    FMT19A              NO - SO CONTINUE                             
*                                                                               
         LA    R0,5                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT19A              NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R1),=C'XEST,'   ELSE - MOVE OUT OPTION                       
         LA    R1,5(R1)            INC A(FIELD POSITION)                        
*                                                                               
FMT19A   EQU   *                                                                
*                                                                               
         TM    COPT2,COP2BP        ELSE - BUY PROG PROFILE ON?                  
         BZ    FMT20               NO - SO CONTINUE                             
*                                                                               
         LA    R0,5                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT20               NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R1),=C'BP=Y,'   ELSE - MOVE OUT OPTION                       
         LA    R1,5(R1)            INC A(FIELD POSITION)                        
*                                                                               
FMT20    EQU   *                                                                
*                                                                               
         OC    CCOST2,CCOST2       ANY COST FACTOR?                             
         BZ    FMT21               NO - SO CONTINUE                             
*                                                                               
         LA    R0,13               MAX LENGTH OF OUTPUT DATA                    
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT21               NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R1),=C'COS2='   MOVE OUT LITERAL                             
         LA    R4,5(R1)            GET A(NEW LINE POSITION)                     
         CLI   CCOST2,X'80'        ZERO AS INPUT DATA?                          
         BNE   FMT20A              NO - SO CONTINUE                             
*                                                                               
         MVC   0(3,R4),=C'0.0'     ELSE - MOVE OUT ZERO                         
         LA    R0,3                INC A(LINE POSITION)                         
         B     FMT20B              AND CONTINUE                                 
*                                                                               
FMT20A   EQU   *                                                                
*                                                                               
         EDIT  CCOST2,(8,0(R4)),6,ALIGN=LEFT,FILL=0,DROP=5                      
*                                                                               
FMT20B   EQU   *                                                                
*                                                                               
         AR    R4,R0               INC A(LINE POSITION)                         
         MVI   0(R4),C','          MOVE OUT COMMA                               
         LA    R1,1(R4)            AND INC A(LINE POSITION) AGAIN               
*                                                                               
FMT21    EQU   *                                                                
*                                                                               
         CLI   SCRNFLAG,0          DID EVERYTHING FIT?                          
         BE    FMT24               YES - SO CONTINUE                            
*                                                                               
         MVI   0(R1),C'*'          ELSE - MOVE OUT 'DIDN'T FIT' FLAG            
         LA    R1,1(R1)            INC A(FIELD POSITION)                        
*                                                                               
FMT24    EQU   *                                                                
*                                                                               
         BCTR  R1,0                REMOVE LAST COMMA                            
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         FOUT  CLTOPTSH                                                         
         OC    CPOLONLY,CPOLONLY                                                
         BZ    FMT25                                                            
         FOUT  CLTOP1AH,CPOLONLY,1                                              
*                                                                               
FMT25    LA    R1,CPROF                                                         
         LA    R4,15               FOR BCT                                      
         LA    R2,CLTOP1H                                                       
*                                                                               
FMT27    FOUT  (R2),(R1),1                                                      
         BAS   RE,FNDNXUF                                                       
         LA    R1,1(R1)                                                         
         BCT   R4,FMT27                                                         
*                                                                               
FMT30    DS    0H                                                               
         LA    R1,CEXTRA           EXTRA PROFILE BYTES                          
         CLI   CEXTRA+2,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+2,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+3,C'0'       DISPLAY AS N                                 
         BNE   *+8                                                              
         MVI   CEXTRA+3,C'N'                                                    
         CLI   CEXTRA+5,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+5,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+6,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+6,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+7,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+7,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+8,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+8,C'N'       CHANGE 0 TO N ON DISPLAY                     
*                                                                               
         CLI   CEXTRA+9,C'0'       CANADIAN CLIENT                              
         BE    *+12                                                             
         CLI   CEXTRA+9,C'N'                                                    
         BNE   FMT31                                                            
         MVI   CEXTRA+9,C'U'       CHANGE 0 TO U ON DISPLAY FOR US AGY          
         CLI   SVAPROF+7,C'C'                                                   
         BNE   FMT31                                                            
         MVI   CEXTRA+9,C'C'       CHANGE 0 TO C ON DISPLAY FOR CAN AGY         
*                                                                               
FMT31    CLI   CEXTRA+10,C'0'                                                   
         BNE   *+8                                                              
         MVI   CEXTRA+10,C'N'     CHANGE 0 TO N ON DIS OUT-OF-WEEKCLT           
*                                                                               
         CLI   CEXTRA+11,C'0'                                                   
         BNE   *+8                                                              
         MVI   CEXTRA+11,C'S'     CHANGE 0 TO S ON DIS GST CODE                 
*                                                                               
         CLI   CEXTRA+12,C'0'                                                   
         BNE   *+8                                                              
         MVI   CEXTRA+12,C'N'     CHANGE 0 TO N ON SPECIAL DEMO ADJ             
*                                                                               
         CLI   CEXTRA+13,C'0'                                                   
         BNE   *+8                                                              
         MVI   CEXTRA+13,C'N'     CHANGE 0 TO N ON PRD REQ FOR ADD              
*                                                                               
FMT32    LA    R4,14             FOR BCT                                        
         LA    R2,CLTEX1H                                                       
FMT34    FOUT  (R2),(R1),1                                                      
         BAS   RE,FNDNXUF                                                       
         LA    R1,1(R1)                                                         
         BCT   R4,FMT34                                                         
*                                                                               
FMT40    DS    0H                                                               
*                                                                               
         FOUT  CLTDLYH,=C'N',1                                                  
*                                                                               
         CLI   CDAILY,0                                                         
         BE    FMT40A                                                           
         FOUT  CLTDLYH,CDAILY,1                                                 
*                                                                               
FMT40A   FOUT  CLTTTLEH,CTITLE,10                                               
*                                                                               
         FOUT  CLTMDNMH,SPACES,11                                               
*                                                                               
         TM    SVAGYFL1,X'40'      IF SPECIAL MEDIA NAMES                       
         BNO   FMT41                                                            
         CLC   CMEDNAME,SPACES                                                  
         BNH   FMT41                                                            
         MVI   CLTMDNM,C'='        START WITH '=' SO CAN IGNORE GARBAGE         
         MVC   CLTMDNM+1(L'CMEDNAME-1),CMEDNAME                                 
*                                                                               
*        CLEAR BOTH HERE                                                        
*                                                                               
FMT41    FOUT  CLTTCLTH,SPACES,6                                                
         FOUT  CLTTPRDH,SPACES,3                                                
         OC    CMCLTCOD,CMCLTCOD   TEST SPCL TRAFFIC CLIENT                     
         BZ    FMT50                                                            
         MVC   DMCB+4(4),=X'D9000A15'   GET CLUNPK ADDRESS                      
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(C'Y',CMCLTCOD),CLTTCLT                                
*                                                                               
         LA    R4,CLTTCLT+2                                                     
         CLI   0(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C'='                                                       
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CMCLTUNQ,2(R4),1,=C'TOG',RR=RELO                 
*                                                                               
         CLI   CMCLTPRD,0          TEST SPCL TRAFFIC PRODUCT                    
         BE    FMT50                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVKEY+1                                                 
         MVC   KEY+2(2),CMCLTCOD                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         LA    R4,CLIST-CLTHDRD+REC2                                            
FMT42    CLC   3(1,R4),CMCLTPRD                                                 
         BE    FMT44                                                            
         LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNL   FMT42                                                            
         DC    H'0'                                                             
FMT44    MVC   CLTTPRD,0(R4)                                                    
*                                                                               
FMT50    BAS   RE,DISPPST                                                       
*--SPECIAL FIX FOR SFM RECORDS MAINTAINED ON FILE                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           2=SPOT,3=NET                                 
         BNE   FMTX                                                             
*                                                                               
         LA    R4,30                                                            
         LA    R2,CLTOP1H                                                       
         LA    R1,DETDEFTB                                                      
*                                                                               
FMT70    CLI   0(R1),X'40'                                                      
         BE    *+10                                                             
         MVC   8(1,R2),0(R1)                                                    
         LA    R1,1(R1)                                                         
         BAS   RE,FNDNXUF                                                       
         BCT   R4,FMT70                                                         
*                                                                               
FMTX     B     EXXMOD                                                           
         DROP  R1,RF                                                            
DETDEFTB DC    XL25'F0F0F0F0F04040F0F0F0F0F0F0F040F0F0D540F0D5D5D5D5F0'         
         DC    XL5'D5E2D5D5D5'                                                  
         EJECT                                                                  
EDT      MVI   COMPCD,0                                                         
         CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY         CHANGE SO REREAD RECORD                        
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
EDT0     LA    R2,CLTNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   CNAME,CLTNAME                                                    
         OC    CNAME,SPACES                                                     
         CLC   =C'DELETE',CNAME                                                 
         BNE   EDT0X                                                            
         MVI   ERRCD,X'02'                                                      
         CLI   SVACT,C'A'          NO DELETE ON ADD                             
         BE    LFMERR                                                           
         MVI   ERRCD,X'E1'                                                      
         OC    CLIST(100),CLIST    MAKE SURE NO PRDS LEFT                       
         BNZ   LFMERR                                                           
         CLI   SVAPROF+7,C'C'     IF CANADIAN AGENCY                            
         BNE   EDT0N                                                            
         CLI   SVEBCMED,C'T'      AND MEDIA T                                   
         BNE   EDT0N                                                            
         BAS   RE,CKCANDN         CHECK MEDIA C AND N TOO                       
         BNE   LFMERR             ERROR - PRODUCTS EXIST                        
*                                                                               
EDT0N    BAS   RE,CK4GRP                                                        
         MVC   HALF2(1),SVKEY+1   SET AGENCY/MEDIA FOR DODEF ROUTINE            
         BAS   RE,DODEF           MARK DIV/DST DEF RECORDS TOO                  
         CLI   SVAPROF+7,C'C'     IF CANADIAN AGENCY                            
         BNE   EDT0S                                                            
         CLI   SVEBCMED,C'T'      AND MEDIA T                                   
         BNE   EDT0S                                                            
         MVC   HALF2(1),HALF      AGENCY/MED FOR MEDIA C                        
         CLI   HALF2,0                                                          
         BE    *+12               NO MEDIA C                                    
         BAS   RE,DODEF           MARK RECORDS FOR MEDIA C                      
         BAS   RE,MRKCLT          MARK MEDIA C CLIENT RECORD                    
*                                                                               
         MVC   HALF2(1),HALF+1    AGY/MED FOR MEDIA N                           
         CLI   HALF2,0                                                          
         BE    *+12               NO MEDIA N                                    
         BAS   RE,DODEF           MARK RECORDS FOR MEDIA N                      
         BAS   RE,MRKCLT          MARK MEDIA N CLIENT RECORD                    
*                                                                               
         BAS   RE,DELNET          DELETE CLT SPECIFIC NETWORK DEF RECS          
*                                                                               
EDT0S    DS    0H                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 GETREC              MUST REREAD CLIENT HEADER                    
         MVI   KEY+13,X'DD'        SET DIR DELETED                              
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVI   REC+15,X'C0'                                                     
         GOTO1 PUTREC                                                           
         MVC   CLTNAME(20),=C'** CLIENT DELETED **'                             
         FOUT  (R2)                                                             
         B     EXXMOD                                                           
EDT0X    DS    0H                                                               
         LA    R2,CLTONUMH                                                      
         MVI   SVCOFFC,0                                                        
         CLI   SVACT,C'A'                                                       
         BE    *+10                                                             
         MVC   SVCOFFC,COFFICE     SAVE OLD OFFICE CODE                         
         MVI   COFFICE,0                                                        
         CLI   SVAPROF+13,C'Y'     TEST OFFICE REQUIRED                         
         BE    EDT1A                                                            
         CLI   5(R2),0                                                          
         BE    EDT1B                                                            
*                                                                               
EDT1A    TM    SVAGYFL1,X'10'      OFF=HEX OPTION IN USE?                       
         BO    EDT1AA              YES                                          
         CLI   8(R2),C'A'                                                       
         BL    EDTPERR                                                          
EDT1AA   CLI   CLTONUM,C'='                                                     
         BE    EDTPERR                                                          
         CLI   CLTONUM,C','                                                     
         BE    EDTPERR                                                          
         CLI   CLTONUM,C'-'                                                     
         BE    EDTPERR                                                          
         GOTO1 ANY                                                              
         MVC   COFFICE,CLTONUM                                                  
*                                                                               
EDT1B    LA    R2,CLTAOFCH                                                      
         CLI   SVACT,C'A'          ALWAYS VALIDATE ON ADD                       
         BE    EDT1B1                                                           
         TM    4(R2),X'20'         HAS FIELD CHANGED                            
         BO    EDT1C               NO, THEN SKIP                                
EDT1B1   BAS   RE,CHKAOFF          CHECK IF OFFICE EXISTS                       
         BNE   MYEXIT                                                           
         B     EDT1C                                                            
*                                                                               
EDT1C    LA    R2,CLTIFCDH                                                      
         CLI   5(R2),0                                                          
         BE    EDT2                                                             
         MVC   CCLTIFC,8(R2)                                                    
         B     GOEDTOP                                                          
*                                                                               
EDT2     LA    R1,AGYTAB              TABLE OF AGENCIES THAT REQUIRE            
         LA    R4,AGYTABN             NUMBER OF AGYS                            
EDT2A    CLC   AGYALPHA,0(R1)                                                   
         BE    EDT2B                                                            
         LA    R1,2(R1)                                                         
         BCT   R4,EDT2A                                                         
         B     GOEDTOP                                                          
*                                                                               
EDT2B    MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR                                                           
*                                                                               
GOEDTOP  GOTO1 =A(EDTOP),RR=RELO                                                
         BNE   LFMERR                                                           
         EJECT                                                                  
EDTP     DS    0H                                                               
         LA    R2,CLTOP1AH         POL ONLY?                                    
         CLI   8(R2),C'N'                                                       
         BE    EDTP1A                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   EDTPERR                                                          
EDTP1A   MVC   CPOLONLY,8(R2)                                                   
*                                                                               
         LA    R2,CLTOP1H                                                       
         OC    CPWPCT,CPWPCT       PW CLT - ALWAYS BRAND POL                    
         BZ    EDTP1B                                                           
         CLI   8(R2),C'0'          IF TRND = 0 - ERROR                          
         BE    EDTPERR                                                          
*                                                                               
EDTP1B   LA    R4,PROFTAB                                                       
         LA    R6,WORK                                                          
         MVC   WORK(30),=30C'0'    INITIALIZE TO 0'S                            
EDTP1    CLI   0(R4),0             END OF TABLE                                 
         BE    EDTP6                                                            
         CLI   2(R4),X'FF'                                                      
         BE    EDTP3            GO TO SPECIAL EDIT FOR 1-9 OR A-Z               
         SR    R5,R5                                                            
         IC    R5,0(R4)         LENGTH OF ENTRY                                 
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LA    R7,2(R4)       SET R7 TO FIRST ENTRY                             
*                 R5 NOW SET FOR BCT-NUMBER OF VALID CODES                      
EDTP2    CLC   8(1,R2),0(R7)                                                    
         BE    EDTP4               VALID VALUE FOUND                            
         LA    R7,1(R7)                                                         
         BCT   R5,EDTP2                                                         
EDTPERR  MVI   ERRCD,INVERR        INVALID INPUT                                
         B     LFMERR                                                           
EDTP3    CLI   8(R2),C'0'               0-9                                     
         BL    EDTP3A                                                           
         CLI   8(R2),C'9'                                                       
         BH    EDTPERR                                                          
         B     EDTP4                                                            
*                                                                               
EDTP3A   CLI   8(R2),C'A'               A-Z                                     
         BL    EDTPERR                                                          
         CLI   8(R2),C'Z'                                                       
         BH    EDTPERR                                                          
         B     EDTP4                                                            
*                                                                               
EDTP4    MVC   0(1,R6),8(R2)        PUT VALIDATED VALUE INTO WORK               
         LA    R6,1(R6)                                                         
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         BAS   RE,FNDNXUF            NEXT UNPROTECTED FIELD                     
         B     EDTP1                                                            
*                                                                               
EDTP6    CLI   SVEBCMED,C'R'       FOR MEDIA R                                  
         BNE   EDTP8                                                            
         MVI   WORK+3,C'1'         RATING SERVICE CODE = 1 ONLY                 
         MVI   CLTOP4,C'1'         ENSURE DISPLAY                               
         OI    CLTOP4H+6,X'80'                                                  
*                                                                               
EDTP8    CLI   SVEBCMED,C'T'       FOR MEDIA T                                  
         BNE   EDTP10                                                           
         CLI   SVAPROF+7,C'C'      AND -NOT- CANADIAN AGENCY                    
         BE    EDTP10                                                           
         MVI   WORK+3,C'0'         RATING SERVICE CODE = 0 ONLY                 
         MVI   CLTOP4,C'0'         ENSURE DISPLAY                               
         OI    CLTOP4H+6,X'80'                                                  
         EJECT                                                                  
*                        NOW CHECK NEW PROFILE VS OLD                           
EDTP10   DS    0H        AND CROSS-CHECK NEW PROFILE                            
         CLI   SVACT,C'A'                                                       
         BE    EDTP10A                                                          
*                                  BILLING PROF CHK BYPASSED                    
         B     EDTP10A                                                          
*                                                                               
         CLC   WORK+5(1),CPROF+5                                                
         BE    EDTP10A                                                          
         LA    R2,CLTOP6H                                                       
         MVI   ERRCD,NOCHGERR                                                   
         B     LFMERR                                                           
*                             GET AGYHDR TO CROSS-CHECK PROFILES                
EDTP10A  DS    0H                                                               
         CLI   WORK+3,C'0'                                                      
         BNE   CKARB                                                            
         CLI   SVAPROF,C'0'                                                     
         BE    EDTP10D                                                          
         CLI   SVAPROF,C'2'                                                     
         BE    EDTP10D                                                          
PROFERR  LA    R2,CLTOP4H                                                       
         MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
CKARB    CLI   SVAPROF,C'1'                                                     
         BE    EDTP10D                                                          
         CLI   SVAPROF,C'2'                                                     
         BE    EDTP10D                                                          
         B     PROFERR                                                          
*                                                                               
EDTP10D  MVC   CPROF,WORK                                                       
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BE    EDTP10E                                                          
         LA    R2,CLTEX1H                                                       
         CLI   WORK+15,C'0'        ELSE MUST BE 0                               
         BNE   EDTPERR             MUST BE 0                                    
         LA    R2,CLTEX2H                                                       
         CLI   WORK+16,C'0'                                                     
         BNE   EDTPERR             MUST BE 0                                    
         LA    R2,CLTEX10H                                                      
         CLI   WORK+24,C'C'        COUNTRY                                      
         BE    EDTPERR             MUST BE U                                    
         MVI   WORK+24,C'U'                                                     
         B     EDTP10F                                                          
*                                                                               
EDTP10E  CLI   WORK+24,C'0'        CANADIAN AGENCY - DEFAULT COUNTRY            
         BNE   *+8                                   IS CANADA                  
         MVI   WORK+24,C'C'                                                     
         CLI   WORK+24,C'C'                                                     
         BE    EDTP10F                                                          
         LA    R2,CLTEX10H         ****                                         
         CLC   SVAALPHA,=C'HD'     **** ONLY HDTO CAN HAVE USA CLIENT           
         BNE   EDTPERR             ****                                         
*                                                                               
EDTP10F  DS    0H                                                               
         MVC   CEXTRA,WORK+15                                                   
         CLI   SVAPROF+12,C'Y'     SEE IF MGD IN MISSED MTH ALLOWED             
         BE    EDTP10G                                                          
         LA    R2,CLTEX8H                                                       
         CLI   CEXTRA+7,C'Y'       NO - THEN DISALLOW AT CLIENT                 
         BE    EDTPERR                                                          
*                                                                               
EDTP10G  DS    0H                                                               
         CLI   SVEBCMED,C'R'       IF MEDIA R                                   
         BNE   EDTP10H                                                          
         CLI   SVACT,C'A'          & ACTION ADD                                 
         BNE   EDTP10H                                                          
         LA    R2,CLTEX11H                                                      
         CLI   CEXTRA+10,C'Y'      DON'T ALLOW OUT-OF-WEEK ROTATOR              
         BE    EDTPERR                                                          
*                                                                               
EDTP10H  DS    0H                                                               
*                                                                               
EDTP15   DS    0H                                                               
         LA    R2,CLTDLYH                                                       
         MVI   CDAILY,0                                                         
         CLI   5(R2),0                                                          
         BE    EDTP15A                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'N'                                                       
         BE    EDTP15A                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   LFMERR                                                           
         MVC   CDAILY,8(R2)                                                     
*                                                                               
EDTP15A  DS    0H                                                               
         LA    R2,CLTTTLEH                                                      
         XC    CTITLE,CTITLE                                                    
         CLI   5(R2),0                                                          
         BE    EDTP15D                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   CEXTRA+2,C'0'                                                    
         BE    LFMERR                                                           
         CLI   CEXTRA+2,C'N'                                                    
         BE    LFMERR                                                           
         MVC   CTITLE,CLTTTLE                                                   
         OC    CTITLE,SPACES                                                    
         B     EDTP15X                                                          
*                                                                               
EDTP15D  MVI   ERRCD,MSSNGERR                                                   
         CLI   CEXTRA+2,C'0'                                                    
         BE    EDTP15X                                                          
         CLI   CEXTRA+2,C'N'                                                    
         BNE   LFMERR              Y OR * REQUIRE TITLE                         
EDTP15X  DS    0H                                                               
         SPACE 1                                                                
*============================================================*                  
* VALIDATE SPECIAL TRAFFIC CLIENT CODES                      *                  
*============================================================*                  
         SPACE 1                                                                
EDTP16   MVI   ERRCD,INVERR                                                     
         LA    R2,CLTTCLTH                                                      
         CLI   5(R2),0                                                          
         BE    EDTP18                                                           
         CLC   SVAALPHA,=C'BS'     ONLY FOR BACKER                              
         BE    EDTP16A                                                          
         CLC   SVAALPHA,=C'CK'     AND COKEAT                                   
         BE    EDTP16A                                                          
         CLC   SVAALPHA,=C'TH'     OR ZENITH                                    
         BE    EDTP16A                                                          
         CLC   SVAALPHA,=C'SJ'     AND SJR                                      
         BE    EDTP16A                                                          
         B     LFMERR                                                           
*                                                                               
EDTP16A  DS    0H                                                               
         GOTO1 VSCANNER,DMCB,(R2),(1,REC2)                                      
         CLI   DMCB+4,1                                                         
         BNE   LFMERR                                                           
*                                                                               
         LA    R5,REC2                                                          
         USING SCAND,R5                                                         
*                                                                               
         XC    DUB,DUB                CLEAR OUTPUT AREA                         
         CLI   FLD1LEN,2                                                        
         BL    LFMERR                                                           
         CLI   FLD1LEN,3                                                        
         BH    LFMERR                                                           
*                                                                               
         MVC   DMCB+4(4),=X'D9000A14' GET CLPACK ADDRESS                        
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),FLD1,DUB                                               
*                                                                               
         CLI   FLD2LEN,2                                                        
         BH    LFMERR                                                           
         ZIC   R0,FLD2LEN                                                       
         GOTO1 =V(HEXIN),DMCB,FLD2,DUB+2,(R0),RR=RELO                           
         OC    12(4,R1),12(R1)                                                  
         BZ    LFMERR                                                           
         CLI   FLD2LEN,1           TEST FIELD ONE BYTE                          
         BNE   EDT16B2             NO                                           
         IC    R0,DUB+2            GET EDITED VALUE                             
         SRL   R0,4                AND RIGHT ALIGN                              
         STC   R0,DUB+2                                                         
*                                                                               
EDT16B2  MVC   CMCLTCOD(3),DUB     SAVE VALUES                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVKEY+1                                                 
         MVC   KEY+2(2),CMCLTCOD                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOFNDERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
         LA    R0,REC2            READ OTHER CLTHDR                             
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVACT,C'A'          TEST ADD                                     
         BE    EDTP16C                                                          
         CLC   DUB(3),CMCLTCOD     TEST CODES CHANGED                           
         BE    EDTP18              NO                                           
         MVI   ERRCD,NOCHGERR                                                   
         OC    CMCLTCOD,CMCLTCOD   TEST ANYTHING THERE BEFORE                   
         BNZ   LFMERR              YES - CANNOT CHANGE                          
*                                                                               
EDTP16C  CLI   CLTTPRDH+5,0       IF SPCL TRF PRD EXISTS                        
         BNE   EDTP18             DISCREPENCIES OKAY                            
** CHECK BOTH LISTS FOR PRODUCT DISCREPENCIES                                   
         LA    R1,CLIST-CLTHDRD+REC   CURRENT CLT HDR                           
         OC    0(100,R1),0(R1)                                                  
         BZ    EDTP18             NO PRDS - CAN'T BE DISCREPENCIES              
EDTP16K  LA    RE,CLIST-CLTHDRD+REC2  OTHER CLT HDR                             
         OC    0(100,RE),0(RE)                                                  
         BZ    EDTP18             NO PRDS - CAN'T BE DISCREPENCIES              
EDTP16M  CLC   0(3,R1),0(RE)      CHECK FOR SAME ALPHA PRDS                     
         BNE   EDTP16R                                                          
         CLC   3(1,R1),3(RE)      SEQUENCE NUMBERS MUST BE THE SAME             
         BNE   EDTP16X            **ERROR                                       
         B     EDTP16V            ALPHA & SEQ MATCH - SKIP TO NEXT              
*                                                                               
EDTP16R  LA    RE,4(RE)           BUMP OTHER CLTHDR LIST                        
         OC    0(4,RE),0(RE)                                                    
         BNZ   EDTP16M                                                          
** ALPHA PRD NOT IN OTHER CLTHR LIST-MAKE SURE ITS SEQ # ISN'T EITHER           
         LA    RE,CLIST-CLTHDRD+REC2                                            
EDTP16S  OC    0(4,RE),0(RE)                                                    
         BZ    EDTP16V            SEQ NUMBER NOT IN LIST                        
         CLC   3(1,R1),3(RE)                                                    
         BE    EDTP16X            **ERROR                                       
         LA    RE,4(RE)                                                         
         B     EDTP16S                                                          
*                                                                               
EDTP16V  LA    R1,4(R1)           BUMP CURRENT CLTHR LIST                       
         OC    0(4,R1),0(R1)                                                    
         BNZ   EDTP16K                                                          
         B     EDTP18                                                           
EDTP16X  LA    R2,CLTTCLTH                                                      
         MVI   ERRAREA,X'FF'                                                    
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(32),=CL32'ERROR-PRDS MUST MATCH TRFC PRDS'                
         FOUT  LFMMSGH                                                          
         B     MYEXIT                                                           
*                                                                               
EDTP18   OC    CMCLTCOD,CMCLTCOD                                                
         BNZ   EDTP18A                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),0                                                          
         BNE   LFMERR                                                           
*                                                                               
EDTP18A  MVI   DUB,0               CLEAR TEMP AREA                              
         LA    R2,CLTTPRDH                                                      
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    EDTP19              NO - MAKE SURE DIDN'T ERASE                  
         MVC   WORK,CLTTPRD                                                     
         OC    WORK,SPACES                                                      
         LA    R4,CLIST-CLTHDRD+REC2                                            
*                                                                               
EDTP18B  CLC   WORK(3),0(R4)                                                    
         BE    EDTP18X                                                          
         LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNL   EDTP18B                                                          
         B     LFMERR                                                           
*                                                                               
EDTP18X  MVC   DUB(1),3(R4)                                                     
*                                                                               
EDTP19   CLI   CMCLTPRD,0          ONLY ALLOWED TO INPUT ONCE                   
         BNE   *+10                                                             
         MVC   CMCLTPRD,DUB                                                     
         CLC   DUB(1),CMCLTPRD     TEST CODES CHANGED                           
         BE    EDTP20                                                           
         B     LFMERR              NOT ALLOWED TO EVER CHANGE                   
*                                                                               
EDTP20   TM    SVAGYFL1,X'40'      IF SPECIAL MEDIA NAMES                       
         BNO   EDTP30                                                           
         XC    CMEDNAME,CMEDNAME   CLEAR MEDIA NAME                             
         LA    R2,CLTMDNMH                                                      
         CLI   5(R2),0                                                          
         BE    EDTP30                                                           
         MVI   ERRCD,INVERR        INVALID INPUT                                
         CLI   8(R2),C'='          INPUT MUST START WITH '='                    
         BNE   LFMERR              ELSE INPUT IS INVALID                        
         MVC   CMEDNAME,9(R2)                                                   
*                                                                               
EDTP30   BAS   RE,VALPST                                                        
         B     OUTPUT                                                           
         EJECT                                                                  
NXTEL    CLI   0(R6),0             END                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                 SAFETY CATCH                                 
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)        RIGHT ELEMENT                                
         BER   RE                                                               
         B     NXTEL                                                            
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  NOT FOUND EXIT                               
         SPACE 2                                                                
FNDUF    TM    1(R2),X'20'    FIND NEXT UNPROTECTED FIELD                       
         BCR   8,RE                                                             
FNDNXUF  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'                END OF SCREEN                                
*                                                                               
OUTPUT   MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         CLI   SVACT,C'A'                                                       
         BNE   OUT1                                                             
         MVC   REC(13),SVKEY                                                    
         MVC   REC+13(2),=H'1280'                                               
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         GOTO1 CNADDSPT                                                         
         B     OUT2                                                             
*                                                                               
OUT1     DS    0H                                                               
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC              REREAD CLTREC BEFORE PUTREC                  
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 CNCHASPT                                                         
*                                                                               
OUT2     DS    0H                                                               
         GOTO1 =A(OFCPTR),RR=RELO                                               
*                                                                               
REQREC   XC    REC(150),REC       GENERATE REQUEST RECORD                       
         LA    R1,REC                                                           
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,REC+26                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),SVEBCMED                                                 
         MVC   5(3,R1),SVEBCCLT                                                 
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'C'                                                      
         MVI   63(R1),C'A'                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+8                                                              
         MVI   63(R1),C'C'                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                      
         B     EXXMOD                                                           
         EJECT                                                                  
*====================================================================*          
*    CHECK IF AUTHORIZED FOR DISPLAY & CHANGE                        *          
*====================================================================*          
         SPACE 1                                                                
CHKAUT   NTR1                                                                   
         LA    R2,LFMKEYH                                                       
         CLI   T219FFD+1,C'*'      DDS TERMINAL?                                
         BE    CHKAUTX                                                          
         OC    T219FFD+6(2),T219FFD+6 TEST ANY SECURITY LIMIT                   
         BZ    CHKAUTX             NO                                           
         CLI   T219FFD+6,C'+'      TEST MKT LOCKOUT                             
         BE    CHKAUTX                                                          
         CLI   T219FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BNE   CHKAUT10                                                         
*                                                                               
         CLC   T219FFD+7(1),COFFICE TEST RIGHT OFFICE                           
         BNE   SECERR                                                           
         B     CHKAUTX                                                          
*                                                                               
CHKAUT10 CLI   T219FFD+6,C'$'      TEST OFFICE LIST LOCKOUT                     
         BNE   CHKAUTX                                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T219FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,VCOMFACS                                           
         CLI   0(R1),0                                                          
         BNE   SECERR                                                           
CHKAUTX  B     XIT                                                              
         EJECT                                                                  
*====================================================================*          
*    CHECK MEDIA C AND N RECORDS TOO                                 *          
*    SETS HALF(1)= AGY/MED FOR C AND HALF+1(1)= AGY/MED FOR N        *          
*====================================================================*          
         SPACE 1                                                                
CKCANDN  NTR1                                                                   
         MVC   HALF,=H'0'                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,6              AGENCY RECORD                                 
         MVC   KEY+1(2),SVAALPHA  AGENCY ALPHA                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'               MUST BE ABLE TO FIND AGENCY RECORD            
         GOTO1 GETREC                                                           
         LA    R6,REC+24                                                        
         MVI   ELCODE,2                                                         
CKCN10   BAS   RE,NEXTEL                                                        
         BNE   CKCN30                                                           
*                                                                               
         CLI   2(R6),C'C'                                                       
         BNE   CKCN20                                                           
         MVC   HALF(1),3(R6)      SAVE BINARY AGY/MED FOR MED 'C'               
         B     CKCN10                                                           
CKCN20   CLI   2(R6),C'N'                                                       
         BNE   CKCN10                                                           
         MVC   HALF+1(1),3(R6)    SAVE BINARY AGY/MED FOR MED C'N'              
         B     CKCN10                                                           
*                                                                               
CKCN30   CLI   HALF,0                                                           
         BE    CKCN40                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),HALF                                                    
         MVC   KEY+2(2),SVKEY+2                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CKCN40                                                           
         GOTO1 GETREC                                                           
         OC    CLIST(100),CLIST                                                 
         BNZ   CKCNNO             ** ERROR - PRODUCTS EXIST                     
*                                                                               
CKCN40   CLI   HALF+1,0                                                         
         BE    CKCNYES                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),HALF+1                                                  
         MVC   KEY+2(2),SVKEY+2                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CKCNYES                                                          
         GOTO1 GETREC                                                           
         OC    CLIST(100),CLIST                                                 
         BNZ   CKCNNO             ** ERROR - PRODUCTS EXIST                     
*                                                                               
CKCNYES  CR    RB,RB                                                            
         B     CKCNX                                                            
CKCNNO   LTR   RB,RB                                                            
CKCNX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    CHECK FOR DIV/DST DEF RECS                                                 
*                                                                               
DODEF    NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),HALF2      AGY/MED                                      
         MVC   KEY+3(2),SVKEY+2    CLT                                          
         GOTO1 HIGH                                                             
         B     DODEF10                                                          
DODEF5   GOTO1 SEQ                                                              
DODEF10  CLC   KEYSAVE(5),KEY                                                   
         BNE   DODEF15                                                          
         MVI   KEY+13,X'DD'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     DODEF5                                                           
*                                                                               
DODEF15  XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),HALF2     AGENCY/MEDIA                                  
         MVC   KEY+3(2),SVKEY+2   CLIENT                                        
         GOTO1 HIGH                                                             
         B     DODEF25                                                          
DODEF20  GOTO1 SEQ                                                              
DODEF25  CLC   KEY(5),KEYSAVE                                                   
         BNE   DODEF40                                                          
         CLI   KEY+8,C'G'                                                       
         BL    DODEF35                                                          
*                            SEE IF I NEED TO ADD TO SCHEME TABLE               
         LA    R5,ELEM                                                          
DODEF28  CLI   0(R5),0                                                          
         BE    DODEF30                                                          
         CLC   0(1,R5),KEY+8                                                    
         BE    DODEF35             FOUND                                        
         LA    R5,1(R5)                                                         
         B     DODEF28                                                          
*                                                                               
DODEF30  MVC   0(1,R5),KEY+8       SAVE LIST OF SCHEMES                         
DODEF35  MVI   KEY+13,X'DD'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     DODEF20                                                          
*                                                                               
DODEF40  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D03'         MKTGRPS ASSGNS                           
         MVC   KEY+2(1),HALF2          AGY/MED                                  
         MVC   KEY+3(2),SVKEY+2        CLIENT                                   
         GOTO1 HIGH                                                             
         B     DODEF50                                                          
DODEF45  GOTO1 SEQ                                                              
DODEF50  CLC   KEY(11),KEYSAVE                                                  
         BNE   DODEF60                                                          
         MVI   KEY+13,X'DD'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     DODEF45                                                          
*                                                                               
DODEF60  LA    R5,ELEM                                                          
DODEF65  CLI   0(R5),0             END OF SCHEME LIST                           
         BE    DODEFX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),HALF2          AGY/MED                                  
         MVC   KEY+3(2),SVKEY+2        CLIENT                                   
         MVC   KEY+8(1),0(R5)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST FIND SCHEME                             
         GOTO1 GETREC                                                           
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'02'                                                     
DODEF70  BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND EXCEPTION ELEM                     
         CLC   2(3,R6),SVEBCCLT                                                 
         BNE   DODEF70                                                          
         GOTO1 VRECUP,DMCB,(0,REC),0(R6),0                                      
         GOTO1 PUTREC                                                           
         LA    R5,1(R5)            NEXT SCHEME                                  
         B     DODEF65                                                          
DODEFX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    DELETE CANADIAN CLIENT SPECIFIC NETWORK DEFINITION RECORDS                 
*                                                                               
DELNET   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NDEFRECD,R2                                                      
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,AGYALPHA   AGY                                          
         GOTO1 HIGH                                                             
         B     DN10                                                             
*                                                                               
DN5      GOTO1 SEQ                                                              
*                                                                               
DN10     CLC   KEYSAVE(4),KEY      STILL THIS AGY                               
         BNE   DNX                                                              
         CLC   NDEFKCLT,SVCLT      THIS CLT                                     
         BNE   DN5                 NO GET NEXT                                  
         MVI   KEY+13,X'DD'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                 DELETE DIRECTORY                             
         GOTO1 GETREC                                                           
         MVI   REC+15,X'C0'                                                     
         GOTO1 PUTREC              DELETE RECORD                                
         B     DN5                                                              
*                                                                               
DNX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE PST CODES                                                     
*                                                                               
VALPST   NTR1                                                                   
         LA    R2,CLTPSTH                                                       
         XC    CPST,CPST                                                        
         CLI   5(R2),0                                                          
         BE    VPX                                                              
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,CLTPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 VPSTVAL,DMCB,(R4)                                                
         MVI   ERRCD,INVERR                                                     
         CLI   PSTERR,0                                                         
         BNE   LFMERR                                                           
         MVC   CPST,PSTOUT         OUTPUT                                       
         BAS   RE,DISPPST                                                       
*                                                                               
VPX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        DISPLAY PST CODES                                                      
*                                                                               
DISPPST  NTR1                                                                   
         MVC   CLTPST,SPACES       OUTPUT                                       
         OI    CLTPSTH+6,X'80'                                                  
         OC    CPST,CPST           IS THERE ANYTHING TO DISPLAY                 
         BZ    DPX                                                              
*                                                                               
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,CPST                                                          
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 VPSTVAL,DMCB,(R4)                                                
         MVC   CLTPST,PSTOUT       OUTPUT                                       
*                                                                               
DPX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* THIS ROUTINE CHECKS TO MAKE SURE THE CURRENT OPTION WILL FIT IN THE           
* OPTIONS FIELD FOR DISPLAY.                                                    
*                                                                               
* I/P:   R0 = L(OF CURRENT OPTION LITERAL)                                      
*        R1 = A(CURRENT POSITION IN OPTIONS FIELD)                              
*                                                                               
* NOTE: THIS ROUTINE DOESN'T SAVE/RESTORE REGISTERS -- BE CAREFUL!!             
*                                                                               
CHECKOPT EQU   *                                                                
*                                                                               
         AR    R0,R1               A(NEW FIELD POSITION)                        
         LA    R3,CLTOPTS          A(SCREEN FIELD)                              
         LA    R3,L'CLTOPTS-1(R3)  A(LAST POSTITION IN FIELD)                   
         CR    R0,R3               WILL CURRENT OPTION FIT IN FIELD?            
         BH    CKOPERR             NO - SO ERROR                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
CKOPEXIT EQU   *                                                                
*                                                                               
         BR    RE                  RETURN TO CALLER                             
*                                                                               
CKOPERR  EQU   *                                                                
*                                                                               
         ZIC   R3,SCRNFLAG         SAVE OLD COUNT OF MISSED OPTS                
         LA    R3,1(R3)            INC COUNT                                    
         STC   R3,SCRNFLAG         STORE IT                                     
         LTR   RC,RC               SET ERROR CC                                 
         B     CKOPEXIT            AND RETURN                                   
         EJECT                                                                  
*                                                                               
*    MARKS CLIENT RECORD                                                        
*                                                                               
MRKCLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),HALF2     AGENCY/MEDIA                                  
         MVC   KEY+2(2),SVKEY+2   CLIENT                                        
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BNE   MRKCLTX                                                          
         GOTO1 GETREC                                                           
         MVI   KEY+13,X'DD'        SET DIR DELETED                              
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVI   REC+15,X'C0'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
MRKCLTX  XIT1                                                                   
         SPACE 2                                                                
*                                                                               
*    CHECK ACCOUNTING OFFICE  MAKE SURE IT EXISTS                               
*                                                                               
CHKAOFF  NTR1                                                                   
         L     RF,VCOMFACS         GET SYS NUM TO SWITCH BACK TO                
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,GTFACTB                                                
         LA    R1,GTFACTB                                                       
         USING FACTSD,R1                                                        
         MVC   SYSSW,FASYS                                                      
         DROP  R1                                                               
*                                                                               
         XC    POWCODE,POWCODE                                                  
         XC    ACCOFF,ACCOFF                                                    
         LA    R2,CLTAOFC          WHAT DID THEY ENTER                          
         LA    R1,0                                                             
         CLI   CLTAOFCH+5,0                                                     
         BE    CO10                                                             
         LA    R1,1                                                             
         LA    R2,1(R2)                                                         
         CLI   CLTAOFCH+5,1                                                     
         BE    CO10                                                             
         LA    R1,2                                                             
         LA    R2,2(R2)                                                         
         CLI   CLTAOFCH+5,2                                                     
         BE    CO10                                                             
         LA    R2,CLTAOFC          WHAT DID THEY ENTER                          
         LA    R3,24                                                            
         LA    R1,0                                                             
COLOOP   CLI   0(R2),C','                                                       
         BE    CO10                                                             
         CLI   0(R2),C'/'                                                       
         BE    CO10                                                             
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,COLOOP                                                        
*                                                                               
CO10     STC   R1,OFFLEN           SAVE OFFICE LENGTH                           
         MVC   ACCOFF(2),CLTAOFC                                                
         CLI   OFFLEN,1                                                         
         BNE   *+8                                                              
         MVI   ACCOFF+1,C' '                                                    
         CLI   1(R2),C' '          IS THERE AN OVERRIDE AGENCY                  
         BNH   *+10                                                             
         MVC   POWCODE,1(R2)       YES - SAVE POWER CODE                        
*                                                                               
         CLI   OFFLEN,2                                                         
         BH    OFCERR              NO OFFICE LENGTH > 2                         
         CLI   OFFLEN,0                                                         
         BNE   CO20                                                             
         XC    CACCOFC,CACCOFC     DEFAULT TO SPOT OFFICE                       
         XC    CACCAGY,CACCAGY                                                  
         MVC   CACCOFC,COFFICE                                                  
         MVI   CACCOFC+1,C' '                                                   
         B     COX                                                              
CO20     CLI   OFFLEN,2            IF THEY ENTERED 2 CHAR OFF                   
         BNE   CO30                                                             
         CLI   SVACCOFC,C'Y'       BUT 2 CHAR NOT REQUIRED                      
         BNE   ONEERR              =ERROR                                       
*                                                                               
CO30     DS    0H                                                               
         OC    POWCODE,POWCODE     OVERRIDE AGY?                                
         BNZ   CO40                                                             
         CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED                              
         BNE   CO90                IF NOT THEN SKIP SWITCH                      
         L     RF,VCOMFACS         SWITCH TO ACC SYSTEM                         
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'ACC',0                                              
         B     CO70                                                             
*                                                                               
CO40     DS    0H                  VALIDATE AGAINST AGY HDR CODE LIST           
         LA    R3,SVACCAGY                                                      
         LA    R1,8                                                             
CO40LP   CLC   0(2,R3),POWCODE     MATCH?                                       
         BE    CO50                                                             
         CLI   0(R3),C' '                                                       
         BNH   AGYERR                                                           
         LA    R3,2(R3)                                                         
         BCT   R1,CO40LP                                                        
         B     AGYERR                                                           
*                                                                               
CO50     MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         XC    CTKEY,CTKEY         ACC AGY CODE                                 
         LA    R6,CTKEY                                                         
         USING CT5REC,R6                                                        
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,POWCODE                                                 
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',CTKEY,REC2               
         CLI   8(R1),0             ERRORS?                                      
         BNE   AGYERR                                                           
         DROP  R6                                                               
*                                                                               
         USING CTSYSD,R6                                                        
         LA    R6,REC2                                                          
         MVI   ELCODE,X'21'        GET SE NUM FOR ACC FILE                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CO60NX   BAS   RE,NEXTEL                                                        
         BNE   AGYERR              ERROR IF NOT FOUND                           
         CLI   CTSYSEL,X'21'       STILL X'21' EL                               
         BNE   AGYERR              ERROR IF NOT FOUND                           
         CLI   CTSYSNUM,X'06'      ACC??                                        
         BNE   CO60NX                                                           
*                                                                               
         MVC   COMPCD(1),CTSYSAGB  AGY BINARY CD                                
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),CTSYSSE     SE NUM                                       
         L     RF,VCOMFACS         SWITCH TO THAT ACC SYSTEM                    
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
*                                                                               
CO70     CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    SYSERR                                                           
         CLI   4(R1),1             ANY OTHER ERRORS?                            
         BE    SWERR                                                            
         CLI   COMPCD,0              TEST SAVED SF CODE BEFORE                  
         BNE   *+10                  YES - BETTER NOT DO IT AGAIN !             
         MVC   COMPCD,0(R1)          SAVE RETURNED AGENCY BINARY CODE           
*                                                                               
         MVC   MYACCKEY,SPACES       READ ACC COMPANY REC                       
         MVC   MYACCKEY(1),COMPCD                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,REC2               
         CLI   8(R1),0                                                          
         BNE   CMPERR                                                           
         LA    R6,REC2                                                          
         AH    R6,=Y(ACCORFST)     FIRST ELEM (IN OLD FILE FORMAT)              
CO80     CLI   0(R6),CPYELQ        X'10' COMPANY ELEM                           
         BE    CO85                                                             
         ZIC   R0,0(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   CO80                                                             
         B     CMPERR                                                           
         USING CPYELD,R6                                                        
CO85     TM    CPYSTAT4,CPYSOFF2   2 CHAR REQ'D                                 
         BO    CO88                YES = VALIDATE OFFICE                        
         CLI   OFFLEN,1            MUST BE ONE                                  
         BNE   ONEERR              NO = NO VALIDATION                           
         B     CO90                OK                                           
         DROP  R6                                                               
*                                                                               
         USING OFFRECD,R6                                                       
CO88     CLI   OFFLEN,2            MUST BE TWO                                  
         BNE   TWOERR                                                           
         LA    R6,MYACCKEY         NEW OFFICE -- LOOK FOR OFFICE REC            
         MVC   MYACCKEY,SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ      X'01'                                      
         MVC   OFFKCPY,COMPCD                                                   
         MVC   OFFKOFF(2),ACCOFF                                                
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,REC2               
         CLI   8(R1),0                                                          
         BNE   OFCERR                                                           
*                                                                               
CO90     DS    0H                  OFFICE CODE IS GOOD                          
         MVC   CACCOFC,ACCOFF      SAVE OFFICE CODE                             
         MVC   CACCAGY,POWCODE     SAVE AGY CODE                                
*                                                                               
COX      DS    0H                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSW       ORIGINAL SYS                                 
         L     RF,VCOMFACS         SWITCH BACK                                  
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,=H'24'     FOR SPOT                                     
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*  READ FOR THIS CLT IN A CLT GROUP RECORD - IF SO, THEN WE CAN'T               
*  DELETE THE CLIENT                                                            
*                                                                               
CK4GRP   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING GRPRECD,R3                                                       
         MVI   GRPKTYP,GRPKTYPQ                                                 
         MVI   GRPKSTYP,GRPKCTYQ                                                
         MVC   GRPKAGMD,CKEYAM                                                  
         GOTO1 HIGH                                                             
         B     CK10                                                             
CK05     GOTO1 SEQ                                                              
*                                                                               
CK10     CLC   KEYSAVE(3),KEY                                                   
         BNE   CKX                                                              
         LA    R3,KEYSAVE                                                       
*                                                                               
         GOTO1 GETREC                                                           
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'30'        VALUE ELEMENT                                
CK15     BAS   RE,NEXTEL                                                        
         BNE   CK05                                                             
         CLC   2(3,R6),SVEBCCLT                                                 
         BNE   CK15                                                             
         LA    R2,CLTNAMEH                                                      
         MVC   NERRCD,=AL2(INAGRP)  CLT IN A GRP->EXIT W/AN ERROR               
         B     LFM2ERR+4           DON'T CHANGE CURSOR PLCMT                    
CKX      B     XIT                CLT ISN'T IN ANY GRP -> RETURN                
         SPACE 2                                                                
*                                                                               
SECERR   NI    LFMKEYH+4,X'FF'-X'20'    TURN OFF PREVIOUSLY VALIDATED           
         MVI   ERRCD,SCRTYERR                                                   
         B     LFMERR                                                           
*                                                                               
SYSERR   MVC   NERRCD,=AL2(ACCNOTOP)                                            
         B     LFM2ERR                                                          
*                                                                               
SWERR    MVC   NERRCD,=AL2(BADACCSW)                                            
         B     LFM2ERR                                                          
*                                                                               
CMPERR   MVC   NERRCD,=AL2(NOACCCMP)                                            
         B     LFM2ERR                                                          
*                                                                               
AGYERR   MVC   NERRCD,=AL2(BADAGYCD)                                            
         B     LFM2ERR                                                          
*                                                                               
OFCERR   MVC   NERRCD,=AL2(BADOFC)                                              
         B     LFM2ERR                                                          
*                                                                               
ONEERR   MVC   NERRCD,=AL2(OFCNOT1)                                             
         B     LFM2ERR                                                          
*                                                                               
TWOERR   MVC   NERRCD,=AL2(OFCNOT2)                                             
         B     LFM2ERR                                                          
*                                                                               
LFM2ERR  LA    R2,CLTAOFCH                                                      
         MVI   ERRCD,NEWERR                                                     
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*                         TABLE OF AGENCIES THAT REQUIRE INTERFACE CDS          
AGYTAB   DC    C'JW'                                                            
         DC    C'AR'                                                            
         DC    C'NH'                                                            
         DC    C'HC'                                                            
         DC    C'CE'                                                            
*                                                                               
AGYTABN  EQU   (*-AGYTAB)/2                                                     
         SPACE 2                                                                
*                TABLE OF PROFILE VALUES                                        
*               LENGTH OF ENTRY,POSITION,VALUES  FF =1-9 OR A-Z                 
PROFTAB  DC    AL1(05),AL1(01),C'012'           BRAND/POL TRNDS                 
         DC    AL1(03),AL1(02),X'FF'            LOCK BOX NUM                    
         DC    AL1(06),AL1(03),C'0123'          MKT/STA TRNDS                   
         DC    AL1(04),AL1(04),C'01'            RATING SERVICE                  
         DC    AL1(09),AL1(05),C'0123459'       BILL FORMULA CNTRL              
         DC    AL1(05),AL1(06),C'012'           BILL ESTIMATE CNTRL             
         DC    AL1(08),AL1(07),C'0123YN'   Y IS ONLY REAL VALUE                 
         DC    AL1(13),AL1(08),C'0123456789*'   PRINT EST SERIES NM             
         DC    AL1(04),AL1(09),C'01'            GOALS CPP OVERRIDE              
         DC    AL1(05),AL1(10),C'012'           PROGRAM ADJ CNTRL               
         DC    AL1(05),AL1(11),C'012'           POL TIMESHEET DEMOS             
         DC    AL1(06),AL1(12),C'01YN'     Y IS ONLY REAL VALUE                 
         DC    AL1(05),AL1(13),C'0YN'           PRD REQ FOR TRUE POL            
         DC    AL1(03),AL1(14),X'FF'            EXCL GROUP CODE                 
         DC    AL1(14),AL1(15),C'0123456789Y*'  CLIENT RATE CNTRL               
*        THE FOLLOWING VALUES GO INTO CEXTRA                                    
         DC    AL1(05),AL1(16),C'0UC'           CANADIAN DEMO OPTION            
         DC    AL1(04),AL1(17),C'01'            CANADIAN NET TAX                
KPROF18  DC    AL1(KPROF18X-KPROF18),AL1(18)    BUY ID REQ                      
         DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ*'                                   
KPROF18X EQU   *                                                                
         DC    AL1(04),AL1(19),C'NY'            EST FILTERS REQ                 
         DC    AL1(04),AL1(20),C'0E'            CAMPAIGNS                       
         DC    AL1(05),AL1(21),C'NYD'           US SPILL                        
         DC    AL1(04),AL1(22),C'NY'            EST=NO EST NAME                 
         DC    AL1(04),AL1(23),C'NY'            MKGDS IN MISSED MTH             
         DC    AL1(04),AL1(24),C'NY'            GOAL REQD FOR BUY               
         DC    AL1(05),AL1(25),C'0CU'           COUNTRY                         
         DC    AL1(04),AL1(26),C'NY'            OUT-OF-WEEK                     
         DC    AL1(06),AL1(27),C'SUXZ'          GST CODE                        
         DC    AL1(04),AL1(28),C'NY'            SPECIAL DEMO ADJ                
         DC    AL1(04),AL1(29),C'NY'            PRD REQ FOR ADDS SEND           
         DC    X'0000'                                                          
         SPACE 2                                                                
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
         EJECT                                                                  
*==============================================================*                
* ADD/DELETE OFFICE PASSIVE POINTERS                           *                
*==============================================================*                
         SPACE 1                                                                
         DS    0D                                                               
OFCPTR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
OFCPTR0  CLC   SVCOFFC,COFFICE     TEST OFFICE CHANGED                          
         BE    OFCPTRX             NO                                           
* DELETE OLD OFFICE (IF ANY)                                                    
         CLI   SVCOFFC,C' '                                                     
         BNH   OFCPTR10                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),SVKEY+1    A/M                                          
         MVC   KEY+9(1),SVCOFFC                                                 
         MVC   KEY+11(2),SVKEY+2   CLT                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR2                                                          
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
OFCPTR2  CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BNE   OFCPTR10                                                         
         CLI   SVEBCMED,C'T'       TV ONLY                                      
         BNE   OFCPTR10                                                         
* NEED TO DO MEDIA N (X'03')                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR4                                                          
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
* NEED TO DO MEDIA C (X'08')                                                    
OFCPTR4  NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR10                                                         
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
* NOW ADD NEW (OR UNDELETE)                                                     
OFCPTR10 CLI   COFFICE,C' '        TEST NEW OFFICE PRESENT                      
         BNH   OFCPTRX                                                          
         MVC   KEY(20),SVKEY                                                    
         BAS   RE,ADDOFC                                                        
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   OFCPTRX                                                          
         CLI   SVEBCMED,C'T'                                                    
         BNE   OFCPTRX                                                          
* DO MEDIA N                                                                    
         MVC   KEY(20),SVKEY                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                READ THE DIRECTORY POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDOFC                                                        
* DO MEDIA C                                                                    
         MVC   KEY(20),SVKEY                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                READ THE DIRECTORY POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDOFC                                                        
*                                                                               
OFCPTRX  NI    DMINBTS,X'F7'                                                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
ADDOFC   NTR1                                                                   
         MVC   WORK(20),KEY        SAVE CLTHDR KEY AND DISK ADDRESS             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),WORK+1     A/M                                          
         MVC   KEY+9(1),COFFICE                                                 
         MVC   KEY+11(2),WORK+2    CLT                                          
         MVC   KEY+14(4),WORK+14   SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ADDOFC10                                                         
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     ADDOFCX                                                          
*                                                                               
ADDOFC10 MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 DIR                                                              
*                                                                               
ADDOFCX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* EDIT CLIENT OPTIONS                                               *           
*===================================================================*           
         SPACE 1                                                                
EDTOP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SCRNFLAG,0          DID EVERYTHING FIT ON DISPLAY?               
         BE    EDTOP01             YES - SO CONTINUE                            
*                                                                               
         MVC   CLTOPTS,SPACES      ELSE - BLANK OUT THE FIELD                   
         FOUT  CLTOPTSH,=C'OPTIONS MUST BE CHANGED ON CL2 SCREEN',37            
         B     EDTOPEQX            AND SKIP THE OPTIONS                         
*                                                                               
EDTOP01  EQU   *                                                                
*                                                                               
         LA    R2,CLTOPTSH         OPTIONS                                      
         MVC   MYBYTE,COPT1                                                     
         MVC   MYBYTE2,COPT2       SAVE THE OLD OPTIONS                         
         XC    MYFULL,MYFULL                                                    
         MVC   MYFULL+1(3),CPWPCT                                               
         XC    CPWPCT,CPWPCT                                                    
         XC    CZENCLT,CZENCLT                                                  
         XC    CCOST2,CCOST2       CLEAR THE COST FACTOR                        
         MVI   COPT1,0                                                          
         MVI   COPT2,0                                                          
*                                                                               
EDTOP02  CLI   5(R2),0                                                          
         BE    EDTOP40                                                          
         MVI   ERRCD,INVERR        INVALID INPUT                                
         GOTO1 VSCANNER,DMCB,(R2),(12,REC2)                                     
         CLI   DMCB+4,0                                                         
         BE    EDTOPNQX                                                         
         LA    R5,REC2                                                          
         USING SCAND,R5                                                         
         ZIC   R0,DMCB+4           N'ENTRIES                                    
*                                                                               
EDTOP10  ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,TSTCOS2                                                       
         BNE   EDTOP11                                                          
* VALIDATE COS2=Y/N                                                             
         BAS   RE,CHKCOS2          VALIDATE THE FIELD                           
         BNZ   EDTOPNQX            ERROR FOUND                                  
         B     EDTOP30             ELSE - CONTINUE                              
* VALIDATE INF=Y                                                                
EDTOP11  EX    R1,TSTINF                                                        
         BNE   EDTOP12                                                          
         CLI   FLD2,C'Y'                                                        
         BNE   EDTOPNQX                                                         
         OI    COPT1,COP1INFQ                                                   
         B     EDTOP30                                                          
* VALIDATE DBL=Y/N                                                              
EDTOP12  EX    R1,TSTDBL                                                        
         BNE   EDTOP13                                                          
         CLI   FLD2,C'Y'                                                        
         BE    EDTOP30                                                          
         CLI   FLD2,C'N'                                                        
         BNE   EDTOPNQX                                                         
         OI    COPT1,COP1DBLQ                                                   
         B     EDTOP30                                                          
* VALIDATE MGR=Y/N TO REQUIRE MGREQ REC IF ID=MKTGRP                            
EDTOP13  EX    R1,TSTMGR                                                        
         BNE   EDTOP14                                                          
         CLI   FLD2,C'N'                                                        
         BE    EDTOP30                                                          
         CLI   FLD2,C'Y'                                                        
         BNE   EDTOPNQX                                                         
         OI    COPT1,COP1MGRQ                                                   
         B     EDTOP30                                                          
* VALIDATE NMG=Y/N                                                              
EDTOP14  EX    R1,TSTNMG                                                        
         BNE   EDTOP15                                                          
         CLI   FLD2,C'Y'                                                        
         BE    EDTOP14A                                                         
         CLI   T219FFD+1,C'*'       IS THIS A DDS TERMINAL                      
         BNE   EDTOPNQX                                                         
         CLI   FLD2,C'N'                                                        
         BNE   EDTOPNQX                                                         
         NI    COPT1,X'FF'-COP1NMG                                              
         B     EDTOP30                                                          
*                                                                               
EDTOP14A OI    COPT1,COP1NMG                                                    
         B     EDTOP30                                                          
* VALIDATE PW=XX.XX                                                             
EDTOP15  CLI   FLD1LEN,2           CAREFUL ABOUT PWB OPTION BELOW !             
         BNE   EDTOP16                                                          
         EX    R1,TSTPW                                                         
         BNE   EDTOP16                                                          
*                                                                               
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=Y(NOPWAUTH)                                              
         TM    SVAGYFL2,AGYFLAG2_PW ONLY IF IN AGENCY RECORD                    
         BZ    LFMERR                                                           
*                                                                               
         ZIC   R4,FLD2LEN                                                       
         GOTO1 VCASHVAL,DMCB,(2,FLD2),(R4)                                      
         MVI   ERRCD,INVERR                                                     
         CLI   DMCB,0                                                           
         BNE   EDTOPNQX                                                         
         L     R3,4(R1)                                                         
         CH    R3,=H'10000'        MAX 100%                                     
         BNL   EDTOPNQX                                                         
         MVC   CPWPCT,DMCB+5                                                    
         OC    CPWPCT,CPWPCT                                                    
         BNZ   *+8                                                              
         OI    CPWPCT,X'80'        SET SPECIAL FLAG FOR PW%=0                   
*                                                                               
         B     EDTOP30                                                          
*                                                                               
* VALIDATE ZENITH CLT ZEN=CC(C), 2-3 ALPHANUMERIC CHARACTERS                    
EDTOP16  DS    0H                                                               
         EX    R1,TSTZEN                                                        
         BNE   EDTOP16X                                                         
         CLI   FLD2LEN,2                                                        
         BL    EDTOPNQX                                                         
         CLI   FLD2LEN,3                                                        
         BH    EDTOPNQX                                                         
         ZIC   R1,FLD2LEN                                                       
         LA    RE,FLD2                                                          
EDTOP16A LA    RF,ALPHANUM                                                      
EDTOP16B CLI   0(RF),0                                                          
         BE    EDTOPNQX                                                         
         CLC   0(1,RE),0(RF)                                                    
         BE    EDTOP16C                                                         
         LA    RF,1(RF)                                                         
         B     EDTOP16B                                                         
EDTOP16C LA    RE,1(RE)                                                         
         BCT   R1,EDTOP16A                                                      
         IC    R1,FLD2LEN                                                       
         BCTR  R1,0                                                             
         EXMVC R1,CZENCLT,FLD2                                                  
         B     EDTOP30                                                          
EDTOP16X DS    0H                                                               
*                                                                               
EDTOP17  EX    R1,TSTCTA                                                        
         BNE   EDTOP17X                                                         
         CLI   FLD2,C'Y'                                                        
         BE    EDTOP17A                                                         
         CLI   T219FFD+1,C'*'       IS THIS A DDS TERMINAL                      
         BNE   EDTOPNQX                                                         
         CLI   FLD2,C'N'                                                        
         BNE   EDTOPNQX                                                         
         NI    COPT1,X'FF'-COP1CTAQ                                             
         B     EDTOP30                                                          
EDTOP17A OI    COPT1,COP1CTAQ                                                   
         B     EDTOP30                                                          
EDTOP17X DS    0H                                                               
*                                                                               
EDTOP18  EX    R1,TSTUPL                                                        
         BNE   EDTOP18D                                                         
         CLI   FLD2,C'Y'                                                        
         BE    EDTOP18A                                                         
         CLI   FLD2,C'N'                                                        
         BNE   EDTOPNQX                                                         
         NI    COPT1,X'FF'-COP1UPLQ                                             
         B     EDTOP30                                                          
EDTOP18A OI    COPT1,COP1UPLQ                                                   
         B     EDTOP30                                                          
*                                                                               
* VALIDATE J1=Y/N TO EXCLUDE CLIENT FROM J1 REPORT                              
EDTOP18D EX    R1,TSTJ1                                                         
         BNE   EDTOP18E                                                         
         CLI   FLD2,C'Y'                                                        
         BE    EDTOP30                                                          
         CLI   FLD2,C'N'                                                        
         BNE   EDTOPNQX                                                         
         OI    COPT2,COP2EXJ1                                                   
         B     EDTOP30                                                          
*                                                                               
* VALIDATE A7=Y/N TO EXCLUDE CLIENT FROM A7 REPORT                              
EDTOP18E EX    R1,TSTA7                                                         
         BNE   EDTOP19                                                          
         CLI   FLD2,C'Y'                                                        
         BE    EDTOP30                                                          
         CLI   FLD2,C'N'                                                        
         BNE   EDTOPNQX                                                         
         OI    COPT2,COP2EXA7                                                   
         B     EDTOP30                                                          
*                                                                               
EDTOP19  EX    R1,TSTGMI                                                        
         BNE   EDTOP20                                                          
         OI    COPT1,COP1GMI       TURN ON GMI BIT (FOREVER)                    
         B     EDTOP30                                                          
*                                                                               
EDTOP20  CLC   =C'PWB',FLD1                                                     
         BNE   EDTOP21                                                          
         CLI   FLD1LEN,3                                                        
         BNE   EDTOP21                                                          
         CLI   FLD2,C'Y'                                                        
         BE    EDTOP30                                                          
         CLI   FLD2,C'N'                                                        
         BNE   EDTOPNQX                                                         
         OI    COPT2,COP2NPWB                                                   
         B     EDTOP30                                                          
*                                                                               
EDTOP21  DS    0H                                                               
*                                                                               
         EX    R1,TSTTRADE         TRADE CLIENT?                                
         BNE   EDTOP22             NO - SO CONTINUE                             
         OI    COPT2,COP2TRAD      ELSE - SET THE BIT                           
         B     EDTOP30             AND GET NEXT ENTRY                           
*                                                                               
EDTOP22  EX    R1,TSTFRZ           FREEZE THE CLIENT?                           
         BNE   EDTOP23             NO - SO CONTINUE                             
         OI    COPT2,COP2FRZ       ELSE - SET THE BIT                           
         B     EDTOP30                                                          
*                                                                               
EDTOP23  EQU   *                                                                
*                                                                               
         EX    R1,TSTXEST          CROSS ESTIMATE REPORTING?                    
         BNE   EDTOP24             NO - SO CONTINUE                             
*                                                                               
         OC    CPWPCT,CPWPCT       PROFIT W/IN ALSO?                            
         BZ    EDTOPNQX            NO - SO ERROR                                
         OI    COPT2,COP2XEST      ELSE - SET THE BIT                           
         B     EDTOP30             AND GET NEXT ENTRY                           
*                                                                               
EDTOP24  CLI   SVAPROF+7,C'C'     IF CANADIAN AGENCY                            
         BE    EDTOP25                                                          
         EX    R1,TSTBP            BUY PROGRAM PROFILE ON                       
         BNE   EDTOP25             NOPERS                                       
         CLI   FLD2,C'N'                                                        
         BE    EDTOP30                                                          
         CLI   FLD2,C'Y'                                                        
         BNE   EDTOPNQX                                                         
         OI    COPT2,COP2BP                                                     
         B     EDTOP30                                                          
*                                                                               
EDTOP25  EQU   *                                                                
*                                                                               
         B     EDTOPNQX                                                         
*                                                                               
EDTOP30  LA    R5,32(R5)           NEXT ENTRY                                   
         BCT   R0,EDTOP10                                                       
*                                                                               
EDTOP40  EQU   *                                                                
*                                                                               
* IF AGENCY IS TRADE, THEN THE CLIENT MUST BE ALSO.                             
*                                                                               
         TM    SVAGYFL1,AGYTRDQ    TRADE AGENCY?                                
         BZ    EDTOP50             NO - SO CONTINUE                             
         OI    COPT2,COP2TRAD      ELSE - SET THE BIT                           
*                                                                               
EDTOP50  EQU   *                                                                
         TM    MYBYTE2,COP2TRAD    IF CLIENT WAS TRADE                          
         BNO   EDTOP52                                                          
         TM    COPT2,COP2TRAD      CANNOT CHANGE                                
         BO    EDTOP52                                                          
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=Y(NOCHGTRD)                                              
         B     EDTOPNQX                                                         
*                                                                               
EDTOP52  CLC   =C'WI',AGYALPHA                                                  
         BNE   EDTOP54                                                          
         MVC   HALF(1),MYBYTE2     MOVE OLD COPT2                               
         NI    HALF,COP2FRZ        DROP ALL BUT FRZ                             
         MVC   HALF+1(1),COPT2     MOVE NEW COPT2                               
         NI    HALF+1,COP2FRZ      DROP ALL BUT FRZ                             
         CLC   HALF(1),HALF+1      OLD NEW VALUES THE SAME                      
         BE    EDTOP54                                                          
         TM    T219FFD+12,X'04'    TEST AUTHORIZED TO CHANGE                    
         BO    EDTOP54                                                          
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=Y(NOCHGFRZ)                                              
         B     EDTOPNQX                                                         
*                                                                               
EDTOP54  EQU   *                                                                
         TM    MYBYTE,COP1INFQ     IF CLIENT WAS INFOMERCIAL                    
         BNO   EDTOP60                                                          
         TM    COPT1,COP1INFQ      CANNOT CHANGE                                
         BO    EDTOP60                                                          
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=Y(NOCHGINF)                                              
         B     EDTOPNQX                                                         
*                                                                               
EDTOP60  TM    MYBYTE,COP1GMI      IF CLIENT WAS GMI                            
         BNO   EDTOPEQX                                                         
         TM    COPT1,COP1GMI       MUST STAY GMI                                
         BO    EDTOPEQX                                                         
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=Y(NOCHGGMI)                                              
         B     EDTOPNQX                                                         
*                                                                               
EDTOPNQX LA    R2,CLTOPTSH                                                      
         LTR   RB,RB                                                            
         B     *+6                                                              
*                                                                               
EDTOPEQX CR    RB,RB               SET CC EQ                                    
         XIT1  REGS=(R2)                                                        
*                                                                               
TSTJ1    CLC   FLD1(0),=C'J1'                                                   
TSTA7    CLC   FLD1(0),=C'A7'                                                   
TSTCOS2  CLC   FLD1(0),=C'COS2'                                                 
TSTINF   CLC   FLD1(0),=C'INF'                                                  
TSTDBL   CLC   FLD1(0),=C'DBL'                                                  
TSTMGR   CLC   FLD1(0),=C'MGR'                                                  
TSTNMG   CLC   FLD1(0),=C'NMG'                                                  
TSTPW    CLC   FLD1(0),=C'PW'                                                   
TSTZEN   CLC   FLD1(0),=C'ZEN'                                                  
TSTCTA   CLC   FLD1(0),=C'CTA'                                                  
TSTUPL   CLC   FLD1(0),=C'UPL'                                                  
TSTGMI   CLC   FLD1(0),=C'GMI'                                                  
TSTTRADE CLC   FLD1(0),=C'TRD'                                                  
TSTFRZ   CLC   FLD1(0),=C'FRZ'                                                  
TSTXEST  CLC   FLD1(0),=C'XEST'                                                 
TSTBP    CLC   FLD1(0),=C'BP'                                                   
         EJECT                                                                  
*                                                                               
* THIS ROUTINE VLAIDATES THE DIFFERENT TYPES OF INPUT ALLOWED FOR               
* THE 'COS2' OPTION.                                                            
*                                                                               
CHKCOS2  NTR1                                                                   
*                                                                               
         TM    SVAGYFL1,AGYCOS2Q   COST FACTOR REQUIRED?                        
         BO    CKC20100            YES - SO CONTINUE                            
*                                                                               
         CLI   FLD2,C'N'           ELSE - CHECK FOR Y/N INPUT                   
         BE    CKC2DONE            'N'O - SO DONE                               
         CLI   FLD2,C'Y'           ELSE - 'Y'ES?                                
         BNE   CKC2ERR             NO - SO ERROR                                
         OI    COPT1,COP1COSQ      ELSE - SET THE BIT                           
         B     CKC2DONE            AND CONTINUE                                 
*                                                                               
CKC20100 EQU   *                                                                
*                                                                               
         ZIC   R4,FLD2LEN                                                       
         GOTO1 VCASHVAL,DMCB,(6,FLD2),(R4)                                      
         CLI   DMCB,0                                                           
         BNE   CKC2ERR                                                          
         L     R3,4(R1)                                                         
         C     R3,=F'9999999'      MAX 9.999999                                 
         BH    CKC2ERR                                                          
*                                                                               
         LTR   R3,R3               .LT. 0?                                      
         BL    CKC2ERR             YES - SO ERROR                               
*                                                                               
         MVC   CCOST2,DMCB+4       ELSE - MOVE IN THE COST FACTOR               
         OC    CCOST2,CCOST2       ZERO?                                        
         BNZ   CKC2DONE            NO - SO CONTINUE                             
*                                                                               
         OI    CCOST2,X'80'        ELSE - SET 'ZERO WAS INPUT' BIT              
*                                                                               
CKC2DONE CR    RB,RB               SET GOOD CC                                  
         B     *+6                                                              
CKC2ERR  LTR   RB,RB               SET ERROR CC                                 
*                                                                               
         XIT1                      RETURN                                       
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMF1                                                                        
       ++INCLUDE SPLFMF1D                                                       
         SPACE 2                                                                
         ORG   SVAPPL                                                           
MYBYTE   DS    CL1                                                              
MYBYTE2  DS    X                                                                
COMPCD   DS    CL1                                                              
MYACCKEY DS    CL42                                                             
CTKEY    DS    CL28                                                             
SENUM    DS    XL1                                                              
SYSSW    DS    XL1                                                              
SVCOFFC  DS    C                   OLD OFFICE CODE                              
MYFULL   DS    F                                                                
DATADISP DS    H                                                                
POWCODE  DS    CL2                                                              
ACCOFF   DS    CL2                                                              
OFFLEN   DS    CL1                                                              
GTFACTB  DS    CL88                                                             
PSTOUT   DS    CL64       64 BYTE OUTPUT AREA                                   
SCRNFLAG DS    X                   'DID ALL OPTIONS FIT ON SCREEN' FLAG         
         EJECT                                                                  
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
*                                                                               
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082SPLFM11   05/01/02'                                      
         END                                                                    
