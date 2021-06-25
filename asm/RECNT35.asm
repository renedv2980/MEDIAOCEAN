*          DATA SET RECNT35    AT LEVEL 007 AS OF 09/15/95                      
*PHASE T80235A                                                                  
         TITLE 'T80235 - UNAPPLY MAKE-GOODS'                                    
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT35 (T80235) --- BUY TO MAKE-GOOD BACK OUT              *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 14FEB95 SKU CONCEPTION, HAPPY VD                                *             
*                                                                 *             
*                   ***  END TOMBSTONE  ***                       *             
*******************************************************************             
*                                                                               
T80235   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80235                                                         
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         L     R2,4(R1)            POINT TO CURRENT SELECT FIELD                
                                                                                
         BAS   RE,CHECKMG          CHECK IF ACTION VALID                        
         BNZ   ERROR                                                            
                                                                                
         L     R2,=A(MGTWA)        DESTINATION                                  
         CNOP  0,4                 ESTABLISH ADDRESSIBILITY                     
         B     *+8                                                              
         DC    A(*)                                                             
         LA    RE,*-4                                                           
         S     RE,*-8                                                           
         AR    R2,RE                                                            
         LR    R6,R2                                                            
                                                                                
         LH    R3,=H'18432'        TO-LENGTH                                    
         LA    R4,0(RA)            SOURCE                                       
         LH    R5,=H'18432'        FROM-LENGTH                                  
         MVCL  R2,R4               MAKE A COPY OF THE TWA                       
                                                                                
         LR    R9,RA               SAVE OFF ADDRESS OF TWA                      
         LR    RA,R6                                                            
         USING TWAD,RA                                                          
                                                                                
         LA    RF,BUYLAST          CLEAR 'FAKE' BUY SCREEN                      
         LA    RE,BUYDAYSH                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
                                                                                
         XC    MGHDRDA,MGHDRDA     CLEAR MG HEADER D/A                          
         XC    KEY,KEY             CLEAR THE KEY                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAMKGD2  SET TO GET SELECTED RECORD                   
         DROP  RF                                                               
                                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),IOAREA                                     
         GOTO1 VHIGH                                                            
         B     MGBAK04                                                          
                                                                                
MGBAK03  DS    0H                                                               
         GOTO1 VSEQ                                                             
                                                                                
MGBAK04  DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   MGBAK60             ALL DONE                                     
         OC    RMKGKPLN(6),RMKGKPLN SKIP COMMENT RECORDS                        
         BNZ   MGBAK05                                                          
         MVC   MGHDRDA,KEY+28      SAVE OFF MG HEADER D/A                       
         B     MGBAK03                                                          
         DROP  R6                                                               
                                                                                
MGBAK05  DS    0H                                                               
         OC    MGHDRDA,MGHDRDA                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   TWAMKGD2,KEY+28     SAVE OFF DISK ADDRESS                        
         DROP  RF                                                               
                                                                                
         MVC   MKGKEY,KEY          SAVE OFF SO WE CAN RESTORE LATER             
                                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R6,IOAREA           GET TARGET MAKE-GOOD BUY                     
         USING RMKGKEY,R6                                                       
MGD      USING RBUYKEY,KEY                                                      
         XC    MGD.RBUYKEY,MGD.RBUYKEY                                          
         MVI   MGD.RBUYKTYP,X'0B'                                               
         MVC   MGD.RBUYKREP,RMKGKREP                                            
         MVC   MGD.RBUYKCON,RMKGKCON                                            
         MVC   MGD.RBUYKPLN,RMKGKPLN                                            
         MVC   MGD.RBUYKMLN,RMKGKMLN                                            
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'20'        GET STATUS CONTROL ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RMKGSTEL,R6                                                      
         CLI   RMKGSTST,C'A'       WAS THIS APPLIED?                            
         BNE   MGBAK03                                                          
         MVC   MGD.RBUYKLIN,RMKGSTL#   LINE NUMBER GENERATED FOR M/G            
*                                                                               
* SPECIFIC BUY NUMBER TO 'CHANGE'                                               
*                                                                               
         MVC   BUYACT(3),=C'CHA'                                                
         EDIT  RMKGSTL#,(3,CONBNUM),ALIGN=LEFT                                  
         STC   R0,CONBNUMH+5       SET LENGTH                                   
         OI    CONBNUMH+4,X'08'    SET NUMERIC                                  
         DROP  MGD                                                              
                                                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 VGETREC,DMCB,RBUYREC                                             
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   TWABADDR,KEY+28     DA OF CURRENT BUY                            
         DROP  RF                                                               
*                                  DISPLAY BUY                                  
         BAS   RE,SETFDHDL         SET OVERALL FIELD LENGTHS                    
         GOTO1 VLOAD,DMCB,(X'25',0),0                                           
         BAS   RE,SETFLDLN         SET ACTUAL DATA FIELD LENGTHS                
                                                                                
MGBAK10  DS    0H                  REMOVE MAKEGOOD REFERENCE                    
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OI    TWAMKGFG+1,X'80'    SKIP ORD COM VALIDATION                      
         DROP  RF                                                               
                                                                                
         GOTO1 VLOAD,DMCB,(X'15',0),0                                           
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         NI    TWAMKGFG+1,X'FF'-X'80'                                           
         DROP  RF                                                               
                                                                                
MGBAK20  DS    0H                  UPDATE BUCKETS                               
         GOTO1 VLOAD,DMCB,(X'30',0),0                                           
                                                                                
         BAS   RE,UPDTTGBY         UPDATE TARGET BUY                            
*                                                                               
* DELETE MAKE-GOOD                                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RMKGSTEL,R6                                                      
         EDIT  RMKGSTL#,(3,CONBNUM),ALIGN=LEFT                                  
         DROP  R6                                                               
                                                                                
         MVC   BUYACT(3),=C'DEL'                                                
         STC   R0,CONBNUMH+5       SET LENGTH                                   
         OI    CONBNUMH+4,X'08'    SET NUMERIC                                  
                                                                                
         BAS   RE,SETFDHDL         SET OVERALL FIELD LENGTHS                    
         GOTO1 VLOAD,DMCB,(X'25',0),0                                           
         BAS   RE,SETFLDLN         SET ACTUAL DATA FIELD LENGTHS                
                                                                                
MGBAK40  DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OI    TWAMKGFG+1,X'80'    SKIP ORD COM VALIDATION                      
         DROP  RF                                                               
                                                                                
         GOTO1 VLOAD,DMCB,(X'15',0),0                                           
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         NI    TWAMKGFG+1,X'FF'-X'80'                                           
         DROP  RF                                                               
                                                                                
MGBAK50  DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'30',0),0                                           
                                                                                
         MVC   KEY,MKGKEY                                                       
         GOTO1 VHIGH                                                            
         B     MGBAK03                                                          
                                                                                
MGBAK60  DS    0H                                                               
         OC    MGHDRDA,MGHDRDA                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),MGHDRDA   GET MKG HEADER REC                           
         DROP  RF                                                               
                                                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING RMKGSDEM,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    RMKGSCST,RMKGSBOQ   MARK BACKED OUT, CLEAR ALL ELSE              
         NI    RMKGSCST,X'FF'-RMKGSAPQ-RMKGSRCQ-RMKGSRJQ-RMKGSRVQ               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         DROP  R6                                                               
                                                                                
MGBAKX   DS    0H                                                               
         LR    RA,R9               RESTORE TWA                                  
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK IF ANY 'OR' MAKEGOODS OFFERS IN A GROUP. IF FOUND, MAKE SURE            
* AT LEAST ONE RECORD WITHIN THE OFFER HAS BEEN SELECTED                        
***********************************************************************         
CHECKMG  NTR1                                                                   
         XC    KEY,KEY                                                          
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAMKGD2  SET TO GET SELECTED RECORD                   
         DROP  RF                                                               
                                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),IOAREA                                     
         GOTO1 VHIGH               GET HEADER REC                               
*                                                                               
* CHECK OFFER STATUS IN COMMENT REC                                             
*                                                                               
         OC    RMKGKPLN(6),RMKGKPLN                                             
         BZ    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6          VALID ACTION IS BACKOUT                      
         CLI   8(R2),C'B'          BACKOUT                                      
         BE    *+6                                                              
         DC    H'0'                HOW DID WE GET HERE?                         
*                                                                               
CHKMG05  DS    0H                  FOR ACTION BACKOUT CHECK IF:                 
         LA    R3,483                                                           
         TM    RMKGSCST,RMKGSAPQ   NOT APPROVED                                 
         BNO   CHKMGNO                                                          
         LA    R3,484                                                           
         TM    RMKGSCST,RMKGSBOQ   ALREADY BACKOUT                              
         BO    CHKMGNO                                                          
         DROP  R6                                                               
*                                                                               
CHKMGYES SR    R1,R1                                                            
         B     *+8                                                              
CHKMGNO  LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
         XIT1  REGS=(R3)           SAVE POINTER TO ERROR MESSAGE                
         EJECT                                                                  
***********************************************************************         
* SET OVERALL FIELD HEADER LENGTHS                                              
***********************************************************************         
SETFDHDL NTR1                                                                   
         LA    R2,FDHDTBL                                                       
                                                                                
SETFDH10 DS    0H                                                               
         ZICM  RF,0(R2),2                                                       
         AR    RF,RA                                                            
         MVC   0(1,RF),2(R2)                                                    
         OI    1(RF),X'02'         SET EXTENDED FIELD HEADER                    
         LA    R2,L'FDHDTBL(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   SETFDH10                                                         
                                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SET ACTUAL LENGTH OF DATA IN FIELDS                                           
* SET ATTRIBUTES                                                                
***********************************************************************         
SETFLDLN NTR1                                                                   
         LA    R2,FDHDTBL                                                       
                                                                                
SETFLD10 DS    0H                                                               
         ZICM  RF,0(R2),2                                                       
         LA    R1,8(RF,RA)         POINT TO FIELD                               
         ZIC   R0,3(R2)            LENGTH OF FIELD                              
                                                                                
SETFLD20 DS    0H                                                               
         CLI   0(R1),C' '          SPACE IS SENTINEL                            
         BE    SETFLD30                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SETFLD20                                                      
                                                                                
SETFLD30 DS    0H                                                               
         ZIC   RE,3(R2)            CALCULATE ACTUAL LENGTH OF DATA              
         SR    RE,R0                                                            
         LA    R1,0(RF,RA)         POINT TO FIELD HEADER                        
         STC   RE,5(R1)            INSERT LENGTH, IF ANY                        
         CLI   4(R2),0                                                          
         BE    SETFLD40                                                         
         TM    4(R2),X'08'                                                      
         BZ    SETFLD40                                                         
         OI    4(R1),X'08'         SET NUMERIC                                  
                                                                                
SETFLD40 DS    0H                                                               
         LA    R2,L'FDHDTBL(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   SETFLD10                                                         
                                                                                
         CLC   =C'CHA',BUYACT      FOR ACTION CHANGE                            
         BNE   SETFLDX                                                          
         CLC   =C'MG=',BUYCOM1     DELETE MAKEGOOD REFERENCE                    
         BNE   SETFLD50                                                         
         XC    BUYCOM1,BUYCOM1                                                  
         MVI   BUYCOM1H+5,0                                                     
         NI    BUYCOM1H+4,X'FF'-X'20'   SET REVALIDATE                          
                                                                                
SETFLD50 DS    0H                                                               
         CLC   =C'MG=',BUYCOM2                                                  
         BNE   SETFLDX                                                          
         XC    BUYCOM2,BUYCOM2                                                  
         MVI   BUYCOM2H+5,0                                                     
         NI    BUYCOM2H+4,X'FF'-X'20'   SET REVALIDATE                          
                                                                                
SETFLDX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CHANGE CORRESPONDING X'76' ELEMENT TO X'66' IN TARGET BUYLINE                 
* ALSO UPDATES X'20' STATUS ELEMENT IN THE MAKE-GOOD RECORD                     
* NOTE:                                                                         
* ASSUMES UPDATED MISSED BUY RECORD IN RBUYREC+3000 AS SPECIFIED IN             
* RECNT30                                                                       
***********************************************************************         
UPDTTGBY NTR1                                                                   
         LA    R6,RBUYREC                                                       
         LA    R6,3000(R6)                                                      
         LR    R5,R6                                                            
         MVC   KEY(L'RBUYKEY),0(R6)                                             
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,(R6)   RE-READ UPDATED MISSED RECORD                
                                                                                
         USING RBMGMSEL,R6                                                      
         MVI   ELCODE,X'76'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPDTBY10 DS    0H                  SAVE OFF A COPY OF X'76'                     
         XC    WORK,WORK                                                        
         ZIC   R1,RBMGMSLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),RBMGMSEL                                                 
         MVI   WORK,X'66'          AND CHANGE IT TO A X'66'                     
         DROP  R6                                                               
*                                                                               
* REMOVE X'76' AND ADD IT BACK AS X'66'                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',(R5)),(R6),0,0                                 
         GOTO1 VADDELEM,DMCB,(R5),WORK                                          
         GOTO1 VPUTREC,DMCB,(R5)                                                
*                                                                               
* UPDATE STATUS ELEMENT IN MAKE-GOOD RECORD                                     
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),TWAMKGD2  SET TO GET SELECTED MG REC                   
         DROP  RF                                                               
                                                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R6,IOAREA                                                        
         USING RMKGSTEL,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RMKGSTST,C'B'       BUY WAS BACKED OUT                           
                                                                                
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         DROP  R6                                                               
                                                                                
UPDTBYX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* AL2 = OFFSET OF FIELD HEADER FROM TWAD                                        
* AL1 = LENGTH OF FIELD HEADER + FIELD + EXTENSION                              
* AL1 = LENGTH OF FIELD                                                         
* AL1 = ATTRIBUTES: X'08' MARK NUMERIC                                          
FDHDTBL  DS    0XL5                                                             
         DC    AL2(BUYDAYSH-TWAD),AL1(BUYTIMSH-BUYDAYSH),AL1(L'BUYDAYS)         
         DC    X'00'                                                            
         DC    AL2(BUYTIMSH-TWAD),AL1(BUYLENH-BUYTIMSH),AL1(L'BUYTIMS)          
         DC    X'00'                                                            
         DC    AL2(BUYLENH-TWAD),AL1(BUYDTESH-BUYLENH),AL1(L'BUYLEN)            
         DC    X'08'                                                            
         DC    AL2(BUYDTESH-TWAD),AL1(BUYDTE2H-BUYDTESH),AL1(L'BUYDTES)         
         DC    X'00'                                                            
         DC    AL2(BUYDTE2H-TWAD),AL1(BUYTYPH-BUYDTE2H),AL1(L'BUYDTE2)          
         DC    X'00'                                                            
         DC    AL2(BUYTYPH-TWAD),AL1(BUYDPTH-BUYTYPH),AL1(L'BUYTYP)             
         DC    X'00'                                                            
         DC    AL2(BUYDPTH-TWAD),AL1(BUYCLSH-BUYDPTH),AL1(L'BUYDPT)             
         DC    X'00'                                                            
         DC    AL2(BUYCLSH-TWAD),AL1(BUYSECH-BUYCLSH),AL1(L'BUYCLS)             
         DC    X'00'                                                            
         DC    AL2(BUYSECH-TWAD),AL1(BUYPLNH-BUYSECH),AL1(L'BUYSEC)             
         DC    X'00'                                                            
         DC    AL2(BUYPLNH-TWAD),AL1(BUYNPWH-BUYPLNH),AL1(L'BUYPLN)             
         DC    X'00'                                                            
         DC    AL2(BUYNPWH-TWAD),AL1(BUYRATEH-BUYNPWH),AL1(L'BUYNPW)            
         DC    X'08'                                                            
         DC    AL2(BUYRATEH-TWAD),AL1(BUYCOM1H-BUYRATEH),AL1(L'BUYRATE)         
         DC    X'08'                                                            
         DC    AL2(BUYCOM1H-TWAD),AL1(BUYCOM2H-BUYCOM1H),AL1(L'BUYCOM1)         
         DC    X'00'                                                            
         DC    AL2(BUYCOM2H-TWAD),AL1(BUYORDCH-BUYCOM2H),AL1(L'BUYCOM2)         
         DC    X'00'                                                            
         DC    AL2(BUYORDCH-TWAD),AL1(BUYORD2H-BUYORDCH),AL1(L'BUYORDC)         
         DC    X'00'                                                            
         DC    AL2(BUYORD2H-TWAD),AL1(BUYLAST-BUYORD2H),AL1(L'BUYORD2)          
         DC    X'00'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* LOCAL VARIABLES                                                               
MKGKEY   DS    CL27                                                             
MSBUYDA  DS    A                   MISSED BUY DISK ADDRESS                      
MGHDRDA  DS    F                   MAKEGOOD HEADER REC DISK ADDRESS             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWRK                                                       
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFED                                                       
         CSECT                                                                  
MGTWA    DS    CL18432             18K OF MY OWN TWA                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007RECNT35   09/15/95'                                      
         END                                                                    
