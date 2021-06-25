*          DATA SET REREP2402Y AT LEVEL 121 AS OF 05/01/02                      
*PHASE RE2402Y,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
         TITLE 'MODULE TO CONVERT GAVAIL TO NEW RDETAIL'                        
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
*******************************************************************             
*                                                                               
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         L     RF,=V(HELLO)                                                     
         L     RE,RELO                                                          
         AR    RF,RE                                                            
         ST    RF,VHELLO                                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    HDRKEY,HDRKEY                                                    
         XC    COUNT,COUNT                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   KEY,X'12'                                                        
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         LA    R6,KEY                                                           
         CLI   KEY,X'12'                                                        
         BNE   PCX                                                              
*                                                                               
         CLI   RINVKSRC,C'M'       MARKET FACT?                                 
         BE    PCSEQ                                                            
         CLI   RINVKSRC,C'S'       STATION FACT?                                
         BE    PCSEQ                                                            
*                                                                               
         CLI   RINVKSRC,0          INVENTORY HEADER?                            
         BNE   PCSEQ                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,HDRIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   CUREQU,X'01'        INITIALIZE TO EQU # 1                        
*                                                                               
PC30     LA    R6,HDRIO                                                         
         XCEF  AVTAB,500                                                        
*                                                                               
         MVI   ELCODE,X'06'        GET AVAIL RATE ELEMENT IN HEADER             
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(5),HDRIO+12      PRINT HEADER AND X'06' ELEMENTS               
         MVC   P+6(4),HDRIO+17                                                  
         GOTO1 HEXOUT,DMCB,HDRIO+21,P+16,3,=C'TOG'                              
         GOTO1 REPORT                                                           
*                                                                               
PC40     CLI   1(R6),X'1B'         OLD STYLE X'06' ELEM?                        
         BNE   PCSEQ               NO - GET NEXT RECORD                         
*                                                                               
         MVC   P(6),2(R6)                                                       
         GOTO1 HEXOUT,DMCB,8(R6),P+6,2,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,10(R6),P+16,17,=C'TOG'                               
         GOTO1 REPORT                                                           
*                                                                               
*******************************************************************             
*     BUILD TABLE OF REP CODE/AVAIL CODE/AVAIL LENGTH (AVTAB)                   
*******************************************************************             
         LA    R2,AVTAB                                                         
         USING AVTABD,R2                                                        
*                                                                               
PC50     OC    0(AVTABLQ,R2),0(R2)     ANY AVAIL ENTRY HERE?                    
         BZ    PC70                                                             
*                                                                               
         CLC   AVREP,2(R6)         SAME REP?                                    
         BNE   PC60                                                             
*                                                                               
         MVC   SVCDE(4),4(R6)                                                   
         OC    SVCDE,=8C' '                                                     
         CLC   AVCDE,SVCDE         SAME AVAIL CODE?                             
         BNE   PC60                                                             
*                                                                               
         CLC   AVLNTH,8(R6)        SAME LENGTH?                                 
         BE    PC100               YES - ALREADY IN TABLE                       
*                                                                               
PC60     LA    R2,AVTABLQ(R2)      CHECK NEXT ENTRY                             
         B     PC50                                                             
*                                                                               
PC70     DS    0H                                                               
         MVC   AVREP,2(R6)         MOVE REP INTO TABLE                          
         MVC   AVCDE(4),4(R6)      MOVE AVAIL CODE INTO TABLE                   
         OC    AVCDE,=8C' '                                                     
*                                                                               
         MVC   AVLNTH,8(R6)        MOVE LENGTH INTO TABLE                       
         MVC   AVEQU,CUREQU        CURRENT EQUATE NUMBER                        
*                                                                               
         ZIC   RF,CUREQU                                                        
         LA    RF,1(RF)                                                         
         STC   RF,CUREQU           INCREMENT EQUATE NUMBER                      
*                                                                               
PC100    BAS   RE,NEXTEL                                                        
         BE    PC40                                                             
*                                                                               
*******************************************************************             
*     GET EQUATE NUMBER TO BUILD NEW RATE RECORD ('Z')                          
*******************************************************************             
PC110    DS    0H                                                               
         LA    R6,HDRIO                                                         
         MVC   HDRKEY,KEY          SAVE AWAY KEY                                
*                                                                               
         MVI   ELCODE,X'06'        GET AVAIL RATE ELEMENT IN HEADER             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               SHOULD BE THERE                              
*                                                                               
PC120    DS    0H                                                               
         LA    R2,AVTAB                                                         
         XC    CUREQU,CUREQU                                                    
*                                                                               
PC130    OC    0(AVTABLQ,R2),0(R2)    ANY AVAIL ENTRY HERE?                     
         BNZ   *+6                                                              
         DC    H'00'                  SHOULD HAVE FOUND A MATCH!!!              
*                                                                               
         CLC   AVREP,2(R6)         SAME REP?                                    
         BNE   PC135                                                            
*                                                                               
         MVC   SVCDE(4),4(R6)                                                   
         OC    SVCDE,=8C' '                                                     
         CLC   AVCDE,SVCDE         SAME AVAIL CODE?                             
         BNE   PC135                                                            
*                                                                               
         CLC   AVLNTH,8(R6)        SAME LENGTH?                                 
         BE    PC140               YES - FOUND MATCH!!!!!                       
*                                                                               
PC135    LA    R2,AVTABLQ(R2)      CHECK NEXT ENTRY                             
         B     PC130                                                            
*                                                                               
PC140    DS    0H                                                               
         MVC   CUREQU,AVEQU        SAVE AWAY EQUATE NUMBER FOR 'Z' REC          
         MVC   CURYEAR,10(R6)      SAVE AWAY YEAR WE ARE PROCESSING             
         MVC   SVCOST1,11(R6)      SAVE AWAY COST QTR 1                         
         MVC   SVCOST2,15(R6)      SAVE AWAY COST QTR 2                         
         MVC   SVCOST3,19(R6)      SAVE AWAY COST QTR 3                         
         MVC   SVCOST4,23(R6)      SAVE AWAY COST QTR 4                         
         DROP  R6                                                               
*                                                                               
*******************************************************************             
*     BUILD NEW 'Z' RATE RECORD IN RATEIO                                       
*******************************************************************             
PC200    DS    0H                                                               
         XCEF  RATEIO,2000                                                      
         LA    R3,RATEIO                                                        
         USING REINVREC,R3                                                      
*                                                                               
         MVC   RINVKEY(24),HDRKEY                                               
         MVI   RINVKSRC,C'Z'       NEW 'Z' RATE RECORD                          
         MVC   RINVKNUM,CUREQU     EQUATE NUMBER                                
         MVC   RINVKYR,CURYEAR     YEAR                                         
         MVC   RINVLEN,=H'34'      INITIALIZE RECORD LENGTH                     
*                                                                               
         LA    R4,QTRTAB                                                        
         LA    R5,BRDST1                                                        
         LA    R7,4                                                             
*                                                                               
PC210    DS    0H                                                               
         XC    WORK,WORK                                                        
         XC    TEMPDATE,TEMPDATE                                                
         XC    TEMPDAT2,TEMPDAT2                                                
*                                                                               
         MVC   WORK(1),CURYEAR                                                  
         MVC   WORK+1(2),1(R4)     START OF QUARTER IN BINARY                   
         MVC   WORK+3(1),CURYEAR                                                
         MVC   WORK+4(2),3(R4)     END OF QUARTER IN BINARY                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TEMPDATE)                                
         GOTO1 GETBROAD,DMCB,(1,TEMPDATE),TEMPDAT2,GETDAY,ADDAY                 
         GOTO1 DATCON,DMCB,(0,TEMPDAT2),(19,0(R5))                              
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TEMPDATE)                              
         GOTO1 GETBROAD,DMCB,(1,TEMPDATE),TEMPDAT2,GETDAY,ADDAY                 
         GOTO1 DATCON,DMCB,(0,TEMPDAT2+6),(19,3(R5))                            
*                                                                               
         LA    R5,6(R5)            BUMP TO NEXT QUARTER                         
         LA    R4,5(R4)                                                         
         BCT   R7,PC210                                                         
         MVI   0(R5),X'FF'         END OF TABLE                                 
*                                                                               
*******************************************************************             
*     ADD X'03' AND X'EF' ELEMENTS TO RATE RECORD                               
*******************************************************************             
         LA    R4,QTRTAB                                                        
         LA    R5,BRDST1                                                        
         LA    R8,SVCOST1                                                       
*                                                                               
         MVC   CURWEEK,0(R5)       INITIALIZE TO 1ST BRD. WEEK                  
         MVC   CURCOST,0(R8)       INITIALIZE TO 1ST QTR COST                   
*                                                                               
PC230    DS    0H                                                               
         LA    R7,ELEM                                                          
         USING RIAVL,R7                                                         
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   RIAVCODE,X'03'                                                   
         MVI   RIAVLEN,RIAVLENQ                                                 
*                                                                               
         CLC   CURWEEK,3(R5)       NEXT QUARTER?                                
         BNH   PC240               NO                                           
*                                                                               
         LA    R5,6(R5)                                                         
         CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    PC250               YES                                          
*                                                                               
         LA    R4,5(R4)            BUMP UP TO NEXT QUARTER                      
         LA    R8,L'SVCOST1(R8)    AND NEXT QTR COST                            
         MVC   CURCOST,0(R8)                                                    
*                                                                               
PC240    DS    0H                                                               
         MVC   RIAVQTR,0(R4)       QUARTER                                      
         MVC   RIAVWEEK,CURWEEK    WEEK DATE (JULIAN)                           
         MVC   RIAVAMT,CURCOST     AMOUNT                                       
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',REPFILE),(R3),(R7),=C'ADD=CODE'                
*                                                                               
         GOTO1 DATCON,DMCB,(8,CURWEEK),(0,TEMPDATE)                             
         LA    R7,7                                                             
         GOTO1 ADDAY,DMCB,TEMPDATE,TEMPDAT2,(R7)                                
         GOTO1 DATCON,DMCB,(0,TEMPDAT2),(19,CURWEEK)                            
         B     PC230                                                            
         DROP  R7                                                               
*                                                                               
PC250    DS    0H                  ADD ACTIVITY ELEMENT                         
         LA    R7,ELEM                                                          
         USING RINVAEL,R7                                                       
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   RINVACOD,X'EF'      ELEMENT CODE                                 
         MVI   RINVALEN,RINVALNQ   ELEMENT LENGTH                               
         GOTO1 DATCON,DMCB,(5,0),(3,RINVAFST)                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',REPFILE),(R3),(R7),=C'ADD=CODE'                
*                                                                               
         MVC   P(5),RATEIO+12      PRINT KEY OF 'Z' RECORD                      
         MVC   P+6(4),RATEIO+17                                                 
         GOTO1 HEXOUT,DMCB,RATEIO+21,P+16,3,=C'TOG'                             
         MVC   P+23(1),RATEIO+24                                                
         GOTO1 HEXOUT,DMCB,RATEIO+25,P+25,2,=C'TOG'                             
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,ADDREC),REPFILE,KEY+28,RATEIO,DMWORK             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 ADD NEW RATE RECORD                          
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY      RESTOR HEADER KEY                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 RESTORE HEADER SEQUENCE                      
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,HDRIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    PC120                                                            
         DROP  R3,R7                                                            
*                                                                               
*******************************************************************             
*     REMOVE OLD AND ADD NEW STYLE X'06' ELEMENTS IN HEADER                     
*******************************************************************             
         LA    R6,HDRIO            REMOVE OLD X'06' ELEMS                       
         GOTO1 VHELLO,DMCB,(C'D',REPFILE),(X'06',HDRIO),0                       
*                                                                               
         LA    R2,AVTAB                                                         
*                                                                               
PC300    DS    0H                  ADD NEW X'06' ELEMS                          
         OC    0(AVTABLQ,R2),0(R2)     ANY AVAIL ENTRY HERE?                    
         BZ    PC350                                                            
*                                                                               
         LA    R7,ELEM                                                          
         USING RIMAELEM,R7                                                      
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   RIMACODE,X'06'      ELEM CODE                                    
         MVI   RIMALEN,RIMALENQ    ELEM LENGTH                                  
         MVC   RIMANUM,AVEQU       EQUATE NUM                                   
         MVC   RIMAREP,AVREP       REP CODE                                     
         MVC   RIMACDE,AVCDE       AVAIL CODE                                   
         MVC   RIMALNTH,AVLNTH     AVAIL LENGTH                                 
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',REPFILE),(R6),(R7),=C'ADD=CODE'                
*                                                                               
         GOTO1 HEXOUT,DMCB,RIMANUM,P,1,=C'TOG'                                  
         MVC   P+2(2),RIMAREP                                                   
         MVC   P+5(8),RIMACDE                                                   
         GOTO1 HEXOUT,DMCB,RIMALNTH,P+15,2,=C'TOG'                              
         GOTO1 REPORT                                                           
         DROP  R7                                                               
*                                                                               
*******************************************************************             
*     UPDATE ACTIVITY ELEMENT                                                   
*******************************************************************             
         GOTO1 VHELLO,DMCB,(C'G',REPFILE),(X'EF',HDRIO),0                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R7,12(R1)                                                        
         USING RINVAEL,R7                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,RINVALST)                                   
         DROP  R7                                                               
*                                                                               
PC320    LA    R2,AVTABLQ(R2)      CHECK NEXT ENTRY                             
         B     PC300                                                            
*                                                                               
PC350    DS    0H                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,HDRIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 UPDATE HEADER RECORD                         
         DC    H'0'                                                             
*                                                                               
         B     PCSEQ               GET NEXT HEADER                              
*                                                                               
*******************************************************************             
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT ======>'                                          
*                                                                               
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
QTRTAB   DC    XL1'01',XL4'010F030F'                                            
         DC    XL1'02',XL4'040F060F'                                            
         DC    XL1'03',XL4'070F090F'                                            
         DC    XL1'04',XL4'0A0F0C0F'                                            
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
COUNT    DS    F                                                                
HDRKEY   DS    CL27                                                             
ELEM     DS    XL50                                                             
*                                                                               
         DC    CL7'CURRENT'                                                     
*                                                                               
CUREQU   DS    X                   EQUATE # TO USE IN NEW 'Z' RECORD            
CURYEAR  DS    X                   CURRENT YEAR PROCESSING IN BINARY            
CURWEEK  DS    XL3                 CURRENT WEEK BEING PROCESSED                 
CURCOST  DS    F                   CURRENT COST BEING PROCESSED                 
*                                                                               
SVREP    DS    CL2                 REP CODE                                     
SVCDE    DS    CL8                 MASTER AVAIL CODE                            
SVLNTH   DS    CL2                 MASTER AVAIL LENGTH                          
SVYEAR   DS    CL1                 MASTER AVAIL YEAR                            
SVCOST1  DS    XL4                 COST FOR 1ST QUARTER                         
SVCOST2  DS    XL4                 COST FOR 2ND QUARTER                         
SVCOST3  DS    XL4                 COST FOR 3RD QUARTER                         
SVCOST4  DS    XL4                 COST FOR 4TH QUARTER                         
*                                                                               
         DC    CL8'QTRDATES'                                                    
*                                                                               
BRDST1   DS    XL3                 START BROADCAST WEEK FOR QTR 1               
BDREN1   DS    XL3                 END BROADCAST WEEK FOR QTR 1                 
BRDST2   DS    XL3                 START " FOR QTR 2                            
BDREN2   DS    XL3                 END " FOR QTR 2                              
BRDST3   DS    XL3                 START " FOR QTR 3                            
BDREN3   DS    XL3                 END " FOR QTR 3                              
BRDST4   DS    XL3                 START " FOR QTR 4                            
BDREN4   DS    XL3                 END " FOR QTR 4                              
         DC    X'FF'                                                            
*                                                                               
TEMPDATE DS    XL12                TEMP STORAGE FOR DATES                       
TEMPDAT2 DS    XL20                TEMP STORAGE FOR DATES                       
*                                                                               
VHELLO   DS    A                                                                
VADDAY   DS    A                                                                
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
*                                                                               
         DC    CL5'HDRIO'                                                       
HDRIO    DS    XL2000              HEADER RECORD TO CHANGE W/ NEW X'06'         
         DC    CL6'RATEIO'                                                      
RATEIO   DS    XL2000              NEW RATE RECORD TO ADD 'Z'                   
         DC    CL5'AVTAB'                                                       
AVTAB    DS    XL500               TABLE OF REP/AVAIL CODE/AVAIL LENGTH         
*                                                                               
         LTORG                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
AVTABD   DSECT                                                                  
AVREP    DS    CL2                 REP CODE                                     
AVCDE    DS    CL8                 MASTER AVAIL CODE                            
AVLNTH   DS    CL2                 MASTER AVAIL LENGTH                          
AVEQU    DS    CL1                 EQUATE NUMBER                                
AVTABLQ  EQU   *-AVREP                                                          
*                                                                               
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'121REREP2402Y05/01/02'                                      
         END                                                                    
