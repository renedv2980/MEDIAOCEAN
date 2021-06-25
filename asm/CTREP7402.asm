*          DATA SET CTREP7402  AT LEVEL 157 AS OF 02/20/98                      
*PHASE CT7402A,+0                                                               
*INCLUDE SORTER                                                                 
         TITLE 'CT7402 - COPY USER PROFILES FROM ONE CLIENT TO ANOTHER'         
CT7402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CT7402                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RNF                                                              
         CLI   MODE,REQFRST                                                     
         BE    RQF                                                              
         CLI   MODE,RUNLAST                                                     
         BE    RNL                                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
RNF      DS    0H                                                               
         XCEF  REQBUFF,1100             CLEAR TABLE FIRST                       
         LA    R3,REQBUFF                                                       
         ST    R3,NEXTREQ                                                       
RNFX     B     XIT                                                              
*                                                                               
RQF      DS    0H                                                               
         MVI   MODE,REQLAST                                                     
         L     R3,NEXTREQ                                                       
         CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                     DIE IF REQ'S > TABLE SIZE               
*                                                                               
* CHECK FOR OVERLAP - REQUESTS WITH SAME FIRST 14 BYTES & "TO CLIENT"           
*                                                                               
         LA    R1,REQBUFF               PT TO START OF REQ TABLE                
RQF10    CR    R3,R1                    REACHED CURRENT REQ?                    
         BE    RQF30                    YES, PUT IN TABLE                       
         CLC   0(5,R3),0(R1)           COMPARE UP TO MEDIA                      
         BNE   RQF20                                                            
         CLC   7(3,R3),0(R1)           SAME "TO CLT"?                           
         BE    RQFX                     YES,DON'T PUT REQ IN TABLE              
RQF20    LA    R1,L'REQBUFF(R1)         CHECK NEXT ENTRY                        
         B     RQF10                                                            
*                                                                               
RQF30    DS    0H                                                               
         MVC   1(2,R3),QAGENCY                                                  
         MVC   3(1,R3),QMEDIA                                                   
         CLI   QMEDIA,C'A'         IS INPUT MEDIA 'ALL' ?                       
         BNE   *+8                                                              
         MVI   3(R3),0           0 MEDIA IN KEY IS 'ALL'                        
         CLI   QMEDIA,C' '         IS INPUT MEDIA 'ALL' ?                       
         BH    *+8                 NO, REQUEST MEDIA IS QMEDIA                  
         MVI   3(R3),0           0 MEDIA IN KEY IS 'ALL'                        
*                                                                               
         MVC   4(3,R3),QCLIENT          COPY FROM CLIENT                        
         MVC   7(3,R3),QCLIENT+3        COPY TO CLIENT                          
*                                                                               
         LA    R3,L'REQBUFF(R3)         READY TO INSERT NEXT REQ                
         ST    R3,NEXTREQ                                                       
RQFX     B     XIT                                                              
*                                                                               
RNL      DS    0H                                                               
         L     R3,NEXTREQ                                                       
         MVI   0(R3),X'FF'              PUT END MARKER IN REQBUFF TABLE         
         OPEN  (TAPEIN,INPUT)                                                   
         OPEN  (TAPEOUT,OUTPUT)                                                 
         XC    NEWUCNT,NEWUCNT                                                  
         XC    PREVKEY,PREVKEY                                                  
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
RNL5     DS    0H                                                               
         GET   TAPEIN,RECALL                                                    
         MVC   SORTALL(29),RECALL                                               
         ZICM  R1,RECLEN,2                                                      
         LA    R1,30(R1)                GET PROPER SORTREC LEN                  
         STCM  R1,3,SORTLEN                                                     
         MVI   SORTSTAT,0          FLAG FOR OLD REC                             
         LA    R3,REQBUFF                                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTALL                                  
         CLC   LASTRECD,REC                                                     
         BE    RNL50                                                            
         CLI   REC,CTUKTYPQ                                                     
         BNE   RNL5                                                             
         CLI   REC+27,X'80'             IS REC. DELETED?                        
         BE    RNL5                     YES, DON'T COPY IF DEL'D                
*                                                                               
* DO COMPARES, BUMP THROUGH REQUEST, DO THEY MATCH RECORD                       
*                                                                               
         USING CTUREC,R5                                                        
         LA    R5,REC                                                           
         CLI   CTUKSYS,C'S'        ONLY FOR SPOT SYSTEM                         
         BNE   RNL5                                                             
         B     RNL15                                                            
*                                                                               
RNL10    LA    R3,L'REQBUFF(R3)                                                 
         CLI   0(R3),X'FF'              END OF REQUESTS?                        
         BE    RNL5                     YES, CHECK NEXT RECORD                  
*                                                                               
RNL15    DS    0H                                                               
         CLC   CTUKAGY,1(R3)                                                    
         BNE   RNL10                                                            
         CLC   CTUKMED,3(R3)                                                    
         BNE   RNL10                                                            
         CLC   CTUKCLT,4(R3)                                                    
         BNE   RNL10                                                            
         DROP  R5                                                               
*                                                                               
*------------------ COPY USER FROM CLIENT1 TO CLIENT2 ----------------*         
*                                                                               
** START BUILDING REC                                                           
*                                                                               
         XCEF  WIO,1000            CLEAR AREA FIRST TO BUILD KEY.               
         LA    R4,WIO                                                           
         USING CTUREC,R4                                                        
         MVC   CTUKEY(19),0(R5)         COPY 1ST 19 BYTES FROM OLD CLI          
         MVC   CTUKCLT,7(R3)           2ND CLI FROM REQUEST                     
*                                                                               
* BUILD REST OF REC - ACTIV + VALUE EL'S                                        
         ZICM  R1,25(R5),2              GETTING DEST LEN                        
         SH    R1,=H'25'                 MINUS THE KEY LENGTH                   
         LA    R0,25(R4)                A(DEST)                                 
         LR    RF,R1                    SOURCE LEN                              
         LA    RE,25(R5)                A(SRC)                                  
         MVCL  R0,RE                                                            
*                                                                               
         XCEF  SORTALL,2048                                                     
         ZICM  R1,CTULEN,2              GETTING DEST LEN                        
         LA    R0,REC                   A(DEST)                                 
         LR    RF,R1                    SOURCE LEN                              
         LA    RE,WIO                   A(SRC)                                  
         MVCL  R0,RE                                                            
*                                                                               
         ZICM  R1,CTULEN,2              GETTING WIO  LEN                        
         LA    R1,4(R1)                4 RECLN                                  
         STCM  R1,3,RECLEN                                                      
         LA    R1,30(R1)                30 FOR SORTREC                          
         STCM  R1,3,SORTLEN                                                     
*                                                                               
         MVC   SORTKEY,WIO                                                      
         MVI   SORTSTAT,1          FLAG FOR NEW REC                             
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTALL                                  
*                                                                               
         MVC   P+2(55),SORTALL                                                  
         GOTO1 REPORT                                                           
*                                                                               
         LH    R1,NEWUCNT               COUNT 'U' REC'S TO TAPEOUT              
         LA    R1,1(R1)                                                         
         STH   R1,NEWUCNT                                                       
*                                                                               
         B     RNL5                                                             
         DROP  R4                                                               
*                                                                               
RNL50    CLOSE TAPEIN                                                           
RNL51    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)                                                      
         BZ    XRNL                                                             
*                                                                               
         XCEF  SORTALL,2048                                                     
         ZICM  R1,0(R4),2              GETTING DEST LEN                         
         LA    R0,SORTALL               A(DEST)                                 
         LR    RF,R1                    SOURCE LEN                              
         LA    RE,0(R4)                 A(SOURCE)                               
         MVCL  R0,RE                                                            
         CLC   PREVKEY,SORTKEY     DUPLICATE KEYS?                              
         BNE   RNL52               NOT SAME KEY, WRITE TO TAPE                  
*                                                                               
         LH    RE,NEWUCNT          DUPLICATE KEYS, DON'T WRITE NEW -            
         BCTR  RE,0                - REC TO TAPEOUT AND DECREMENT -             
         STH   RE,NEWUCNT          - # OF REC'S ADDED                           
         B     RNL51                                                            
*                                                                               
RNL52    PUT   TAPEOUT,RECALL                                                   
         MVC   PREVKEY,SORTKEY                                                  
*                                                                               
         B     RNL51                                                            
*                                                                               
XRNL     GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE TAPEOUT                                                          
         MVC   P(24),=C'ADDED USERPROF RECORDS: '                               
         EDIT  NEWUCNT,(6,P+28)                                                 
         GOTO1 REPORT                                                           
*                                                                               
XIT      XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),                      +        
               RECFM=VB,LRECL=2048,EODAD=RNL50                                  
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=2048,BUFNO=2,BLKSIZE=8200                         
*                                                                               
** COUNTERS **                                                                  
NEWUCNT  DS    H                        'U' REC'S AFTER FIX                     
NEXTREQ  DS    F                                                                
*                                                                               
** SORTER'S CARDS **                                                            
SORTCARD DC    CL80'SORT FIELDS=(5,26,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2048'                                  
*                                                                               
LASTRECD DC    25X'FF'          LAST RECORD MARKER.                             
*                                                                               
         DS    0F                                                               
WIO      DS    CL1000                                                           
PREVKEY  DS    CL25                                                             
*                                                                               
REQBUFF  DS    100CL11                  TABLE OF REQUESTS                       
RQBFLNEQ EQU   RQBFEND-REQBUFF                                                  
RQBFEND  DC    X'FF'                                                            
*                                                                               
SORTALL  DS    0CL2048                                                          
SORTLEN  DS    CL4                                                              
SORTKEY  DS    CL25                                                             
SORTSTAT DS    CL1                                                              
*                                                                               
RECALL   DS    0CL2018                                                          
RECLEN   DS    CL4                                                              
REC      DS    CL2014                                                           
*                                                                               
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'157CTREP7402 02/20/98'                                      
         END                                                                    
