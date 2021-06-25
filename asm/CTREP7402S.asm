*          DATA SET CTREP7402S AT LEVEL 105 AS OF 12/12/97                      
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
         XCEF  REQBUFF,2000             CLEAR TABLE FIRST                       
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
         CLC   0(14,R3),0(R1)           COMPARE UP TO MEDIA                     
         BNE   RQF20                                                            
         CLC   17(3,R3),0(R1)           SAME "TO CLT"?                          
         BE    RQFX                     YES,DON'T PUT REQ IN TABLE              
RQF20    LA    R1,L'REQBUFF(R1)         CHECK NEXT ENTRY                        
         B     RQF10                                                            
*                                                                               
RQF30    DS    0H                                                               
         MVC   9(2,R3),QAGENCY                                                  
         MVC   13(1,R3),QMEDIA                                                  
         MVC   14(3,R3),QCLIENT          COPY FROM CLIENT                       
         MVC   17(3,R3),QCLIENT+3        COPY TO CLIENT                         
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
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
RNL5     DS    0H                                                               
         GET   TAPEIN,RECALL                                                    
         LA    R3,REQBUFF                                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECALL                                   
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
         CLC   CTUKAGY,9(R3)                                                    
         BNE   RNL10                                                            
         CLC   CTUKMED,13(R3)                                                   
         BNE   RNL10                                                            
         CLC   CTUKCLT,14(R3)                                                   
         BNE   RNL10                                                            
         DROP  R5                                                               
*                                                                               
*------------------ COPY USER FROM CLIENT1 TO CLIENT2 ----------------*         
*                                                                               
** START BUILDING REC                                                           
*                                                                               
         XC    WIO,WIO             CLEAR AREA FIRST TO BUILD KEY.               
         LA    R4,WIO                                                           
         USING CTUREC,R4                                                        
         MVC   CTUKEY(19),0(R5)         COPY 1ST 19 BYTES FROM OLD CLI          
         MVC   CTUKCLT,17(R3)           2ND CLI FROM REQUEST                    
*                                                                               
* BUILD REST OF REC - ACTIV + VALUE EL'S                                        
         ZICM  R1,25(R5),2                                                      
         SH    R1,=H'25'                 MINUS THE KEY LENGTH                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   25(0,R4),25(R5)         COPY STATUS, LEN + EL'S                  
*                                                                               
         XCEF  RECALL,2048                                                      
         ZICM  R1,CTULEN,2                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RECALL+4(0),WIO                                                  
         LA    R1,5(R1)                1 BCTR, 4 RECLN                          
         STCM  R1,3,RECALL                                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECALL                                   
*                                                                               
         MVC   P+2(44),REC                                                      
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
         MVC   RECALL(200),0(R4)                                                
         PUT   TAPEOUT,RECALL                                                   
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
SORTCARD DC    CL80'SORT FIELDS=(4,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2048'                                  
*                                                                               
LASTRECD DC    25X'FF'          LAST RECORD MARKER.                             
*                                                                               
WIO      DS    CL210                                                            
*                                                                               
REQBUFF  DS    100CL20                  TABLE OF REQUESTS                       
RQBFLNEQ EQU   RQBFEND-REQBUFF                                                  
RQBFEND  DC    X'FF'                                                            
*                                                                               
RECALL   DS    0CL2048                                                          
         DS    CL4                                                              
REC      DS    CL2044                                                           
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
**PAN#1  DC    CL21'105CTREP7402S12/12/97'                                      
         END                                                                    
