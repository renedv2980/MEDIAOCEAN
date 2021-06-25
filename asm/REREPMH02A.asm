*          DATA SET REREPMH02A AT LEVEL 027 AS OF 04/15/97                      
*PHASE REMH02A,*                                                                
*INCLUDE SORTER                                                                 
         TITLE 'REREPMH02 - REMH02 - CONTRACT MOVE HISTORY REPORT'              
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREPMH02 --- CONTRACT MOVE HISTORY REPORT                 *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 04APR97 (RHV) --- CLISERV ASKS AND THEY SHALL RECEIVE!            *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
REMH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STOREX-STORED,**REMH02,R8,RR=R5                                  
         USING STORED,RC                                                        
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
                                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCCONT                                                    
         BE    PROC                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
                                                                                
         B     EXIT                                                             
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XMOD1                                                                  
                                                                                
**********************************************************************          
* REQL - RETRIEVE RECS FROM SORTER & PRINT                                      
**********************************************************************          
REQL     NTR1                                                                   
         LA    R5,P                                                             
         USING PLINE,R5                                                         
*                                                                               
REQL10   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R7,4(R1)                                                         
         LTR   R7,R7               END OF SORT RECS?                            
         BZ    REQLX               YES - DONE, PRINT TOTAL                      
*                                                                               
         USING SORTD,R7                                                         
         OC    LASTREP,LASTREP     DO WE HAVE A LAST REP?                       
         BZ    REQL20              NO - FIRST TIME                              
         CLC   LASTREP,SSOURCE     SAME AS LAST REP?                            
         BE    REQL20              YES                                          
         BAS   RE,DORTOT           NO - SUBTOTAL                                
*                                                                               
REQL20   DS    0H                                                               
         MVC   PSOURCE,SSOURCE                                                  
         GOTO1 DATCON,DMCB,(2,SDATE),(11,PDATE)                                 
         GOTO1 HEXOUT,SORGCON,PORGCON,L'SORGCON                                 
         ICM   R3,15,SKTOTAL                                                    
         SLL   R3,1                ROUND TO DOLLAR                              
         A     R3,=F'100'                                                       
         SR    R2,R2                                                            
         D     R2,=F'100'                                                       
         SRL   R3,1                                                             
         EDIT  (R3),(10,PKTOTAL),ZERO=NOBLANK,FLOAT=$                           
         GOTO1 REPORT                                                           
*                                                                               
         L     R2,REPTOT           ADD TO REP TOTAL                             
         AR    R2,R3                                                            
         ST    R2,REPTOT                                                        
         L     R2,GRANDTOT         ADD TO GRAND TOTAL                           
         AR    R2,R3                                                            
         ST    R2,GRANDTOT                                                      
         MVC   LASTREP,SSOURCE     SET LAST REP                                 
         B     REQL10              NEXT SORT REC                                
*                                                                               
REQLX    DS    0H                                                               
         BAS   RE,DORTOT           FINAL SUBTOTAL                               
         BAS   RE,DOGTOT           GRAND TOTAL                                  
         GOTO1 =V(SORTER),DMCB,=C'END',RR=Y                                     
         B     EXIT                                                             
         DROP  R7                                                               
*                                                                               
DORTOT   DS    0H                                                               
         MVI   ALLOWLIN,3                                                       
         MVC   PKTOTAL,=C'------------'                                         
         EDIT  REPTOT,(12,PKTOTAL),ZERO=NOBLANK,FLOAT=$,COMMAS=YES              
         MVC   PKTOTAL,=C'------------'                                         
                                                                                
         DROP  R5                                                               
**********************************************************************          
* REQFRST -                                                                     
**********************************************************************          
REQF     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,SRTCARD,RECCARD,0,RR=Y                           
*                                                                               
         XC    LASTREP,LASTREP                                                  
         XC    REPTOT,REPTOT                                                    
         XC    GRANDTOT,GRANDTOT                                                
*                                                                               
SRTCARD  DC    CL80'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=SRTRECL'                               
*                                                                               
       ++INCLUDE REGENCDE                                                       
*                                                                               
         EJECT                                                                  
STORED   DSECT                                                                  
RELO     DS    A                                                                
ELCODE   DS    C                                                                
LASTREP  DS    CL2                 SOURCE REP OF LAST K                         
REPTOT   DS    F                   SUBTOTAL BY REP                              
GRANDTOT DS    F                   GRAND TOTAL                                  
         DS    0D                                                               
IOAREA   DS    CL1000                                                           
*                                                                               
* SORTER RECORD                                                                 
*                                                                               
SORTD    DSECT                                                                  
SSOURCE  DS    CL2                 SOURCE REP CODE                              
SDATE    DS    XL2                 MOVE DATE (COMPRESSED)                       
SORGCON  DS    CL4                 ORIGINAL CONTRACT NUMBER                     
SKTOTAL  DS    CL4                 CONTRACT TOTAL                               
SRTRECL  EQU   *-SRTREC                                                         
STOREX   EQU   *                                                                
*                                                                               
* PRINT LINE DSECT                                                              
*                                                                               
PLINE    DSECT                                                                  
         DS    CL5                                                              
PSOURCE  DS    CL2                                                              
         DS    CL4                                                              
PDATE    DS    CL8                                                              
         DS    CL4                                                              
PORGCON  DS    CL8                                                              
         DS    CL4                                                              
PKTOTAL  DS    CL10                                                             
*              FILE CONTROL AND WORKD DSECTS                                    
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027REREPMH02A04/15/97'                                      
         END                                                                    
