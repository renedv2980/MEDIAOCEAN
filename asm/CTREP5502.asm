*          DATA SET CTREP5502  AT LEVEL 015 AS OF 05/01/02                      
*PHASE CT5502A                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'ID REPORT'                                                      
CT5502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**IDS***,RR=R2                                                 
         ST    R2,RELO                                                          
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         CLI   MODE,RUNFRST                                                     
         BE    XIT                                                              
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   TR10                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         L     R2,=A(SYSBUFF)      NEED SYSTEM LIST RECORD                      
         USING CTWKEY,R2                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R2),(R2),0                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     XIT                                                              
         SPACE 3                                                                
TR10     CLI   MODE,REQLAST                                                     
         BE    DOLAST                                                           
*                                                                               
         CLI   MODE,PROCID                                                      
         BNE   XIT                                                              
         EJECT                                                                  
         L     R2,ADRECORD                                                      
         USING CTIREC,R2                                                        
         CLI   CTIKID,0                                                         
         BE    XIT                 IGNORE PASSIVES                              
*                                                                               
         MVC   SORTKEY+10(10),CTIKID                                            
         LR    R4,R2                                                            
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SORTKEY+8(2),2(R4)                                               
*                                                                               
         LR    R4,R2                                                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   TR30                                                             
*                                                                               
TR20     MVC   SORTKEY+7(1),4(R4)                                               
         MVC   BYTE,3(R4)                                                       
         BAS   RE,GETSNAME                                                      
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTKEY                                  
         MVI   ELCODE,X'21'                                                     
         BAS   RE,NEXTEL                                                        
         BE    TR20                                                             
*                                                                               
TR30     B     XIT                                                              
         EJECT                                                                  
DOLAST   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    WRAPUP                                                           
         L     R6,DMCB+4                                                        
         MVC   SORTKEY,0(R6)                                                    
*                                                                               
         CLC   OLDSYS,SORTKEY                                                   
         BE    *+14                                                             
         MVC   OLDSYS,SORTKEY                                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P+1(7),SORTKEY                                                   
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,SORTKEY+7,P+11,1,=C'TOG'                         
         CLC   DMCB+16(4),=F'2'                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+17(2),SORTKEY+8                                                
         MVC   P+23(10),SORTKEY+10                                              
         GOTO1 REPORT                                                           
*                                                                               
         B     DOLAST                                                           
         SPACE 5                                                                
WRAPUP   GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO EXPAND SYSTEM NAME                                                 
*                                                                               
GETSNAME NTR1                                                                   
*                                                                               
         L     R4,=A(SYSBUFF)                                                   
         MVI   ELCODE,X'A4'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
GETFN2   BE    GETFN3                                                           
         MVC   SORTKEY(7),SPACES                                                
         GOTO1 =V(HEXOUT),DMCB,BYTE,SORTKEY,1,=C'TOG'                           
         CLC   DMCB+16(4),=F'2'                                                 
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
GETFN3   CLC   BYTE,11(R4)         CHECK MATCH ON NUMBER                        
         BNE   GETFN4                                                           
         MVC   SORTKEY(7),3(R4)    FOUND DISPLAY NAME                           
         B     XIT                                                              
*                                                                               
GETFN4   BAS   RE,NEXTEL                                                        
         B     GETFN2                                                           
         EJECT                                                                  
         LTORG                                                                  
         SPACE 5                                                                
         GETEL R4,28,ELCODE                                                     
         SPACE 2                                                                
RELO     DS    A                                                                
ELCODE   DS    X                                                                
SENUMBER DS    X                                                                
OLDSYS   DC    CL7' '                                                           
SORTKEY  DS    XL20                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=20'                                    
         SPACE                                                                  
SYSBUFF  DS    1000C               SYSTEM LIST RECORD HERE                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015CTREP5502 05/01/02'                                      
         END                                                                    
