*          DATA SET CTREP6102  AT LEVEL 002 AS OF 08/22/00                      
*PHASE CT6102A                                                                  
         TITLE 'LIBRARY/JCL/FILE BOOK LISTINGS'                                 
CT6102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CT6102                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA          RA=A(GLOBAL W/S)                             
         CLI   MODE,REQFRST                                                     
         BE    CT2                                                              
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                  SET-UP FOR NEXT REQUEST                      
CT2      MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         LA    R2,BOOKEY                                                        
         USING CTJREC,R2           R2=A(KEY)                                    
         XC    CTJKEY,CTJKEY                                                    
*                                  GET VALUES FROM TYPE TABLE                   
         LA    R1,TYPETAB                                                       
CT4      CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(1,R1),QOPT1       QOPT1 IS BOOK TYPE                           
         BE    *+12                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         B     CT4                                                              
         MVC   TYPEVALS,0(R1)                                                   
         MVC   CTJKTYP,TYPETYP     SET RECORD TYPE IN KEY                       
         MVC   RCSUBPRG,TYPESPRG                                                
         CLC   QID,SPACES                                                       
         BE    *+14                                                             
         MVC   CTJKID,QID                                                       
         B     CT10                                                             
*                                  FIND FIRST/NEXT BOOK INDEX RECORD            
CT6      GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',BOOKEY,IO                 
         CLI   8(R1),0                                                          
         BNE   EXIT                                                             
         CLC   BOOKEY(1),IO                                                     
         BNE   EXIT                                                             
         MVC   BOOKEY,IO                                                        
         CLI   CTJKSUB,0                                                        
         BE    CT12                                                             
CT8      MVI   CTJKSUB,X'FF'       SET HIGH SEQNO TO GET NEXT BOOK              
         B     CT6                                                              
         EJECT                                                                  
*                                                                               
CT10     GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',BOOKEY,IO                 
         CLI   8(R1),0                                                          
         BNE   EXIT                                                             
*                                  FIND DESCRIPTION/ACTIVITY ELEMENTS           
CT12     LA    R3,IO+(CTJDATA-CTJREC)                                           
         MVC   BOOKDESC,SPACES                                                  
CT14     CLI   0(R3),0                                                          
         BE    CT22                                                             
         CLI   0(R3),X'01'                                                      
         BE    CT18                                                             
         CLI   0(R3),X'02'                                                      
         BE    CT20                                                             
CT16     ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CT14                                                             
*                                  FORMAT ACTIVITY ELEMENT                      
CT18     GOTO1 DATCON,DMCB,(3,2(R3)),(8,ACTDATE)                                
         B     CT16                                                             
*                                  FORMAT DESCRIPTION ELEMENT                   
CT20     IC    R1,1(R3)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     CT16                                                             
         MVC   BOOKDESC(0),2(R3)                                                
*                                  READ BOOK VIA GETBOOK                        
CT22     MVC   BOOKEY,IO           SAVE CURRENT BOOK KEY                        
         MVC   SAVEMID,SPACES      FORMAT MIDLINE FOR PRINTING                  
         MVC   SAVEMID+1(4),=C'BOOK'                                            
         MVC   SAVEMID+6(L'CTJKID),CTJKID                                       
         LA    R1,SAVEMID+6+L'CTJKID                                            
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'BOOKDESC,R1),BOOKDESC                                        
         LA    R1,L'BOOKDESC+2(R1)                                              
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(14,R1),=C'LAST ACTIVE ON'                                      
         MVC   17(L'ACTDATE,R1),ACTDATE                                         
         MVI   FORCEMID,C'Y'                                                    
         GOTO1 ,GBCB,(1,IO),CARDIO,DATAMGR                                      
         CLI   QOPT2,C'Y'                                                       
         BNE   *+8                                                              
         MVI   GBCB,0              SET TO EXPAND INCLUDES                       
*                                                                               
CT24     GOTO1 GETBOOK,GBCB                                                     
         TM    8(R1),X'80'         TEST E-O-B                                   
         BO    CT26                                                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+1(L'CARDIO),CARDIO                                             
         MVI   MID1,0                                                           
         MVC   MID2,SAVEMID                                                     
         GOTO1 REPORT                                                           
         B     CT24                                                             
*                                                                               
CT26     CLC   QID,SPACES          TEST IF SINGLE BOOK PRINT                    
         BNE   EXIT                                                             
         B     CT8                                                              
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
*                                                                               
TYPETAB  DS    0XL3                                                             
         DC    C'JJ',AL1(1)                                                     
         DC    C'LL',AL1(2)                                                     
         DC    C'FF',AL1(3)                                                     
         DC    X'FF'                                                            
*                                                                               
GBCB     DS    6F                                                               
TYPEVALS DS    0CL3                                                             
TYPEIN   DS    CL1                                                              
TYPETYP  DS    CL1                                                              
TYPESPRG DS    XL1                                                              
BOOKEY   DS    CL25                                                             
BOOKDESC DS    CL60                                                             
ACTDATE  DS    CL8                                                              
CARDIO   DS    CL80                                                             
SAVEMID  DS    CL132                                                            
IO       DS    1200C                                                            
*                                                                               
* CTREPWORKD/CTREPMODES/CTGENFILE                                               
         PRINT OFF                                                              
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTREP6102 08/22/00'                                      
         END                                                                    
