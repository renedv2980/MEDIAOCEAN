*          DATA SET JHIGDM2    AT LEVEL 080 AS OF 10/27/00                      
*PHASE JHIGDM2A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'ANOTHER DATAMGR EXAMPLE'                                        
JHIGDMGR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,JHIGDMGR,=V(REGSAVE),R9                                        
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(JHIGDMGR),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
* DON'T WORRY ABOUT THIS DMOPEN CALL                                            
*                                                                               
MAIN     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IO,0                                               
*                                                                               
* READ RECORDS AND PROCESS THEM                                                 
*                                                                               
*                                                                               
         USING CTPHRECD,R4                                                      
         LA    R4,KEY                                                           
         XC    KEY,KEY                   BUILD KEY                              
         MVI   CTPHID,CTPHIDQ                                                   
         MVI   CTPHSUBI,CTPHSUBQ                                                
         MVI   CTPHHEXN,X'02'                                                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
*                                                                               
LOOP     CLI   8(R1),0                   DIE IF NO RECORDS FOUND                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         CLC   KEY(CTPHNAME-CTPHPKEY),CTPHPKEY                                  
         BNE   GOODBYE                   EXIT WHEN KEY DOES NOT MATCH           
*                                                                               
         CLI   CTPHHEXN,X'02'                                                   
         BNE   NEXT                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CTPHHEXN,PHNAME,L'CTPHHEXN                       
         MVI   PHNAME,C'T'                                                      
         MVC   P(L'PHNAME),PHNAME                                               
         GOTO1 =V(HEXOUT),DMCB,CTPHLVL,PHLVL,L'CTPHLVL                          
         MVC   P+6(L'PHLVL),PHLVL                                               
*                                                                               
         USING LANGTABD,R5               PREPARE LANGTAB                        
         LA    R5,LANGTAB                                                       
         LA    R5,6(R5)                  A(FIRST ENTRY)                         
*                                                                               
         ZIC   R6,CTPHLANG                                                      
         MHI   R6,LANGTABL               GO TO CURRENT LANGUAGE                 
         AR    R5,R6                                                            
         MVC   P+8(L'LANGSHR),LANGSHR   MOVE LANG CODE TO P                     
*                                                                               
         LA    R4,CTPHOVEQ(R4)           GO TO FIRST ELEMENT                    
LOOP2    CLI   0(R4),0                   CHECK FOR END                          
         BE    NOTFOUND                                                         
         CLI   0(R4),X'25'               LOOK FOR DESC. ELEMENT                 
         BE    GOTIT                                                            
         ZIC   R0,1(R4)                  GET LENGTH OF ELEMENT                  
         AR    R4,R0                     GO TO NEXT ELEMENT                     
         B     LOOP2                                                            
         DROP  R4                                                               
*                                                                               
GOTIT    DS    0H                                                               
         USING CTPHDSCD,R4                                                      
         ZIC   R9,CTPHDLEN               INSERT LENGTH OF ELEMENT               
         SHI   R9,3                      SUB CODE, LENGTH AND ADJUST            
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),CTPHDDSC                                                 
         DROP  R4                                                               
*                                                                               
         USING CTPHRECD,R4                                                      
         LA    R4,IO                                                            
         LA    R4,CTPHOVEQ(R4)           GO TO FIRST ELEMENT                    
LOOP3    CLI   0(R4),0                   CHECK FOR END                          
         BE    NOTFOUND                                                         
         CLI   0(R4),X'05'               LOOK FOR SYSTEM ELEMENT                
         BE    GOTDATE                                                          
         ZIC   R0,1(R4)                  GET LENGTH OF ELEMENT                  
         AR    R4,R0                     GO TO NEXT ELEMENT                     
         B     LOOP3                                                            
         DROP  R4                                                               
*                                                                               
GOTDATE  DS    0H                                                               
         USING CTPHSYSD,R4                                                      
         GOTO1 =V(DATCON),DMCB,(8,CTPHSRDT),(11,PHDATE)                         
         MVC   P+69(L'PHDATE),PHDATE                                            
*                                                                               
         TM    CTPHSFL1,X'10'             CHECK IF SCREEN OR PROG               
         BZ    ISPROG                                                           
         MVC   P+79(6),=C'SCREEN'                                               
         B     *+10                                                             
ISPROG   MVC   P+79(4),=C'PROG'                                                 
         DROP  R4                                                               
*                                                                               
         USING CTPHRECD,R4                                                      
         LA    R4,IO                                                            
         LA    R4,CTPHOVEQ(R4)           GO TO FIRST ELEMENT                    
LOOP4    CLI   0(R4),0                   CHECK FOR END                          
         BE    CONT                                                             
         CLI   0(R4),X'15'               LOOK FOR RELOAD LIST ELEMENT           
         BE    RELOAD                                                           
         ZIC   R0,1(R4)                  GET LENGTH OF ELEMENT                  
         AR    R4,R0                     GO TO NEXT ELEMENT                     
         B     LOOP4                                                            
         DROP  R4                                                               
*                                                                               
RELOAD   DS    0H                                                               
         USING CTPHLSTD,R4                                                      
         TM    CTPHLFLG,X'80'            CHECK RELOAD FLAG                      
         BO    RLALL                                                            
         LA    R2,P                                                             
         LA    R2,87(R2)                                                        
         ZIC   R5,CTPHLLEN               GET LENGTH                             
         SHI   R5,CTPHLOVQ               MINUS FIXED                            
         LA    R6,CTPHLLST                                                      
LIST     GOTO1 =V(HEXOUT),DMCB,0(R6),0(R2),L'CTPHLLST                           
         SHI   R5,1                                                             
         LTR   R5,R5                                                            
         BZ    CONT                                                             
         LA    R2,2(R2)                  MOVE UP PRINT LINE                     
         MVC   0(1,R2),=C','                                                    
         LA    R2,1(R2)                  MOVE UP PRINT LINE                     
         LA    R6,1(R6)                  GET NEXT ITEM ON LIST                  
         B     LIST                                                             
         DROP  R4                                                               
*                                                                               
RLALL    DS    0H                                                               
         MVC   P+87(3),=C'ALL'                                                  
*                                                                               
CONT     DS    0H                                                               
*                                                                               
         USING CTPHRECD,R4                                                      
         LA    R4,IO                                                            
         LA    R4,CTPHOVEQ(R4)           GO TO FIRST ELEMENT                    
LOOP5    CLI   0(R4),0                   CHECK FOR END                          
         BE    PRINT                                                            
         CLI   0(R4),X'35'               LOOK FOR COMMENT ELEMENT               
         BE    GOTCOM                                                           
         ZIC   R0,1(R4)                  GET LENGTH OF ELEMENT                  
         AR    R4,R0                     GO TO NEXT ELEMENT                     
         B     LOOP5                                                            
         DROP  R4                                                               
*                                                                               
GOTCOM   DS    0H                                                               
         USING CTPHCOMD,R4                                                      
         LA    R2,P                                                             
         LA    R2,101(R2)                                                       
         ZIC   R9,CTPHCLEN               GET LENGTH OF ELEMENT                  
         SHI   R9,3                      ADJUST FOR LENGTH OF COMMENT           
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CTPHCTXT          MOVE COMMENT ONTO PRINTLINE            
         AR    R2,R9                     MOVE UP PRINTLINE                      
         AR    R4,R9                     GO TO NEXT ELEMENT                     
         B     LOOP5                                                            
*                                                                               
PRINT    GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IO                
         B     LOOP                                                             
*                                                                               
NOTFOUND DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
GOODBYE  XBASE                                                                  
         EJECT                                                                  
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                                     
*                                                                               
UTL      DC    F'0',X'0A'          FOR OFFLINE DATAMGR                          
*                                                                               
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
KEY      DS    CL25                CTFILE KEY                                   
PHDATE   DS    CL8                                                              
PHNAME   DS    CL6                                                              
PHLVL    DS    C                                                                
PHLLST   DS    C                                                                
IO       DS    1000X               I/O AREA FOR CTFILE                          
       ++INCLUDE FALANGTAB         LANGUAGE TABLE                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FALANG           DSECT FOR LANGUAGE TABLE                      
         EJECT                                                                  
       ++INCLUDE CTGENPHASE                                                     
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080JHIGDM2   10/27/00'                                      
         END                                                                    
