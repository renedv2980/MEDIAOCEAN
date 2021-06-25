*          DATA SET JHIGDM3    AT LEVEL 059 AS OF 10/31/00                      
*PHASE JHIGDM3                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE FATABOFF                                                               
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
         MVC   P(28),=C'ID DATE MOD  CTL PID    NAME'                           
         MVC   P+59(41),=C'ADDRESS                              AUTH'           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(132),DASHES                                                    
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         USING CT5REC,R4                                                        
         LA    R4,KEY                    BUILD KEY                              
         XC    KEY,KEY                                                          
         MVI   CT5KTYP,CT5KTYPQ                                                 
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
*                                                                               
LOOP     CLI   8(R1),0                   DIE IF NO RECORDS FOUND                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         CLC   KEY(CT5KTYP-CT5KEY),CT5KEY                                       
         BNE   GOODBYE                   EXIT WHEN KEY DOES NOT MATCH           
*                                                                               
         MVC   KEY,CT5KEY                SAVE OFF KEY                           
         MVC   P(L'CT5KALPH),CT5KALPH                                           
         MVI   ELCODE,X'01'              LOOK FOR 01 ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   DESCRIP                   IF NOT FOUND, LOOK FOR NEXT            
         USING CTACTD,R4                                                        
         GOTO1 =V(DATCON),DMCB,(3,CTACTDT),(11,ACTDT)                           
         MVC   P+3(L'ACTDT),ACTDT                                               
*                                                                               
DESCRIP  DS    0H                                                               
         USING CTIREC,R6                                                        
         XC    IDKEY,IDKEY                                                      
         LA    R6,IDKEY                                                         
         MVI   ELCODE,X'02'              LOOK FOR 02 ELEMENT                    
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BNE   NAME                                                             
         USING CTDSCD,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ          BUILD IDKEY                            
         MVC   CTIKNUM,CTDSC                                                    
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',IDKEY,IO              
         LA    R4,IO                                                            
         BAS   RE,GETEL                  LOOK FOR 02 ELEMENT IN ID REC          
         ZIC   R5,CTDSCLEN               GET LENGTH                             
         SHI   R5,3                      ADJUST LENGTH                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),CTDSC             PRINT DESCRIPTION                      
*                                                                               
NAME     DS    0H                                                               
         LA    R4,IO                                                            
         MVI   ELCODE,X'30'              LOOK FOR 30 ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   AUTH                                                             
         USING CTDSTD,R4                                                        
         MVC   P+24(L'CTDSTNAM),CTDSTNAM PRINT NAME AND ADDRESS                 
         MVC   P+59(L'CTDSTADD),CTDSTADD                                        
*                                                                               
AUTH     DS    0H                                                               
         LA    R4,IO                                                            
         MVI   ELCODE,X'21'              LOOK FOR 21 ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   RESTORE                                                          
         USING CTSYSD,R4                                                        
         USING SELISTD,R9                                                       
         LA    R8,P+96                                                          
LOOP3    L     R9,=V(SELIST)                                                    
         LH    R6,0(R9)                  LOAD REGISTERS FOR BXLE LOOP           
         L     R7,2(R9)                                                         
         LA    R9,6(R9)                                                         
LOOP2    CLC   SESYS,CTSYSNUM                                                   
         BE    GOTSYS                                                           
         BXLE  R9,R6,LOOP2                                                      
GOTSYS   MVC   0(L'SENAME,R8),SENAME     PRINT AUTHORIZED SYSTEM                
         AHI   R8,L'SENAME+1                                                    
         BAS   RE,NEXTEL                 GET NEXT 21 ELEMENT                    
         BE    LOOP3                                                            
*                                                                               
RESTORE  GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IO                
*                                                                               
NOTFOUND GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
*                                                                               
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',,IO                   
         B     LOOP                                                             
*                                                                               
GOODBYE  XBASE                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R4,28,ELCODE                                                     
*                                                                               
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
IDKEY    DS    CL25                                                             
ACTDT    DS    CL8                                                              
ELCODE   DS    X                                                                
DASHES   DC    132C'-'                                                          
IO       DS    1000X               I/O AREA FOR CTFILE                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059JHIGDM3   10/31/00'                                      
         END                                                                    
