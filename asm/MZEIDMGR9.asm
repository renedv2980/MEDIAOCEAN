*          DATA SET MZEIDMGR9  AT LEVEL 094 AS OF 10/02/98                      
*PHASE MZEIDMG9                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE FATABOFF                                                               
         TITLE 'ANOTHER DATAMGR EXAMPLE'                                        
DEISDMGR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DEISDMGR,=V(REGSAVE),R9                                        
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
         DC    A(DEISDMGR),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
* DON'T WORRY ABOUT THIS DMOPEN CALL                                            
*                                                                               
MAIN     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IO,0                                               
*                                                                               
*                                      PRINT COLUMN HEADERS                     
*                                                                               
         MVC   ACODE,=C'ACCESS CODE'                                            
         MVC   PID(7),=C'ID NAME'                                               
         MVC   PDEST(11),=C'DESTINATION'                                        
         MVC   PSENAME,=C'SE NAME'                                              
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
* READ RECORDS AND PROCESS THEM                                                 
         XC    KEY,KEY                                                          
         XC    KEY2,KEY2                                                        
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
*                                                                               
*                                             SET UP KEY                        
*                                                                               
         MVI   CT5KTYP,C'5'                                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
*                                                                               
LOOP     CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,IO                         RESTORE KEY                       
*                                             ACCESS RECORD                     
         LA    R4,IO                          CHECK TYPE                        
         CLI   CT5KTYP,C'5'                                                     
         BNE   GOODBYE                                                          
*                                                                               
         MVI   ELCODE,X'02'                   DOES ID EXIST?                    
         BAS   RE,GETEL                                                         
         BNE   NEXT                                                             
         USING CTDSCD,R4                                                        
         MVC   IDNUM,CTDSC                    STORE ID NUM                      
         DROP  R4                                                               
*                                                                               
         LA    R4,IO                          RESET POINTER                     
         MVI   ELCODE,X'B4'                   ELEMENT WITH CTRY CODE            
         BAS   RE,GETEL                                                         
         BNE   NEXT                                                             
         USING CTAGDD,R4                      AGENCY GROUP DETAILS              
         USING CTRYTABD,R5                                                      
         LA    R5,CTRYTAB0                                                      
COMPARE  CLC   CTAGDCTY,CTRYCODE              COMPARE COUNTRY CODE              
         BE    MATCH                                                            
         LA    R5,CTRYTABL(R5)                BUMP R5 IF NO MATCH               
         B     COMPARE                                                          
MATCH    CLC   CTRYSHR(3),=C'USA'             IS COUNTRY USA?                   
         BNE   NEXT                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
         USING CT5REC,R4                                                        
         LA    R4,IO                                                            
         MVC   ACODE(2),CT5KALPH              PRINT ACCESS CODE                 
         DROP  R4                                                               
*                                                                               
*                                          SET UP KEY FOR CTIREC                
*                                                                               
         USING CTIKEY,R4                                                        
         LA    R4,KEY2                                                          
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,IDNUM                                                    
*                                                                               
         LA    R4,IO                                                            
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY2,IO               
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         MVC   PDEST(24),=C'NO MATCHING RECORD FOUND'                           
         B     PRINT                                                            
*                                                                               
         MVI   ELCODE,X'02'                ID ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   DEST                                                             
         USING CTDSCD,R4                                                        
         ZIC   RE,CTDSCLEN                 RE = ELEMENT LENGTH                  
         LA    R0,CTDSCELQ                 R0 = OVERHEAD LENGTH                 
         SR    RE,R0                       RE = LENGHT ID                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PID(0),CTDSC                PRINT ID NAME                        
*                                                                               
DEST     LA    R4,IO                       RESET POINTER                        
         MVI   ELCODE,X'30'                DESTINATION ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   PRINT                                                            
         USING CTDSTD,R4                                                        
         MVC   PDEST,CTDSTNAM              PRINT DESTINATION                    
PRINT    GOTO1 =V(PRINTER)                                                      
*                                                                               
OLDKEY   GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IO                
*                                                                               
         LA    R4,IO                       RESET POINTER                        
         MVI   ELCODE,X'21'                SYSTEM ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   NEXT                                                             
         USING CTSYSD,R4                                                        
         USING SELISTD,R8                  SYSTEM EXECUTIVE LIST                
SYS      L     R8,=V(SELIST)               BEGINNING OF TABLE                   
         ZICM  R6,0(R8),2                  R6 = LENGTH OF ENTRY                 
         L     R7,2(R8)                    R7 = A(EOT)                          
         LA    R8,6(R8)                    R8 = ADDRESS OF 1ST ENTRY            
LOOP2    CLC   CTSYSSE,SESYS                                                    
         BNE   NOMATCH                                                          
         MVC   PSENAME,SENAME              PRINT SENAME                         
         GOTO1 =V(PRINTER)                                                      
NOMATCH  BXLE  R8,R6,LOOP2                                                      
*                                                                               
         MVI   ELCODE,X'21'                GET NEXT SYS ELEMENT                 
         BAS   RE,NEXTEL                                                        
         BNE   NEXT                                                             
         B     SYS                                                              
         DROP  R4                                                               
                                                                                
*                                                                               
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IO                
         B     LOOP                                                             
*                                                                               
GOODBYE  XBASE                                                                  
         EJECT                                                                  
* DON'T WORRY ABOUT THESE NEXT TWO FIELDS                                       
*                                                                               
UTL      DC    F'0',X'0A'          FOR OFFLINE DATAMGR                          
SSB      DC    F'0'                FOR OFFLINE DATAMGR                          
*                                                                               
*                                                                               
         GETEL R4,28,ELCODE                                                     
ELCODE   DS    X                                                                
IDNUM    DS    XL2                                                              
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
KEY      DS    CL25                CTFILE KEY                                   
KEY2     DS    CL25                                                             
IO       DS    1000X               I/O AREA FOR CTFILE                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FACTRYTAB                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE DDDPRINT                                                       
         ORG    P                                                               
ACODE    DS     CL11                                                            
         DS     CL2                                                             
PID      DS     CL10                                                            
         DS     CL2                                                             
PDEST    DS     CL33                                                            
         DS     CL2                                                             
PSENAME  DS     CL7                                                             
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE FACTRY                                                         
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094MZEIDMGR9 10/02/98'                                      
         END                                                                    
