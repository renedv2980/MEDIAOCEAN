*          DATA SET PPDRD03    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T40903A,+0    ** NOTE "A" APPENDED TO PHASE                              
         TITLE 'T40903  PRINTPAK LOGICAL FILE MAINT.  DST REC'                  
*                                                                               
****   CHANGE LOG                                                               
*                                                                               
*  SMYE  05/06/96  CHANGED DST REC ELEM AND RECORD LENGTHS                      
*                                                                               
T40903   CSECT                                                                  
         NMOD1 0,T40903                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T409FFD,RA                                                       
         USING PDSTREC,R9                                                       
         EJECT                                                                  
         MVC   PDSTKEY,KEY                                                      
****     MVC   PDSTELEM(2),=X'058C'                                             
****     MVC   PDSTLEN(2),=X'00AD'                                              
         MVC   PDSTELEM(2),=X'0520'    NEW RECORD AND ELEMENT LENGTHS           
         MVC   PDSTLEN(2),=X'0041'      (FROM 140 + 33  TO  32 + 33)            
         CLI   BACT,1                                                           
         BE    DSTSCRN                                                          
         MVC   KEY+27(4),DSTADDR                                                
         BAS   RE,GETREC                                                        
DSTSCRN  CLI   BYTE2,0                                                          
         BNE   FORMAT                                                           
*            DISTRICT SCREEN IN TWA SO EDIT IT UNLESS                           
*                    ACTION = DISPLAY                                           
         CLI   BACT,3                                                           
         BE    FORMAT                                                           
         LA    R2,DSTNAMEH                                                      
         BAS   RE,ANY                                                           
         XC    PDSTNAME,PDSTNAME                                                
         MVC   PDSTNAME,DSTNAME                                                 
         CLI   BACT,1                                                           
         BNE   DSTCHG                                                           
         BAS   RE,ADDREC                                                        
         B     DONE                                                             
*                                                                               
DSTCHG   BAS   RE,PUTREC                                                        
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
FORMAT   LA    R6,HDRLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90409F3'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRERR                                                           
         CLI   BACT,1                                                           
         BNE   PUTFLD                                                           
         FOUT  DSTNAMEH,SPACES,20                                               
         LA    R2,DSTNAMEH                                                      
         B     EXIT                                                             
*                                                                               
PUTFLD   FOUT  DSTNAMEH,PDSTNAME,20                                             
         CLI   BACT,2                                                           
         BNE   DONE                                                             
         LA    R2,DSTNAMEH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
SPACES   DC    CL40' '                                                          
VIRERR   DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENEROL                                                       
*                                                                               
       ++INCLUDE GENOLD                                                         
       ++INCLUDE PDSTREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPDRDFFD                                                       
         ORG   HDRLAST                                                          
       ++INCLUDE PPDRDF3D                                                       
         ORG   T409FFD                                                          
         DS    CL16                                                             
BREC     DS    CL1                                                              
BACT     DS    CL1                                                              
OLNUM    DS    CL1                                                              
DIVADDR  DS    F                                                                
REGADDR  DS    F                                                                
DSTADDR  DS    F                                                                
CPROFLE  DS    CL20                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPDRD03   05/01/02'                                      
         END                                                                    
