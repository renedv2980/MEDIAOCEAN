*          DATA SET DDPAPTLBCX AT LEVEL 004 AS OF 07/12/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTLBCA                                                                 
*INCLUDE REGSAVE                                                                
***********************************************************************         
*                                                                     *         
* PANAPT CONTAINS A LIBCODE EXTRACT FACILITY VIA PROC APCS5970. THIS  *         
* IS DOCUMENTED IN CHAPTER 6 OF THE PANAPT ADMINISTRATOR GUIDE,       *         
* VERSION 3.1.                                                        *         
*                                                                     *         
* APCS5970 REQUIRES AN INPUT DATASET OF VARIABLE-LENGTH SPANNED       *         
* RECORD FORMAT. THE DATASET CONTAINS A SINGLE "REQUEST RECORD". THIS *         
* MODULE GENERATES THAT REQUEST RECORD. IT NEVER NEEDS TO BE MODIFIED *         
* OR RERUN AS LONG AS WE ARE RUNNING PANAPT 3.1. IT MAY NEED TO BE    *         
* UPDATED FOR FUTURE PANAPT RELEASES.                                 *         
*                                                                     *         
* UPDATE FROM DEIS, JUL12/2018:                                       *         
*   DURING THE TERRIFYING FEW HOURS WHEN WE THOUGHT WE WERE GOING TO  *         
*   HAVE TO UPGRADE TO PANAPT 3.2 (DUE TO THE Z/OS 2.3 UPGRADE), DEIS *         
*   DID SOME EXPERIMENTATION WITH THIS MODULE, IN AN ATTEMPT TO       *         
*   PARAMETERIZE THE PANAPT VERSION STAMP. HE DISCOVERED (TO HIS      *         
*   AMUSEMENT) THAT THIS PROGRAM WORKS PERFECTLY REGARDLESS OF WHAT   *         
*   VERSION NUMBER (IF ANY) IS PRESENT IN APCSIREQ_VERSION_STAMP .    *         
*   THIS WOULD SEEM TO IMPLY THAT EVEN IF WE EVER DID HAVE TO UPGRADE *         
*   TO A HIGHER VERSION OF PANAPT, WE MIGHT STILL NOT NEED TO MODIFY  *         
*   THIS PROGRAM.                                                     *         
*                                                                     *         
* NOTE THAT THE BLKSIZE OF THE OUTPUT FILE SHOULD BE FORCED TO BE     *         
* LARGER THAN THE LRECL OF 20004. AS LONG AS THAT'S THE CASE, THE     *         
* "SPANNED" RECORD WON'T REALLY SPAN ACROSS MULTIPLE BLOCKS!          *         
*                                                                     *         
***********************************************************************         
         TITLE 'CREATE PANAPT BATCH INTERFACE INPUT RECORD'                     
         PRINT NOGEN                                                            
PAPTLBCX CSECT                                                                  
         NBASE 0,PAPTLBCX,=V(REGSAVE)                                           
*                                                                               
         LA    R2,APCSIREQ_RECORD_TYPE  START OF DATA                           
         LHI   R3,DATALENQ                                                      
         ICM   R4,15,*             ANY VALID ADDRESS                            
         ICM   R5,15,=X'40000000'  FILL WITH BLANKS                             
         MVCL  R2,R4                                                            
         JO    *+2                 DIE ON DESTRUCTIVE MOVE                      
*                                                                               
         USING IHADCB,REQFILE                                                   
         MVC   DCBLRECL,=Y(DATALENQ+4)                                          
         MVC   DCBBLKSI,=Y(DATALENQ+8)                                          
*                                                                               
         MVC   APCSIREQ_RDW,=Y(DATALENQ+4)                                      
         MVC   APCSIREQ_RDW+2(2),=H'0'                                          
         MVC   APCSIREQ_RECORD_LEN,=A(DATALENQ)                                 
         MVC   APCSIREQ_RECORD_TYPE,=AL2(APCSIREQ_RECORD_IS_REQUEST)            
         MVC   APCSIREQ_USER_ID,=C'APT*****'                                    
         MVC   APCSIREQ_NUMBER,=C'000000'                                       
         MVC   APCSIREQ_VERSION_STAMP,=C'03.1'                                  
         MVI   APCSIREQ_PROCESS,APCSIREQ_PROCESS_IS_LLB                         
         MVI   APCSIREQ_BATCH_INPUT,C'Y'                                        
*                                                                               
         OPEN  (REQFILE,OUTPUT)                                                 
         PUT   REQFILE,APCSIREQ_INPUT_REQUEST                                   
         CLOSE REQFILE                                                          
*                                                                               
         XBASE ,                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
DATALENQ EQU   20000               AS PER PANAPT DOCUMENTATION                  
*                                                                               
REQFILE  DCB   DDNAME=REQFILE,DSORG=PS,RECFM=VS,MACRF=PM                        
         SPACE 3                                                                
       ++INCLUDE DDPAPTREQD                                                     
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDPAPTLBCX07/12/18'                                      
         END                                                                    
