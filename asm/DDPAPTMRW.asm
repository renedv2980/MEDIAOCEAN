*          DATA SET DDPAPTMRW  AT LEVEL 009 AS OF 04/21/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTMRWA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SMTP                                                                   
*INCLUDE STXITER                                                                
         TITLE 'PANAPT: WARN PROGRAMMERS ABOUT TEMPORARY MOVE REQUESTS'         
PAPTMRW  CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,PAPTMRW,=V(REGSAVE)                                            
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(PAPTMRW),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
         MVC   TITLE,=CL60'PANAPT - WARN PROGRAMMERS ABOUT TEMPORARY MO+        
               VE REQUESTS'                                                     
*                                                                               
         OPEN  MRFILE                                                           
*                                                                               
* FILTER PARAMETERS ARE ASSUMED TO HAVE BEEN APPLIED TO THE APCS5955            
* JOB WHICH CREATES THE INPUT FILE TO THIS PROGRAM. THE ONLY MOVE               
* REQUESTS CONTAINED IN MRFILE ARE:                                             
*   MOVE TYPE = T                                                               
*   STATUS = MVS, AWP, OR AWPS                                                  
* IN OTHER WORDS, "TEMPORARY" MOVE REQUESTS WHICH HAVE BEEN PROMOTED            
* TO STGE. (DDPAPTMRM PREVENTS THE PROMOTION TO PROD OF A "T" MR.)              
*                                                                               
         XC    DMCB(24),DMCB       SO TABLE COUNTER IS CLEARED                  
         LA    R6,PAPTREC+4                                                     
         USING APAMMDES,R6                                                      
*                                                                               
NEXT     GET   MRFILE,PAPTREC                                                   
*                                                                               
         CLC   DESTYPE,=C'01'                                                   
         BNE   NEXT                                                             
*                                                                               
         MVC   SRTUSER,DESADDID    OWNER                                        
         MVC   SRTMR#,DESNUMB      MR #                                         
         MVC   SRTDESC,DESDESCR    DESCRIPTION                                  
         DROP  R6                                                               
*                                                                               
* DMCB+8 ALWAYS CONTAINS THE NUMBER OF RECORDS IN THE TABLE!!!                  
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,(X'01',SRTREC),MRTABLE,,SRTRECLQ,      +        
               SRTKEYLQ,MAXMRS                                                  
         B     NEXT                                                             
*                                                                               
CLOSE    CLOSE MRFILE                                                           
         EJECT                                                                  
         OC    DMCB+8(4),DMCB+8    NUMBER OF MOVE REQUESTS FOUND                
         BZ    XBASE               NONE                                         
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)    ATTACH AND INIT. JESMAIL         
*                                                                               
         LA    R6,MRTABLE                                                       
REC      USING SRTREC,R6                                                        
*                                                                               
NEXTUSER DS    0H                                                               
         MVC   WORKUSER,REC.SRTUSER                                             
         MVC   WORK,SPACES         BUILD RECIPIENT LIST IN WORK                 
         LA    RF,WORK                                                          
         LA    RE,WORKUSER         CURRENT USER                                 
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&UK*&& MVC   0(4,RF),=C'DDLO'                                                 
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPRS',WORK),(L'SUBJECT,SUBJECT)              
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MESSAGE)   FIRST BODY LINE             
*                                                                               
NEXT4YOU DS    0H                                                               
         MVC   PRTUSER,REC.SRTUSER                                              
         MVC   PRTMR#,REC.SRTMR#                                                
         MVC   PRTDESC,REC.SRTDESC                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTMR#),REC.SRTMR#                                        
         MVC   WORK+L'SRTMR#+3(L'SRTDESC),REC.SRTDESC 3 BLANKS BTWN COL         
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',WORK)                                  
*                                                                               
         CLC   REC.SRTUSER,REC.SRTUSER+SRTRECLQ  NEXT MR FOR SAME USER?         
         BNE   *+12                NO                                           
         LA    R6,SRTRECLQ(R6)     BUMP TO NEXT MR TABLE ENTRY                  
         B     NEXT4YOU                                                         
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPASND',0)   SEND THE BUFFERED E-MAIL          
*                                                                               
         LA    R6,SRTRECLQ(R6)     BUMP TO NEXT MR TABLE ENTRY                  
         CLC   REC.SRTUSER,SPACES  ANY MORE MOVE REQUESTS?                      
         BNE   NEXTUSER            YES                                          
         DROP  REC                                                              
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)   DETACH JESMAIL                    
*                                                                               
XBASE    DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DMCB     DS    6F                                                               
NUMMRS   DS    F                   NUMBER OF MOVE REQUESTS FOUND                
WORKUSER DC    CL8' ',C' '                                                      
*                                                                               
MRFILE   DCB   DDNAME=MRFILE,DSORG=PS,MACRF=GM,EODAD=CLOSE                      
*                                                                               
SRTREC   DS    0C                                                               
SRTUSER  DS    CL8                 MR OWNER                                     
SRTMR#   DS    CL6                 MR NUMBER                                    
SRTKEYLQ EQU   *-SRTREC                                                         
SRTDESC  DS    CL55                MR DESCRIPTION                               
SRTRECLQ EQU   *-SRTREC                                                         
*                                                                               
SUBJECT  DC    CL80'PANAPT WARNING! TEMPORARY MOVE REQUESTS ARE ACTIVE'         
MESSAGE  DC    CL80'THE FOLLOWING MOVE REQUESTS ARE OF MOVE TYPE "T", A+        
               ND MAY NEED TO BE BACKED OUT:'                                   
WORK     DS    CL80                                                             
*                                                                               
PAPTREC  DS    2000C               EXTRACTED PANAPT RECORD                      
         EJECT                                                                  
         DS    0L                                                               
         DC    CL16'*****SSB********'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*MRTABLE'                                                      
MRTABLE  DC    (MAXMRS+1)CL(SRTRECLQ)' '     TABLE OF MOVE REQUESTS             
* THERE IS NO THEORETICAL MAXIMUM NUMBER OF ENTRIES IN THIS TABLE.              
* HOWEVER (AS OF APR/11), DDSMTP CAN'T HANDLE MORE THAN 200 LINES OF            
* TEXT IN ITS BUFFER. SO WE'RE CAPPING THIS TABLE SIZE TO PREVENT A             
* BUFFER OVERFLOW DEATH IN DDSMTP.                                              
MAXMRS   EQU   150                                                              
         EJECT                                                                  
         PRINT GEN                                                              
         APAMMDES                                                               
         PRINT NOGEN                                                            
         SPACE 3                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   P                                                                
PRTUSER  DS    CL8                                                              
         DS    CL3                                                              
PRTMR#   DS    CL6                                                              
         DS    CL3                                                              
PRTDESC  DS    CL55                                                             
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDPAPTMRW 04/21/16'                                      
         END                                                                    
