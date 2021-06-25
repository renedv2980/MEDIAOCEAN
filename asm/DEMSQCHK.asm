*          DATA SET DEMSQCHK   AT LEVEL 002 AS OF 11/04/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DESEQCKA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'DEMOGRAPHICS: POST-CONVERSION SEQUENCE CHECK'                   
***********************************************************************         
*                                                                     *         
* Perform a pre-update integrity check on a demographics input file   *         
* to a VSAM update. We must catch situations where a conversion might *         
* be relying (deliberately or otherwise) on some of the "features" of *         
* the DELDXMOD update algorithm (pertaining to the handling of        *         
* duplicate keys). Such "features" *might* result in occasional       *         
* errors in a DANDX update, but they *will* result in incorrect       *         
* results under VSAM. We look for:                                    *         
*                                                                     *         
*  1. Duplicate passive keys (extended or otherwise). RC is set to 8. *         
*  2. Non-contiguous groups of demo records with the same major key.  *         
*      RC is set to 8.                                                *         
*  3. Contiguous groups of demo records with the same major key (aka  *         
*      "minor key sequence error"). RC is set to 16.                  *         
*  4. Groups of demo records with too many duplicate minor keys       *         
*      within a given major key. RC is set to 8.                      *         
*                                                                     *         
* If no issues are found, RC is set to 0.                             *         
*                                                                     *         
***********************************************************************         
DESEQCHK CSECT                                                                  
       PRINT NOGEN                                                              
*                                                                               
       ENTRY SSB                                                                
*                                                                               
       NBASE 0,DESEQCHK,=V(REGSAVE)                                             
*                                                                               
       USING DPRINT,RA                                                          
       L     RA,=V(CPRINT)                                                      
*                                                                               
       BAS   RE,INIT             INITIALIZE                                     
*                                                                               
       BAS   RE,READCRDS         READ PARAMETER CARDS                           
       JNE   *+2                 INVALID PARAMETER CARD SEEN                    
*                                                                               
       MVI   P,0                 SKIP A LINE                                    
       GOTO1 =V(PRINTER)                                                        
*                                                                               
       GOTO1 =V(DYNALLOC),DMCB,(C'D',=CL8'IN'),CARD                             
       CLI   DMCB+4,0            IS DDNAME "IN" ALLOCATED?                      
       JZ    *+2                 NO ?!?                                         
*                                                                               
       LLC   R1,DMCB+4           L'RETURNED DSN                                 
       BCTR  R1,0                                                               
       EX    R1,*+8                                                             
       B     *+10                                                               
       MVC   DSN(0),CARD                                                        
       MVC   P(30),=C'PERFORMING INTEGRITY CHECK ON '                           
       MVC   P+30(L'DSN),DSN                                                    
       GOTO1 =V(PRINTER)                                                        
*                                                                               
       BAS   RE,CALLICE          PERFORM THE INTEGRITY CHECK                    
*                                                                               
       SELECT CHI,R3,EQ          R3 = ICETOOL RETURN CODE                       
         WHEN (0)                RETURN CODE 0: NO ERRORS FOUND                 
           MVC  P(15),=C'NO ERRORS FOUND'                                       
         WHEN (8)                RETURN CODE 8: ERROR(S) FOUND                  
           MVC  P(40),=C'** ERROR(S) FOUND: SEE "SHOWDUPS" OUTPUT'              
           NEXTWHEN ,                                                           
         WHEN (16)               RETURN CODE 16: ERROR(S) FOUND                 
           MVC  P(40),=C'** ERROR(S) FOUND: MINOR KEY SEQ. ERROR.'              
           NEXTWHEN ,                                                           
         WHEN (8,16)             SEND AUTONOTE FOR ANY ERROR                    
           IF (CLI,WARNFLAG,EQ,C'Y')                                            
             GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',('MAILMS1Q',MAILMSG1)             
           ENDIF ,                                                              
         OTHRWISE ,                                                             
           J  *+2                UNEXPECTED ICETOOL RETURN CODE !               
       ENDSEL ,                                                                 
       GOTO1 =V(PRINTER)                                                        
*                                                                               
       XBASE RC=(R3)             SET RETURN CODE UPON EXIT                      
*                                                                               
       EJECT                                                                    
*                                                                               
*======================================================================         
*                                                                               
* INITIALIZE                                                                    
*                                                                               
INIT   NTR1  ,                                                                  
*                                                                               
       LA    R2,FULL                                                            
       EXTRACT (2),'S',FIELDS=(ASID)                                            
       LA    R1,FULL                                                            
       L     R2,0(R1)                                                           
       LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                       
       L     R2,ASCBASSB-ASCB(R1) R2 = A(ASSB)                                  
       SAM31 ,                   SWITCH TO 31-BIT MODE                          
       L     R1,ASSBJSAB-ASSB(R2) R1 = A(JSAB)                                  
       MVC   MAILJOB#,JSABJBID-JSAB(R1)  JOBID (E.G., JOB12345)                 
       SAM24 ,                   SWITCH BACK TO 24-BIT MODE                     
       LA    R2,FULL                                                            
       EXTRACT (2),FIELDS=TIOT                                                  
       L     R2,FULL                                                            
       MVC   MAILJOBN,0(R2)      JOBNAME                                        
*                                                                               
       MVC   TITLE(27),=C'POST-CONVERSION CHECK: JOB '                          
       MVC   TITLE+27(8),MAILJOBN                                               
       MVC   TITLE+35(2),=C'(J'                                                 
       MVC   TITLE+37(5),MAILJOB#+3                                             
       MVI   TITLE+42,C')'                                                      
*                                                                               
       J     XIT                                                                
*                                                                               
       EJECT                                                                    
*                                                                               
*======================================================================         
*                                                                               
* READ PARAMETER CARDS                                                          
*                                                                               
READCRDS NTR1  ,                                                                
*                                                                               
       MVC   P(15),=C'PARAMETER CARDS'                                          
       GOTO1 =V(PRINTER)                                                        
       MVC   P(15),=C'---------------'                                          
       GOTO1 =V(PRINTER)                                                        
*                                                                               
       DO  INF                     READ CARDS UNTIL EOF                         
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE10'                                     
*                                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)         PRINT THE PARAMETER CARD                     
*                                                                               
         DOEXIT (CLC,=C'/*',EQ,CARD)   EXIT ON EOF                              
*                                                                               
         IF (CLI,CARD,EQ,C'*')     COMMENT?                                     
           ITERATE ,                                                            
*                                                                               
         ELSEIF (CLC,=C'DDSIO=',EQ,CARD)                                        
           L     RF,=V(DDSIO)                                                   
           MVC   0(8,RF),CARD+6                DDSIO= OVERRIDE                  
           ITERATE ,                                                            
*                                                                               
         ELSEIF (CLC,=C'DSPACE=',EQ,CARD)                                       
           LA    RF,SSB                                                         
           MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE= OVERRIDE                 
           ITERATE ,                                                            
                                                                                
         ELSEIF (CLC,=C'WARN=N',EQ,CARD)                                        
           MVI   WARNFLAG,C'N'                 WARN= OVERRIDE                   
           ITERATE ,                                                            
                                                                                
         ELSE ,                                                                 
           J     NO                  **UNSTRUCTURED EXIT** INVALID CARD         
                                                                                
         ENDIF ,                                                                
*                                                                               
       ENDDO ,                                                                  
*                                                                               
       J     YES                 **UNSTRUCTURED EXIT** ALL CARDS VALID          
*                                                                               
       ANSR  ,                                                                  
       EJECT                                                                    
*                                                                               
*======================================================================         
*                                                                               
* CALL ICETOOL                                                                  
*                                                                               
CALLICE  NTR1  ,                                                                
*                                                                               
         MVC   P(42),=C'CALLING ICETOOL TO PERFORM INTEGRITY CHECK'             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    R1,R1               USE TOOLIN INTERFACE                         
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LR    R3,RF               SAVE ICETOOL RETURN CODE IN R3               
*                                                                               
         MVC   P(22),=C'ICETOOL RETURN CODE = '                                 
         EDIT  (R3),(2,P+22),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
WORK     DS    CL17                FOR EDIT MACRO                               
WARNFLAG DC    C'Y'                DEFAULT TO WARN=YES                          
DSN      DC    CL44' '             "IN" DSN                                     
MAILJOBN DS    CL8                 MVS JOBNAME                                  
*                                                                               
MAILMSG1 DC    C'AUTONOTE*'                                                     
         DC    C'US-MFDemosProgrammers,US-OperationsSupportNY:'                 
         DC    C'('                                                             
MAILJOB# DS    CL8                 MVS JOB NUMBER                               
         DC    C') '                                                            
         DC    C'DEMOS PRE-UPDATE INTEGRITY CHECK FAILURE'                      
MAILMS1Q EQU   *-MAILMSG1                                                       
*                                                                               
         DS    0D                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DEMSQCHK  11/04/19'                                      
         END                                                                    
