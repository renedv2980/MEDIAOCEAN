*          DATA SET DEMRGTOOL  AT LEVEL 005 AS OF 10/31/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMRGTOA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'DEMRGTO - BUILD TOOLIN DD FOR GENERALIZED DEMOS MERGE'          
***********************************************************************         
*                                                                     *         
* THIS MODULE IS A COMPONENT OF THE GENERALIZED DEMO MERGE UTILITY.   *         
* IT READS A DATASET WHICH HAS BEEN PRODUCED BY A CONVERSION PROGRAM, *         
* AND WHICH IS THE INPUT TO A MAJOR/MINOR KEY MERGE.                  *         
*                                                                     *         
* ICETOOL IS THE PRIMARY UTILITY WHICH WILL PERFORM THE MERGE.        *         
* PAN MEMBER DEMRGICEG CONTAINS ALL OF THE CONTROL CARDS FOR ICETOOL  *         
* AND DFSORT TO PERFORM THE MERGE. ICETOOL'S CONTROL CARDS MUST BE IN *         
* A DATASET CALLED "TOOLIN", BUT THERE ARE ACTUALLY TWO DIFFERENT     *         
* WAYS THAT THIS MERGE CAN OPERATE, AND WE NEED TO READ THE           *         
* CONVERSION OUTPUT TO FIGURE OUT WHICH TOOLIN OPERATORS WILL BE      *         
* NEEDED TO DO THE MERGE.                                             *         
*                                                                     *         
* THIS MODULE READS THE CONVERSION OUTPUT DATASET, LOOKING FOR ONE OF *         
* THE FOLLOWING:                                                      *         
*   1. A "SIGNAL" RECORD, INDICATING A FULL REPLACEMENT DATA RANGE    *         
*   2. A RECORD WHICH IS MARKED FOR DELETION                          *         
* IF EITHER OF THESE TWO CASES IS TRUE, THEN ICETOOL MUST PERFORM A   *         
* MUCH MORE SOPHISTICATED MERGE ALGORITHM, AS DEFINED IN COMPONENT    *         
* MEMBER "TOOLINR" WITHIN PAN MEMBER DEMRGICEG. OTHERWISE, WE DO NOT  *         
* NEED TO SUPPORT FULL REPLACEMENT MODE IN THE MERGE (WHICH IS MUCH   *         
* QUICKER), AND WE READ COMPONENT MEMBER "TOOLINN" INSTEAD.           *         
*                                                                     *         
* ULTIMATELY, THE PURPOSE OF THIS MODULE IS TO READ MEMBER TOOLINR    *         
* **OR** TOOLINN, COPY THOSE RECORDS TO TOOLIN, AND INVOKE ICETOOL    *         
* PROGRAMMATICALLY VIA THE TOOLIN INTERFACE.                          *         
*                                                                     *         
* THE RETURN CODE FROM ICETOOL IS PASSED BACK VIA THE XBASE MACRO,    *         
* SO THAT THE JCL CAN REACT ACCORDINGLY.                              *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
DEMRGTO  CSECT                                                                  
         NBASE 0,*DEMRGTO,=V(REGSAVE)                                           
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(32),=C'DEMOFILE MINOR KEY MERGE UTILITY'                   
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         LA    R1,FULL                                                          
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R2 = A(ASSB)                                
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     R1,ASSBJSAB-ASSB(R2) R1 = A(JSAB)                                
         PACK  DUB,JSABJBID-JSAB+3(5,R1)  JOB#####                              
         SAM24 ,                   SWITCH BACK TO 24-BIT MODE                   
         OI    DUB+7,X'0F'                                                      
         UNPK  TITLE+43(5),DUB                                                  
         LA    R2,FULL                                                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   TITLE+33(8),0(R2)   JOBNAME                                      
         MVC   TITLE+41(2),=C'(J'                                               
         SR    R1,R1                                                            
         MVI   TITLE+48,C')'                                                    
*                                                                               
         MVC   P(25),=C'OPENING CONVERSION OUTPUT'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OPEN  INFILE              THE CONVERSION OUTPUT FILE                   
*                                                                               
         MVI   ELCODE,MRGRCDEQ     LOOK FOR REPLACEMENT ELEMENTS                
NEXTREC  DS    0H                                                               
         GET   INFILE,RDW                                                       
*                                                                               
         LA    R3,IO               A(RECORD)                                    
         TM    22(R3),X'80'        IS KEY MARKED FOR DELETION?                  
         BZ    CHKELEM             NO                                           
         MVC   P(34),=C'KEY MARKED FOR DELETION IS PRESENT'                     
         GOTO1 =V(PRINTER)                                                      
         B     REPLACE             MUST DO REPLACEMENT MODE                     
*                                                                               
CHKELEM  DS    0H                                                               
         BAS   RE,GETEL            IS THERE A REPLACEMENT ELEMENT?              
         BNE   NEXTREC             NO                                           
*                                                                               
         USING MRGRELEM,R3                                                      
         MVC   P(L'MRGREYEC),MRGREYEC   YES: PRINT EYE CATCHER                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   MRGREYEC,=C'*REPLACEMENT MODE REQUIRED*'                         
         BE    REPLACE             OVERRIDE DDNAME (FORCE REPLACEMENT)          
         CLC   MRGREYEC,=C'**NO REPLACEMENT ELEMENTS**'                         
         BE    EOFIN               DEFAULT DDNAME (NO REPLACEMENT)              
         CLC   MRGREYEC,=C'*MERGE REPLACEMENT ELEMENT*'                         
         BE    *+6                                                              
         DC    H'0'                THIS IS A RESERVED ELEMENT CODE!             
*                                                                               
REPLACE  DS    0H                                                               
         MVC   P(39),=C'REPLACEMENT MODE WILL BE USED FOR MERGE'                
         GOTO1 =V(PRINTER)                                                      
         MVC   TOOLINDD,=C'TOOLINR '  DO REPLACEMENT MODE                       
         DROP  R3                                                               
*                                                                               
EOFIN    DS    0H                                                               
         CLOSE INFILE                                                           
*                                                                               
         MVC   P(25),=C'CLOSING CONVERSION OUTPUT'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* READ THE APPROPRIATE MEMBER AND COPY ALL THE ICETOOL CONTROL CARDS            
* TO TOOLIN.                                                                    
*                                                                               
         MVC   P(23),=C'CREATING TOOLIN DATASET'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    RF,TOOLINX          A(TOOLIN DCB)                                
         MVC   DCBDDNAM-IHADCB(8,RF),TOOLINDD   OVERRIDE THE DDNAME             
*                                                                               
         OPEN  TOOLINX                                                          
         OPEN  (TOOLIN,OUTPUT)                                                  
*                                                                               
NEXTCARD DS    0H                                                               
         GET   TOOLINX,CARD                                                     
         PUT   TOOLIN,CARD                                                      
         B     NEXTCARD                                                         
*                                                                               
EOFTOOL  DS    0H                                                               
         CLOSE TOOLINX                                                          
         CLOSE TOOLIN                                                           
*                                                                               
         MVC   P(15),=C'CALLING ICETOOL'                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    R1,R1               USE THE TOOLIN INTERFACE TO ICETOOL          
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LR    R4,RF               SAVE ICETOOL RETURN CODE IN R4               
*                                                                               
         MVC   P(26),=C'ICETOOL COMPLETED: RC = XX'                             
         EDIT  (R4),(2,P+24),ZERO=NOBLANK                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* R4 CONTAINS THE RETURN CODE FROM ICETOOL, WHICH IS THE HIGHEST RETURN         
* CODE FROM ALL OF THE TOOLIN OPERATORS. THE EXPECTATION IS THAT ANY            
* NON-ZERO RETURN CODE REPRESENTS A FATAL ERROR REQUIRING EXAMINATION.          
*                                                                               
         XBASE RC=(R4)             RETURN WITH ICETOOL RETURN CODE              
         EJECT                                                                  
         GETEL R3,23,ELCODE                                                     
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
INFILE   DCB   DDNAME=INFILE,DSORG=PS,RECFM=VB,LRECL=2000,             +        
               MACRF=GM,EODAD=EOFIN                                             
*                                                                               
TOOLINX  DCB   DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,EODAD=EOFTOOL                
*                                                                               
TOOLIN   DCB   DDNAME=TOOLIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM                
         SPACE 3                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
WORK     DS    CL17                                                             
CARD     DS    CL80                                                             
ELCODE   DS    X                                                                
TOOLINDD DC    C'TOOLINN '         ASSUME NON-REPLACEMENT MODE                  
*                                                                               
RDW      DS    F                                                                
IO       DS    2000X                                                            
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 3                                                                
         PRINT OFF                                                              
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DEMRGTOOL 10/31/19'                                      
         END                                                                    
