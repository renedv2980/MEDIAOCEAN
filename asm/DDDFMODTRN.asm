*          DATA SET DDDFMODTRN AT LEVEL 003 AS OF 08/01/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DFMODTRA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
***********************************************************************         
*                                                                     *         
* THIS UTILITY READS "MUSTTEST=" CONTROL CARDS FROM SYSIN, AND        *         
* BUILDS THE NECESSARY DFSORT CONTROL CARDS TO REPLACE THE            *         
* CORRESPONDING MODS PRODUCTION EXIT NAMES WITH THE SUPPLIED TEST     *         
* EXIT NAMES. THIS ALLOWS US TO USE THE PRODUCTION NAMES IN THE       *         
* DFSORT SOURCE, AND OVERRIDE THESE NAMES WITH TEST NAMES DURING      *         
* DEVELOPMENT.                                                        *         
*                                                                     *         
* THIS UTILITY ASSUMES THAT THE PRODUCTION EXIT NAME WILL BE EXACTLY  *         
* SEVEN CHARACTERS IN LENGTH.                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'DFMODTRN - SUPPORT TEST VERSIONS OF DFSORT EXITS'               
         PRINT NOGEN                                                            
DFMODTRN CSECT                                                                  
         NBASE 0,DFMODTRN,=V(REGSAVE)                                           
*                                                                               
         OPEN  (CNTLCRDS,OUTPUT)                                                
*                                                                               
         LA    R5,OUTREC_FINDREPS  BUILD (CONTINUED) OUTREC STMT HERE           
*                                                                               
NEXTIN   DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE10'  ***NOTE P2***                      
         CLC   =C'/*',CARD                                                      
         BE    EOF                                                              
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    NEXTIN              YES                                          
*                                                                               
         CLC   =C'MUSTTEST=',CARD  ONLY MUSTTEST= CARDS ARE SUPPORTED           
         BE    *+14                                                             
         MVC   RETCODE,=F'8'       INVALID PARAMETER CARD                       
         B     XBASE                                                            
*                                                                               
         CLI   CARD+16,C' '        IS THERE A TEST SUFFIX?                      
         BE    NEXTIN              NO: DON'T SUBSTITUTE ANY TEXT                
*                                                                               
         MVC   0(2,R5),=C'C'''     START "IN" TEXT                              
         MVC   2(7,R5),CARD+9      PRODUCTION EXIT NAME                         
         MVC   9(4,R5),=C''',C'''  END "IN" TEXT, START "OUT" TEXT              
         MVC   13(8,R5),CARD+9     PRODUCTION EXIT NAME                         
         MVC   21(2,R5),=C''','    END "OUT" TEXT (POSIT THERE'S MORE)          
         LA    R5,23(R5)           READY FOR NEXT SUBSTITUTION                  
         B     NEXTIN              GET NEXT INPUT CARD                          
*                                                                               
EOF      DS    0H                                                               
         C     R5,=A(OUTREC_BUFFER_X) BUFFER OVERFLOW?                          
         BL    *+6                                                              
         DC    H'0'                   YES: INCREASE OUTREC_BUFFER SIZE          
*                                                                               
         C     R5,=A(OUTREC_FINDREPS) ANYTHING PLACED IN THE BUFFER?            
         BE    BLDCOPY                NO: THERE'S NOTHING TO SUBSTITUTE         
*                                                                               
         BCTR  R5,0                BACK UP TO LAST COMMA                        
         MVC   0(16,R5),=C'),OVERRUN=TRUNC)'  END THE OUTREC STATEMENT          
*                                                                               
         LA    R5,OUTREC_BUFFER    A(COMPLETE OUTREC STATEMENT)                 
NEXTOUT  DS    0H                                                               
         MVC   CARD,=CL80' '       PRE-FILL OUTPUT RECORD WITH BLANKS           
         MVC   CARD+1(70),0(R5)    MOVE DATA OUT OF BUFFER                      
         LA    R5,70(R5)           BUMP TO REMAINING BUFFERED DATA              
         CLI   0(R5),C' '          IS THERE ANY MORE TO BUILD?                  
         BE    *+8                 NO                                           
         MVI   CARD+71,C'*'        YES: SET CONTINUATION CHARACTER              
         PUT   CNTLCRDS,CARD                                                    
         CLI   0(R5),C' '          ARE WE DONE?                                 
         BNE   NEXTOUT             NO                                           
*                                                                               
BLDCOPY  DS    0H                                                               
         MVC   CARD,=CL80' OPTION COPY'  ALWAYS GENERATE THIS CARD              
         PUT   CNTLCRDS,CARD                                                    
*                                                                               
XBASE    DS    0H                                                               
         CLOSE CNTLCRDS                                                         
*                                                                               
         XBASE RC=RETCODE                                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
CNTLCRDS DCB   DDNAME=CNTLCRDS,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM              
         SPACE 3                                                                
DMCB     DS    6F                                                               
RETCODE  DC    F'0'                RETURN CODE FROM PROGRAM                     
CARD     DS    CL80                                                             
*                                                                               
OUTREC_BUFFER DC 1000C' '          BUFFER TO BUILD OUTREC STATEMENT             
OUTREC_BUFFER_X EQU *                                                           
         ORG   OUTREC_BUFFER                                                    
         DC    C'OUTREC FINDREP=(INOUT=('                                       
OUTREC_FINDREPS EQU *                                                           
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDDFMODTRN08/01/19'                                      
         END                                                                    
