*          DATA SET ACREPZZ02  AT LEVEL 119 AS OF 03/09/00                      
*PHASE ACZZ02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'ACZZ - CHECK TEMPO X-REF RECORDS'                               
***********************************************************************         
*              OPT3:' '=                                              *         
*              OPT7:'Y'=PRINTABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
ACZZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZZ**,R8,R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZZ02D,RC                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                INITIALIZATIONS FOR THE RUN.                 
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         LA    R0,PKNUM                                                         
         LA    R1,PKFLDS                                                        
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,P                                                             
*                                                                               
         USING TSXRECD,R3                                                       
         LA    R3,SVKEY                                                         
         MVC   SVKEY,SPACES                                                     
         MVI   TSXKTYP,TSXKTYPQ             X'3E'                               
         MVI   TSXKSUB,TSXKSUBQ             X'13'                               
         MVC   TSXKCPY,RCCOMPFL             COMPANY CODE                        
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         B     REQF10                                                           
*                                                                               
REQF05   GOTO1 =A(DMSEQDR),DMCB,(RC)       READ HIGH                            
REQF10   CLC   SVKEY(TSXKPER-TSXKEY),IOKEY  SAME KEY??                          
         BNE   REQFX                                                            
*                                                                               
         LA    R3,IOKEY                                                         
         CLC   TSXKEND,=X'A00109'                                               
         BNE   REQF05                                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET RECORD                            
         LA    R3,IO                                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PPER,TSXKPER        PERSON CODE                                  
         MVC   PAUL,=C'1R'                                                      
         MVC   PAODS,TSXKODS       OFF/DPT/SUBD                                 
         MVC   PAPER,TSXKPER       PERSON                                       
         GOTO1 DATCON,DMCB,(1,TSXKEND),(X'20',PPEREND)                          
*                                                                               
         LA    R2,TSXRFST          POINT TO FIRST ELEMENT                       
REQF20   CLI   0(R2),0                                                          
         BE    REQF05                                                           
         CLI   0(R2),TIMELQ        X'8B' - TIME ELEMENT                         
         BE    REQF40                                                           
REQF30   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF20                                                           
*                                                                               
         USING TIMELD,R2                                                        
REQF40   CLI   TIMETYP,TIMEXREF    RIGHT ELEMENT?                               
         BNE   REQF30                                                           
         EDIT  TIMXPED#,PPER#                                                   
         GOTO1 DATCON,DMCB,(1,TIMXPSDT),(X'20',PPERSTR)                         
         CLI   TIMXPED#,X'02'      PERIOD SHOULD BE #1                          
         BE    REQF50                                                           
         CLC   TIMXPSDT,=X'A00103'                                              
         BE    REQF50                                                           
         CLI   QOPT1,C'Y'          INCLUDE ALL                                  
         BNE   REQF30                                                           
         AP    PKGDCNT,=P'1'       ADD 1 TO COUNTER                             
         MVC   PDESC,=CL40'RECORD IS FINE - NOT CHANGED'                        
         GOTO1 ACREPORT                                                         
         B     REQF05                                                           
*                                                                               
REQF50   DS    0H                                                               
         MVC   MSG,=CL10'D/A IN'                                                
         GOTO1 ADUMP,DMCB,(RC),SVDA,L'SVDA                                      
*                                                                               
         MVC   MSG,=CL10'REC IN'                                                
         SR    R6,R6                                                            
         ICM   R6,3,TSXRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
*                                                                               
         AP    PKBDCNT,=P'1'       ADD 1 TO COUNTER                             
         MVI   TIMXPED#,X'01'       FIX PERIOD NUMBER                           
         MVC   TIMXPSDT,=X'A00101'  FIX START DATE                              
         EDIT  TIMXPED#,NPPER#                                                  
         GOTO1 DATCON,DMCB,(1,TIMXPSDT),(X'20',NPPERSTR)                        
         MVC   PDESC,=CL40'PERIOD # AND/OR START DATE WRONG - FIXED'            
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   MSG,=CL10'REC OUT'                                               
         SR    R6,R6                                                            
         ICM   R6,3,TSXRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
*                                                                               
         MVC   MSG,=CL10'D/A OUT'                                               
         GOTO1 ADUMP,DMCB,(RC),SVDA,L'SVDA                                      
*                                                                               
         CLI   QOPT2,C'Y'                 DO WE WANT TO UPDATE?                 
         BNE   REQF05                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    REQF05                                                           
         GOTO1 =A(DMPUTREC),DMCB,(RC)     WRITE RECORD BACK                     
         CLI   DMCB+8,0                   DUMP ON ERROR                         
         BZ    REQF05                                                           
         DC    H'0'                                                             
*                                                                               
REQFX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   RCSUBPRG,2                                                       
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,P                                                             
         MVC   P,SPACES                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+5(20),=CL20'RECORD COUNT'                                      
         MVC   PSECOND+5(20),=CL20'------------'                                
         GOTO1 ACREPORT                                                         
         MVC   PRECS,=CL30'RECORDS UNCHANGED :'                                 
         EDIT  PKGDCNT,PRECCNT                                                  
         GOTO1 ACREPORT                                                         
         MVC   PRECS,=CL30'RECORDS   CHANGED :'                                 
         EDIT  PKBDCNT,PRECCNT                                                  
         GOTO1 ACREPORT                                                         
         MVC   PRECS,=CL30'TOTAL RECORDS READ:'                                 
         ZAP   PKTOTAL,PKGDCNT                                                  
         AP    PKTOTAL,PKBDCNT                                                  
         EDIT  PKTOTAL,PRECCNT                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* GETEL2                                                             *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R4,DISP2,ELCODE,2                                               
         EJECT                                                                  
**********************************************************************          
* TAPE NAMES & ADDRESS CONSTANTS                                     *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(PRNTBL)                                                        
         DC    A(DUMP)             DUMP ROUTINE                                 
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZZ02D  DSECT                                                                  
VTYPES   DS    0A                                                               
PRNTBL   DS    A                                                                
ADUMP    DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DISP2    DS    H                                                                
*                                                                               
MSG      DS    CL10                MESSAGE FOR PRINTABLE                        
ELCODE   DS    CL1                                                              
COMMAND  DS    CL6                                                              
*                                                                               
FLAG     DS    CL1                                                              
*                                                                               
SVDA     DS    CL4                                                              
SVACT    DS    CL12                                                             
SVKEY    DS    CL42                                                             
*                                                                               
PKFLDS   DS    0PL8                                                             
PKTOTAL  DS    PL8                 TOTAL RECORD COUNT                           
PKGDCNT  DS    PL8                 TOTAL RECORDS UNCHANGED                      
PKBDCNT  DS    PL8                 TOTAL RECORDS CHANGED                        
PKNUM    EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* 'PRINT LINE DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PL       DS    CL5                                                              
PPER     DS    CL8                 PERSON CODE                                  
         DS    CL5                                                              
PACCT    DS    0CL14               1R ACCOUNT                                   
PAUL     DS    CL2                 U/L - 1R                                     
PAODS    DS    CL7                 OFF/DEP/SUBD                                 
PAPER    DS    CL5                 PERSON                                       
         DS    CL5                                                              
PPEREND  DS    CL6                 PERIOD END DATE                              
         DS    CL5                                                              
PPER#    DS    CL2                 PERIOD NUMBER FROM ELEMENT                   
         DS    CL5                                                              
PPERSTR  DS    CL6                 PERIOD START DATE FROM ELEMENT               
         DS    CL5                                                              
NPPER#   DS    CL2                 CORRECTED PERIOD NUMBER                      
         DS    CL5                                                              
NPPERSTR DS    CL6                 CORRECTED PERIOD START DATE                  
         DS    CL5                                                              
PDESC    DS    CL40                DESCRIPTION                                  
         ORG   PL                                                               
         DS    CL10                                                             
PRECS    DS    CL30                RECORD DESCRIPTION                           
         DS    CL5                                                              
PRECCNT  DS    CL16                RECORD COUNT                                 
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
*ACREPWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENMODES                                                                     
*DDLOGOD                                                                        
*ACMASTD                                                                        
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'119ACREPZZ02 03/09/00'                                      
         END                                                                    
