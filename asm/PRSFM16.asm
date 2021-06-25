*          DATA SET PRSFM16    AT LEVEL 082 AS OF 05/01/02                      
*PHASE T41C16A,*                                                    L01         
         TITLE 'T41C16  CHANGE LOG'                                 L01         
* ROSA 2/2/89 NO DELETE FUNTION FOR OAN RECORDS.. OAN RECORDS    BUG01          
*          GO ACROSS CLIENTS AND ALL PRODUCTS FOR ALL CLIENTS    BUG01          
*          MUST BE TESTED.. TOO TIME CONSUMING ON LINE.  AN OFF- BUG01          
*          LINE PGM M/B WRITTEN TO HANDLE THIS IN THE FUTURE     BUG01          
*                                                                BUG01          
* ROSA 7/26/88    MODIFY TO PRINT SYSTEM                                        
         TITLE 'T41C16  OTHER AGENCY NAME RECORDS'              L01             
T41C16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C16                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE FUNCTION                BUG01         
         BNE   EXIT                                               BUG01         
         XC    CONHEAD,CONHEAD     CANNOT DELETE OAN RECORDS// M/B G01          
         MVC   CONHEAD(25),=C'CANNOT DELETE OAN RECORDS'                        
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R2,AGRMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R6,SVKEY                                                         
         USING POTHKEY,R6                                                       
         XC    POTHKEY,POTHKEY        CLEAR                         L01         
         MVC   POTHKAGY,AGENCY  CREATE KEY  -- AGENCY               L01         
         MVC   POTHKMED,QMED                   MEDIA CODE           L01         
         MVI   POTHKRCD,X'16'                  ID                               
         LA    R2,AGRCODH                                                       
         CLI   5(R2),0           ANYTHING ENTERED                               
         BNE   *+12                                                             
         CLI   ACTNUM,ACTLIST    IS OK IF LIST                                  
         BE    VK6                                                              
         CLI   5(R2),2            LENGTH M/B 2                      L01         
         BE    CODIS2                                               L01         
NERROR   OI    6(R2),X'40'   FORCE CURSOR TO OTHER AGENCY NAME      L01         
         MVI   ERROR,INVALID                                        L01         
         B     TRAPERR                                              L01         
*                                                                               
*                                                                               
CODIS2   MVC   POTHCODE,AGRCOD                                      L01         
*                                                                   L01         
*                                                                   L01         
VK6      DS    0H                                                               
         MVC   KEY(25),SVKEY       SET KEY                          L01         
*                                                                               
VK900    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING POTHREC,R6                                                       
*                                                                               
         FOUT  AGRCODH,POTHCODE,2                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         MVC   SVKEY,KEY           SAVE THE RECORD KEY                          
         MVI   ELCODE,X'01'        REMOVE ADDRESS ELEM                          
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           BUILD NEW ELEMENTS                           
         LA    R6,ELEM                                                          
         USING POTHELEM,R6                                                      
         MVI   ELEM,X'01'          ELEM CODE                                    
         MVI   ELEM+1,177          LENGTH                                       
*                                                                               
         MVC   ELEM+2(177),SPACES                                               
*======= FOUT  AGRNAMH,SPACES,33                                                
*======= FOUT  AGRADDH,SPACES,33                                                
*                                                                               
*                                                                               
*                                                                               
*======= CLI   ERROR,0                                                          
*======= BNE   VR99C                                                            
*                                                                               
         LA    R2,AGRNAMH                                                       
         CLI   5(R2),0          MUST HAVE NAME                                  
         BNE   *+14                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   POTHNAME(0),AGRNAM                                               
*                                                                               
         LA    R2,AGRADDH                                                       
         CLI   5(R2),0          MUST HAVE ADDRESS                               
         BNE   *+14                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   POTHADDR(0),AGRADD                                               
*                                                                               
         LA    R2,AGRADD2H                                                      
         CLI   5(R2),0                                                          
         BE    LOOKADD3                                                         
*                                                                               
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   POTHADD2(0),AGRADD2                                              
*                                                                               
LOOKADD3 LA    R2,AGRADD3H                                                      
         CLI   5(R2),0                                                          
         BE    LOOKADD4                                                         
*                                                                               
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   POTHADD3(0),AGRADD3                                              
*                                                                               
*                                                                               
LOOKADD4 LA    R2,AGRADD4H                                                      
         CLI   5(R2),0                                                          
         BE    ADDTHEEL                                                         
*                                                                               
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   POTHADD4(0),AGRADD4                                              
ADDTHEEL GOTO1 ADDELEM                                                          
         B     VR900                                                            
*                                                                               
*                                                                               
VR900    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         LA    R2,AGRNAMH         CLEAR SCREEN                                  
         BAS   RE,CLRSCRN                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         USING POTHELEM,R6                                                      
         FOUT  AGRNAMH,POTHNAME                                                 
         FOUT  AGRADDH,POTHADDR                                                 
         FOUT  AGRADD2H,POTHADD2                                                
         FOUT  AGRADD3H,POTHADD3                                                
         FOUT  AGRADD4H,POTHADD4                                                
*                                                                               
DR10     DS    0H                                                               
*                                                                               
DR20     DS    0H                                                               
DR900    B     EXIT                                                             
         SPACE 3                                                                
*        SETGN- SET GROSS/NET BASIS                                             
         SPACE 2                                                                
*                                                                               
CHKAX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING POTHREC,R6                                                       
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR020                                                            
*                                                                               
         MVC   POTHKAGY,AGENCY  CREATE KEY  -- AGENCY               L01         
         MVC   POTHKMED,QMED                   MEDIA CODE           L01         
         MVI   POTHKRCD,X'16'                  ID                               
         MVC   POTHCODE,SVKEY+4                                   L01           
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(4),KEYSAVE      TEST FOR ALL DONE               L01          
         BNE   LR900                                                            
         GOTO1 GETREC              GET THE POTH RECORD                          
         L     R6,AIO                                                           
*                                                                               
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    LR090                                                            
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
         MVC   SELHED(11),=C'CODE       '                          L01          
         MVC   0(2,R5),POTHCODE                                                 
         B     DDISP                                                            
DDISP    DS    0H                                                               
         FOUT  SELHEDH                                                          
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR090    DS    0H                  **NB- PRINTREP NOT FULLY CODED               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR020                                                            
*                                                                               
LR900    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    DS    0H                                                               
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'20'         TEST PROTECTED                               
         BNZ   CS020               YES- DON'T CLEAR                             
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                          L01          
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFME6D                                          L01          
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF6D                                          L01          
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                         L01         
*                                                                               
SVCOMP   DS    X                                                                
SVPRD    DS    CL3                                                              
SVEST    DS    XL2                                                              
SVCLS    DS    CL3                                                              
GNSW     DS    CL1                                                              
X        DS    XL100                                                            
*                                                                               
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE PPRDREC                                           L01          
         ORG   PPRDBILP                                                         
       ++INCLUDE PBILPROF                                                       
         ORG                                                                    
MKTRECD  DSECT                                                                  
       ++INCLUDE POTHAGY                                            L01         
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082PRSFM16   05/01/02'                                      
         END                                                                    
