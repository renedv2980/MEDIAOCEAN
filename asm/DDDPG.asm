*          DATA SET DDDPG      AT LEVEL 009 AS OF 10/22/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DPGA                                                                     
*INCLUDE REGSAVE                                                                
*INCLUDE DRIVAL                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PANIC                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*INCLUDE STXITER                                                                
*INCLUDE KHDUMMY                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        DPG -- DRIVER COMPILER                               *         
*                                                                     *         
*  COMMENTS:     TRANSLATES DRIVER PROGRAMS WRITTEN IN DPG INTO       *         
*                HEX PROGRAM RECORDS WHICH ARE THE INPUT TO DRIVER.   *         
*                                                                     *         
*  CALLS TO:     DRIVAL (DRIVER VALIDATION ROUTINES)                  *         
*                                                                     *         
*  INPUTS:       DPG PROGRAM -- EITHER IN CARD DECK OR PANVALET BOOK  *         
*                                                                     *         
*  OUTPUTS:      PROGRAM RECORDS WHICH ARE THE INPUT TO DRIVER.       *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- OUTPUT ELEMENT ADDRESS (TOUCH THIS = DEATH)    *         
*                R8 -- PRINTER                                        *         
*                R9 -- THIRD BASE                                     *         
*                RA -- SECOND BASE                                    *         
*                RB -- FIRST BASE                                     *         
*                RC -- SYSTEM                                         *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'DPG - DRIVER COMPILER'                                          
DPG      CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,**DPG***,=V(REGSAVE),RA,R9                                     
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         MVC   TITLE(35),=C'DPG SOURCE LISTING AND ERROR REPORT'                
         MVC   SUB1(12),=C'SOURCE CARDS'                                        
         MVC   SUB2(12),=C'------------'                                        
         MVC   SUB1+72(11),=C'CARD ERRORS'                                      
         MVC   SUB2+72(11),=C'-----------'                                      
*                                                                               
         XC    DRVLBLK,DRVLBLK     INITIALIZE DRIVAL PARAMETER BLOCK            
         MVC   DBELNADR,=A(PROGRAM)                                             
         MVC   DBENDADR,=A(PROGRAMX)                                            
         MVC   DBCOMADR,=A(COMFACS)                                             
*                                                                               
         LAY   R7,PROGRAM          STORAGE AREA FOR GENERATED ELEMENTS          
         L     R5,=A(MACROS)       STORAGE AREA FOR MACROS                      
         ST    R5,ANEXTMAC                                                      
         EJECT                                                                  
* MAIN LOGIC FLOW                                                               
*                                                                               
RE10     GOTO1 =V(CARDS),DMCB,P,=C'RE00'                                        
         CLC   P(2),=C'/*'         TEST FOR END OF FILE                         
         BE    RE50                                                             
*                                                                               
         CLI   P,C'*'              ASTERISK IN COLUMN 1 DENOTES COMMENT         
         JE    RE10                                                             
*                                                                               
         CLC   =C'DDSIO=',P        DDSIO= CONTROL CARD?                         
         JNE   RE15                                                             
         ICM   RF,15,=V(DDSIO)     YES                                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,RF),P+6         DDSIO LOAD MODULE NAME                       
         J     RE10                                                             
*                                                                               
RE15     DS    0H                                                               
         CLC   =C'DSPACE=',P       DSPACE= CONTROL CARD?                        
         JNE   *+18                                                             
         LA    RF,SSB              YES                                          
         MVC   SSODSPAC-SSOOFF(,RF),P+7  DSPACE IDENTIFIER                      
         J     RE10                                                             
*                                                                               
         CLC   P(4),=C'PAN='       DID USER SPECIFY A PAN BOOK                  
         BNE   RE40                                                             
         MVC   PANNAME,P+4                                                      
         GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',PANNAME,CARD             
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BZ    RE20                                                             
         MVC   P+72(23),=C'PAN-NAME NOT IN LIBRARY'                             
         GOTO1 =V(PRINTER)                                                      
         B     RE10                GET NEXT CARD                                
*                                                                               
RE20     MVC   P(80),CARD                                                       
*                                                                               
RE30     BAS   RE,VCARD            LOOP ROUND PAN BOOK                          
*                                                                               
         CLI   FROMMAC,C'Y'                                                     
         BE    RE35                                                             
         GOTO1 =V(PRINTER)                                                      
         CLI   TRACE,C'Y'          TEST DUMP ELEMENTS                           
         BNE   RE35                                                             
         BAS   RE,GENOUT                                                        
*                                                                               
RE35     MVI   FROMMAC,C'N'                                                     
         L     R7,DBELNADR         SAVE NEW ELEMENT ADDRESS                     
*                                                                               
         GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',PANNAME,P                
         CLC   P(2),=C'/*'                                                      
         BNE   RE30                                                             
*                                                                               
         MVC   P,=CL132'*END INCLUDE++'                                         
         GOTO1 =V(PRINTER)         END OF PAN BOOK                              
         B     RE10                                                             
*                                                                               
RE40     BAS   RE,VCARD            DATA CARDS SUBMITTED DIRECT                  
*                                                                               
         CLI   FROMMAC,C'Y'                                                     
         BE    RE45                                                             
         GOTO1 =V(PRINTER)                                                      
         CLI   TRACE,C'Y'          TEST DUMP ELEMENTS                           
         BNE   RE45                                                             
         BAS   RE,GENOUT                                                        
*                                                                               
RE45     MVI   FROMMAC,C'N'                                                     
         L     R7,DBELNADR         SAVE NEW ELEMENT ADDRESS                     
         B     RE10                GET NEXT CARD                                
*                                                                               
RE50     MVC   0(2,R7),=X'0000'    TERMINATION CHARACTERS                       
         LA    R7,2(R7)                                                         
*                                                                               
         L     R3,ASTAMPS          INCLUDE LEVEL STAMPS                         
         LAY   R2,STAMPS                                                        
RE55     CR    R2,R3                                                            
         BE    RE60                                                             
         MVC   0(72,R7),0(R2)                                                   
         LA    R7,72(R7)                                                        
         LA    R2,72(R2)                                                        
         B     RE55                                                             
*                                                                               
RE60     MVC   P(2),=C'  '         SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
         CLI   ERSW,C'N'           TEST SOURCE ERRORS FOUND                     
         BE    NOERROR                                                          
*                                                                               
         MVC   P(23),=C'*** ERROR(S) FOUND ***'                                 
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
         LHI   R6,16               SET CONDITION CODE                           
         B     EXBASE                                                           
*                                                                               
NOERROR  MVC   P(15),=C'NO ERRORS FOUND'                                        
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
         BAS   RE,PUNCH            PUNCH THE OBJECT DECK                        
         SR    R6,R6                                                            
         CLI   OUTFLAG,C'Y'        TEST ANY CARDS PUNCHED                       
         BE    *+8                                                              
         LHI   R6,2                IF NOT, SET CONDITION CODE                   
*                                                                               
EXBASE   XBASE RC=(R6)                                                          
         EJECT                                                                  
* CONTROL THE DATA VALIDATION - SPECIAL CARDS                                   
*                                                                               
VCARD    NTR1                                                                   
*                                                                               
         MVC   P+72(60),SPACES                                                  
         MVC   CARD,P                                                           
*                                                                               
         CLC   CARD(19),=C'*          DATA SET'                                 
         BNE   VCARD10                                                          
         L     R3,LENSTAMP                                                      
         C     R3,=F'720'                                                       
         BNL   VCARD40                                                          
         LA    R3,72(R3)                                                        
         ST    R3,LENSTAMP                                                      
         L     R3,ASTAMPS                                                       
         MVC   0(72,R3),CARD                                                    
         LA    R3,72(R3)                                                        
         ST    R3,ASTAMPS                                                       
         B     EXIT                                                             
*                                                                               
VCARD10  BC    0,VCARD15            FIRST CARD MUST BE *DPG                     
         MVI   *-3,X'F0'            * SELF-MODIFYING CODE *                     
         CLC   =C'*DPG ',CARD                                                   
         BE    VCARD15                                                          
         MVC   DBERRMSG,=CL40'FIRST CARD MUST BE "*DPG"'                        
         B     ERR                                                              
*                                                                               
VCARD15  CLI   CARD,C'*'                                                        
         BE    EXIT                                                             
*                                                                               
         CLC   CARD(15),=C'         MEND  '                                     
         BNE   VCARD20                                                          
         XC    ATHISMAC,ATHISMAC                                                
         L     R5,ANEXTMAC                                                      
         MVC   0(80,R5),CARD                                                    
         LA    R5,80(R5)                                                        
         ST    R5,ANEXTMAC                                                      
         MVI   MACDEF,C'N'                                                      
         B     EXIT                                                             
*                                                                               
VCARD20  CLI   MACDEF,C'Y'         TEST MACRO CURRENTLY BEING DEFINED           
         BNE   VCARD40                                                          
         BAS   RE,MACCARD                                                       
         CLI   MACOK,C'Y'                                                       
         BE    VCARD25                                                          
         L     R3,ATHISMAC                                                      
         MVI   0(R3),C'N'                                                       
         MVC   DBERRMSG,=CL40'INVALID SYMBOLIC PARAMETER'                       
         B     ERR                                                              
VCARD25  L     R5,ANEXTMAC                                                      
         MVC   0(80,R5),CARD                                                    
         LA    R5,80(R5)                                                        
         ST    R5,ANEXTMAC                                                      
         B     EXIT                                                             
*                                                                               
VCARD40  CLI   CARD+8,C' '                                                      
         BE    *+14                                                             
         MVC   DBERRMSG,=CL40'COLUMN 9 MUST BE BLANK'                           
         B     ERR                                                              
         CLI   CARD+14,C' '                                                     
         BE    *+14                                                             
         MVC   DBERRMSG,=CL40'COLUMN 15 MUST BE BLANK'                          
         B     ERR                                                              
         CLI   CARD+71,C' '                                                     
         BE    *+14                                                             
         MVC   DBERRMSG,=CL40'COLUMN 72 MUST BE BLANK'                          
         B     ERR                                                              
*                                                                               
         CLC   CARD(15),=C'         TITLE '                                     
         BNE   VCARD80                                                          
         MVC   TITLE,SPACES                                                     
         LA    R3,CARD+16                                                       
         MVC   TITLE(48),0(R3)                                                  
         LA    R2,TITLE+48                                                      
VCARD60  CLI   0(R2),C''''                                                      
         BE    *+12                                                             
         BCTR  R2,0                                                             
         CR    R2,R3                                                            
         BNE   VCARD60                                                          
         MVI   0(R2),C' '                                                       
         MVC   P,SPACES                                                         
         B     EXIT                                                             
*                                                                               
VCARD80  CLC   CARD(15),=C'         SPACE '                                     
         BNE   VCARD100                                                         
         MVC   P,SPACES                                                         
         CLI   CARD+15,C' '                                                     
         BE    EXIT                                                             
         ZIC   R2,CARD+15                                                       
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         BCT   R2,*+8                                                           
         B     EXIT                                                             
*                                                                               
VCARD90  GOTO1 =V(PRINTER)                                                      
         BCT   R2,VCARD90                                                       
         B     EXIT                                                             
*                                                                               
VCARD100 CLC   CARD(15),=C'         EJECT '                                     
         BNE   VCARD120                                                         
         MVC   P,SPACES                                                         
         ZAP   LINE,=P'75'                                                      
         B     EXIT                                                             
*                                                                               
VCARD120 CLC   CARD(15),=C'         PRINT '                                     
         BNE   VCARD130                                                         
         CLC   CARD+15(3),=C'GEN'                                               
         BNE   *+12                                                             
         MVI   PRINTGEN,C'Y'                                                    
         B     EXIT                                                             
         CLC   CARD+15(5),=C'NOGEN'                                             
         BNE   *+12                                                             
         MVI   PRINTGEN,C'N'                                                    
         B     EXIT                                                             
         MVC   DBERRMSG,=CL40'OPTION MUST BE ''GEN'' OR ''NOGEN'''              
         B     ERR                                                              
*                                                                               
VCARD130 CLC   CARD(15),=C'         TRACE '                                     
         BNE   VCARD140                                                         
         CLC   CARD+15(2),=C'ON'                                                
         BNE   *+12                                                             
         MVI   TRACE,C'Y'                                                       
         B     EXIT                                                             
         CLC   CARD+15(3),=C'OFF'                                               
         BNE   *+12                                                             
         MVI   TRACE,C'N'                                                       
         B     EXIT                                                             
         MVC   DBERRMSG,=CL40'OPTION MUST BE ''ON'' OR ''OFF'''                 
         B     ERR                                                              
*                                                                               
VCARD140 CLC   CARD(15),=C'         PHASE '                                     
         BNE   VCARD160                                                         
         MVC   PHASENM,CARD+15                                                  
         B     EXIT                                                             
*                                                                               
VCARD160 CLC   CARD(15),=C'         DECK  '                                     
         BNE   VCARD180                                                         
         MVI   DECK,C'Y'                                                        
         B     EXIT                                                             
*                                                                               
VCARD180 CLC   CARD(15),=C'         MACRO '                                     
         BNE   VCARD190                                                         
         L     R5,ANEXTMAC                                                      
         MVC   ATHISMAC,ANEXTMAC                                                
         MVC   0(80,R5),CARD                                                    
         LA    R5,80(R5)                                                        
         ST    R5,ANEXTMAC                                                      
         MVI   MACDEF,C'Y'                                                      
         B     EXIT                                                             
*                                                                               
VCARD190 CLC   CARD(15),=C'         DICT2 '                                     
         BNE   VCARD200                                                         
         CLC   CARD+15(8),SPACES                                                
         BNE   *+14                                                             
         MVC   DBERRMSG,=CL40'NO ALTERNATE DICTIONARY NAME GIVEN'               
         B     ERR                                                              
         CLC   CARD+23(48),SPACES                                               
         BE    *+14                                                             
         MVC   DBERRMSG,=CL40'INVALID DICTIONARY NAME'                          
         B     ERR                                                              
         MVC   DBALTDIC,CARD+15                                                 
         B     EXIT                                                             
*                                                                               
VCARD200 BAS   RE,TESTMAC          TEST MACRO CALL                              
         OC    AMACRO,AMACRO                                                    
         BZ    VSTMT               IT IS NOT                                    
         BAS   RE,TXTSUB           IT IS                                        
         MVI   FROMMAC,C'Y'                                                     
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE THE DPG STATEMENT                                                    
*                                                                               
VSTMT    DS    0H                                                               
         BC    0,VSTMT05            ONLY OPEN CONTROL SYSTEM ONCE               
         MVI   *-3,X'F0'            * SELF-MODIFYING CODE *                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',FLIST,AREC               
*                                                                               
VSTMT05  DS    0H                                                               
         CLC   CARD(15),SPACES     TEST BEGINNING OF STATEMENT                  
         BE    VSTMT10             IT IS A CONTINUATION OF A STATEMENT          
*                                                                               
         MVI   DBOPCODE,0          FILL IN DRIVAL PARAMETER BLOCK               
         MVC   DBSRCADR,=A(CARD)                                                
         MVC   DBELOADR,DBELNADR                                                
         MVI   DBERRNUM,DBOK                                                    
         XC    DBERRMSG,DBERRMSG                                                
*                                                                               
         GOTO1 =V(DRIVAL),DMCB,DRVLBLK    VALIDATE FIRST 15 COLUMNS             
*                                                                               
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BNE   ERR                                                              
         L     R2,DBELOADR         SAVE THE OPCODE OF THE STATEMENT             
         MVC   OPCODE,0(R2)                                                     
*                                                                               
VSTMT10  XC    WORK,WORK                                                        
*                                                                               
         LA    R2,CARD+15          CREATE SCREEN FIELD FROM REMAINDER           
         LA    R3,WORK              OF CARD                                     
         MVI   BYTE,56                                                          
         BAS   RE,FLDGEN                                                        
*                                                                               
         LA    R2,BLOCK            CLEAR SCANNER BLOCK                          
         LHI   R0,20                                                            
         XC    0(74,R2),0(R2)                                                   
         LA    R2,74(R2)                                                        
         BCT   R0,*-10                                                          
         XC    BLOCKOFL,BLOCKOFL                                                
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(52,WORK),(21,BLOCK),C',=,='                    
*                                                                               
         CLI   4(R1),20            NO MORE THAN 20 OPTIONS PER CARD             
         BNH   *+14                                                             
         MVC   DBERRMSG,=CL40'TOO MANY OPTIONS IN THIS STATEMENT'               
         B     ERR                                                              
*                                                                               
         XC    BLOCKOFL,BLOCKOFL                                                
         MVC   DBSCANLN,4(R1)                                                   
*                                                                               
         MVC   DBOPCODE,OPCODE     FILL IN THE DRIVAL PARAMETER BLOCK           
         MVC   DBSRCADR,=A(BLOCK)                                               
         MVC   DBELOADR,DBELNADR                                                
         MVI   DBERRNUM,DBOK                                                    
         XC    DBERRMSG,DBERRMSG                                                
*                                                                               
         GOTO1 =V(DRIVAL),DMCB,DRVLBLK    VALIDATE COLUMNS 16-72                
*                                                                               
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BNE   ERR                                                              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE THE MACRO CARD                                                       
*                                                                               
MACCARD  NTR1                                                                   
*                                                                               
         LA    R3,CARD             STATEMENT IN MACRO                           
         LHI   R0,71               POSSIBLE POSITIONS FOR '&'                   
         MVI   MACOK,C'Y'                                                       
*                                                                               
MACCARD2 CLI   0(R3),X'50'         '&' -- SYMBOLIC PARAMETER                    
         BNE   MACCARD8                                                         
*                                                                               
         LA    R3,1(R3)            LOOK AT DIGIT                                
         CLI   0(R3),X'F0'         &0 NOT ALLOWED                               
         BNE   *+12                                                             
         MVI   MACOK,C'N'                                                       
         B     MACCARD8                                                         
*                                                                               
         MVC   BYTE,0(R3)          MAKE SURE IT'S &1 TO &9                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'F0'                                                       
         BE    *+12                                                             
         MVI   MACOK,C'N'                                                       
         B     MACCARD8                                                         
*                                                                               
         NI    0(R3),X'0F'         CHANGE EBCDIC NUMBER TO BINARY               
*                                                                               
MACCARD8 LA    R3,1(R3)                                                         
         BCT   R0,MACCARD2                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* SEE IF CURRENT STATEMENT IS A MACRO CALL                                      
*                                                                               
TESTMAC  NTR1                                                                   
*                                                                               
         L     R3,=A(MACROS)       BEGINNING OF MACRO STORAGE                   
         L     R5,ANEXTMAC         END OF MACRO STORAGE                         
         XC    AMACRO,AMACRO       CLEAR MACRO ADDRESS                          
*                                                                               
TESTMAC5 CR    R3,R5               TEST END OF MACRO STORAGE                    
         BNL   TESTMACX            LEAVE -- NO MACRO FOUND                      
*                                                                               
         CLC   =C'MACRO',9(R3)     LOOK FOR BEGINNING OF MACRO                  
         BE    *+12                                                             
         LA    R3,80(R3)           TRY NEXT STATEMENT                           
         B     TESTMAC5                                                         
*                                                                               
         CLI   0(R3),C'N'          TEST BAD MACRO                               
         BE    TESTMACX            YES -- LEAVE                                 
         LA    R3,80(R3)           NAME OF MACRO                                
         CLC   9(5,R3),CARD+9      TEST MATCH OF MACRO                          
         BNE   TESTMAC5                                                         
*                                                                               
         ST    R3,AMACRO           SAVE ADDRESS OF MACRO                        
*                                                                               
TESTMACX B     EXIT                                                             
         EJECT                                                                  
* SUBSTITUTE PARAMETERS IN MACRO                                                
*                                                                               
TXTSUB   NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINTER)         PRINT MACRO CALL                             
*                                                                               
         L     R3,AMACRO           LOCATION OF MACRO                            
         LA    R3,80(R3)           SKIP PAST MACRO NAME                         
*                                                                               
         MVC   WORK,SPACES         PARAMETERS TO MACRO                          
         MVC   WORK(56),CARD+15                                                 
*                                                                               
         LA    R2,MACBLOCK         CLEAR SCANNER BLOCK                          
         LHI   R0,9                                                             
*                                                                               
TXTSUB10 XC    0(12,R2),0(R2)                                                   
         MVC   12(20,R2),SPACES                                                 
         LA    R2,32(R2)                                                        
         BCT   R0,TXTSUB10                                                      
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(C'C',WORK),(9,MACBLOCK),C',=, '                
*                                                                               
TXTSUB20 MVC   WORK,SPACES                                                      
         MVC   WORK(80),0(R3)                                                   
*                                                                               
         CLC   WORK(15),=C'         MEND  '                                     
         BE    TXTSUBX             LEAVE                                        
*                                                                               
         CLI   WORK,C'*'           TEST COMMENT                                 
         BE    TXTSUB70                                                         
*                                                                               
         LA    R4,WORK                                                          
*                                                                               
TXTSUB30 LA    R0,WORK+72          TEST END OF LINE                             
         CR    R4,R0                                                            
         BNL   TXTSUB70            COMPILE THIS STATEMENT                       
*                                                                               
         CLI   0(R4),X'50'         '&' -- SYMBOLIC PARAMETER                    
         BNE   TXTSUB60                                                         
*                                                                               
         LA    R2,MACBLOCK                                                      
         ZIC   R5,1(R4)            BINARY PARAMETER NUMBER                      
         BCTR  R5,0                                                             
         LTR   R5,R5               TEST PARAMETER &1                            
         BZ    *+12                FIND SCANNER ENTRY                           
         LA    R2,32(R2)                                                        
         BCT   R5,*-4                                                           
*                                                                               
         MVC   0(2,R4),SPACES      PUT SPACES OVER SYMBOLIC PARAMETER           
         LR    R0,R4                                                            
         S     R4,=A(WORK)                                                      
         CHI   R4,8                TEST LABEL PORTION OF CARD                   
         BH    TXTSUB40                                                         
*                                                                               
         SHI   R4,6                LENGTH OF REMAINDER OF LABEL                 
         LPR   R1,R4                                                            
         LR    R4,R0               RESET POSITION POINTER                       
*                                                                               
         MVC   WORK8,SPACES                                                     
         BCTR  R1,0                SHIFT REMAINDER OF LABEL TO LEFT             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK8(0),2(R4)                                                   
         LA    R1,2(R1)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),WORK8                                                    
*                                                                               
         CLI   0(R2),0             TEST NO PARAMETER                            
         BE    TXTSUB60                                                         
*                                                                               
         MVC   WORK8,SPACES                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK8(0),0(R4)                                                   
         ZIC   RF,0(R2)            LENGTH OF PARAMETER                          
         SR    R1,RF                                                            
         AR    RF,R4                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),WORK8       MAKE ROOM FOR PARAMETER                      
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R2)      SUBSTITUTE PARAMETER IN MACRO                
*                                                                               
         B     TXTSUB60            LOOK FOR NEXT SYMBOLIC PARAMETER             
*                                                                               
TXTSUB40 SHI   R4,69               LENGTH OF REMAINDER OF CARD                  
         LPR   R1,R4                                                            
         LR    R4,R0               RESET POSITION POINTER                       
*                                                                               
         MVC   WORKOUT,SPACES                                                   
         BCTR  R1,0                SHIFT REMAINDER OF CARD TO LEFT              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORKOUT(0),2(R4)                                                 
         LA    R1,2(R1)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),WORKOUT                                                  
*                                                                               
         CLI   0(R2),0             TEST NO PARAMETER                            
         BE    TXTSUB60                                                         
*                                                                               
         MVC   WORKOUT,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORKOUT(0),0(R4)                                                 
         ZIC   RF,0(R2)            LENGTH OF PARAMETER                          
         SR    R1,RF                                                            
         AR    RF,R4                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),WORKOUT     MAKE ROOM FOR PARAMETER                      
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R2)      SUBSTITUTE PARAMETER IN MACRO                
*                                                                               
TXTSUB60 LA    R4,1(R4)            NEXT CHARACTER                               
         B     TXTSUB30                                                         
*                                                                               
TXTSUB70 MVC   CARD,WORK           PUT EXPANDED MACRO IN CARD                   
         MVC   P(80),CARD                                                       
*                                                                               
         BAS   RE,VCARD            VALIDATE THE EXPANDED CARD                   
*                                                                               
         CLI   PRINTGEN,C'Y'                                                    
         BNE   TXTSUB80                                                         
         CLI   P,C'*'              TEST COMMENT                                 
         BE    *+8                                                              
         MVI   P+8,C'+'            EXPANDED LINE OF CODE                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   TRACE,C'Y'          TEST DUMP ELEMENTS                           
         BNE   *+8                                                              
         BAS   RE,GENOUT                                                        
         B     TXTSUB90                                                         
*                                                                               
TXTSUB80 CLC   P+72(L'DBERRMSG),SPACES   TEST ANY ERRORS FOUND                  
         BE    TXTSUB90                                                         
         MVI   P+8,C'+'            EXPANDED LINE OF CODE HAS AN ERROR           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
TXTSUB90 L     R7,DBELNADR         SAVE NEW ELEMENT ADDRESS                     
         MVC   P,SPACES                                                         
         LA    R3,80(R3)           NEXT LINE OF MACRO                           
         B     TXTSUB20                                                         
*                                                                               
TXTSUBX  B     EXIT                                                             
         EJECT                                                                  
* CREATE SCREEN HEADER AND FIELD FOR SCANNER INPUT                              
*                                                                               
FLDGEN   NTR1                                                                   
*                                                                               
         CLI   BYTE,0              TEST LENGTH OF ZERO                          
         BE    FLDGENX             IF SO, DO NOTHING                            
*                                                                               
         XC    0(8,R3),0(R3)       CLEAR FIELD HEADER                           
         MVC   5(1,R3),BYTE        STORE INPUT LENGTH                           
*                                                                               
         ZIC   R4,5(R3)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),0(R2)       PUT DATA INTO DATA FIELD                     
*                                                                               
         ZIC   R4,5(R3)            INPUT LENGTH                                 
         AHI   R4,8                HEADER LENGTH                                
         STC   R4,0(R3)            TOTAL FIELD LENGTH                           
*                                                                               
         LA    R5,0(R4,R3)         JUST PAST END OF OUTPUT FIELD                
*                                                                               
BACKUP   BCTR  R5,0                MOVE BACKWARDS, LOOK AT EACH CHAR.           
         CLI   0(R5),C' '          IF BLANK OR NULL, KEEP BACKING UP            
         BE    *+12                                                             
         CLI   0(R5),0                                                          
         BNE   FLDGENX                                                          
*                                                                               
         ZIC   R4,0(R3)            ADJUST TOTAL FIELD LENGTH                    
         BCTR  R4,0                                                             
         STC   R4,0(R3)                                                         
*                                                                               
         ZIC   R4,5(R3)            ADJUST INPUT LENGTH                          
         BCTR  R4,0                                                             
         STC   R4,5(R3)                                                         
*                                                                               
         B     BACKUP                                                           
*                                                                               
FLDGENX  B     EXIT                                                             
         EJECT                                                                  
* PRINT THE GENERATED ELEMENTS IN HEX                                           
*                                                                               
GENOUT   NTR1                                                                   
*                                                                               
         C     R7,DBELNADR         TEST ANY CODE FOR THIS CARD                  
         BE    GENOUTX                                                          
*                                                                               
         XC    WORKOUT,WORKOUT                                                  
         L     R2,DBELNADR                                                      
         SR    R2,R7               LENGTH OF DATA                               
*                                                                               
         BCTR  R2,0                PRINT ELEMENTS IN EBCDIC                     
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R7)                                                       
         GOTO1 =V(PRINTER)                                                      
         AHI   R2,1                RESTORE LENGTH                               
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,(R7),WORKOUT,(R2),=C'SEP'                        
         OC    16(4,R1),16(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,16(R1)           PRINT ELEMENTS IN HEX                        
         SRL   R2,1                                                             
         LR    R3,R2                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),WORKOUT                                                     
         GOTO1 =V(PRINTER)                                                      
         LA    R3,WORKOUT+0(R3)                                                 
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GENOUTX  B     EXIT                                                             
         EJECT                                                                  
* PUNCH THE OBJECT DECK                                                         
*                                                                               
PUNCH    NTR1                                                                   
*                                                                               
         CLI   DECK,C'Y'           TEST PRINT OBJECT DECK FLAG ON               
         BNE   PUNCH10                                                          
*                                                                               
         ZAP   LINE,=P'75'         HEADINGS ON OBJECT DECK PRINTOUT             
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(20),=C'OBJECT DECK -- NAME '                               
         MVC   TITLE+20(8),PHASENM                                              
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PUNCH10  LA    R2,LNKFIL           OPEN LINK FILE                               
         OPEN  ((2),OUTPUT)                                                     
*                                                                               
         LR    R3,R7               END OF PROGRAM                               
         LAY   R2,PROGRAM          BEGINNING OF PROGRAM                         
         SR    R3,R2               LENGTH OF PROGRAM                            
*                                                                               
         MVI   D,X'02'             ESD CARD                                     
         MVC   D+1(3),=C'ESD'                                                   
         MVC   D+10(2),=H'16'                                                   
         MVC   D+14(2),=H'01'                                                   
         MVC   D+16(6),PHASENM     CSECT NAME                                   
         MVC   D+24(4),=F'0'                                                    
         MVI   D+29,0                                                           
         STCM  R3,3,D+30                                                        
         MVC   D+72(8),=C'00000001'                                             
         BAS   RE,PUTLNK                                                        
*                                                                               
         SR    R4,R4                                                            
         LHI   R5,2                                                             
*                                                                               
TXTLOOP  LTR   R3,R3               NOW LOOP WITH THE TEXT CARDS                 
         BZ    ENDCARD                                                          
         MVI   D,X'02'                                                          
         MVC   D+1(3),=C'TXT'                                                   
         ST    R4,DUB                                                           
         MVC   D+5(3),DUB+1        ORIGIN                                       
         MVC   D+14(2),=H'1'                                                    
         CVD   R5,DUB                                                           
         MVC   D+72(4),=C'0000'                                                 
         UNPK  D+76(4),DUB                                                      
         OI    D+79,X'F0'                                                       
         CHI   R3,56                                                            
         BNH   TXT2                                                             
         MVC   D+10(2),=H'56'      FULL 56 BYTE CARD                            
         MVC   D+16(56),0(R2)                                                   
         BAS   RE,PUTLNK                                                        
         LA    R2,56(R2)                                                        
         SHI   R3,56                                                            
         LA    R4,56(R4)                                                        
         LA    R5,1(R5)                                                         
         B     TXTLOOP                                                          
*                                                                               
TXT2     STH   R3,DUB              LAST TEXT CARD MAY BE LESS THAN 56           
         MVC   D+10(2),DUB                                                      
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   D+16(0),0(R2)                                                    
         BAS   RE,PUTLNK                                                        
         LA    R5,1(R5)                                                         
*                                                                               
ENDCARD  MVI   D,X'02'             END CARD                                     
         MVC   D+1(3),=C'END'                                                   
         CVD   R5,DUB                                                           
         MVC   D+72(4),=C'0000'                                                 
         UNPK  D+76(4),DUB                                                      
         OI    D+79,X'F0'                                                       
         BAS   RE,PUTLNK                                                        
*                                                                               
         MVC   D(80),=CL80' IDENTIFY XXXXXX(''DPG:UUUUUUU BBBBBBBBBB MM+        
               MDD/YY HH:MM'')'                                                 
         L     RF,PSAAOLD-PSA(,0)       GET CURRENT/HOME ASCB                   
         L     RF,(ASCBASXB-ASCB)(,RF)  GET ASXB ADDRESS                        
         L     RF,(ASXBSENV-ASXB)(,RF)  GET ACEE ADDRESS                        
         CLC   =CL4'ACEE',0(RF)         VALID ACEE?                             
         JE    *+6                      YES: EXTRACT RACF USERID                
         DC    H'0'                     NO: IMPOSSIBLE                          
         MVC   D+10(6),PHASENM     CSECT NAME                                   
         MVC   D+22(7),(ACEEUSRI-ACEE)(RF)  USER WHO SUBMITTED COMPILE          
         MVC   D+30(10),PANNAME    PAN SOURCE BOOK NAME                         
         GOTO1 =V(DATCON),DMCB,(5,0),(11,D+41)                                  
         THMS                                                                   
         ST    R1,FULL             R1=0HHMMSS+                                  
         OI    FULL+3,X'0F'                                                     
         UNPK  DUB(6),FULL         CONSTRUCT COMPILE TIME                       
         MVC   D+50(2),DUB         HOURS                                        
         MVI   D+52,C':'                                                        
         MVC   D+53(2),DUB+2       MINUTES                                      
         BAS   RE,PUTLNK                                                        
*                                                                               
         MVC   D(80),=CL80' NAME ' NAME CARD                                    
         LA    R2,D+6                                                           
         MVC   0(8,R2),PHASENM                                                  
         LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BNE   *-8                                                              
         MVC   0(3,R2),=C'(R)'                                                  
         BAS   RE,PUTLNK                                                        
*                                                                               
         LA    R2,LNKFIL           CLOSE LINK FILE                              
         CLOSE ((2))                                                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINES TO PUNCH OBJECT CARD                                                 
*                                                                               
PUTLNK   NTR1                                                                   
*                                                                               
         LA    R1,LNKFIL           WRITE DATA TO DECK                           
         PUT   (R1),D                                                           
         CLI   DECK,C'Y'           TEST PRINT OBJECT DECK FLAG ON               
         BNE   PUTLNK10                                                         
*                                                                               
         MVC   P(80),D             PRINT ALPHA DATA                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XC    WORKOUT,WORKOUT                                                  
         GOTO1 =V(HEXOUT),DMCB,D,WORKOUT,80,=C'SEP'                             
         OC    16(4,R1),16(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(80),WORKOUT       PRINT HEX DATA                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),WORKOUT+80                                                 
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PUTLNK10 MVC   D,SPACES            CLEAR OUTPUT LINE                            
         MVI   OUTFLAG,C'Y'                                                     
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* ERROR ROUTINES                                                                
*                                                                               
ERR      MVC   P+72(L'DBERRMSG),DBERRMSG                                        
         MVI   ERSW,C'Y'                                                        
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
LNKFIL   DCB   DDNAME=LNKFIL,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM                
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
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
         ORG   SSODSPAC                                                         
         DC    C'N'                FORCE DSPACE=N                               
         ORG                                                                    
*                                                                               
         SPACE 3                                                                
UTL      DC    F'0',X'0A'                                                       
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'NGENFILE'                                                    
         DC    CL8'NGENDIR '                                                    
         DC    CL8'X       '                                                    
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(DPG),V(DUMMY)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
COMFACS  DS    0D                                                               
*                                  COMMON FACILITIES LIST                       
         DC    V(DATAMGR)                                                       
         DC    A(0)                CALLOFF                                      
         DC    A(0)                GETMSG                                       
         DC    A(0)                SCROUT                                       
         DC    A(0)                FLDVAL                                       
         DC    A(0)                HELLO                                        
         DC    V(SCANNER)                                                       
         DC    A(0)                UNSCAN                                       
         DC    V(HEXIN)                                                         
         DC    V(HEXOUT)                                                        
         DC    V(CASHVAL)                                                       
         DC    A(0)                DATVAL                                       
         DC    A(0)                DATCON                                       
         DC    A(0)                TERMVAL                                      
         DC    A(0)                SCUNKEY                                      
         DC    A(0)                ADDAY                                        
         DC    A(0)                GETDAY                                       
         DC    A(0)                GETPROF                                      
         DC    A(0)                PERVERT                                      
         DC    A(0)                GETFACT                                      
         DC    A(0)                XSORT                                        
         DC    A(0)                REQTWA                                       
         DC    A(0)                SYSCON                                       
         DC    A(0)                DDISPSRT                                     
         DC    A(0)                DEMADDR                                      
         DC    A(0)                DEMDISP                                      
         DC    A(0)                DBOOK                                        
         DC    A(0)                DSTATION                                     
         DC    A(0)                DMASTER                                      
         DC    A(0)                DFORMULA                                     
         DC    A(0)                DNAME                                        
         DC    A(0)                DCODE                                        
         DC    A(0)                DCONTROL                                     
         DC    A(0)                DADJUST                                      
         DC    A(0)                DEMOUT                                       
         DC    A(0)                DEMEL                                        
         DC    A(0)                DEMAINT                                      
         DC    A(0)                DEMAND                                       
         DC    A(0)                DEMOMATH                                     
         DC    A(0)                DEMOVAL                                      
         EJECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
BYTE     DS    X                                                                
OPCODE   DC    X'00'               OPERATION CODE FOR DRIVAL                    
TRACE    DC    C'N'                'Y' = PRINT ELEMENTS IN HEX                  
DECK     DC    C'N'                'Y' = PRINT OBJECT DECK                      
PRINTGEN DC    C'N'                'Y' = PRINT MACRO EXPANSIONS                 
PANNAME  DC    CL10' '             NAME OF PANVALET BOOK                        
PHASENM  DC    CL8' '              NAME OF GENERATED PHASE                      
D        DC    CL80' '             OBJECT CARD                                  
OUTFLAG  DC    C'N'                'Y' = THERE IS AN OBJECT DECK                
ERSW     DC    C'N'                'Y' = SOURCE ERRORS WERE FOUND               
MACDEF   DC    C'N'                'Y' = MACRO IS NOW BEING DEFINED             
MACOK    DC    C'N'                'Y' = VALID MACRO STATEMENT                  
FROMMAC  DC    C'N'                'Y' = A MACRO WAS JUST EXPANDED              
ATHISMAC DC    A(0)                ADDRESS OF THE CURRENT MACRO                 
ANEXTMAC DC    A(0)                ADDRESS OF NEXT MACRO AREA                   
AMACRO   DC    A(0)                ADDRESS OF UNEXPANDED MACRO                  
LENSTAMP DC    F'0'                LENGTH OF LEVEL STAMPS                       
ASTAMPS  DC    A(STAMPS)           A(LEVEL STAMPS)                              
AREC     DC    A(REC)              A(I/O AREA FOR CONTROL FILE GENFIL)          
WORKOUT  DS    XL160               USED FOR HEXOUT OUTPUT                       
WORK     DS    XL132               WORK AREA                                    
WORK8    DS    XL8                 WORK AREA                                    
*                                                                               
         DS    0D                                                               
         DC    C'**CARD**'                                                      
CARD     DS    CL80                INPUT SOURCE CARD                            
         EJECT                                                                  
       ++INCLUDE DRIVALBLKD                                                     
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*SCANBLK'                                                      
BLOCK    DS    20XL74              SCANNER BLOCK                                
BLOCKOFL DS    XL74                SCANNER BLOCK OVERFLOW                       
*                                                                               
         DS    0D                                                               
         DC    C'*MACBLK*'                                                      
MACBLOCK DS    9XL32               MACRO PARAMETER SCANNER BLOCK                
*                                                                               
         DS    0D                                                               
         DC    C'**REC***'                                                      
REC      DS    XL2000              I/O AREA FOR CONTROL FILE GENFIL             
*                                                                               
         DS    0D                                                               
         DC    C'*STAMPS*'         PAN BOOK AND LEVEL STAMPS                    
STAMPS   DS    CL720                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*PROGRAM'                                                      
PROGRAM  DS    32767X              OBJECT CODE                                  
*                                                                               
PROGRAMX EQU   *                                                                
         SPACE 5                                                                
         DS    0D                                                               
         DC    C'*MACROS*'                                                      
MACROS   DS    32767X              MACROS                                       
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
         IHAPSA                                                                 
         IHAACEE                                                                
         IHAASCB                                                                
         IHAASXB                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDDPG     10/22/13'                                      
         END                                                                    
