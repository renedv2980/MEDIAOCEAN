*          DATA SET DDDSGEN    AT LEVEL 001 AS OF 05/14/12                      
*PROCESS USING(WARN(15))                                                        
*PHASE DSGENA                                                                   
**********************************************************************          
*                                                                    *          
* TITLE: DDDSGEN - MANUAL ASSEMBLER FOR LINKER (DSECTS ONLY)         *          
*                                                                    *          
* OUTPUTS: OBJECT MODULE FOR A PROC TO CREATE PHASES                 *          
*          ALL PHASES WILL BE SUFFIXED WITH AN "A"                   *          
*                                                                    *          
* INPUTS: FILE CONTAINING SYSTEM NAMES AND DSECTS                    *          
*                                                                    *          
* USES REGISTERS:                                                    *          
*                                                                    *          
* R2: POINTS TO TEMP BUFFER                                          *          
* R3: POINTS TO BUFFER                                               *          
* R4: WORK                                                           *          
* R5: WORK                                                           *          
* R6: WORK                                                           *          
* R7: NOT USED                                                       *          
* R8: NOT USED                                                       *          
* R9: SECOND BASE                                                    *          
* RA: BASE FOR DDDPRINT                                              *          
* RB: FIRST BASE                                                     *          
*                                                                    *          
* NOTE: T15E00(PROGRAM) AND T15EFF(TWA) ARE RESERVED                 *          
*                                                                    *          
**********************************************************************          
*                                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*                                                                               
         TITLE 'DDDSGEN - DSECT FILE PARSER'                                    
DSGEN    CSECT                                                                  
*                                                                               
**************************** INITS **********************************           
*                                                                               
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,DSGEN,=V(REGSAVE),R9,RA                                        
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     START                                                            
         SPACE 3                                                                
DUMPLIST DS    0F                                                               
         DC    A(DSGEN),V(DUMMY)                                                
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
**************************** MAIN **********************************            
* 'DSECTIN' IS THE INPUT FILE CONTAINING DSECT MEMBERS.                         
* 'OBJFIL' IS THE VIRTUAL OUTPUT FILE FOR THE LINKER                            
* 'DSECTIN' HAS THE FOLLOWING FORMAT:                                           
*     DISPLACEMENT LABEL DEFINITION TYPE COMMENT                                
* IE: 00087A       JUNK     DS      CL4  PILE OF GARBAGE                        
*********************************************************************           
*                                                                               
START    OPEN  (DSECTIN,INPUT,OBJFIL,OUTPUT)                                    
         LA    R2,TEMPBUFF         POINTS TO TEMP BUF                           
         LA    R3,DSBUFF           POINTS TO BEGINNING OF BUFFER                
*                                                                               
*********************************************************************           
* PROCESS HEADER LINES.                                                         
*********************************************************************           
*                                                                               
*                                                                               
GETSYS10 DS    0H                  GET SYSTEM NAME                              
         GET   DSECTIN,DLINE                                                    
         CLC   =C'*SYSTEM',DLINE+41                                             
         BNE   GETSYS10                                                         
*                                                                               
GETSYS13 DS    0H                                                               
         XC    PHCNT,PHCNT         ALL PHASES EXCEPT FACPAK START AT 0          
         MVC   SYSNAME,DLINE+49    GET ASSIGNED SYSTEM NAME                     
         MVC   P(8),=C'SYSTEM: '                                                
         MVC   P+8(L'SYSNAME),SYSNAME                                           
         GOTO1 =V(PRINTER)                                                      
         CLC   =C'FACPAK',SYSNAME  IS SYSTEM FACPAK?                            
         BNE   GETSYS15            NO,LOOK UP TABLE                             
*                                                                               
         MVC   PHCNT,=H'1'         FACPAK PHASE COUNTER STARTS AT 1             
         MVC   PHASENM,=C'T15E01A '                                             
         MVC   P(7),=C'PHASE: '                                                 
         MVC   P+7(L'PHASENM),PHASENM                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'  MEMBER      SIZE   TOTAL'                             
         GOTO1 =V(PRINTER)                                                      
         B     GETDS10             FACPAK PHASE COUNTER STARTS AT 1             
*                                                                               
GETSYS15 DS    0H                                                               
         LA    R4,SYSLST           POINT TO SYSTEM NAME TABLE                   
         LH    R0,0(R4)            L'TABLE ENTRY                                
         LA    R4,6(R4)            POINT TO FIRST SYSTEM NAME                   
         B     *+6                                                              
*                                                                               
         USING SYSLSTD,R4                                                       
GETSYS20 DS    0H                                                               
         AR    R4,R0               BUMP TO NEXT ENTRY                           
         CLI   0(R4),0             TABLE TERMINATOR?                            
         BE    ERROR60             ERROR, CAN'T FIND SYSTEM NAME                
         CLC   SYSNAME,SYSLNAME    IS SYSTEM NAME IN TABLE?                     
         BNE   GETSYS20                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,SYSLNUM,PHASENM+3,1                              
         L     R5,DMCB+16          INSERT SYSTEM NUMBER AT FIFTH DIGIT          
         CIJE  R5,0,ERROR60        ANY ERROR IN HEXOUT?                         
         DROP  R4                                                               
*                                                                               
         MVC   PHASENM(4),=C'T15E' MOVE IN PHASE PREFIX                         
         MVI   PHASENM+5,C'0'      LAST DIGIT IS ALWAYS ZERO                    
         MVI   PHASENM+6,C'A'      TEST SUFFIX IS ALWAYS "A"                    
         MVC   P(7),=C'PHASE: '                                                 
         MVC   P+7(L'PHASENM),PHASENM                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'  MEMBER      SIZE   TOTAL'                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GETDS10  DS    0H                                                               
         GET   DSECTIN,DLINE       CHECK FOR HEADER                             
         CLC   =C'*SYSTEM',DLINE+41                                             
         BNE   GETDS11                                                          
*                                                                               
         LA    R3,DSBUFF                                                        
         A     R3,ICOUNTER                                                      
         MVC   0(3,R3),=X'FFFF00'  EOF MARKER                                   
         L     R4,ICOUNTER                                                      
         LA    R4,3(R4)                                                         
         ST    R4,ICOUNTER                                                      
         BAS   RE,DISPLAY          SHOW SIZES                                   
         BAS   RE,PRTDS            PRINT DECK                                   
         B     GETSYS13                                                         
*                                                                               
GETDS11  DS    0H                                                               
         CLC   =C'END',DLINE+50    IS IT EOF??                                  
         BE    DSEOF               YUP GO PRINT                                 
         CLC   =C'DATA SET',DLINE+52                                            
         BNE   GETDS15                                                          
         CLI   DFLAG,X'01'         IF DSECT CARD FLAG TURNED ON                 
         BNE   GETDS12                                                          
*                                                                               
         L     R4,ICOUNTER         SWITCH DSECT AND DATA SET CARDS              
         S     R4,DLEN                                                          
         ST    R4,ICOUNTER                                                      
         B     GETDS13                                                          
*                                                                               
GETDS12  DS    0H                                                               
         MVI   DSFLAG,1            SET DATASET FLAG ON                          
         MVC   DSNAME,DLINE+61     STORE DATASET NAME                           
*                                                                               
GETDS13  DS    0H                                                               
         L     R6,ICOUNTER         DONT SHOW SIZES AT THE BEGINNING             
         CIJNH R6,13,GETDS14                                                    
         BAS   RE,DISPLAY          SHOW SIZES                                   
         ST    R6,OLDCOUNT                                                      
*                                                                               
GETDS14  DS    0H                                                               
         MVC   P(10),DLINE+61      HEADER HARD CODED TO 13 CHARS.               
         LA    R2,TEMPBUFF                                                      
         MVC   0(3,R2),=X'FFFFFF'                                               
         MVC   3(10,R2),DLINE+61   SHOW MEMBER NAME                             
         LA    R4,13                                                            
         ST    R4,TEMPLEN                                                       
         BAS   RE,APPEND           ADD TO BUFFER                                
         BNE   EXBASE              EXIT ON ERROR                                
*                                                                               
         CLI   DFLAG,X'01'         DO THE FOLLOWING IF DSECT FLAG ON            
         BNE   GETDS15                                                          
*                                                                               
         MVC   TEMPBUFF,DSTRING                                                 
         L     R4,DLEN                                                          
         ST    R4,TEMPLEN                                                       
         BAS   RE,APPEND                                                        
         BNE   EXBASE              EXIT ON ERROR                                
*                                                                               
         SR    R4,R4                                                            
         STC   R4,DFLAG            CLEAR DSECT FLAG                             
         STC   R4,DSFLAG           CLEAR DATASET FLAG                           
         B     GETDS65                                                          
*                                                                               
GETDS15  DS    0H                                                               
         CLI   DLINE+41,C'*'       IF THIS IS A COMMENT LINE                    
         BNE   GETDS16                                                          
         CLC   =C'<   >',DLINE+50  AND DSECT COMMENT SYMBOL PRESENT             
         BNE   GETDS16                                                          
         MVC   DSECTCMT,DLINE+56   SAVE IT FOR LATER                            
         B     GETDS65             SKIP TO NEXT LINE                            
*                                                                               
GETDS16  DS    0H                                                               
         MVC   SRFADDSP,DLINE+1    GET DISPLACEMENT                             
         MVC   DSEQU,DLINE+23      GET EQU'S                                    
         MVC   SRFADLBL,DLINE+41   GET LABEL                                    
*                                  GET DEFINITION                               
         CLC   =C'DS',DLINE+50                                                  
         BNE   *+8                 LOOKS FOR 'DS'                               
         MVI   SRFADDEF,C'S'                                                    
         CLC   =C'ECT',DLINE+52                                                 
         BNE   *+8                 LOOKS FOR 'ECT' IN 'DSECT'                   
         MVI   SRFADDEF,C'D'                                                    
         CLC   =C'DC',DLINE+50                                                  
         BNE   *+8                 LOOKS FOR 'DC'                               
         MVI   SRFADDEF,C'C'                                                    
         CLC   =C'EQU',DLINE+50                                                 
         BNE   *+8                 LOOKS FOR 'EQU'                              
         MVI   SRFADDEF,C'E'                                                    
         CLC   =C'ORG',DLINE+50                                                 
         BNE   *+8                 LOOKS FOR 'ORG'                              
         MVI   SRFADDEF,C'O'                                                    
*                                                                               
         GOTO1 =V(HEXIN),DMCB,SRFADDSP,DSPCHK,L'SRFADDSP                        
         L     R4,DMCB+12                                                       
         CIJE  R4,0,GETDS17        WAS THERE A DISPL? IF NO, CHECK IF =         
         SR    R4,R4                                                            
         ICM   R4,B'0111',DSPCHK                                                
         C     R4,=X'0000FFFF'     IF DISP. IS > FFFF, THEN ERROR               
         BH    ERROR50                                                          
         B     GETDS20             ELSE,STORE IN BUFFER                         
*                                                                               
GETDS17  DS    0H                                                               
         GOTO1 =V(HEXIN),DMCB,DSEQU,EQUCHK,L'DSEQU                              
         L     R4,DMCB+12          IS THIS AN EQU?                              
         CIJE  R4,0,GETDS65        YES=STORE,NO=BUMP TO NEXT                    
         SR    R4,R4               IF EQU                                       
         ST    R4,DSPCHK           SET DISPL. TO 0'S                            
*                                                                               
GETDS20  DS    0H                  STORE THE DSECT LINE TO BUFFER               
         CLI   DFLAG,1             CURRENT CARD ISNT A DATASET HEADER           
         BNE   *+8                 TURN DSECT CARD FLAG OFF                     
         MVI   DFLAG,0                                                          
         CLI   SRFADDEF,C'D'       IF CURRENT CARD A DSECT                      
         BNE   GETDS23                                                          
         MVI   DFLAG,1             TURN DSECT FLAG ON                           
*                                                                               
         CLC   DSECTCMT,SPACES     IF DSECT COMMENT SAVED                       
         BE    GETDS23                                                          
         CLC   DLINE+70(L'DSECTCMT),SPACES    THEN IF NOTHING ON LINE           
         BNE   *+10                                                             
         MVC   DLINE+70(L'DSECTCMT),DSECTCMT  MOVE IN SAVED COMMENT             
         MVC   DSECTCMT,SPACES     CLEAR SAVED COMMENT                          
*                                                                               
GETDS23  DS    0H                                                               
         CLI   DSFLAG,0            IF PREVIOUS IS DATASET HEADER                
         BE    GETDS25             DO THE FOLLOWING                             
         MVI   DSFLAG,0            RESET FLAG                                   
         CLI   SRFADDEF,C'D'       IF CURRENT LINE NOT A DSECT CARD?            
         BE    GETDS25             KEEP GOING                                   
*                                                                               
* SPECIAL FOR TALENT TO SKIP THIS                                               
*                                                                               
         CLC   =C'TALENT',SYSNAME                                               
         BE    GETDS25                                                          
*                                                                               
         LA    R2,TEMPBUFF         STICK IN A DSECT CARD                        
         MVC   0(2,R2),=X'0000'    WITH DATASET HEADER NAME                     
         MVC   2(L'DSNAME,R2),DSNAME      AS THE LABEL                          
         MVI   10(R2),C'D'                                                      
         MVC   11(2,R2),=X'0000'                                                
         LA    R4,13                                                            
         ST    R4,TEMPLEN                                                       
         BAS   RE,APPEND           STORE DSECT CARD IN BUFFER                   
         BNE   EXBASE              EXIT ON ERROR                                
*                                                                               
GETDS25  LA    R2,TEMPBUFF                                                      
         MVC   0(2,R2),DSPCHK+1    STORE DISPL. AS HEX                          
         LA    R2,2(R2)                                                         
         MVC   0(L'SRFADLBL,R2),SRFADLBL                                        
         LA    R2,L'SRFADLBL(R2)   STORE LABELS                                 
         MVC   0(L'SRFADDEF,R2),SRFADDEF                                        
         LA    R2,L'SRFADDEF(R2)   STORE DEFINITIONS                            
         L     R4,TEMPLEN                                                       
         LA    R4,2+L'SRFADLBL+L'SRFADDEF(R4)                                   
         ST    R4,TEMPLEN                                                       
*                                                                               
         LA    R4,DLINE+56         POINT TO BEGINNING OF TYPE                   
         OC    0(1,R4),0(R4)       IS TYPE NULL?                                
         BNZ   GETDS30             NOPE, BRANCH TO CHECK FOR A SPACE            
         MVI   0(R2),0             YUP, PUT ZERO FOR LENGTH IN BUFFER           
         LA    R2,1(R2)            BUMP BUFFER FORWARD 1 CHAR.                  
         L     R6,TEMPLEN                                                       
         LA    R6,1(R6)                                                         
         ST    R6,TEMPLEN                                                       
         B     GETDS50             GO PARSE COMMENT                             
*                                                                               
GETDS30  DS    0H                                                               
         CLI   0(R4),C' '          IS THERE A SPACE??                           
         BNE   GETDS40             NOPE, GO GET LENGTH                          
         MVI   0(R2),0             YUP, PUT ZERO FOR LENGTH                     
         LA    R2,1(R2)                                                         
         L     R6,TEMPLEN                                                       
         LA    R6,1(R6)                                                         
         ST    R6,TEMPLEN                                                       
         B     GETDS50             GO PARSE COMMENT                             
*                                                                               
GETDS40  DS    0H                  GETS TYPE LENGTH                             
         SR    R5,R5               SET TYPE LENGTH COUNTER                      
GETDS45  LA    R4,1(R4)            LOOK AT NEXT CHAR IN TYPE                    
         LA    R5,1(R5)            KEEP COUNT                                   
         CIJH  R5,55,ERROR30       DID WE PARSE TOO FAR? IF YES...              
*                                  ...INPUT LINE IS TOO LONG.                   
         CLI   0(R4),C' '          HAS TYPE ENDED?                              
         BE    *+14                                                             
         OC    0(1,R4),0(R4)                                                    
         BNZ   GETDS45                                                          
*                                  ...INPUT LINE IS TOO LONG.                   
         STCM  R5,1,0(R2)          STORE LENGTH TO BUFFER                       
         L     R4,TEMPLEN          ADD LENGTH TO TEMP COUNTER                   
         LA    R4,1(R4,R5)                                                      
         ST    R4,TEMPLEN                                                       
         LA    R2,1(R2)                                                         
         LA    R4,DLINE+56         POINT R4 TO BEGINNING OF TYPE                
         MVC   0(1,R2),0(R4)       STORE TYPE TO BUFFER                         
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
         SHI   R5,1                                                             
         BP    *-18                                                             
*                                                                               
GETDS50  DS    0H                  GETS COMMENT LENGTH                          
         LA    R5,DLINE+111        R5 POINTS TO END OF INPUT LINE               
GETDS55  LA    R4,1(R4)            LOOK FOR BEGINNING OF COMMENT                
         CRJNH R4,R5,GETDS56       IF EOL, THEN THERE'S NO COMMENT              
*                                                                               
         MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
         L     R6,TEMPLEN                                                       
         LA    R6,1(R6)                                                         
         ST    R6,TEMPLEN                                                       
         B     GETDS60             GET NEXT DSECT LINE                          
*                                                                               
GETDS56  CLI   0(R4),C' '          KEEP LOOKING                                 
         BE    GETDS55                                                          
         OC    0(1,R4),0(R4)                                                    
         BZ    GETDS55                                                          
*                                                                               
GETDS57  DS    0H                                                               
         CLI   0(R5),C' '          PARSE FROM THE BACK                          
         BE    *+14                                                             
         OC    0(1,R5),0(R5)                                                    
         BNZ   GETDS58             LOOK FOR FIRST NON-BLANK CHAR.               
*                                                                               
         BCTR  R5,0                                                             
         CRJH  R4,R5,ERROR40                                                    
         B     GETDS57                                                          
*                                                                               
GETDS58  DS    0H                                                               
         LA    R5,1(R5)            MOVE POINTER BACK TO LAST CHAR.              
         SR    R5,R4               R5 GETS LENGTH OF COMMENT                    
         STCM  R5,1,0(R2)          STORE LENGTH IN BUFFER                       
         L     R6,TEMPLEN          ADD LENGTH TO INPUT COUNTER                  
         LA    R6,1(R6,R5)                                                      
         ST    R6,TEMPLEN                                                       
         LA    R2,1(R2)                                                         
*                                                                               
GETDS59  DS    0H                                                               
         MVC   0(1,R2),0(R4)       STORE COMMENT TO BUFFER                      
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
         SHI   R5,1                                                             
         BP    GETDS59                                                          
*                                                                               
GETDS60  DS    0H                  GET NEXT DSECT LINE                          
         CLI   SRFADDEF,C'D'                                                    
         BNE   GETDS63                                                          
         L     R4,TEMPLEN                                                       
         ST    R4,DLEN                                                          
         MVC   DSTRING,TEMPBUFF                                                 
*                                                                               
GETDS63  BAS   RE,APPEND                                                        
         BNE   EXBASE              EXIT ON ERROR                                
*                                                                               
GETDS65  MVC   OBLINE,SPACES                                                    
         B     GETDS10                                                          
*                                                                               
DSEOF    DS    0H                                                               
         LA    R3,DSBUFF                                                        
         A     R3,ICOUNTER                                                      
         MVC   0(3,R3),=X'FFFF00'  EOF MARKER                                   
         L     R4,ICOUNTER                                                      
         LA    R4,3(R4)                                                         
         ST    R4,ICOUNTER                                                      
         BAS   RE,DISPLAY          SHOW SIZES                                   
         CLOSE (DSECTIN)                                                        
         CLC   =C'T15E',PHASENM                                                 
         BNE   ERROR20                                                          
         BAS   RE,PRTDS            PUNCH THE OBJECT FILE                        
*                                                                               
* GENERATE EMPTY PHASES FOR ALL REMAINING OVERLAY NUMBERS FOR THE               
* SYSTEM.                                                                       
*                                                                               
         LHI   R0,X'0F'            DEFAULT HIGH OVERLAY NUMBER IS X'F'          
         CLI   PHASENM+4,C'F'      LAST SYSTEM HAS PHASE UP TO E ONLY           
         BNE   *+8                                                              
         LHI   R0,X'0E'            YES: HIGH OVERLAY NUMBER IS X'E'             
*                                                                               
DUMMYPHS DS    0H                                                               
         LH    R4,PHCNT            BUMP TO NEXT PHASE                           
         AHI   R4,1                                                             
         STH   R4,PHCNT                                                         
         CH    R0,PHCNT            ANY MORE PHASES TO GENERATE?                 
         BL    CLOSEOBJ            NO                                           
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PHCNT+1,TEMP,1                                   
         L     R5,DMCB+16                                                       
         MVC   PHASENM+5(1),TEMP+1 SET PHASE NAME                               
         MVC   DSBUFF(3),=X'FFFF00'  SET EOF MARKER                             
         MVC   ICOUNTER,=F'3'                                                   
*                                                                               
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(8),=C'SYSTEM: '                                                
         MVC   P+8(L'SYSNAME),SYSNAME                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(7),=C'PHASE: '                                                 
         MVC   P+7(L'PHASENM),PHASENM                                           
         MVC   P+20(9),=C'**EMPTY**'                                            
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTDS            PRINT DECK                                   
         B     DUMMYPHS                                                         
*                                                                               
CLOSEOBJ DS    0H                                                               
         CLOSE (OBJFIL)                                                         
         B     EXBASE                                                           
*                                                                               
ERROR10  GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: (HEADER) DSECT TOO BIG! ***'                 
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     EXBASE                                                           
ERROR15  GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: (BODY) DSECT TOO BIG! ***'                   
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     EXBASE                                                           
ERROR20  GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: INCORRECT PHASE NUMBER! ***'                 
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     EXBASE                                                           
ERROR30  GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: PARSE ERROR AT TYPE! ***'                    
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     EXBASE                                                           
ERROR40  GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: PARSE ERROR AT COMMENT! ***'                 
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     EXBASE                                                           
ERROR50  GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: DISPLACEMENT > FFFF! ***'                    
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     EXBASE                                                           
ERROR60  GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: SYSTEM NAME NOT FOUND! ***'                  
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     EXBASE                                                           
ERROR70  GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: NOT ENOUGH SPACE! ***'                       
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     EXBASE                                                           
*                                                                               
EXBASE   DS    0H                                                               
         XBASE RC=RETCODE                                                       
*                                                                               
*********************************************************************           
* DISPLAYS INDIVIDUAL AND TOTAL SIZES OF DATASETS                               
*********************************************************************           
*                                                                               
DISPLAY  NTR1                                                                   
         L     R6,ICOUNTER                                                      
         S     R6,OLDCOUNT                                                      
         EDIT  (R6),(8,P+10)                                                    
         EDIT  ICOUNTER,(8,P+18)                                                
         GOTO1 =V(PRINTER)                                                      
         B     XIT                                                              
*                                                                               
*********************************************************************           
* APPENDS TEMPBUFF TO DSBUFF AND INCREMENTS COUNTER                             
*********************************************************************           
*                                                                               
APPEND   NTR1                                                                   
         LA    R3,DSBUFF                                                        
         A     R3,ICOUNTER         SET R3 TO BUFFER                             
         L     R4,ICOUNTER         THIS IS WHAT WE HAVE IN BUFFER NOW           
         A     R4,TEMPLEN          IF WE ADD THIS DSECT,                        
         C     R4,MAXBYTES         WILL BUFFER OVERFLOW?                        
         BNH   APPEND10            IF NOT, ADD TO BUFFER                        
*                                                                               
         MVC   0(3,R3),=X'FFFF01'  ELSE, INSERT EOF MARK                        
         L     R4,ICOUNTER         ADD TO COUNTER                               
         LA    R4,3(R4)            AND PRINT THE BUFFER                         
         ST    R4,ICOUNTER                                                      
         BAS   RE,DISPLAY          SHOW SIZES                                   
         BAS   RE,PRTDS            PRINT DECK                                   
         LH    R4,PHCNT            BUMP TO NEXT PHASE                           
         AHI   R4,1                                                             
         STH   R4,PHCNT                                                         
         CLI   PHASENM+4,C'F'      LAST SYSTEM HAS PHASE UP TO E ONLY           
         BNE   APPEND05                                                         
         CIJL  R4,15,APPEND07                                                   
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: NOT ENOUGH SPACE! ***'                       
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     NO                  ERROR ENCOUNTERED, STOP PROGRAM              
*                                                                               
APPEND05 DS    0H                                                               
         CIJL  R4,16,APPEND07      DID WE RUN OUT OF PHASES? IF NOT,ADD         
         GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: NOT ENOUGH SPACE! ***'                       
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     NO                  ERROR ENCOUNTERED, STOP PROGRAM              
*                                                                               
APPEND07 DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,PHCNT+1,TEMP,1                                   
         L     R5,DMCB+16                                                       
         CIJNE R5,0,APPEND08                                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'*** ERROR: SYSTEM NAME NOT FOUND! ***'                  
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'12'                                                   
         B     NO                  ERROR ENCOUNTERED, STOP PROGRAM              
*                                                                               
APPEND08 DS    0H                                                               
         MVC   PHASENM+5(1),TEMP+1 START PROCESSING WITH NEXT PHASE             
         LA    R3,DSBUFF           RESET R3 TO HEAD OF DSBUFF                   
*                                                                               
         MVC   P,SPACES            PRINT HEADER FOR NEXT PHASE                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(8),=C'SYSTEM: '                                                
         MVC   P+8(L'SYSNAME),SYSNAME                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(7),=C'PHASE: '                                                 
         MVC   P+7(L'PHASENM),PHASENM                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P,=CL132'  MEMBER      SIZE   TOTAL'                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(10),=C'  (SAME)  '                                             
*                                                                               
APPEND10 DS    0H                  APPEND STRING IN TEMP TO DSBUFF              
         L     R4,TEMPLEN                                                       
         BCTR  R4,0                                                             
         EXRL  R4,*+10                                                          
         B     *+10                                                             
         MVC   0(0,R3),TEMPBUFF                                                 
         AR    R3,R4               BUMP R3 TO NEXT AVAILABLE SPACE              
         L     R4,ICOUNTER                                                      
         A     R4,TEMPLEN                                                       
         ST    R4,ICOUNTER                                                      
         XC    TEMPBUFF,TEMPBUFF   CLEAR TEMPBUFF                               
         LA    R4,0                                                             
         ST    R4,TEMPLEN          RESET LENGTH TO 0                            
         B     YES                                                              
         EJECT                                                                  
*********************************************************************           
* PRINT ESD CARD, BUFFER CONTENTS, END CARD AND NAME CARDS TO OBJFIL.           
*********************************************************************           
*                                                                               
PRTDS    NTR1                                                                   
         MVC   OBLINE,SPACES       PRINT ESD CARD                               
         MVI   OBLINE,X'02'                                                     
         MVC   OBLINE+1(3),=C'ESD'                                              
         MVC   OBLINE+10(2),=H'16'                                              
         MVC   OBLINE+14(2),=H'01'                                              
         MVC   OBLINE+16(L'PHASENM),PHASENM                                     
         MVC   OBLINE+24(4),=F'0'                                               
         MVI   OBLINE+29,0                                                      
         L     R4,ICOUNTER                                                      
         STCM  R4,3,OBLINE+30                                                   
         MVC   OBLINE+72(8),=C'00000001'                                        
         PUT   OBJFIL,OBLINE                                                    
         MVC   OBLINE,SPACES                                                    
*                                                                               
         LA    R3,DSBUFF                                                        
         SR    R4,R4                                                            
         SR    R5,R5               SET PRINT COUNTER TO 0                       
         ST    R5,OCOUNTER                                                      
         LA    R5,2                R5 IS CURRENT SIZE                           
*                                                                               
PRTDS10  DS    0H                  PRINT BUFFER TO CARDS                        
         MVI   OBLINE,X'02'        PRINT HEADER FOR LINE                        
         MVC   OBLINE+1(3),=C'TXT'                                              
         ST    R4,DUB                                                           
         MVC   OBLINE+5(3),DUB+1                                                
         MVC   OBLINE+14(2),=H'1'                                               
         CVD   R5,DUB                                                           
         MVC   OBLINE+72(4),=C'0000'                                            
         UNPK  OBLINE+76(4),DUB                                                 
         OI    OBLINE+79,X'F0'                                                  
         L     R6,OCOUNTER                                                      
         LA    R6,56(R6)                                                        
         ST    R6,OCOUNTER                                                      
         C     R6,ICOUNTER         IS LAST LINE 56 CHARS OR LESS??              
         BNL   PRTDS20             YUP, GO AND PRINT LAST CARD                  
*                                  PRINT LAST TEXT CARD                         
         MVC   OBLINE+10(2),=H'56' ELSE PRINT FULL 56 BYTE CARD                 
         MVC   OBLINE+16(56),0(R3)                                              
         PUT   OBJFIL,OBLINE                                                    
         MVC   OBLINE,SPACES                                                    
         LA    R3,56(R3)           BUMP TO NEXT LINE IN BUFFER                  
         LA    R4,56(R4)                                                        
         LA    R5,1(R5)            INCREMENT ALL COUNTERS                       
         B     PRTDS10                                                          
*                                  PRINT LAST TEXT CARD                         
PRTDS20  DS    0H                                                               
         L     R6,ICOUNTER         LAST CARD IS 56 BYTES OR LESS                
         LA    R6,56(R6)                                                        
         S     R6,OCOUNTER                                                      
         STH   R6,DUB                                                           
         MVC   OBLINE+10(2),DUB                                                 
         BCTR  R6,0                                                             
         EXRL  R6,*+10                                                          
         B     *+10                                                             
         MVC   OBLINE+16(0),0(R3)                                               
         PUT   OBJFIL,OBLINE                                                    
         MVC   OBLINE,SPACES                                                    
         LA    R5,1(R5)                                                         
*                                                                               
* LAST CARD MAY BE LESS THAN 56!                                                
*                                  PRINT END CARD                               
         MVI   OBLINE,X'02'                                                     
         MVC   OBLINE+1(3),=C'END'                                              
         CVD   R5,DUB                                                           
         MVC   OBLINE+72(4),=C'0000'                                            
         UNPK  OBLINE+76(4),DUB                                                 
         OI    OBLINE+79,X'F0'                                                  
         PUT   OBJFIL,OBLINE                                                    
         MVC   OBLINE,SPACES                                                    
*                                  PRINT NAME CARD                              
         MVC   OBLINE(80),=CL80' NAME '                                         
         LA    R4,OBLINE+6                                                      
         MVC   0(L'PHASENM,R4),PHASENM                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   *-8                                                              
         MVC   0(3,R4),=C'(R)'                                                  
         PUT   OBJFIL,OBLINE                                                    
*                                                                               
         XC    ICOUNTER,ICOUNTER   RESET COUNTERS AND BUFFER                    
         XC    OLDCOUNT,OLDCOUNT                                                
*                                                                               
         B     XIT                                                              
*                                                                               
*********************************************************************           
*                                                                               
         ANSR                                                                   
*                                                                               
**************************** DEFS ***********************************           
*                                                                               
DSECTIN  DCB   DDNAME=DSECTIN,DSORG=PS,RECFM=FBM,LRECL=121,            +        
               MACRF=GM,EODAD=DSEOF                                             
OBJFIL   DCB   DDNAME=OBJFIL,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM                
*                                                                               
UTL      DS    0D                                                               
         DC    4X'00',X'0A'        UTL FOR CONTROL SYSTEM                       
*                                                                               
         DS    0L                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
         SPACE 2                                                                
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL20                                                             
*                                                                               
TEMP     DC    256C' '             ALL PURPOSE TEMPORARY BUFFER                 
DLINE    DS    CL121               HOLDS ONE INPUT LINE                         
OBLINE   DC    CL80' '             HOLDS ONE OUTPUT LINE                        
PHASENM  DC    CL8' '              PHASE NAME                                   
SYSNAME  DC    CL7' '              SYSTEM NAME                                  
PHCNT    DC    H'0'                PHASE COUNTER                                
DSEQU    DC    CL5' '              CHECKS FOR EQU'S                             
ICOUNTER DC    F'0'                INPUT LINE ICOUNTER                          
OCOUNTER DC    F'0'                OUTPUT LINE ICOUNTER                         
OLDCOUNT DC    F'0'                KEEP CURRENT COUNT                           
RETCODE  DC    F'0'                RETURN CODE                                  
DSPCHK   DS    F                   CHECK IF DISPL. HEX                          
EQUCHK   DS    F                   CHECK IF EQU. HEX                            
DLEN     DS    F                   LENGTH OF DSECT CARD                         
DSFLAG   DC    X'00'               SIGNALS IF DATASET HEADER FOUND              
DFLAG    DC    X'00'               SIGNALS IF DSECT CARD FOUND                  
DSNAME   DS    CL8                 DATASET NAME                                 
SRFADDSP DS    CL6                                                              
SRFADLBL DS    CL8                                                              
SRFADDEF DS    CL1                                                              
MAXBYTES DC    F'20308'            CHANGED PER HABER                            
*MAXBYTES DC    F'32596'            MAXBYTE=DSBUFF-(ESD+END+NAME)-3             
TEMPBUFF DS    0CL256              TEMPORARY BUFFER                             
         DC    256C' '                                                          
DSTRING  DS    0CL256              BUFFER HOLDING A DSECT CARD                  
         DC    256C' '                                                          
TEMPLEN  DS    F                   STRING LENGTH IN TEMP BUFF                   
DSECTCMT DC    CL36' '             DSECT COMMENT FROM '<   >'                   
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
DSBUFF   DS    32767C              MAX SIZE FOR 1 PHASE (585 LINES)             
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 3                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDDSGEN   05/14/12'                                      
         END                                                                    
