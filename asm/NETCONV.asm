*          DATA SET NETCONV    AT LEVEL 003 AS OF 05/01/02                      
*PHASE NETCONVA                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE DEMADDR                                                                
*INCLUDE DEMDISP                                                                
*INCLUDE DEMAINT                                                                
*INCLUDE DEMEL                                                                  
*INCLUDE DMDMGR                                                                 
*INCLUDE DMFILES,(DMCNTL,SYSAFLES)                                              
*INCLUDE DMISDDS                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE TICTOC                                                                 
*INCLUDR IJDFYZZZ                                                               
*INCLUDR IJFVZZWZ                                                               
         SPACE 2                                                                
*                                                                               
* THIS PROGRAM CONVERTS THE OLD NETWORK FILE TO THE NEW FORMAT.                 
* DEMO ELEMENTS ON THE OLD RECORDS ARE CONVERTED TO NTI FILE FORMAT.            
* THE OLD DEMO-LOOKUPS ARE USED TO EXTRACT VPH VALUES FOR THE NEW FILE.         
* NETCONV CONVERTS NON VAR 'P' RECORDS DIRECTLY TO PAV DEMO FORMAT.             
* THE PROGRAM CONVERTS VAR 'P' RECORDS INTO 'Q' OR NETWORK PROGRAM              
* RECORDS.  IT CONSTRUCTS PASSIVE 'N' RECORDS FOR EVERY 'P' AND 'Q'             
* RECORD.  FOR M-F AND M-SUN PROGRAMS, PASSIVE RECORDS ARE ADDED UNDER          
* THE INDIVIDUAL DAYS AS WELL AS UNDER THE ROTATION.                            
*                                                                               
         TITLE 'CONVERT OLD NETWORK FILE TO NEW FORMAT'                         
NETCONV  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**NETCON,=V(DEWORK),R9,RR=RE,CLEAR=YES               
         USING WORKD,RC                                                         
         USING NETCONV+4096,R9                                                  
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         ZAP   LINE,=PL2'99'                                                    
         MVC   TITLE+14(23),=C'NETWORK FILE CONVERSION'                         
         GOTO1 PRINTER                                                          
         SPACE 2                                                                
* INITIALIZATION CODE                                                           
*                                                                               
INITIAL  DS    0H                                                               
         OPEN  IN,OUT                                                           
         L     R2,=A(SORTC)                                                     
         GOTO1 SORTER,DMCB,SORTFLD,RECTYPE,(200,(R2))                           
         SPACE 1                                                                
         L     R8,ACOMFACS         MAKE COMFACS ADDRESSABLE AND OPEN            
         USING COMFACSD,R8         CONTROL FILE                                 
         GOTO1 CDATAMGR,DMCB,=CL8'DMOPEN',=C'CONTROL',=C'NCTFILE X',   X        
               OREC                                                             
         SPACE 1                                                                
         MVC   DUB,=CL8'T00A05'    LOAD IN DEMEX                                
         L     R0,=V(T00A05)                                                    
         GOTO1 LOADER,DMCB,DUB,(R0)                                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DEMEX,4(R1)                                                      
         MVC   DUB,=CL8'T00A0B'    NOW LOAD IN PAVEXPL                          
         L     R0,=V(T00A0B)                                                    
         GOTO1 (RF),(R1),DUB,(R0)                                               
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   PAVEXPL,4(R1)                                                    
         EJECT                                                                  
********************************************************************            
* ---READ INPUT TAPE AND PROCESS---                                *            
* IREC HAS OLD RECORD, OREC HAS 'P' RECORD, OREC2 HAS 'Q' RECORD   *            
*                                                                  *            
* R3 POINTS TO NETWORK RUN TIME ELEM  R4 POINTS TO 'Q' RECORD      *            
* R5 POINTS TO PAV DAY/TIME ELEM      R6 POINTS TO PAV 'P' RECORD  *            
* R7 POINTS TO OLD RECORD                                          *            
********************************************************************            
         SPACE 1                                                                
INPUT    DS    0H                                                               
         LA    R2,IREC-4                                                        
         GET   IN,(R2)                                                          
         L     R1,RECIN            INCREMENT INPUT RECORDS COUNT                
         LA    R1,1(R1)                                                         
         ST    R1,RECIN                                                         
         CLI   IREC,C'P'           TEST FOR PAV DEMO RECORDS                    
         BNE   INPUT                                                            
*                                                                               
         LH    R1,IREC-4           RECORD LENGTH                                
         LA    RE,IREC-4(R1)       POINT TO EOR                                 
         XC    0(10,RE),0(RE)      AND CLEAR A MARKER                           
         L     R1,RECPROC                                                       
         LA    R1,1(R1)            INCREMENT P RECORD COUNT                     
         ST    R1,RECPROC                                                       
         SPACE 1                                                                
INPUT2   DS    0H                                                               
         MVI   BYTE,C'I'                                                        
         GOTO1 DUMPREC,DMCB,IREC-4                                              
         BAS   RE,CNVEL            CONVERT THE DEMO ELEMENTS                    
         LA    RE,OREC-4                                                        
         LA    RF,1004                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR OUTPUT RECORD AREA                     
*                                                                               
         LA    R6,OREC                                                          
         USING PRKEY,R6                                                         
         LA    R7,IREC             POINT TO INPUT RECORD                        
*                                                                               
         MVI   PRCODE,PRCODEQU     BUILD NEW KEY FROM OLD REC                   
         MVI   PRMEDIA,C'N'        SET TO NETWORK                               
         MVI   PRSRC,C'N'          SET TO NSI                                   
         MVC   PRSTAT,3(R7)        STATION FIELD IS NETWORK OR HUT              
         MVC   PRKMKT,8(R7)        SPILL MARKET                                 
         MVC   PRBOOK,12(R7)       BOOK (YYMM)                                  
         MVC   PRSTIM,10(R7)       START QTR HR                                 
         MVC   PRDW,11(R7)         DAY                                          
         SPACE 1                                                                
* MOVE ALL ELEMENTS EXCEPT THE X'14' (FILE TYPE) ELEMENT                        
* FROM OLD TO NEW 'P' RECORDS.                                                  
*                                                                               
INPUT4   DS    0H                                                               
         LA    R2,PRFRSTEL-PRKEY   R2 INITIALIZED TO KEY LENGTH                 
         LA    R4,0(R2,R6)         POINT R4 AT FIRST ELEM ON NEW REC            
         LA    R7,16(R7)           POINT TO FIRST ELEM ON OLD REC               
         SPACE 1                                                                
INPUT5   CLI   0(R7),0             TEST FOR END OF RECORD                       
         BE    INPUT8                                                           
         CLI   0(R7),X'14'         BYPASS SPOT/NETWORK ELEMENT                  
         BNE   INPUT6                                                           
         ZIC   R3,1(R7)                                                         
         AR    R7,R3                                                            
         B     INPUT5                                                           
         SPACE 1                                                                
INPUT6   DS    0H                                                               
         ZIC   R3,1(R7)            ELEMENT LENGTH                               
         AR    R2,R3               UPDATE NEW RECORD LENGTH                     
         BCTR  R3,0                                                             
         EX    R3,*+8              MOVE EL TO NEW RECORD                        
         B     *+10                                                             
         MVC   0(0,R4),0(R7)                                                    
         LA    R3,1(R3)            RESTORE ELEMENT LENGTH                       
         AR    R7,R3               NEXT ELEMENT ON OLD REC                      
         AR    R4,R3               NEXT POSITION ON NEW REC                     
         STH   R2,PRRLEN           SAVE RECORD LENGTH                           
         B     INPUT5                                                           
         SPACE 1                                                                
* CORRECT DURATION ON DAY/QTR HR ELEMENT BY MAKING IT EVEN                      
* AND AT LEAST 1 - R5 POINTS TO DAY QUARTER HOUR ELEMENT                        
*                                                                               
INPUT8   DS    0H                                                               
         LR    R5,R6               START OF 'P' RECORD                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PHELEM,R5                                                        
         ZIC   R0,PHDUR                                                         
         SRL   R0,1                SHIFT OUT THE ONES BIT                       
         SLL   R0,1                TO MAKE DURATION EVEN                        
         CH    R0,=H'1'            TEST FOR DURATION GT 1                       
         BH    *+8                                                              
         LA    R0,1                DURATION MUST BE AT LEAST 1                  
         STC   R0,PHDUR                                                         
         SPACE 1                                                                
* SET RECORD AND TAPE LENGTHS AND PUT 'P' RECORDS TO SORTER                     
*                                                                               
INPUT9   DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,3,PRRLEN         RECORD LENGTH                                
         LA    RE,OREC(R1)         POINT TO END OF RECORD                       
         MVI   0(RE),0             AND ADD A ZERO FOR HELLO                     
         LA    R1,1(R1)            COMPATIBILITY AND ADJUST RECORD              
         STCM  R1,3,PRRLEN         FOR ZERO AT EOR.                             
         LA    R1,4(R1)            NOW ADJUST SORT RECORD LENGTH FOR            
         STCM  R1,3,OREC-4         VARIABLE REC HEADER.                         
*                                                                               
         ZIC   R1,PRDW                                                          
         SRL   R1,4                ZERO OUT WEEK NIBBLE                         
         STC   R1,BYTE             DAY NIBBLE                                   
         CLI   BYTE,9              TEST FOR VAR RECORD                          
         BE    INPUT10                                                          
*                                                                               
         LA    R2,OREC-4                                                        
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         L     R1,PREC             INCREMENT SORT RECORD COUNT                  
         LA    R1,1(R1)                                                         
         ST    R1,PREC                                                          
         MVI   BYTE,C'P'                                                        
         GOTO1 DUMPREC,DMCB,OREC-4                                              
         B     INPUT15             GO TO PASSIVE RECORD ROUTINE                 
         SPACE 1                                                                
* ADD A 'P' VAR RECORD AS A 'Q' BY COPYING THE RECORD TO OREC2,                 
* CHANGING THE KEY, AND ADDING NETWORK PROGRAM RUN/TIME ELEMENT                 
*                                                                               
INPUT10  DS    0H                                                               
         LA    RE,OREC2-4          MOVE RECORD TO OUTPUT AREA 2                 
         LA    RF,1004             LENGTH OF HEADER PLUS BUFFER                 
         LA    R0,OREC-4                                                        
         LH    R1,OREC-4           ACTUAL RECORD LENGTH                         
         MVCL  RE,R0                                                            
         SPACE 1                                                                
* BUILD 'Q' RECORD KEY                                                          
*                                                                               
INPUT11  DS    0H                                                               
         LA    R4,OREC2                                                         
         USING PMKEY,R4                                                         
         XC    PMKMAJOR,PMKMAJOR   CLEAR AREA FOR MAJOR KEY                     
         MVI   PMCODE,PMCODEQU     RECORD TYPE                                  
         MVC   PMMEDIA(2),PRMEDIA  TRANSFER MEDIA AND SOURCE                    
         MVC   PMBOOK,PRBOOK                                                    
         MVC   PMSTAT,PRSTAT                                                    
         MVC   PMPNUM,PHPNUM       PROGRAM NUMBER                               
         SPACE                                                                  
* BUILD NETWORK PROGRAM RUN/TIME ELEMENT                                        
*                                                                               
INPUT12  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING NTELEM,R3                                                        
         MVI   NTCODE,NTCODEQU                                                  
         MVI   NTLEN,NTLENEQ                                                    
         MVC   NTSQH,PRSTIM        START QUARTER HOUR                           
         ZIC   R0,NTSQH                                                         
         ZIC   R1,PHDUR            QH DURATION FROM X'20' ELEMENT               
         LR    RE,R1                                                            
         MH    RE,=H'15'           CONVERT QH DUR TO MINUTES                    
         STC   RE,NTDUR                                                         
         BCTR  R1,0                DURATION LESS ONE IS END QTR HR              
         AR    R1,R0                                                            
         STC   R1,NTEQH            END QUARTER HOUR                             
         LA    R1,1(R1)                                                         
         MVC   HALF(1),NTSQH                                                    
         STC   R1,HALF+1                                                        
         BAS   RE,GETTIME          CONVERT TO MILITARY TIMES                    
         MVC   NTSTIM,DUB                                                       
         MVC   NTETIM,DUB+2                                                     
         OI    NTDAY,X'80'         OLD RECORDS HAVE INVALID DAY                 
         SPACE 1                                                                
* ADD ELEMENT THEN WRITE TO SORT FILE                                           
*                                                                               
INPUT13  DS    0H                                                               
         MVC   DUB,=CL8'PAVFIL'    HELLO FILE NAME                              
         GOTO1 HELLO,DMCB,(C'P',DUB),(R4),WORK,0                                
         CLI   DMCB+12,0           TEST FOR SUCCESSFUL COMPLETION               
         BE    *+6                                                              
         DC    H'0'                FAILURE IN HELLO                             
         SR    R1,R1               UPDATE TAPE RECORD LENGTH                    
         ICM   R1,3,PMRLEN                                                      
         LA    R1,4(R1)                                                         
         STH   R1,OREC2-4                                                       
         SPACE 1                                                                
         LA    R2,OREC2-4                                                       
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         L     R1,QREC                                                          
         LA    R1,1(R1)                                                         
         ST    R1,QREC                                                          
         MVI   BYTE,C'Q'                                                        
         GOTO1 DUMPREC,DMCB,OREC2-4                                             
         B     INPUT15                                                          
         DROP  R3,R4                                                            
         SPACE 1                                                                
* GENERATE PASSIVE 'N' RECORDS FOR THE CORRESPONDING 'P' RECORD                 
* EXPLODE M-F AND M-SU PROGRAMS INTO INDIVIDUAL DAYS AS WELL                    
* BUILDING A POINTER UNDER M-F OR M-SU                                          
*                                                                               
INPUT15  DS    0H                                                               
         CLC   PRSTAT(3),=C'HUT'   TEST FOR HUT RECORD                          
         BE    INPUT               SKIP PASSIVE REC GENERATION FOR IT           
         ZIC   R1,PRDW                                                          
         SRL   R1,4                ISOLATE DAY NIBBLE                           
         STC   R1,DAY                                                           
         LA    R2,5                COUNTER-NUMBER OF SINGLE DAYS                
         LA    R3,1                START EXPLODE AT MONDAY                      
         CLI   DAY,0               TEST FOR M-F RECORD                          
         BE    INPUT16                                                          
         LA    R2,7                                                             
         CLI   DAY,8               TEST FOR M-SUN                               
         BE    INPUT16                                                          
         LA    R2,1                SET COUNTER TO ONE                           
         MVI   DAY,C'Y'            SET SWITCH FOR INDIVIDUAL DAY                
         SPACE 1                                                                
INPUT16  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK+4           POINT R4 AT PASSIVE RECORD                   
         USING PNKEY,R4                                                         
         MVI   PNCODE,PNCODEQU                                                  
         MVC   PNMEDIA(2),PRMEDIA  MEDIA AND SOURCE                             
         MVC   PNBOOK,PRBOOK                                                    
         MVC   PNSTAT,PRSTAT                                                    
         MVC   PNDW,PRDW                                                        
         CLI   DAY,C'Y'            TEST FOR INDIVIDUAL DAY                      
         BE    INPUT17                                                          
         LR    R0,R3               SET DAY USING INCREMENTED VALUE              
         SLL   R0,4                SHIFT TO DAY NIBBLE                          
         MVC   BYTE2,PRDW          EXTRACT WEEK NIBBLE FROM 'P' REC             
         NI    BYTE2,X'0F'                                                      
         STC   R0,PNDW                                                          
         OC    PNDW,BYTE2          COMBINE DAY AND WEEK NIBBLES                 
         SPACE 1                                                                
INPUT17  DS    0H                                                               
         MVC   PNSTIM,PRSTIM                                                    
         MVC   PNPNUM,PHPNUM       PROGRAM NUMBER FROM X'20' ELEM               
         MVC   PNACTDAY,PRDW                                                    
         ZIC   R1,PHDUR            DURATION IN QUARTER HOURS                    
         MH    R1,=H'15'           CONVERT TO MINUTES                           
         STC   R1,PNACTDUR                                                      
         MVC   PRRLEN-PRKEY(L'PRRLEN,R4),=X'FFFF'                               
         MVC   WORK(2),=H'27'      SET RECORD LENGTH                            
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',WORK                                         
         L     R1,NREC             INCREMENT SORT RECORD COUNT                  
         LA    R1,1(R1)                                                         
         ST    R1,NREC                                                          
         MVI   BYTE,C'N'                                                        
         GOTO1 DUMPREC,DMCB,WORK                                                
         LA    R3,1(R3)            BUMP DAY NUMBER FOR EXPLOSIONS               
         BCT   R2,INPUT16                                                       
*                                                                               
         CLI   DAY,C'Y'            TEST FOR INDIVIDUAL DAY                      
         BE    INPUT               ALL DONE                                     
         MVI   DAY,C'Y'            NOW ADD POINTER UNDER M-F OR M-SU            
         LA    R2,1                SET COUNTER TO ONE                           
         B     INPUT16                                                          
         EJECT                                                                  
* ROUTINE TO DEAL WITH EOF CONDITION ON INPUT TAPE                              
*                                                                               
ENDIN    CLOSE IN                                                               
         SPACE                                                                  
ENDIN2   LA    R0,L'INMSG                                                       
         GOTO1 LOGIO,DMCB,1,((R0),INMSG)                                        
         MVI   ANSWER,C' '                                                      
         GOTO1 (RF),(R1),0,(3,ANSWER)                                           
         CLC   =C'EOF',ANSWER                                                   
         BE    ENDIN4                                                           
         CLC   =C'EOV',ANSWER                                                   
         BNE   ENDIN2                                                           
         OPEN  IN                                                               
         B     INPUT                                                            
         SPACE                                                                  
ENDIN4   DS    0H                                                               
         MVI   BYTE2,0             FOR LAST RECORD TYPE                         
         XC    FULL,FULL           RECD COUNT FOR LAST TYPE                     
         B     SORTIN                                                           
         EJECT                                                                  
* PROCESS RECORDS FROM SORT FILE                                                
*                                                                               
SORTIN   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,DMCB+4                                                     
         BZ    FINAL               END OF SORT FILE                             
         PUT   OUT,(R3)                                                         
         L     R1,RECOUT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECOUT                                                        
         SPACE 1                                                                
SORTIN2  DS    0H                                                               
         CLC   BYTE2,4(R3)         TEST FOR CHANGE IN REC TYPE                  
         BE    SORTIN3                                                          
         MVC   BYTE2,4(R3)         UPDATE REC TYPE                              
         XC    FULL,FULL           CLEAR RECD COUNTER                           
         SPACE 1                                                                
SORTIN3  DS    0H                                                               
         LA    RF,1000             PRINT ONE OF EVERY 1000 'N' AND 'P'          
         CLI   4(R3),C'Q'          RECORDS AND ONE OF EVERY 500 'Q'             
         BNE   *+8                 RECORDS                                      
         LA    RF,500                                                           
         L     R1,FULL             UPDATE COUNT FOR THIS TYPE                   
         LA    R1,1(R1)                                                         
         ST    R1,FULL                                                          
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         LTR   R0,R0               TEST FOR REMAINDER                           
         BNZ   SORTIN                                                           
         MVI   BYTE,C'O'           DUMP THIS RECORD                             
         GOTO1 DUMPREC,DMCB,(R3)                                                
         B     SORTIN                                                           
         EJECT                                                                  
* END OF SORT FILE - CLOSE FILES AND PRINT SUMMARY COUNTS                       
*                                                                               
FINAL    DS    0H                                                               
         CLOSE OUT                                                              
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         MVC   MID1+10(26),=C'NETWORK CONVERSION SUMMARY'                       
         MVI   MID2+10,C'-'                                                     
         MVC   MID2+11(25),MID2+10                                              
         GOTO1 PRINTER                                                          
*                                                                               
         LA    R3,BUCKTAB          POINT TO BUCKET TABLE                        
         LA    R4,BUCKETS          COUNTER                                      
         SPACE 1                                                                
FINAL2   DS    0H                                                               
         MVC   P+10(20),4(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            VALUE                                        
         EDIT  (R2),(7,P+32)                                                    
         GOTO1 PRINTER                                                          
         LA    R3,L'BUCKTAB(R3)                                                 
         BCT   R4,FINAL2                                                        
         SPACE 1                                                                
         XBASE                                                                  
         EJECT                                                                  
* ROUTINE TO CONVERT THE DEMO ELEMENTS TO NEW NTI FILE FORMAT                   
*                                                                               
CNVEL    NTR1                                                                   
         LA    RE,OREC             CLEAR WORK RECORD AREA                       
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   DUB,C'N'            SOURCE                                       
         LA    R1,IREC                                                          
         MVC   DUB+1(2),12(R1)     YYWW                                         
         GOTO1 PAVEXPL,DMCB,DUB,IREC+16,OREC                                    
         SPACE 1                                                                
         LA    R2,DEMOS            COUNTER                                      
         LA    R3,DEMLIST          DEMOS ON NEW RECORD                          
         LA    R4,OREC2            OUTPUT AREA                                  
         SPACE 1                                                                
CNVEL2   DS    0H                                                               
         MVC   BYTE,1(R3)          MODIFIER                                     
         MVC   BYTE2,2(R3)         DEMO NUMBER                                  
         GOTO1 DEMEX,DMCB,(BYTE,OREC),(BYTE2,(R4))                              
         LA    R3,L'DEMLIST(R3)                                                 
         LA    R4,4(R4)            NEXT OUTPUT AREA                             
         BCT   R2,CNVEL2                                                        
         SPACE 1                                                                
CNVEL4   DS    0H                                                               
         LA    RE,IREC             POINT TO FIRST ELEMENT OF RECORD             
         LA    RE,16(RE)           AND MARK ALL DEMO ELEMENTS AS X'FF'          
         SR    R1,R1                                                            
         SPACE 1                                                                
CNVEL5   DS    0H                                                               
         CLI   0(RE),0             TEST FOR EOR                                 
         BE    CNVEL7                                                           
         CLI   0(RE),X'30'         TEST FOR LOWEST DEMO EL CODE                 
         BL    CNVEL6                                                           
         CLI   0(RE),X'5F'         TEST FOR HIGHEST DEMO EL CODE                
         BH    CNVEL7              ALL DONE                                     
         MVI   0(RE),X'FF'         MARK ELEMENT FOR DELETION                    
         SPACE 1                                                                
CNVEL6   DS    0H                                                               
         IC    R1,1(RE)            BUMP TO NEXT ELEMENT                         
         AR    RE,R1                                                            
         B     CNVEL5                                                           
         SPACE 1                                                                
CNVEL7   DS    0H                                                               
         MVC   DUB,=CL8'PAVFLN'    SET FILE TO OLD NETWORK                      
         GOTO1 HELLO,DMCB,(C'D',DUB),(X'FF',IREC),0,0                           
         SPACE 1                                                                
CNVEL8   DS    0H                  CONDENSE THE NEW ELS AND ADD TO REC          
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5          R5 COVERS DBLOCK                             
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         LA    RE,IREC                                                          
         ST    RE,DBAREC                                                        
         LA    RE,16(RE)           POINT AT FIRST ELEMENT                       
         ST    RE,DBAQUART                                                      
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBINTFIL,C'P'       PAV FILE INTERNALLY                          
         MVI   DBINTMED,C'N'                                                    
         LA    R1,IREC                                                          
         MVC   DBSELBK,12(R1)      YYWW                                         
         ST    R8,DBCOMFCS                                                      
         MVC   DBNUMVLS,=AL2(DEMOS)                                             
         MVC   OBLOCK,OFORMAT      SET UP OUTPUT FORMAT BLOCK TO FORCE          
         MVC   OBLOCK+7(2),12(R1)  DEMEL TO USE KEY'S BOOK IN BOOK EL           
         GOTO1 CDEMEL,DMCB,(C'C',OBLOCK),DBLOCK,OREC2                           
         SPACE 1                                                                
         CLI   DBERROR,0           TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    OREC2(2),OREC2      TEST FOR ANY ELEMENTS                        
         BZ    CNVELX                                                           
         LA    R4,OREC2+2          POINT TO FIRST ELEMENT                       
         SR    R6,R6                                                            
         SPACE 1                                                                
CNVEL10  DS    0H                                                               
         CLI   0(R4),0             TEST FOR END OF ELEMENTS                     
         BE    CNVELX                                                           
         GOTO1 HELLO,DMCB,(C'P',DUB),DBAREC,(R4),0                              
         CLI   12(R1),0            TEST FOR SUCCESSFUL COMPLETION               
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R6,1(R4)                                                         
         AR    R4,R6               BUMP TO NEXT ELEMENT                         
         B     CNVEL10                                                          
         SPACE 1                                                                
CNVELX   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
* ROUTINE TO CONVERT FROM QUARTER HOURS TO MILITARY TIMES                       
*                                                                               
GETTIME  DS    0H                                                               
         ST    RE,FULL             SAVE RETURN POINT                            
         ZIC   R1,HALF             START QUARTER HOUR                           
         BAS   RE,GETTIME2                                                      
         STH   R1,DUB                                                           
         ZIC   R1,HALF+1           END QUARTER HOUR                             
         BAS   RE,GETTIME2                                                      
         STH   R1,DUB+2                                                         
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
GETTIME2 DS    0H                                                               
         SR    R0,R0                                                            
         D     R0,=F'4'            R1=HOURS, R0=QUARTER HOURS                   
         MH    R1,=H'100'          CALCULATE MILITARY HOUR                      
         MH    R0,=H'15'           CALCULATE MILITARY QUARTER HOUR              
         AR    R1,R0               R1=MILITARY TIME                             
         AH    R1,=H'600'          ADD BASE TIME (6AM)                          
         CH    R1,=H'2400'         TEST IF PAST MIDNIGHT                        
         BNH   *+8                                                              
         SH    R1,=H'2400'         YES-SUBTRACT 24 HOURS                        
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO PRINT RECORDS IN DUMP FORMAT - P1 IS A(RECORD)                     
*                                                                               
DUMPREC  NTR1                                                                   
         CLI   BYTE,C'O'           TEST FOR OUTPUT RECORD                       
         BE    *+14                                                             
         CLC   RECPROC,=F'200'     PRINT FIRST 200 INPUT RECORDS                
         BH    DUMPRECX                                                         
         LR    R7,R1               PARAMETER LIST                               
         MVC   MSG,SPACES          CLEAR MESSAGE AREA                           
         LA    RE,MSGTAB                                                        
         LA    R0,MESSAGES                                                      
         CLC   BYTE,0(RE)                                                       
         BE    *+14                                                             
         LA    RE,L'MSGTAB(RE)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   MSG(L'MSGTAB-1),1(RE) MOVE OUT MESSAGE                           
         LA    R0,L'MSGTAB-1                                                    
         L     R2,0(R7)            ADDRESS OF RECORD                            
         MVC   HALF,0(R2)          RECORD LENGTH                                
         LH    R3,HALF                                                          
         GOTO1 PRNTBL,DMCB,((R0),MSG),(R2),C'DUMP',(R3),=C'2D'                  
         SPACE 1                                                                
DUMPRECX XIT1                                                                   
         EJECT                                                                  
         GETEL (R5),23,ELCODE                                                   
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* ROUTINE TABLE                                                                 
*                                                                               
         DS    0F                                                               
DEMEX    DS    V                                                                
HELLO    DC    V(HELLO)                                                         
LOADER   DC    V(LOADER)                                                        
LOGIO    DC    V(LOGIO)                                                         
PAVEXPL  DS    V                                                                
PRINTER  DC    V(PRINTER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
SORTER   DC    V(SORTER)                                                        
         SPACE 2                                                                
* ADDRESS CONSTANTS                                                             
*                                                                               
ACOMFACS DC    A(COMFACS)                                                       
         SPACE 2                                                                
* BUCKET TABLE                                                                  
*        BYTES 0-3 = BUCKET                                                     
*        BYTES 4-23= BUCKET DESCRIPTION                                         
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
RECIN    DC    F'0',CL20'RECORDS IN'                                            
RECPROC  DC    F'0',CL20'RECORDS PROCESSED'                                     
NREC     DC    F'0',CL20'PASSIVE RECORDS'                                       
PREC     DC    F'0',CL20'PAV DEMO RECORDS'                                      
QREC     DC    F'0',CL20'NETWORK PROG RECS'                                     
RECOUT   DC    F'0',CL20'RECORDS OUT'                                           
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* PRNTBL MESSAGE TABLE                                                          
*              BYTE 0 = RECORD CODE                                             
*              BYTES 1-12 = MESSAGE                                             
*                                                                               
         DS    0F                                                               
MSGTAB   DS    0CL13                                                            
         DC    C'O',CL12'OUTPUT RECD'                                           
         DC    C'I',CL12'INPUT RECORD'                                          
         DC    C'P',CL12'PAV RECORD'                                            
         DC    C'Q',CL12'NETWORK PROG'                                          
         DC    C'N',CL12'PASSIVE RECD'                                          
MESSAGES EQU   (*-MSGTAB)/L'MSGTAB                                              
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
SORTFLD  DC    CL80'SORT FIELDS=(5,20,A),FORMAT=BI,WORK=1'                      
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=1024'                                  
INMSG    DC    CL40'SYS004 (PAVFLN) IS IT EOF OR EOV'                           
OFORMAT  DS    0CL10                                                            
         DC    C'NTINPNN',3X'00'                                                
         SPACE 2                                                                
* DEMO LIST FOR NTI RECORD                                                      
*                                                                               
DEMLIST  DS    0CL3                                                             
         DC    X'00',C'V',AL1(127) VV2+                                         
         DC    X'00',C'V',AL1(064) VLOH                                         
         DC    X'00',C'V',AL1(065) VWWRK                                        
         DC    X'00',C'V',AL1(045) VW18+                                        
         DC    X'00',C'V',AL1(041) VW1834                                       
         DC    X'00',C'V',AL1(042) VW1849                                       
         DC    X'00',C'V',AL1(048) VW2554                                       
         DC    X'00',C'V',AL1(058) VW5564                                       
         DC    X'00',C'V',AL1(059) VW55+                                        
         DC    X'00',C'V',AL1(095) VM18+                                        
         DC    X'00',C'V',AL1(091) VM1834                                       
         DC    X'00',C'V',AL1(092) VM1849                                       
         DC    X'00',C'V',AL1(098) VM2554                                       
         DC    X'00',C'V',AL1(108) VM5564                                       
         DC    X'00',C'V',AL1(109) VM55+                                        
         DC    X'00',C'V',AL1(125) VTEENS                                       
         DC    X'00',C'V',AL1(025) VW1217                                       
         DC    X'00',C'V',AL1(122) VCHILD                                       
         DC    X'00',C'V',AL1(123) VCH611                                       
         DC    X'00',C'R',AL1(001) RHOMES                                       
         DC    X'00',C'T',AL1(001) IHOMES ('T' USED IN DEMEX)                   
         DC    X'00',C'S',AL1(001) SHOMES                                       
DEMOS    EQU   (*-DEMLIST)/L'DEMLIST                                            
         SPACE 2                                                                
* DBLOCK AREA                                                                   
*                                                                               
         DS    0F                                                               
DBLOCKA  DS    XL256                                                            
         EJECT                                                                  
* COMMON FACILITIES FOR DEMO SYSTEM                                             
*                                                                               
COMFACS  DS    0F                                                               
         DC    V(DATAMGR)                                                       
         DC    4A(0)                                                            
         DC    V(HELLO)                                                         
         DC    17A(0)                                                           
         DC    V(T00AE1)                                                        
         DC    V(DEMADDR)                                                       
         DC    V(DEMDISP)                                                       
         DC    V(T00AD1)                                                        
         DC    V(T00AD2)                                                        
         DC    V(T00AD3)                                                        
         DC    V(T00AD4)                                                        
         DC    V(T00AD5)                                                        
         DC    V(T00AD6)                                                        
         DC    V(T00AD7)                                                        
         DC    V(T00AD8)                                                        
         DC    A(0)                                                             
         DC    V(DEMEL)                                                         
         DC    V(DEMAINT)                                                       
         DC    2A(0)                                                            
         EJECT                                                                  
* DTF'S PLUS I/O AND SORT BUFFERS                                               
*                                                                               
IN       DTFMT DEVADDR=SYS004,BLKSIZE=8500,RECFORM=VARBLK,             X        
               TYPEFLE=INPUT,IOAREA1=IN1,FILABL=STD,                   X        
               EOFADDR=ENDIN,REWIND=UNLOAD,WORKA=YES                            
         SPACE 2                                                                
OUT      DTFMT DEVADDR=SYS006,BLKSIZE=8200,RECFORM=VARBLK,             X        
               TYPEFLE=OUTPUT,IOAREA1=OUT1,FILABL=STD,                 X        
               REWIND=UNLOAD,WORKA=YES                                          
         SPACE 2                                                                
IN1      DS    CL8500                                                           
OUT1     DS    CL8200                                                           
         DS    0D                                                               
SORTC    DC    205000X'00'                                                      
         EJECT                                                                  
* BUFFERS FOR LOADED MODULES AND WORKING STORAGE                                
*                                                                               
T00A05   CSECT                                                                  
         DS    10000C                                                           
         SPACE 2                                                                
T00A0B   CSECT                                                                  
         DS    3000C                                                            
         SPACE 2                                                                
DEWORK   CSECT                                                                  
         DC    10000X'00'                                                       
         SPACE 2                                                                
* DEMO LOOK-UP TABLES                                                           
*                                                                               
T00AD1   CSECT                                                                  
         DC    1000X'00'           DBOOK                                        
         SPACE 1                                                                
T00AD2   CSECT                                                                  
         DC    2000X'00'           DSTATION                                     
         SPACE 1                                                                
T00AD3   CSECT                                                                  
         DC    25000X'00'          DMASTER                                      
         SPACE 1                                                                
T00AD4   CSECT                                                                  
         DC    65000X'00'          DFORMULA                                     
         SPACE 1                                                                
T00AD5   CSECT                                                                  
         DC    15000X'00'          DNAME                                        
         SPACE 1                                                                
T00AD6   CSECT                                                                  
         DC    2000X'00'           DCODE                                        
         SPACE 1                                                                
T00AD7   CSECT                                                                  
         DC    5000X'00'           DCONTROL                                     
         SPACE 1                                                                
T00AD8   CSECT                                                                  
         DC    5000X'00'           DADJUST                                      
         SPACE 1                                                                
T00AE1   CSECT                                                                  
         DC    15000X'00'          DDISPSRT                                     
         SPACE 1                                                                
         EJECT                                                                  
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
WORK     DS    CL60                                                             
MSG      DS    CL40                                                             
OBLOCK   DS    CL10                                                             
ELCODE   DS    C                                                                
BYTE     DS    C                                                                
BYTE2    DS    C                                                                
DAY      DS    X                                                                
ANSWER   DS    CL3                                                              
         DS    F                                                                
IREC     DS    CL1000                                                           
         DS    F                                                                
OREC     DS    CL1000                                                           
         DS    F                                                                
OREC2    DS    CL1000                                                           
WORKX    EQU   *                                                                
         EJECT                                                                  
* COMFACSD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DBLOCKD                                                                       
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NETCONV   05/01/02'                                      
         END                                                                    
