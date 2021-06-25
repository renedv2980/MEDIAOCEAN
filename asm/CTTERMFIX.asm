*          DATA SET CTTERMFIX  AT LEVEL 003 AS OF 03/16/18                      
*PHASE CTTRMFXA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE PRTREC                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCINKEY                                                                
*                                                                               
         TITLE 'CTTERMFIX - UPDATE TERMINAL RECORDS ID LIST'                    
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
CTTRMFX  CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**CTFX**,=V(REGSAVE),CLEAR=YES                       
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BRAS  RE,INIT             READ CARDS ECT                               
         BRAS  RE,MAIN             MAIN LOOP                                    
         BRAS  RE,DONE             CLOSE ALL                                    
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
                                                                                
***********************************************************************         
* PROCESS RECORDS                                                               
***********************************************************************         
MAIN     NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,READINP          READ INPUT DATA, AND SAVE IN TABLES          
*                                                                               
         MVC   ATERM,ATERMTAB      CURRENT TERMINAL                             
*                                                                               
MAIN10   DS    0H                  TERMINAL LOOP                                
         MVI   ERRCODE,X'00'       RE-SET ERROR CODE FOR THIS TERMINAL          
*                                                                               
* READ TERMINAL RECORD INTO IOAREA1                                             
*                                                                               
         GOTO1 =A(READTERM),DMCB,ATERM,AIO1                                     
         BE    MAIN15              RECORD FOUND                                 
*                                                                               
* TERMINAL *NOT* FOUND HERE                                                     
*                                                                               
* THIS MAY BE OK, IF WE'RE ADDING NEW TERMINALS                                 
* CHECK IF FIRST ACTION IN THE ACTION TABLE IS "N"                              
* IF YES, THEN NOT FINDING THE TERMINAL IS EXPECTED                             
*                                                                               
         L     RF,AACTTAB          ACTIONS TABLE                                
         CLI   INAACT-INACTD(RF),C'N' FIRST ACTION = NEW?                       
         BE    MAIN18              OK TO PROCEED                                
*                                                                               
* TERMINAL NOT FOUND, *AND* ACTION != "NEW"                                     
*                                                                               
         GOTO1 =A(PRTERR),=AL4(TERMERRQ)                                        
         B     MAIN90              PROCESS NEXT TERMINAL                        
*                                                                               
* TERMINAL RECORD FOUND                                                         
*                                                                               
MAIN15   DS    0H                                                               
*                                                                               
* MAKE SURE WE'RE NOT ADDING.  CAN'T ADD EXISTING RECORDS                       
*                                                                               
         L     RF,AACTTAB          ACTIONS TABLE                                
         CLI   INAACT-INACTD(RF),C'N' FIRST ACTION = NEW?                       
         BNE   MAIN16              OK TO PROCEED                                
*                                                                               
         GOTO1 =A(PRTERR),=AL4(ADDERRQ)                                         
         B     MAIN90              PROCESS NEXT TERMINAL                        
*                                                                               
MAIN16   DS    0H                                                               
         CLI   RCTRACE,C'Y'                                                     
         BNE   MAIN18                                                           
*                                                                               
* SAVE COPY OF THE RECORD IN IOAREA2 FOR TRACING                                
*                                                                               
         GOTO1 =A(COPYREC),DMCB,AIO2,AIO1 FROM AIO1 TO AIO2                     
*                                                                               
* NOW PROCESS ALL ACTIONS FOR CURRENT TERMINAL                                  
*                                                                               
MAIN18   DS    0H                                                               
         MVC   AACTION,AACTTAB     CURRENT DATA ENTRY = START OF TABLE          
*                                                                               
MAIN20   DS    0H                  ACTIONS LOOP                                 
         L     R3,AACTION                                                       
         USING INACTD,R3                                                        
*                                                                               
         CLI   INAACT,C'C'         CLEARING ALL IDS?                            
         BNE   MAIN22                                                           
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'CTFILE '),(X'20',AIO1),0,0,0             
         CLI   12(R1),0                                                         
         BE    MAIN60              PROCESS NEXT ACTION                          
*                                                                               
         GOTO1 =A(PRTERR),=AL4(CLEARERRQ)                                       
         B     MAIN90              PROCESS NEXT TERMINAL                        
*                                                                               
MAIN22   DS    0H                                                               
         CLI   INAACT,C'N'         ADDING NEW TERMINALS?                        
         BNE   MAIN24                                                           
*                                                                               
* READ THE "BASELINE" TERMINAL RECORD                                           
*                                                                               
         LR    RF,R3               AACTION                                      
         AHI   RF,INADATA-INACTD                                                
         GOTO1 =A(READTERM),DMCB,(RF),AIO1                                      
         BE    MAIN23D                                                          
*                                                                               
         GOTO1 =A(PRTERR),=AL4(BASEERRQ)                                        
         B     MAIN200 MAKES NO SENSE TO GO FURTHER.  EXIT PROGRAM.             
*                                                                               
* BASELINE AND NEW TERMINALS PASSED CHECKS, ADD NEW RECORD TO FILE              
*                                                                               
MAIN23D  DS    0H                                                               
         CLI   RCTRACE,C'Y'                                                     
         BNE   MAIN23F                                                          
*                                                                               
         GOTO1 =A(COPYREC),DMCB,AIO2,AIO1 FROM AIO1 TO AIO2                     
*                                                                               
MAIN23F  DS    0H                                                               
         L     R6,AIO1                                                          
         USING CTTREC,R6           BASELINE TERMINAL RECORD                     
         L     RF,ATERM                                                         
         MVC   CTTKTID,0(RF) OVERWRITE BASELINE TERMINAL NAME WITH NEW          
*                                                                               
         B     MAIN60              PROCESS NEXT CHANGE                          
*                                                                               
* ACTION *NOT* "N"(NEW) HERE                                                    
* THIS MEANS WE'RE UPDATING VALID ID ELEMENTS                                   
*                                                                               
MAIN24   DS    0H                                                               
         BRAS  RE,BLDELEM          BUILD NEW ID ELEMENT IN ELEM                 
*                                                                               
         CLI   INAACT,C'D'         DELETING?                                    
         BNE   MAIN30                                                           
*                                                                               
         LLC   RF,ELEM+1                                                        
         AHI   RF,-2                                                            
         STC   RF,BYTE                                                          
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'CTFILE '),(X'20',AIO1),         X        
               (BYTE,ELEM+2),0,0                                                
         CLI   12(R1),0                                                         
         BE    MAIN60              PROCESS NEXT CHANGE                          
*                                                                               
         GOTO1 =A(PRTERR),=AL4(DELERRQ)                                         
         B     MAIN60              PROCESS NEXT CHANGE                          
*                                                                               
MAIN30   DS    0H                                                               
         CLI   INAACT,C'A'         ADDING?                                      
         BE    MAIN35                                                           
*                                                                               
         GOTO1 =A(PRTERR),=AL4(ACTERRQ)                                         
         B     MAIN60              PROCESS NEXT CHANGE                          
*                                                                               
MAIN35   DS    0H                                                               
         BRAS  RE,CHKDUP           CHECK IF ELEMENT ALREADY ON RECORD           
         BE    MAIN40                                                           
*                                                                               
         GOTO1 =A(PRTERR),=AL4(DUPERRQ)                                         
         B     MAIN60              PROCESS NEXT CHANGE                          
*                                                                               
MAIN40   DS    0H                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'CTFILE '),AIO1,ELEM,            X        
               =C'ADD=CODE'                                                     
         CLI   12(R1),0                                                         
         BE    MAIN60                                                           
*                                                                               
         GOTO1 =A(PRTERR),=AL4(BIGERRQ)                                         
         B     MAIN90              PROCESS NEXT TERMINAL                        
*                                                                               
MAIN60   DS    0H                  GET NEXT ACTION                              
         L     R3,AACTION          CURRENT ACTION                               
         AHI   R3,INACTDLQ         ADVANCE TABLE POINTER                        
         ST    R3,AACTION          SAVE CURRENT ACTION                          
*                                                                               
         OC    0(INACTDLQ,R3),0(R3)                                             
         BNZ   MAIN20              PROCESS NEXT ACTION                          
*                                                                               
* TERMINAL RECORD PROCESSING COMPLETE HERE                                      
*                                                                               
MAIN90   DS    0H                                                               
         CLI   ERRCODE,X'00'       PROCESSING SUCCESSFUL?                       
         BNE   MAIN90E             NO, PRINT ERROR                              
*                                                                               
* TERMINAL RECORD BUILT OK                                                      
* CHECK IF IDS WILL FIT ON ID RECORD SCREEN                                     
*                                                                               
         BRAS  RE,CHKFIT                                                        
         BE    MAIN110                                                          
*                                                                               
         XC    AACTION,AACTION                                                  
         GOTO1 =A(PRTERR),=AL4(FITERRQ)                                         
*                                                                               
* NONZERO ERROR CODE HERE.  PRINT ERROR AND DON'T UPDATE THIS TERMINAL          
*                                                                               
MAIN90E  DS    0H                                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(16),=CL16'TERMINAL RECORD '                                    
         L     RF,ATERM                                                         
         MVC   P+16(L'CTID),0(RF)                                               
         MVC   P+16+L'CTID(35),=CL35' NOT PROCESSED BECAUSE OF ERRORS'          
         GOTO1 =V(PRINTER)                                                      
         B     MAIN120             NEXT TERMINAL                                
*                                                                               
* TERMINAL RECORD READY TO BE WRITTEN TO DISK HERE                              
*                                                                               
MAIN110  DS    0H                                                               
         AP    CTPROC,=P'1'        PROCESSED RECORD COUNT++                     
*                                                                               
* TRACE IT, IF NECESSARY                                                        
*                                                                               
         CLI   RCTRACE,C'Y'                                                     
         BNE   MAIN118                                                          
*                                                                               
         GOTO1 =A(TRACEREC),DMCB,=CL30'COPY:',AIO2                              
*                                                                               
         L     RF,AACTTAB                                                       
         CLI   INAACT-INACTD(RF),C'N' ACTION = NEW?                             
         BNE   MAIN115                                                          
*                                                                               
         GOTO1 =A(TRACEREC),DMCB,=CL30'ADD:',AIO1                               
         B     MAIN118                                                          
*                                                                               
MAIN115  DS    0H                                                               
         GOTO1 =A(TRACEREC),DMCB,=CL30'CHANGE:',AIO1                            
*                                                                               
* WRITE THE TERMINAL RECORD TO DISK                                             
*                                                                               
MAIN118  DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   MAIN120                                                          
*                                                                               
         L     RF,AACTTAB                                                       
         CLI   INAACT-INACTD(RF),C'N' ACTION = NEW?                             
         BE    MAIN119                                                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=CL8'DMWRT',=CL8'CTFILE',AIO1,AIO1              
         CLI   8(R1),0                                                          
         BE    MAIN119OK                                                        
         DC    H'0'                                                             
*                                                                               
MAIN119  DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=CL8'DMADD',=CL8'CTFILE',AIO1,AIO1              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN119OK DS   0H                                                               
         AP    CTUPD,=P'1'         UPDATED RECORD COUNT++                       
*                                                                               
* ADVANCE TO THE NEXT TERMINAL IN BINSRCH TABLE                                 
*                                                                               
MAIN120  DS    0H                                                               
         L     RF,ATERM                                                         
         AHI   RF,L'CTID                                                        
         ST    RF,ATERM                                                         
*                                                                               
         OC    0(L'CTID,RF),0(RF)                                               
         BNZ   MAIN10              PROCESS NEXT TERMINAL                        
*                                                                               
* ALL DONE HERE                                                                 
*                                                                               
MAIN200  DS    0H                                                               
         GOTO1 =V(PRINTER)         BLANK LINE                                   
*                                                                               
         MVC   P(30),=CL30'DRAFT MODE'                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   P(30),=CL30'*UPDATIVE* MODE'                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(30),=CL30'TERMINALS:'                                          
         EDIT  NUMTERM,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(30),=CL30'PROCESSED SUCCESSFULLY:'                             
         EDIT  CTPROC,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(30),=CL30'RECORDS UPDATED:'                                    
         EDIT  CTUPD,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(30),=CL30'ERRORS:'                                             
         EDIT  CTERR,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MAINX    J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* FINISHED, CLOSE THINGS UP                                                     
***********************************************************************         
DONE     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
*                                                                               
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   DONE010                                                          
         GOTO1 =V(DATAMGR),DMCB,=C'ENQCTL ',(C'D',=C'CTRL')                     
*                                                                               
DONE010  GOTO1 =V(DATAMGR),DMCB,=C'DMCLSE ',=C'CONTROL ',AFLIST,AIO1            
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
*                                                                               
*                                                                               
TRACEREC NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            30-CHAR NARRATION                            
         L     R3,4(R1)            A(RECORD)                                    
         MVC   P(30),0(R2)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVI   BYTE,CTTDATA-CTTREC                                              
         XR    RF,RF                                                            
         LHI   RF,CTTLEN-CTTREC                                                 
         GOTO1 =V(PRTREC),DMCB,(C'E',(R3)),(BYTE,(RF)),                X        
               V(PRINT),V(HEXOUT)                                               
         GOTO1 =V(PRINTER)         BLANK LINE                                   
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* BUILD ID ELEMENT (CTIDD, X'20') FROM BINSRCH TABLE ENTRY                      
BLDELEM  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R2,ELEM                                                          
         USING CTIDD,R2                                                         
*                                                                               
         L     R3,AACTION                                                       
         USING INACTD,R3                                                        
*                                                                               
         MVI   CTIDEL,CTIDELQ     X'20', ID ELEMENT                             
*                                                                               
         CLC   =C'L=',INADATA     LIST?                                         
         BNE   BLDEL20                                                          
*                                                                               
         MVI   CTIDLEN,12                                                       
         MVC   CTID(2),=X'0000'                                                 
         MVC   CTIDAGY(8),INADATA+2 LIST NAME FOLLOWS THE 'L='                  
         B     BLDEL50                                                          
*                                                                               
BLDEL20  DS    0H                                                               
         CLC   =C'A=',INADATA     AGENCY?                                       
         BNE   BLDEL30                                                          
*                                                                               
         MVI   CTIDLEN,6                                                        
         MVC   CTID(2),=X'0001'                                                 
         MVC   CTIDAGY(2),INADATA+2 AGENCY CODE FOLLOWS THE 'A='                
         B     BLDEL50                                                          
*                                                                               
BLDEL30  DS    0H                  NOT A= OR L=, USER ID PRESUMED               
         MVI   CTIDLEN,12                                                       
         MVC   CTID,INADATA                                                     
         B     BLDEL50                                                          
*                                                                               
BLDEL50  DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* ELEM EXPECTED TO HAVE THE ID ELEMENT (CTIDD, X'20')                           
* CHECK IF THIS ELEMENT ALREADY EXISTS ON TERM RECORD(IN IOAREA1)               
CHKDUP   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO1                                                          
         MVC   ELCODE,ELEM         X'20', ID ELEMENT                            
         BRAS  RE,GETEL                                                         
*                                                                               
CHKDUP10 DS    0H                                                               
         JNE   EQXIT                                                            
*                                                                               
         CLC   1(1,R6),ELEM+1      SAME LENGTH?                                 
         BNE   CHKDUP20                                                         
*                                                                               
         LLC   RF,1(R6)            ELEMENT LENGTH                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),ELEM                                                     
         JE    NEQXIT              ELEMENT ALREADY ON RECORD                    
*                                                                               
CHKDUP20 DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         B     CHKDUP10                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* CHECK IF ALL IDS WILL FIT ON THE SCREEN                                       
CHKFIT   NTR1  BASE=*,LABEL=*                                                   
         LA    RE,BLOCK1                                                        
         LHI   RF,20*32                                                         
         XCEFL                                                                  
*                                                                               
* XC FAKE SCREEN FIELDS.  FIELD SIZE - SAME AS ON ID RECORD SCREEN              
         LA    RE,SCRN                                                          
         LHI   RF,3*(L'TRMIDSH+L'TRMIDS)                                        
         XCEFL                                                                  
* SET FAKE FIELD LENGTHS                                                        
         MVI   SCRN,L'TRMIDSH+L'TRMIDS                                          
         MVI   SCRN+(L'TRMIDSH+L'TRMIDS),(L'TRMIDSH+L'TRMIDS)                   
         MVI   SCRN+2*(L'TRMIDSH+L'TRMIDS),(L'TRMIDSH+L'TRMIDS)                 
*                                                                               
         L     R6,AIO1             A(RECORD)                                    
         LA    R5,BLOCK1                                                        
         XR    R4,R4               ELEMENT COUNT                                
         MVI   ELCODE,CTIDELQ      X'20', ID ELEMENT                            
         BRAS  RE,GETEL                                                         
*                                                                               
CHKFIT10 DS    0H                                                               
         JNE   CHKFIT80                                                         
*                                                                               
         AHI   R4,1                ELEMENT COUNT                                
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         MHI   RF,L'BLOCK1                                                      
         LA    RF,BLOCK1(RF)                                                    
*                                                                               
         CLC   =X'0000',2(R6)                                                   
         BNE   CHKFIT20                                                         
         MVC   0(2,RF),=C'L='                                                   
         MVC   2(8,RF),4(R6)                                                    
         B     CHKFIT50                                                         
*                                                                               
CHKFIT20 DS    0H                                                               
         CLC   =X'0001',2(R6)                                                   
         BNE   CHKFIT30                                                         
         MVC   0(2,RF),=C'A='                                                   
         MVC   2(2,RF),4(R6)                                                    
         B     CHKFIT50                                                         
*                                                                               
CHKFIT30 DS    0H                                                               
         MVC   0(8,RF),2(R6)                                                    
*                                                                               
CHKFIT50 DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         B     CHKFIT10                                                         
*                                                                               
CHKFIT80 DS    0H                                                               
         LTR   R4,R4                                                            
         JZ    EQXIT                                                            
*                                                                               
         GOTO1 =V(SCINKEY),DMCB,(3,SCRN),(20,BLOCK1),(R4)                       
         OC    DMCB(4),DMCB  NUMBER OF FIELDS *NOT* FITTING ON SCREEN           
         JZ    EQXIT                                                            
         J     NEQXIT                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* READ INPUT FILES                                                              
***********************************************************************         
READINP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ TERMINALS                                                                
         MVC   P(30),=CL30'TERMINALS'                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=CL30'========='                                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LAY   RF,INTERM                                                        
         OPEN  ((RF),INPUT)                                                     
*                                                                               
         L     R2,ATERMTAB         TERMINAL TABLE POINTER                       
*                                                                               
READIN10 DS    0H                                                               
         LAY   RF,INTERM                                                        
         GET   (RF),CARD                                                        
*                                                                               
         MVC   P(L'CTTKTID),CARD   PRINT THE INPUT DATA LINE                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    READIN10            SKIP IT                                      
*                                                                               
         L     RF,NUMTERM          UPDATE TERMINAL COUNT                        
         AHI   RF,1                                                             
         ST    RF,NUMTERM                                                       
*                                                                               
         CHI   RF,INTERMNQ         REACHED MAX?                                 
         BNL   READIN15                                                         
*                                                                               
         MVC   0(L'CTID,R2),CARD   SAVE TERMINAL IN TABLE                       
         AHI   R2,L'CTID           ADVANCE TABLE POINTER                        
         B     READIN10                                                         
*                                                                               
READIN15 MVC   P(30),=CL30'ERROR: TERMINAL TABLE FULL'                          
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                TABLE FULL                                   
*                                                                               
INTRMEOF DS    0H                                                               
         LAY   RF,INTERM                                                        
         CLOSE (RF)                                                             
*                                                                               
         OC    NUMTERM,NUMTERM     NUMBER OF RECORDS IN BINSRCH TABLE           
         BNZ   READIN20                                                         
*                                                                               
         MVC   P(30),=CL30'ERROR: TERMINAL FILE EMPTY'                          
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                TABLE FULL                                   
*                                                                               
* READ ACTIONS                                                                  
*                                                                               
READIN20 DS    0H                                                               
         GOTO1 =V(PRINTER)         BLANK LINE FOR READABILITY                   
*                                                                               
         MVC   P(10),=CL10'ACTIONS'                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(10),=CL10'========='                                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LAY   RF,INACT                                                         
         OPEN  ((RF),INPUT)                                                     
*                                                                               
         L     R2,AACTTAB           A(ACTION TABLE)                             
*                                                                               
READIN30 DS    0H                                                               
         LAY   RF,INACT                                                         
         GET   (RF),CARD                                                        
*                                                                               
* PRINT THE INPUT ACTION LINE                                                   
*                                                                               
         MVC   P(INACTDLQ),CARD                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT                                      
         BE    READIN30                                                         
*                                                                               
         L     RF,NUMACT                                                        
         AHI   RF,1                                                             
         ST    RF,NUMACT                                                        
*                                                                               
         CHI   RF,INACTNQ          REACHED MAX?                                 
         BNL   READIN35                                                         
*                                                                               
         MVC   0(INACTDLQ,R2),CARD SAVE ACTION IN TABLE                         
         AHI   R2,INACTDLQ         ADVANCE TABLE POINTER                        
         B     READIN30                                                         
*                                                                               
READIN35 MVC   P(30),=CL30'ERROR: DATA TABLE FULL'                              
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                TABLE FULL                                   
*                                                                               
INACTEOF DS    0H                                                               
         LAY   RF,INACT                                                         
         CLOSE (RF)                                                             
*                                                                               
         OC    NUMACT,NUMACT                                                    
         BNZ   READIN50                                                         
*                                                                               
         MVC   P(30),=CL30'ERROR: DATA FILE EMPTY'                              
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                TABLE FULL                                   
*                                                                               
READIN50 DS    0H                                                               
         GOTO1 =V(PRINTER)         BLANK LINE FOR READABILITY                   
*                                                                               
READINX  J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* READ TERMINAL RCECORD                                                         
* P1 - A(TERMINAL)                                                              
* P2 - IO AREA                                                                  
***********************************************************************         
READTERM NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)         R2=A(TERMINAL),R3=A(IO AREA)                 
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R6,IOKEY                                                         
         USING CTTREC,R6           TERMINAL RECORD                              
*                                                                               
         MVI   CTTKTYP,CTTKTYPQ    C'T'                                         
         MVC   CTTKTID,0(R2)       TERMINAL                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(DMBITS,=CL8'DMRDHI'),                 X        
               =CL8'CTFILE',IOKEY,(R3)                                          
         LR    R6,R3                                                            
*                                                                               
         CLC   CTTKEY,IOKEY                                                     
         JE    EQXIT                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=CL8'**IOA1**'                                           
*                                                                               
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=CL8'**IOA2**'                                           
*                                                                               
         L     R1,=A(TERMTAB-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,ATERMTAB                                                      
         SHI   R1,8                                                             
         MVC   0(8,R1),=CL8'**TERM**'                                           
*                                                                               
         L     R1,=A(ACTTAB-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AACTTAB                                                       
         SHI   R1,8                                                             
         MVC   0(8,R1),=CL8'**ACTS**'                                           
*                                                                               
         LAY   RF,FLIST                                                         
         ST    RF,AFLIST                                                        
*                                                                               
         MVI   RCWRITE,C'N'                                                     
         MVI   RCTRACE,C'N'                                                     
*                                                                               
         MVI   DMBITS,0                                                         
*                                                                               
         ZAP   CTPROC,=P'0'        RECORDS PROCESSED SUCCESSFULLY               
         ZAP   CTUPD,=P'0'         UPDATED RECORDS COUNTER                      
         ZAP   CTERR,=P'0'         ERROR COUNTER                                
*                                                                               
         LA    R3,CARD                                                          
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'*',0(R3)         COMMENT                                      
         BE    INIT010                                                          
         CLC   =C'/*',0(R3)        SYSIN EOF                                    
         BE    INIT100                                                          
*                                                                               
         MVC   P(80),0(R3)         CONTROL CARD                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   =C'DDSIO=',0(R3)                                                 
         BNE   INIT014                                                          
         L     RF,=V(DDSIO)        OVERRIDE DDSIO NAME                          
         MVC   0(8,RF),6(R3)                                                    
         B     INIT010                                                          
*                                                                               
INIT014  CLC   =C'DSPACE=',0(R3)                                                
         BNE   INIT018                                                          
         LAY   RF,SSB              SET DSPACE ID IN SSB                         
         MVC   SSODSPAC-SSOOFF(1,RF),7(R3)                                      
         B     INIT010                                                          
*                                                                               
INIT018  DS    0H                                                               
         CLC   =C'WRITE=',0(R3)                                                 
         BNE   INIT022                                                          
         MVC   RCWRITE,6(R3)                                                    
         CLI   RCWRITE,C'Y'                                                     
         BE    INIT010                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    INIT010                                                          
         B     INIT090             INVALID CONTROL CARD                         
*                                                                               
INIT022  DS    0H                                                               
         CLC   =C'TRACE=',0(R3)                                                 
         BNE   INIT090                                                          
         MVC   RCTRACE,6(R3)                                                    
         B     INIT010                                                          
*                                                                               
INIT090  DS    0H                                                               
         MVC   P(30),=CL30'ERROR: INVALID CONTROL CARD'                         
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
* SYSIN EOF HERE                                                                
*                                                                               
INIT100  DS    0H                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=Y?                                     
         BNE   *+12                . NO                                         
         MVI   FCTFILE,C'U'        OPEN CTFILE FOR UPDATE                       
         MVI   DMBITS,X'80'        READ FOR UPDATE                              
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN  ',=C'CONTROL ',AFLIST,AIO1           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   INIT110                                                          
         GOTO1 =V(DATAMGR),DMCB,=C'ENQCTL ',(C'E',=C'CTRL')                     
         TM    8(R1),X'04'                                                      
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
INIT110  DS    0H                                                               
         XC    DATADISP,DATADISP                                                
         MVI   DATADISP+1,CTTDATA-CTTREC                                        
         XC    ATERM,ATERM         CURRENTLY PROCCESSED TERMINAL                
         XC    AACTION,AACTION     CURRENTLY PROCESSED ACTION                   
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        CONSTANTS & LTORG                                                      
***********************************************************************         
         DC    0D                                                               
         DC    C'**FLST**'                                                      
FLIST    DS    0CL8                                                             
FCTFILE  DC    C'NCTFILE '                                                      
         DC    C'X       '                                                      
*                                                                               
                                                                                
***********************************************************************         
*        DCBS & ADCONS                                                          
***********************************************************************         
         DS    0D                                                               
INACT    DCB   DDNAME=INACT,                                           C        
               DSORG=PS,                                               C        
               EODAD=INACTEOF,                                         C        
               LRECL=80,                                               C        
               RECFM=FB,                                               C        
               MACRF=GM                                                         
*                                                                               
         DS    0D                                                               
INTERM   DCB   DDNAME=INTERM,                                          C        
               DSORG=PS,                                               C        
               EODAD=INTRMEOF,                                         C        
               LRECL=80,                                               C        
               RECFM=FB,                                               C        
               MACRF=GM                                                         
*                                                                               
*                                                                               
         DC    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
*                                                                               
         DC    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    X'0000'                                                          
         DC    X'FF'                                                            
         DC    AL1(SSOSNRCV)                                                    
         DC    1024X'00'                                                        
*                                                                               
       ++INCLUDE FATABSDEQU                                                     
         DS    16D                                                              
*                                                                               
*                                                                               
*                                                                               
ERRTAB   DS    0H                                                               
*                                                                               
         DC    AL1(TERMERRLQ)                                                   
TERMERRQ EQU   *                                                                
         DC    C'TERMINAL RECORD NOT FOUND'                                     
TERMERRLQ EQU  *-TERMERRQ                                                       
*                                                                               
         DC    AL1(CLEARERRLQ)                                                  
CLEARERRQ EQU  *                                                                
         DC    C'IDS NOT CLEARED'                                               
CLEARERRLQ EQU *-CLEARERRQ                                                      
*                                                                               
         DC    AL1(ADDERRLQ)                                                    
ADDERRQ  EQU   *                                                                
         DC    C'TERMINAL RECORD ALREADY EXISTS'                                
ADDERRLQ EQU   *-ADDERRQ                                                        
*                                                                               
         DC    AL1(DUPERRLQ)                                                    
DUPERRQ  EQU   *                                                                
         DC    C'DUPLICATE DATA'                                                
DUPERRLQ EQU   *-DUPERRQ                                                        
*                                                                               
         DC    AL1(FITERRLQ)                                                    
FITERRQ  EQU   *                                                                
         DC    C'IDS WILL NOT FIT ON TERMINAL RECORD SCREEN'                    
FITERRLQ EQU   *-FITERRQ                                                        
*                                                                               
         DC    AL1(ACTERRLQ)                                                    
ACTERRQ  EQU   *                                                                
         DC    C'INVALID ACTION'                                                
ACTERRLQ EQU   *-ACTERRQ                                                        
*                                                                               
         DC    AL1(BASEERRLQ)                                                   
BASEERRQ EQU   *                                                                
         DC    C'SOURCE TERMINAL NOT FOUND'                                     
BASEERRLQ EQU  *-BASEERRQ                                                       
*                                                                               
         DC    AL1(BIGERRLQ)                                                    
BIGERRQ  EQU   *                                                                
         DC    C'RECORD TOO BIG'                                                
BIGERRLQ EQU   *-BIGERRQ                                                        
*                                                                               
         DC    AL1(DELERRLQ)                                                    
DELERRQ  EQU   *                                                                
         DC    C'ID ELEMENT NOT DELETED'                                        
DELERRLQ EQU   *-DELERRQ                                                        
*                                                                               
*                                                                               
*                                                                               
PRTERR   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            ERROR EQUATE                                 
*                                                                               
         MVC   P(13),=CL13'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LR    RF,R2               A(ERROR MESSAGE)                             
         BCTR  RF,0                BACK UP 1, TO MESSAGE LENGTH                 
         LLC   RF,0(RF)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R2)          PRINT ERROR MESSAGE                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         ICM   R2,15,ATERM         A(CURRENT TERMINAL)                          
         BZ    PRTERR10                                                         
*                                                                               
         MVC   P(10),=CL10'TERMINAL:'                                           
         MVC   P+10(L'CTTKTID),0(R2)                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRTERR10 DS    0H                                                               
         ICM   R2,15,AACTION       A(CURRENT ACTION)                            
         BZ    PRTERR20                                                         
*                                                                               
         MVC   P(10),=CL10'ACTION:'                                             
         MVC   P+10(INACTDLQ),0(R2)                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRTERR20 DS    0H                                                               
         MVI   ERRCODE,X'FF'                                                    
         AP    CTERR,=P'1'         ERROR COUNTER                                
         GOTO1 =V(PRINTER)         BLANK LINE FOR READABILITY                   
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COPYREC  NTR1  BASE=*,LABEL=*                                                   
         L     RE,4(R1)            SOURCE                                       
         L     R0,0(R1)            DESTINATION                                  
         XR    R1,R1                                                            
         ICM   R1,3,CTTLEN-CTTREC(RE)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
*                                                                               
AIO1     DS    A                   A(IO AREA 1)                                 
AIO2     DS    A                   A(IO AREA 2)                                 
AFLIST   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
DMBITS   DS    X                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
WORK     DS    CL64                                                             
*                                                                               
CTERR    DS    PL8                                                              
CTPROC   DS    PL8                                                              
CTUPD    DS    PL8                                                              
*                                                                               
RCWRITE  DS    C                                                                
RCTRACE  DS    C                                                                
*                                                                               
FLAG     DS    X                                                                
ERRCODE  DS    X                                                                
ELCODE   DS    X                                                                
DATADISP DS    H                                                                
*                                                                               
TODAY    DS    CL3                 YYMMDD PWOS                                  
TODAYC   DS    H                   TODAY COMP                                   
TODAY0   DS    CL6                 TODAY YYMMDD                                 
DATE0    DS    CL6                 DATE YYMMDD                                  
*                                                                               
CARD     DS    CL80                                                             
IOKEY    DS    CL42                                                             
IOKEYSV  DS    CL42                                                             
*                                                                               
ELEM     DS    XL256               GENERAL USE ELEMENT                          
*                                                                               
ATERMTAB DS    A                   TABLE OF TERMINALS                           
AACTTAB  DS    A                   TABLE OF ACTIONS                             
*                                                                               
ATERM    DS    A                   A(CURRENT TERMINAL)                          
AACTION  DS    A                   A(CURRENT ACTION)                            
*                                                                               
NUMTERM  DS    A                   NUMBER OF TERMINALS PROCESSED                
NUMACT   DS    A                                                                
*                                                                               
BLOCK1   DS    64CL20                                                           
SCRN     DS    3XL(L'TRMIDSH+L'TRMIDS)                                          
*                                                                               
         DS    CL8                                                              
IOAREA1  DS    2048C               IO AREA 1                                    
         DS    CL8                                                              
IOAREA2  DS    2048C               IO AREA 2                                    
*                                                                               
         DS    CL8                                                              
TERMTAB  DS    CL(INTERMNQ*L'CTTKTID)                                           
*                                                                               
         DS    CL8                                                              
ACTTAB   DS    CL(INACTNQ*INACTDLQ)                                             
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
         DCBD    DSORG=QS,DEVD=DA                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ACTION DATA READ UPFRONT, SAVED IN TWO BINSRCH-31 TABLES                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INTERMNQ EQU   1000                MAX ENTRIES IN TERMINAL TABLE                
INACTNQ EQU    100                 MAX ENTRIES IN DATA TABLE                    
*                                                                               
INACTD   DSECT                                                                  
INAACT   DS    C                   ACTION                                       
         DS    X                   BLANK                                        
INADATA  DS    CL(L'CTID)          DATA                                         
*                                                                               
INACTDLQ EQU *-INACTD                                                           
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENF2D                                                       
         PRINT ON                                                               
                                                                                
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTTERMFIX 03/16/18'                                      
         END                                                                    
