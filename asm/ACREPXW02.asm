*          DATA SET ACREPXW02  AT LEVEL 024 AS OF 02/24/20                      
*PHASE ACXW02B                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
**********************************************************************          
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* VGUP 024 07AUG19 SPEC-37703 INCLUDE NEW COLUMS TO ATTACHED WORKCODE           
*                             USED COUNT FOR EACH RECORD SEPARATELY             
* VGUP 023 11MAR19 SPEC-32910 INCLUDED RATE RECORDS, AJRATE RECORD,             
*                             LIMLIST/GRPLIST AND ETYPE RECORDS CHECK           
**********************************************************************          
         TITLE 'MEDIA/WORKCODE RECORD PURGE'                                    
ACXW02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXW**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXWD,RC                                                         
***********************************************************************         
*        OPTION 1 = "L"  FOR LIVE REPORT                              *         
*                                                                     *         
*        OPTION 2 = "Y" TO GET A LIST OF THE MEDIAS AND WORKCODES     *         
*                   USED BY THE AGENCY AND THE NUMBER OF OCCURANCES   *         
*                   WORKCODE OCCURANCES INCLUDE THEIR USE IN EST.     *         
***********************************************************************         
         EJECT                                                                  
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
*                                                                               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         L     R1,ACMAOFL                                                       
         USING OFFALD,R1                                                        
         OC    OFFALIMA,OFFALIMA                                                
         BZ    EXIT                                                             
         DROP  RF                                                               
*                                                                               
         MVC   P(39),=C'ERROR - CANNOT RUN ON LIMITED ACCESS ID'                
         GOTO1 ACREPORT                                                         
         ZAP   RCRQTOT,RCENNUM                                                  
         B     EXIT                                                             
         EJECT                                                                  
REQF     CLI   MODE,REQFRST        FIRST TIME IN?                               
         BNE   LEDF                NO                                           
*                                                                               
         ZAP   DLTCNT,=P'0'        CLEAR DELETE COUNTER                         
         MVC   PAGE,=H'1'          SET PAGE NUMBER                              
         MVI   FORCEHED,C'Y'       FORCE HEADING                                
*                                                                               
         L     R2,AMEWCTAB         A(BIN TABLE)                                 
         USING BIND,R2             DSECT FOR BIN TABLE PARMS                    
         XC    BININ,BININ         CLEAR TABLE                                  
*                                                                               
         L     R3,AMONACC          SET THE JOBBER COLUMNS                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CPYELD,RF                                                        
         L     RF,ADCMPEL                                                       
         TM    CPYSTAT7,CPYSTMSY   ARE THEY ON TMS?                             
         BNO   *+8                 NO, DON'T READ 1R                            
         BAS   RE,BLD1R            YES, GET 1R TIME POSTINGS                    
         BAS   RE,BLD2C            GET CATEGORY/WORKCODE RECORDS                
         BAS   RE,BLD1D            GET WORKCODE TYPE LIST RECORDS               
         BAS   RE,BLD2A            GET RATE RECORD                              
         BAS   RE,BLD19            GET ADJUSTMENT RATE RECORD                   
         BAS   RE,BLD37            GET EXPENDITURE TYPE RECORD                  
         BAS   RE,BLDLL            GET LIMIT LIST RECORD                        
         BAS   RE,BLDGL            GET GROUP LIST RECORD                        
         B     EXIT                                                             
         DROP  R2,R3,RF                                                         
         EJECT                                                                  
***********************************************************************         
*        SET TO READ N AND R TIME                                     *         
***********************************************************************         
*                                                                               
LEDF     CLI   MODE,LEDGFRST                                                    
         BNE   PROCA                                                            
         MVI   FCRNTIME,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        BUILD TABLE OF MEDIA CODES FROM SJ ACCOUNTS AND WORKCODES    *         
*        FROM ESTIMATES                                               *         
***********************************************************************         
*                                                                               
PROCA    CLI   MODE,PROCACC                                                     
         BNE   PROCT                                                            
         USING ACKEYD,R3                                                        
         L     R3,ADACC                                                         
*                                                                               
         USING MEWCD,R6                     TABLE ENTRY DSECT                   
         LA    R6,MEWCWK                    WORK AREA INTO R6                   
         XC    MEWCWK(MEWCLEN),MEWCWK       CLEAR TO 0'S                        
*                                                                               
         MVI   MEWCTYPE,C'M'                SET TYPE TO M=MEDIA                 
         MVI   MEWCCODE,C' '                BLANK FOR MEDIA                     
         MVC   MEWCCODE+1(1),ACKEYACC+9     MEDIA CODE INTO TAB                 
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'               SET USED BUCKET TO 1                
         ZAP   MEWCFOND,=P'0'               SET FOUND TO 0                      
*                                                                               
         GOTO1 BINADD,DMCB,(R6),AMEWCTAB    ADD MEDIA TO TAB                    
*                                                                               
         BAS   RE,LOOKUP                    ADD W/C FROM ESTIMATE               
*                                                                               
         USING JBLOCKD,R5                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BE    PROCA06             YES                                          
*                                                                               
         LH    R2,JBNROWS                                                       
         USING JBCOLD,R3                                                        
PROCA02  CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   PROCA04                                                          
         XC    MEWCWK(MEWCLEN),MEWCWK       CLEAR TO 0'S                        
         MVI   MEWCTYPE,C'W'                SET TYPE TO W=WORKCODE              
         MVC   MEWCCODE,JBCOLWC             WORKCODE INTO TAB                   
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'               SET USED BUCKET TO 1                
         ZAP   MEWCFOND,=P'0'               SET FOUND TO 0                      
         ZAP   MEWCSJAC,=P'1'               SET USED BUCKET TO 1                
*                                                                               
         GOTO1 BINADD,DMCB,(R6),AMEWCTAB    ADD MEDIA TO TAB                    
*                                                                               
PROCA04  AH    R3,JBLCOL                                                        
         BCT   R2,PROCA02                                                       
         B     EXIT                                                             
*                                                                               
         USING MJETABD,R3                                                       
PROCA06  CLI   MJETTYP,MJETTEQ              ARE WE AT THE END?                  
         BE    EXIT                         YES                                 
         CLI   MJETTYP,MJETTWQ              LOOK FOR WORKCODES ONLY             
         BNE   PROCA08                                                          
         XC    MEWCWK(MEWCLEN),MEWCWK       CLEAR TO 0'S                        
         MVI   MEWCTYPE,C'W'                SET TYPE TO W=WORKCODE              
         MVC   MEWCCODE,MJETWCD             WORKCODE INTO TAB                   
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'               SET USED BUCKET TO 1                
         ZAP   MEWCFOND,=P'0'               SET FOUND TO 0                      
         ZAP   MEWCSJAC,=P'1'               SET USED BUCKET TO 1                
*                                                                               
         GOTO1 BINADD,DMCB,(R6),AMEWCTAB    ADD MEDIA TO TAB                    
*                                                                               
PROCA08  XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    R3,R1                                                            
         B     PROCA06                                                          
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        BUILD TABLE OF WORK CODES FROM SJ TRANSACTIONS               *         
***********************************************************************         
*                                                                               
PROCT    CLI   MODE,PROCTRNS                                                    
         BNE   REQL                                                             
         L     R3,ADTRANS                   ADDR OF TRANS INTO R3               
         CLI   0(R3),X'44'                  IS IT A TRANSACTION ELEMENT         
         BNE   EXIT                         NO GET OUT                          
*                                                                               
         SH    R3,DATADISP                  R3 TO BEGINNING OF KEY              
         USING ACKEYD,R3                                                        
         CLC   ACKEYWRK,=C'**'              IS THIS AN ORDER?                   
         BE    PROC2                        YES, FIND THE WORKCODES             
                                                                                
         USING MEWCD,R5                     TABLE ENTRY DSECT                   
         LA    R5,MEWCWK                    WORK AREA INTO R5                   
         XC    MEWCWK(MEWCLEN),MEWCWK       CLEAR TO 0'S                        
*                                                                               
         MVI   MEWCTYPE,C'W'                SET TYPE TO W=WORKCODE              
         MVC   MEWCCODE,ACKEYWRK            WORK CODE INTO TAB                  
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'               SET USED BUCKET TO 1                
         ZAP   MEWCFOND,=P'0'               SET FOUND TO 0                      
         ZAP   MEWCSJTR,=P'1'               SET USED BUCKET TO 1                
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB    ADD W/C TO TAB                      
         B     EXIT                                                             
                                                                                
PROC2    LR    R4,R3                                                            
         MVI   ELCODE,OAMELQ                ORDER ELEMENTS                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
                                                                                
PROC4    BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         USING OAMELD,R4                                                        
         USING MEWCD,R5                                                         
         LA    R5,MEWCWK                                                        
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'                SET TYPE TO W=WORKCODE              
         MVC   MEWCCODE,OAMWORK             WORK CODE INTO TABLE                
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'       SET USED BUCKET TO 1                        
         ZAP   MEWCFOND,=P'0'       SET FOUND TO 0                              
         ZAP   MEWCSJTR,=P'1'               SET USED BUCKET TO 1                
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
         B     PROC4               GET NEXT ELEMENT                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        LOOK UP MEDIA RECORDS, ADD TO TABLE AND MARK NOT USED        *         
*        UPDATE MEDIA NAMES, IF FOUND                                 *         
***********************************************************************         
*                                                                               
REQL     CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         USING ACKEYD,R3                                                        
         LA    R3,IOA                                                           
         MVC   ACKEYACC(49),SPACES      READ FOR MEDIA RECORDS                  
         MVI   ACKEYACC,X'09'                                                   
         MVC   ACKEYACC+1(1),RCCOMPFL                                           
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
REQL02   BAS   RE,SEQ                                                           
         CLC   ACKEYACC(2),SAVEKEY          ANYMORE?                            
         BNE   REQL04                       NO                                  
*                                                                               
         USING MEWCD,R5                                                         
         LA    R5,MEWCWK                                                        
         XC    MEWCWK(MEWCLEN),MEWCWK       CLEAR WORK AREA                     
*                                                                               
         MVI   MEWCTYPE,C'M'                SET TYPE TO M=MEDIA                 
         MVI   MEWCCODE,C' '                BLANK FOR MEDIA                     
         MVC   MEWCCODE+1(1),ACKEYACC+2     MEDIA CODE INTO TABLE               
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'0'               SET USED BUCKET TO 0                
         ZAP   MEWCFOND,=P'1'               SET FOUND TO 1                      
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB    ADD MEDIA TO TAB                    
         L     R5,DMCB                      ADDR OF RECORD FOUND OR             
*                                           WHERE NEW ENTRY WAS ADDED           
         LR    R4,R3                                                            
         USING ACMEDIAD,R4                                                      
         MVI   ELCODE,X'11'                 LOOK FOR NAME                       
         BAS   RE,GETEL                                                         
         MVC   MEWCNAME,=C'NAME NOT FOUND ' ASSUME NOT FOUND                    
         BNE   REQL02                                                           
         MVC   MEWCNAME,ACMDDESC            REPLACE WITH NAME                   
         B     REQL02                                                           
         EJECT                                                                  
***********************************************************************         
*        LOOK UP WORKCODE RECORDS, ADD TO TABLE AND MARK NOT USED     *         
*        UPDATE WORKCODE NAMES, IF FOUND                              *         
***********************************************************************         
*                                                                               
         USING ACKEYD,R3                                                        
REQL04   LA    R3,IOA                                                           
         MVC   ACKEYACC(49),SPACES                                              
         MVI   ACKEYACC,X'0A'                                                   
         MVC   ACKEYACC+1(1),RCCOMPFL                                           
         MVC   ACKEYACC+2(2),QUNIT                                              
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
REQL06   BAS   RE,SEQ                                                           
         CLC   ACKEYACC(4),SAVEKEY          ANYMORE?                            
         BNE   REQL08                       NO                                  
*                                                                               
         LA    R5,MEWCWK                                                        
         XC    MEWCWK(MEWCLEN),MEWCWK       CLEAR WORK AREA                     
*                                                                               
         MVI   MEWCTYPE,C'W'                SET TYPE TO W=WORKCODE              
         MVC   MEWCCODE,ACKEYACC+4          WORK CODE INTO TABLE                
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'0'               SET USED BUCKET TO 0                
         ZAP   MEWCFOND,=P'1'               SET FOUND TO 1                      
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB    ADD W/C TO TAB                      
         L     R5,DMCB                                                          
*                                                                               
         LR    R4,R3                                                            
         USING ACANALD,R4                                                       
         MVI   ELCODE,X'12'                 FIND NAME                           
         BAS   RE,GETEL                                                         
         MVC   MEWCNAME,=C'NAME NOT FOUND ' ASSUME NOT FOUND                    
         BNE   REQL06                                                           
         MVC   MEWCNAME,ACANDESC            REPLACE WITH NAME                   
         B     REQL06                                                           
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL PULL OUT THE MEDIAS AND WORKCODES THAT     *         
*        ARE AVAILABLE FOR DELETION                                   *         
*        QOPT1 DETERMINES IF THE UNUSED ARE TO BE DELETED             *         
***********************************************************************         
*                                                                               
         USING BIND,R2                                                          
         USING MEWCD,R5                                                         
REQL08   MVI   CODESW,C'M'         SET CODE SWITCH TO MEDIA                     
         MVI   RCSUBPRG,0          MEDIA HEADLINES                              
*                                                                               
REQL10   L     R2,AMEWCTAB         ADDR OF TABLE IN R5                          
         LA    R5,BINTABLE                                                      
         L     R6,BININ            NUMBER OF ENTRIES IN TABLE                   
*                                                                               
REQL12   CLC   MEWCTYPE,CODESW     IS THIS THE CODE (M OR W) WE WANT            
         BNE   REQL16              NO, GET NEXT                                 
         CP    MEWCUSED,=P'0'      YES, CAN WE DELETE IT?                       
         BNE   REQL16              NO, GET NEXT                                 
         CLI   QOPT1,C'L'          YES, DO WE WANT TO DELETE IT?                
         BNE   REQL14              NO, JUST PRINT IT                            
         BAS   RE,DELETE           YES, DELETE IT                               
*                                                                               
REQL14   BAS   RE,PRNTIT           PRINT ITEMS ELIGIBLE OR DELETED              
*                                                                               
REQL16   LA    R5,MEWCLEN(R5)      NEXT TABLE ENTRY                             
         BCT   R6,REQL12                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'          RESET TO PAGE 1                              
         CLI   CODESW,C'W'         DID WE PROCESS THE WORKCODES?                
         BNE   REQL18              NO, GO DO THEM                               
         CLI   QOPT2,C'Y'          YES, MEDIA W/C USED LIST NEEDED?             
         BE    REQL20              YES                                          
         CP    DLTCNT,=P'0'        NO, WERE ANY RECORDS DEETED?                 
         BE    REQLX               NO, EXIT                                     
         MVI   RCSUBPRG,2          YES, CHANGE HEADINGS                         
         MVI   DLTSWITC,C'Y'       AND PRINTED DELETED DATA                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         BAS   RE,PRNTIT                                                        
         B     REQLX                                                            
*                                                                               
REQL18   MVI   CODESW,C'W'         SET CODE SWITCH TO WORKCODES                 
         MVI   RCSUBPRG,1          WORKCODE HEADLINES                           
         B     REQL10                                                           
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL PULL OUT THE MEDIAS AND WORKCODES THAT     *         
*        ARE ACTUALLY USED BY THE AGENCY                              *         
***********************************************************************         
*                                                                               
REQL20   MVI   CODESW,C'M'         SET CODE SWITCH TO MEDIA                     
         MVI   RCSUBPRG,0          MEDIA HEADLINES                              
*                                                                               
REQL22   L     R2,AMEWCTAB         ADDR OF TABLE IN R5                          
         LA    R5,BINTABLE                                                      
         L     R6,BININ            TABLE ENTRY COUNT INTO R6                    
*                                                                               
REQL24   CLC   MEWCTYPE,CODESW     IS THIS THE CODE (M OR W) WE WANT            
         BNE   REQL26              NO -  LOOK AT NEXT ENTRY                     
         CP    MEWCUSED,=P'0'      IS THIS ENTRY UNUSED                         
         BE    REQL26              YES-  LOOK AT NEXT ENTRY                     
         BAS   RE,PRNTIT           PRINT                                        
*                                                                               
REQL26   LA    R5,MEWCLEN(R5)      NEXT TABLE ENTRY                             
         BCT   R6,REQL24                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'          RESET TO PAGE 1                              
         CLI   CODESW,C'W'         DID WE ALREADY GO THRU THE W/C'S             
         BNE   REQL28              NO - GO PROCESS FOR W/C                      
*                                                                               
         CP    DLTCNT,=P'0'        HAVE ANY RECORDS BEEN DELETED                
         BE    REQLX               NO -- GET OUT YOUR DONE                      
         MVI   RCSUBPRG,2          DELETE CONTROL SHEET HEADLINES               
         MVI   DLTSWITC,C'Y'       SET SWITCH TO PRINT CONTROL SHEET            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'          RESET TO PAGE 1                              
         BAS   RE,PRNTIT                                                        
         B     REQLX                                                            
*                                                                               
REQL28   MVI   CODESW,C'W'         SET CODE SWITCH TO WORKCODES                 
         MVI   RCSUBPRG,1          WORKCODE HEADLINES                           
         B     REQL22                                                           
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL READ 1R TRANSACTIONS LOOKING FOR WORKCODES *         
***********************************************************************         
*                                                                               
         USING TIMRECD,R3                                                       
BLD1R    NTR1                                                                   
         LA    R3,IOA               READ 1R RECORDS                             
         MVC   TIMKEY,SPACES                                                    
         MVC   TIMKCPY,RCCOMPFL                                                 
         MVC   TIMKUNT(2),=C'1R'                                                
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
BLD1R02  BAS   RE,SEQ                                                           
         CLC   TIMKEY(3),SAVEKEY    ANYMORE?                                    
         BNE   BLD1RX               NO                                          
         CLC   TIMKREF,=C'*TIME*'   YES, IT IS TIME?                            
         BNE   BLD1R02              NO, GET NEXT                                
*                                                                               
         LR    R4,R3                                                            
         MVI   ELCODE,TIMELQ        FIND TIME ELEMENT                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLD1R04  BAS   RE,NEXTEL                                                        
         BNE   BLD1R02                                                          
         USING TIMELD,R4                                                        
         CLI   TIMETYP,TIMEINP      IS THIS INPUT DETAIL?                       
         BNE   BLD1R04              NO, CHECK NEXT                              
         CLC   TIMTSK,SPACES        YES, IS THERE A WORKCODE?                   
         BNH   BLD1R04              NO                                          
*                                                                               
         USING MEWCD,R5                                                         
         LA    R5,MEWCWK            YES, ADD TO TABLE                           
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'        SET TYPE TO W=WORKCODE                      
         MVC   MEWCCODE,TIMTSK      WORK CODE INTO TABLE                        
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'       SET USED BUCKET TO 1                        
         ZAP   MEWCFOND,=P'0'       SET FOUND TO 0                              
         ZAP   MEWC1RTR,=P'1'       SET USED BUCKET TO 1                        
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
         B     BLD1R04             GET NEXT ELEMENT                             
*                                                                               
BLD1RX   XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL READ 2C32 RECORDS LOOKING FOR WORKCODES    *         
***********************************************************************         
*                                                                               
         USING CATRECD,R3                                                       
BLD2C    NTR1                                                                   
         LA    R3,IOA                                                           
         XC    CATKEY,CATKEY                                                    
         MVI   CATKTYP,CATKTYPQ                                                 
         MVI   CATKSUB,CATKSUBQ                                                 
         MVC   CATKCPY,RCCOMPFL                                                 
         MVC   CATKUNT(2),QUNIT                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
BLD2C02  BAS   RE,SEQ                                                           
         CLC   CATKEY(5),SAVEKEY     ANY RECORDS?                               
         BNE   BLD2CX                NO, DONE                                   
*                                                                               
         LR    R4,R3                                                            
         MVI   ELCODE,CWKELQ        FIND CATEGORY WORKCODE ELEMENT              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLD2C04  BAS   RE,NEXTEL                                                        
         BNE   BLD2C02                                                          
         USING CWKELD,R4                                                        
         CLI   CWKTYPE,CWKTWORK     IS THIS A WORKCODE?                         
         BNE   BLD2C04              NO, CHECK NEXT                              
*                                                                               
         USING MEWCD,R5                                                         
         LA    R5,MEWCWK            YES, ADD TO TABLE                           
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'        SET TYPE TO W=WORKCODE                      
         MVC   MEWCCODE,CWKWORK     WORK CODE INTO TABLE                        
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'       SET USED BUCKET TO 1                        
         ZAP   MEWCFOND,=P'0'       SET FOUND TO 0                              
         ZAP   MEWCCATG,=P'1'       SET USED BUCKET TO 1                        
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
         B     BLD2C04                                                          
*                                                                               
BLD2CX   XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL READ 1D RECORDS LOOKING FOR WORKCODES      *         
***********************************************************************         
*                                                                               
         USING LSTRECD,R3                                                       
BLD1D    NTR1                                                                   
         LA    R3,IOA                                                           
         XC    LSTKEY,LSTKEY                                                    
         MVI   LSTKTYP,LSTKTYPQ                                                 
         MVC   LSTKCPY,RCCOMPFL                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
BLD1D02  BAS   RE,SEQ                                                           
         CLC   LSTKEY(2),SAVEKEY     ANY RECORDS?                               
         BNE   BLD1DX                NO, DONE                                   
*                                                                               
         LR    R4,R3                                                            
         MVI   ELCODE,LITELQ        FIND LIST TYPE ELEMENT                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLD1D04  BAS   RE,NEXTEL                                                        
         BNE   BLD1D02                                                          
         USING LITELD,R4                                                        
         CLI   LITTYPE,LITTWRK      IS THIS A WORKCODE?                         
         BNE   BLD1D04              NO, CHECK NEXT                              
*                                                                               
         LR    R4,R3                                                            
         MVI   ELCODE,LIDELQ        YES, GET LIST DATA ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   BLD1D02              NO ELEMENTS, GET NEXT RECORD                
*                                                                               
         USING LIDELD,R4                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,LIDLN            FIND OUT THE # OF ENTRIES IN LIST            
         SHI   RF,LIDDACCS-LIDELD                                               
*                                                                               
         SR    R2,R2                                                            
         IC    R2,LIDITLN          LENGTH OF EACH ITEM                          
         LTR   R2,R2                                                            
         BZ    BLD1D02             THIS SHOULD NOT HAPPEN                       
*                                                                               
         DR    RE,R2               LENGTH ELEMENT/LENGTH OF DATA                
         LTR   RF,RF                                                            
         BZ    BLD1D02             THIS SHOULDN'T HAPPEN EITHER                 
*                                                                               
         LR    R0,RF               NUMBER OF ENTRIES IN LIST                    
         LA    R2,LIDDACCS         POINT TO ACCOUNTS                            
*                                                                               
         USING MEWCD,R5                                                         
BLD1D06  LA    R5,MEWCWK           YES, ADD TO TABLE                            
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'       SET TYPE TO W=WORKCODE                       
         MVC   MEWCCODE,0(R2)      WORK CODE INTO TABLE                         
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'      SET USED BUCKET TO 1                         
         ZAP   MEWCFOND,=P'0'      SET FOUND TO 0                               
         ZAP   MEWCWKLT,=P'1'      SET USED BUCKET TO 1                         
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
         SR    R1,R1                                                            
         IC    R1,LIDITLN          ADD LENGTH OF ENTRY                          
         AR    R2,R1               BUMP TO NEXT ENTRY                           
         BCT   R0,BLD1D06                                                       
         B     BLD1D02             GO GET NEXT RECORD                           
*                                                                               
BLD1DX   XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL READ 2A RECORDS LOOKING FOR WORKCODES      *         
***********************************************************************         
*                                                                               
         USING PCHRECD,R3                                                       
BLD2A    NTR1                                                                   
         LA    R3,IOA                                                           
         XC    PCHKEY,PCHKEY                                                    
         MVI   PCHKTYP,PCHKTYPQ                                                 
         MVC   PCHKCPY,RCCOMPFL                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
BLD2A02  BAS   RE,SEQ                                                           
         CLC   PCHKEY(2),SAVEKEY     ANY RECORDS?                               
         BNE   BLD2AX                NO, DONE                                   
*                                                                               
         USING MEWCD,R5                                                         
         LA    R5,MEWCWK           YES, ADD TO TABLE                            
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'       SET TYPE TO W=WORKCODE                       
         MVC   MEWCCODE,PCHKTSK    WORK CODE INTO TABLE                         
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'      SET USED BUCKET TO 1                         
         ZAP   MEWCFOND,=P'0'      SET FOUND TO 0                               
         ZAP   MEWCRATE,=P'1'      SET USED BUCKET TO 1                         
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
*                                                                               
         B     BLD2A02             GO GET NEXT RECORD                           
*                                                                               
BLD2AX   XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL READ 19 RECORDS LOOKING FOR WORKCODES      *         
***********************************************************************         
*                                                                               
         USING PAJRECD,R3                                                       
BLD19    NTR1                                                                   
         LA    R3,IOA                                                           
         XC    PAJKEY,PAJKEY                                                    
         MVI   PAJKTYP,PAJKTYPQ                                                 
         MVC   PAJKCPY,RCCOMPFL                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
BLD1902  BAS   RE,SEQ                                                           
         CLC   PAJKEY(2),SAVEKEY     ANY RECORDS?                               
         BNE   BLD19X                NO, DONE                                   
*                                                                               
         USING MEWCD,R5                                                         
         LA    R5,MEWCWK           YES, ADD TO TABLE                            
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'       SET TYPE TO W=WORKCODE                       
         MVC   MEWCCODE,PAJKTSK    WORK CODE INTO TABLE                         
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'      SET USED BUCKET TO 1                         
         ZAP   MEWCFOND,=P'0'      SET FOUND TO 0                               
         ZAP   MEWCADJR,=P'1'      SET USED BUCKET TO 1                         
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
*                                                                               
         B     BLD1902             GO GET NEXT RECORD                           
*                                                                               
BLD19X   XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL READ ETYPE RECORDS LOOKING FOR WORKCODES   *         
***********************************************************************         
*                                                                               
         USING ETYRECD,R3                                                       
BLD37    NTR1                                                                   
         LA    R3,IOA                                                           
         XC    ETYKEY,ETYKEY                                                    
         MVI   ETYKTYP,ETYKTYPQ                                                 
         MVI   ETYKSUB,ETYKSUBQ                                                 
         MVC   ETYKCPY,RCCOMPFL                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
BLD3702  BAS   RE,SEQ                                                           
         CLC   ETYKEY(3),SAVEKEY     ANY RECORDS?                               
         BNE   BLD37X                NO, DONE                                   
*                                                                               
         LR    R4,R3                                                            
         MVI   ELCODE,LIDELQ        FIND LIST TYPE ELEMENT                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLD3704  BAS   RE,NEXTEL                                                        
         BNE   BLD3702                                                          
         USING LIDELD,R4                                                        
         CLI   LIDTYPE,LIDTWKCD     IS THIS A WORKCODE?                         
         BNE   BLD3704              NO, CHECK NEXT                              
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,LIDLN            FIND OUT THE # OF ENTRIES IN LIST            
         SHI   RF,LIDDATA-LIDELD                                                
*                                                                               
         SR    R2,R2                                                            
         IC    R2,LIDITLN          LENGTH OF EACH ITEM                          
         LTR   R2,R2                                                            
         BZ    BLD3702             THIS SHOULD NOT HAPPEN                       
*                                                                               
         DR    RE,R2               LENGTH ELEMENT/LENGTH OF DATA                
         LTR   RF,RF                                                            
         BZ    BLD3702             THIS SHOULDN'T HAPPEN EITHER                 
*                                                                               
         LR    R0,RF               NUMBER OF ENTRIES IN LIST                    
         LA    R2,LIDDATA          POINT TO ACCOUNTS                            
*                                                                               
         USING MEWCD,R5                                                         
BLD3706  LA    R5,MEWCWK           YES, ADD TO TABLE                            
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'       SET TYPE TO W=WORKCODE                       
         MVC   MEWCCODE,0(R2)      WORK CODE INTO TABLE                         
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'      SET USED BUCKET TO 1                         
         ZAP   MEWCFOND,=P'0'      SET FOUND TO 0                               
         ZAP   MEWCEXPT,=P'1'      SET USED BUCKET TO 1                         
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
         SR    R1,R1                                                            
         IC    R1,LIDITLN          ADD LENGTH OF ENTRY                          
         AR    R2,R1               BUMP TO NEXT ENTRY                           
         BCT   R0,BLD3706                                                       
         B     BLD3702             GO GET NEXT RECORD                           
*                                                                               
BLD37X   XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
*      THIS ROUTINE WILL READ LIMIT LIST RECORDS LOOKING FOR WORKCODES          
**********************************************************************          
         USING LLSRECD,R3                                                       
BLDLL    NTR1                                                                   
         LA    R3,IOA                                                           
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,RCCOMPFL                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
BLDLL02  BAS   RE,SEQ                                                           
         CLC   LLSKEY(3),SAVEKEY     ANY RECORDS?                               
         BNE   BLDLLX                NO, DONE                                   
*                                                                               
         LR    R4,R3                                                            
         MVI   ELCODE,LIDELQ        FIND LIST TYPE ELEMENT                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLDLL04  BAS   RE,NEXTEL                                                        
         BNE   BLDLL02                                                          
*                                                                               
         USING LIDELD,R4                                                        
         CLI   LIDTYPE,LIDTWCL      IS THIS A WORKCODE?                         
         BNE   BLDLL04              NO, CHECK NEXT                              
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,LIDLN            FIND OUT THE # OF ENTRIES IN LIST            
         SHI   RF,LIDDATA-LIDELD                                                
*                                                                               
         SR    R2,R2                                                            
         IC    R2,LIDITLN          LENGTH OF EACH ITEM                          
         LTR   R2,R2                                                            
         BZ    BLDLL02             THIS SHOULD NOT HAPPEN                       
*                                                                               
         DR    RE,R2               LENGTH ELEMENT/LENGTH OF DATA                
         LTR   RF,RF                                                            
         BZ    BLDLL02             THIS SHOULDN'T HAPPEN EITHER                 
*                                                                               
         LR    R0,RF               NUMBER OF ENTRIES IN LIST                    
         LA    R2,LIDDATA          POINT TO ACCOUNTS                            
*                                                                               
         USING MEWCD,R5                                                         
BLDLL06  LA    R5,MEWCWK           YES, ADD TO TABLE                            
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'       SET TYPE TO W=WORKCODE                       
         MVC   MEWCCODE,2(R2)      WORK CODE INTO TABLE                         
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'      SET USED BUCKET TO 1                         
         ZAP   MEWCFOND,=P'0'      SET FOUND TO 0                               
         ZAP   MEWCLLST,=P'1'      SET USED BUCKET TO 1                         
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
         SR    R1,R1                                                            
         IC    R1,LIDITLN          ADD LENGTH OF ENTRY                          
         AR    R2,R1               BUMP TO NEXT ENTRY                           
         BCT   R0,BLDLL06                                                       
         B     BLDLL02             GO GET NEXT RECORD                           
*                                                                               
BLDLLX   XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
*      THIS ROUTINE WILL READ GROUP LIST RECORDS LOOKING FOR WORKCODES          
**********************************************************************          
         USING GLSRECD,R3                                                       
BLDGL    NTR1                                                                   
         LA    R3,IOA                                                           
         XC    GLSKEY,GLSKEY                                                    
         MVI   GLSKTYP,GLSKTYPQ                                                 
         MVI   GLSKSUB,GLSKSUBQ                                                 
         MVC   GLSKCPY,RCCOMPFL                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
BLDGL02  BAS   RE,SEQ                                                           
         CLC   GLSKEY(3),SAVEKEY     ANY RECORDS?                               
         BNE   BLDGLX                NO, DONE                                   
*                                                                               
         LR    R4,R3                                                            
         MVI   ELCODE,LIDELQ        FIND LIST TYPE ELEMENT                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLDGL04  BAS   RE,NEXTEL                                                        
         BNE   BLDGL02                                                          
*                                                                               
         USING LIDELD,R4                                                        
         CLI   LIDTYPE,LIDTWCL      IS THIS A WORKCODE?                         
         BNE   BLDGL04              NO, CHECK NEXT                              
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,LIDLN            FIND OUT THE # OF ENTRIES IN LIST            
         SHI   RF,LIDDATA-LIDELD                                                
*                                                                               
         SR    R2,R2                                                            
         IC    R2,LIDITLN          LENGTH OF EACH ITEM                          
         LTR   R2,R2                                                            
         BZ    BLDGL02             THIS SHOULD NOT HAPPEN                       
*                                                                               
         DR    RE,R2               LENGTH ELEMENT/LENGTH OF DATA                
         LTR   RF,RF                                                            
         BZ    BLDGL02             THIS SHOULDN'T HAPPEN EITHER                 
*                                                                               
         LR    R0,RF               NUMBER OF ENTRIES IN LIST                    
         LA    R2,LIDDATA          POINT TO ACCOUNTS                            
*                                                                               
         USING MEWCD,R5                                                         
BLDGL06  LA    R5,MEWCWK           YES, ADD TO TABLE                            
         XC    MEWCWK(MEWCLEN),MEWCWK                                           
*                                                                               
         MVI   MEWCTYPE,C'W'       SET TYPE TO W=WORKCODE                       
         MVC   MEWCCODE,2(R2)      WORK CODE INTO TABLE                         
*                                                                               
         BAS   RE,INIBUKT0                                                      
         ZAP   MEWCUSED,=P'1'      SET USED BUCKET TO 1                         
         ZAP   MEWCFOND,=P'0'      SET FOUND TO 0                               
         ZAP   MEWCGLST,=P'1'      SET USED BUCKET TO 1                         
*                                                                               
         GOTO1 BINADD,DMCB,(R5),AMEWCTAB                                        
         SR    R1,R1                                                            
         IC    R1,LIDITLN          ADD LENGTH OF ENTRY                          
         AR    R2,R1               BUMP TO NEXT ENTRY                           
         BCT   R0,BLDGL06                                                       
         B     BLDGL02             GO GET NEXT RECORD                           
*                                                                               
BLDGLX   XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*        THIS ROUTINE WILL DELETE UNUSED MEDIA AND WORKCODES          *         
***********************************************************************         
*                                                                               
DELETE   NTR1                                                                   
         USING ACKEYD,R3                                                        
         USING MEWCD,R5                                                         
         LA    R3,IOA                                                           
         MVC   ACKEYACC(49),SPACES     CLEAR KEY                                
         CLI   MEWCTYPE,C'M'           IS THIS A MEDIA?                         
         BNE   DELE02                  NO                                       
*                                                                               
         MVI   ACKEYACC,X'09'          YES, BUILD MEDIA KEY                     
         MVC   ACKEYACC+1(1),RCCOMPFL                                           
         MVC   ACKEYACC+2(1),MEWCCODE+1                                         
         B     DELE04                                                           
*                                                                               
DELE02   CLI   MEWCTYPE,C'W'           MUST BE A WORKCODE                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ACKEYACC,X'0A'          BUILD WORKCODE KEY                       
         MVC   ACKEYACC+1(1),RCCOMPFL                                           
         MVC   ACKEYACC+2(2),QUNIT                                              
         MVC   ACKEYACC+4(2),MEWCCODE                                           
*                                                                               
DELE04   BAS   RE,READ                 READ THE RECORD                          
         MVI   ACSTATUS,X'80'          MARK IR FOR DELETION                     
         BAS   RE,WRITE                WRITE IT                                 
         AP    DLTCNT,=P'1'            ADD TO DELETE COUNTER                    
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        PRINT THE REPORTS                                            *         
***********************************************************************         
*                                                                               
PRNTIT   NTR1                                                                   
         USING MEWCD,R5                                                         
         USING PRNTRECD,R3                                                      
         LA    R3,P                                                             
         CLI   QOPT1,C'L'          IS REPORT 'LIVE'                             
         BE    PRNT02                                                           
         MVC   HEAD4+51(7),=C'*DRAFT*'                                          
         B     PRNT04                                                           
*                                                                               
PRNT02   MVC   HEAD4+50(6),=C'*LIVE*'                                           
         CLI   DLTSWITC,C'Y'       DO YOU WANT A DELETE CONTROL SHEET           
         BE    PRNT14              YES                                          
*                                                                               
PRNT04   CP    MEWCUSED,=P'0'      IS IT AVAILABLE FOR DELETION                 
         BE    PRNT06              YES - PRINT DELETION HEADLINE                
         MVC   HEAD6+43(22),=C'CURRENT RECORDS IN USE'                          
         MVC   HEAD8+32(8),PRTHDR                                               
         MVC   HEAD9+32(8),=C'--------'                                         
         CLI   MEWCTYPE,C'W'       SET TYPE TO W=WORKCODE                       
         BNE   PRNT10                                                           
         MVC   HEAD8+32(L'PRTHDR),PRTHDR                                        
         LA    R7,HEAD9                                                         
         LA    R7,32(R7)                                                        
         LHI   R0,MEWPRTBK         NUMBER OF BUCKET TO PRINT                    
PRNT05   MVC   0(L'MEWCUSED,R7),=C'--------'                                    
         LA    R7,L'MEWCUSED(R7)                                                
         MVI   0(R7),C' '                                                       
         LA    R7,1(R7)                                                         
         BCT   R0,PRNT05                                                        
         B     PRNT10                                                           
*                                                                               
PRNT06   CLI   QOPT1,C'L'          IS REPORT 'LIVE'                             
         BNE   PRNT08                                                           
         MVC   HEAD6+43(24),=C'UNUSED RECORDS (DELETED)'                        
         B     PRNT12                                                           
*                                                                               
PRNT08   MVC   HEAD6+36(37),=C'UNUSED RECORDS AVAILABLE FOR DELETION'           
         B     PRNT12                                                           
*                                                                               
PRNT10   LA    R2,MEWCUSED         NUMBER OF TIMES USED INTO R2                 
         EDIT  (P8,0(R2)),(8,PRNTBUKS),,COMMAS=YES                              
         CLI   MEWCTYPE,C'W'       SET TYPE TO W=WORKCODE                       
         BNE   PRNT12                                                           
         LA    R7,PRNTBUKS                                                      
         LA    R2,MEWCUSED                                                      
         LHI   R0,MEWPRTBK         NUMBER OF BUCKET TO PRINT                    
PRNT11   EDIT  (P8,0(R2)),(8,0(R7)),,COMMAS=YES                                 
         LA    R2,L'MEWCUSED(R2)                                                
         LA    R7,L'MEWCUSED(R7)                                                
         LA    R7,1(R7)                                                         
         BCT   R0,PRNT11                                                        
*                                                                               
PRNT12   MVC   PRNTCODE,MEWCCODE     MEDIA OR W/C TO PRINT LINE                 
         CP    MEWCFOND,=P'0'      IS RECORD ON FILE                            
         BNE   *+14                YES -- PRINT THE NAME                        
         MVC   PRNTNONF,=C'*RECORD NOT ON FILE*'                                
         B     PRNT16                                                           
*                                                                               
         MVC   PRNTNAME,MEWCNAME   NAME TO PRINT LINE                           
         B     PRNT16                                                           
*                                                                               
PRNT14   MVC   HEAD8+1(25),=C'NUMBER OF RECORDS DELETED'                        
         MVC   HEAD9+1(25),=C'-------------------------'                        
         LA    R2,DLTCNT           NUMBER OF DELETIONS INTO R2                  
         EDIT  (P2,0(R2)),(3,PRNTDELC),,COMMAS=YES                              
*                                                                               
PRNT16   GOTO1 ACREPORT                                                         
*                                                                               
EXIT     XMOD1 1                                                                
         DROP  R5,R3                                                            
         EJECT                                                                  
*              ADD ITEM TO BINSRCH TABLE                                        
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
*                                                                               
BINADD   NTR1                                                                   
         USING BIND,R2                                                          
         L     R2,4(R1)            BINSRCH PARMS INTO R2                        
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTABLE         A(TABLE)                                     
         L     R4,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              IS THE RECORD FOUND--1 MEANS NO              
         BE    EXIT                NOT FOUND  - ADDED TO TABLE                  
         L     R5,DMCB             A(RECORD FOUND)                              
         ZIC   R7,BINFRST          DISP TO 1ST BUCKET INR7                      
         AR    R5,R7               RECORD FOUND                                 
         AR    R4,R7               NEW RECORD                                   
         ZIC   R0,BINNUMB          NUMBER OF BUCKETS                            
*                                                                               
BINLOOP  AP    0(8,R5),0(8,R4)      UPDATE NUMBER USED AND FOUND BUCKS          
         LA    R5,8(R5)                                                         
         LA    R4,8(R4)                                                         
         BCT   R0,BINLOOP                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        DATAMGR ROUTINES                                             *         
***********************************************************************         
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'           READ HIGH                           
         MVC   SAVEKEY,0(R3)                SAVE KEY BEFORE READ                
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'           READ SEQ                            
         MVC   SAVEKEY,0(R3)                SAVE KEY BEFORE READ                
         B     GTREC                                                            
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'           A SPECIFIC READ                     
         B     GTREC                                                            
*                                                                               
WRITE    MVC   COMMAND,=C'DMWRT'            WRITE RECORD BACK                   
         CLI   RCWRITE,C'N'                 WRITE=NO CARD                       
         BER   RE                           YES--SKIP DATAMGR                   
*                                                                               
GTREC    NTR1                                                                   
         LA    R3,IOA                                                           
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R3),(R3)                       
         TM    DMCB+8,X'FF'-X'02'           TEST FOR ERROR CONDITIONS           
         BZ    EXIT                                                             
         DC    H'0'                         DIE IF ERRORS                       
         EJECT                                                                  
***********************************************************************         
*        INITILIZE BUCKET                                             *         
***********************************************************************         
         USING MEWCD,R5                                                         
INIBUKT0 NTR1                                                                   
         LA    R5,MEWCWK                                                        
         LA    R3,MEWCBUKS         LOAD ADDRESS OF FIRST BUCKET                 
         LHI   R0,MEWCBKCT         R0=NUMBER OF COLUMNS                         
INIBUKT5 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R0,INIBUKT5                                                      
         XIT1                                                                   
***********************************************************************         
*        JOBBER LOOKUP                                                *         
***********************************************************************         
*                                                                               
LOOKUP   NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO                                                    
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE'                                                            
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
*                                                                               
PRTHDR   DS    0CL98                                                            
         DC    CL8'TOTAL   '                                                    
         DC    CL1' '                                                           
         DC    CL8'SJ ACCT '                                                    
         DC    CL1' '                                                           
         DC    CL8'SJ TRANS'                                                    
         DC    CL1' '                                                           
         DC    CL8'1R TRANS'                                                    
         DC    CL1' '                                                           
         DC    CL8'CATG REC'                                                    
         DC    CL1' '                                                           
         DC    CL8'WRK LIST'                                                    
         DC    CL1' '                                                           
         DC    CL8'RATE REC'                                                    
         DC    CL1' '                                                           
         DC    CL8'ADJ RATE'                                                    
         DC    CL1' '                                                           
         DC    CL8'EXP TYPE'                                                    
         DC    CL1' '                                                           
         DC    CL8'GRP LIST'                                                    
         DC    CL1' '                                                           
         DC    CL8'LIM LIST'                                                    
*                                                                               
AMEWCTAB DC    A(MEWCTAB)                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        BINSRCH TABLE OF MEDIA AND WORKCODES                                   
*                                                                               
MEWCTAB  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(MEWCLEN)        RECORD LENGTH                                
         DC    AL4(MEWCLKEY)       DISP. TO KEY/KEY LENGTH                      
         DC    AL4(MEWCMAX)        MAX NUMBER OF RECORDS                        
         DC    AL1(MEWCBKCT)       NUMBER OF BUCKETS                            
         DC    AL1(MEWCBKDS)       DISP TO 1ST BUCKET                           
         DS    (MEWCMAX*MEWCLEN)C  THE TABLE                                    
*                                                                               
MEWCMAX  EQU   1500                                                             
         EJECT                                                                  
ACXWD    DSECT                                                                  
ATYPES   DS    0A                                                               
SQUASHER DS    V                                                                
*                                                                               
ELCODE   DS    CL1                                                              
MEWCSTAT DS    CL1                                                              
*                                                                               
MEWCWK   DS    CL(MEWCLEN)         RECORD WORK AREA                             
DLTCNT   DS    PL2                 DELETED RECORD COUNT                         
DLTSWITC DS    CL1                 SWITCH FOR DELETE CONTROL SHEET              
SAVEKEY  DS    CL49                                                             
COMMAND  DS    CL6                                                              
CODESW   DS    CL1                                                              
IOA      DS    CL2000             I/O AREA                                      
         EJECT                                                                  
*              DSECT FOR BINSRCH PARAMETERS                                     
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP TO 1ST BUCKET                           
BINEQU   EQU   *-BIND                                                           
BINTABLE DS    0CL1                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*              DSECT FOR MEDIA/WORK CODES                                       
MEWCD    DSECT                                                                  
MEWCTYPE DS    CL1                 M--MEDIA  W--WORKCODE                        
MEWCCODE DS    CL2                 ACTUAL MEDIA OR WORKCODE                     
MEWCLKEY EQU   *-MEWCD                                                          
MEWCNAME DS    CL15                CODE NAME                                    
MEWCBKDS EQU   *-MEWCD             DISP TO 1ST BUCKET                           
MEWCBUKS DS    0CL96               **THE BUCKETS**                              
MEWCUSED DS    PL8                 TIMES USED IN ACCTS(M) OR TRANS(WC)          
MEWCSJAC DS    PL8                 TIMES USED IN SJ ACCOUNT REC                 
MEWCSJTR DS    PL8                 TIMES USED IN SJ TRANSACTION REC             
MEWC1RTR DS    PL8                 TIMES USED IN 1R TRANSACTION REC             
MEWCCATG DS    PL8                 TIMES USED IN CATG REC                       
MEWCWKLT DS    PL8                 TIMES USED IN WORK CODE LIST REC             
MEWCRATE DS    PL8                 TIMES USED IN RATE REC                       
MEWCADJR DS    PL8                 TIMES USED IN ADJ RATE REC                   
MEWCEXPT DS    PL8                 TIMES USED IN EXP TYPE REC                   
MEWCLLST DS    PL8                 TIMES USED IN LIMIT LIST                     
MEWCGLST DS    PL8                 TIMES USED IN GROUP LIST                     
MEWPRTBK EQU   (*-MEWCBUKS)/8      NUMBER OF BUCKETS TO PTINT                   
MEWCFOND DS    PL8                 0=NOT IN MED OR W/C REC 1=IT EXISTS          
MEWCBKCT EQU   (*-MEWCBUKS)/8      NUMBER OF BUCKETS                            
MEWCLEN  EQU   *-MEWCD             ENTRY LENGTH                                 
         EJECT                                                                  
*              DSECT FOR PRINT LINE                                             
PRNTRECD DSECT                                                                  
         DS    CL3        1-3                                                   
PRNTCODE DS    CL2        4-5      MEDIA/WORKCODE                               
         DS    CL5        6-10                                                  
PRNTDELC DS    CL3        11-13    NUMBER OF DELETED CODE                       
         ORG   PRNTDELC                                                         
PRNTNAME DS    CL15       11-25    MEDIA/WORKCODE NAME                          
         ORG   PRNTNAME                                                         
PRNTNONF DS    CL20       11-30    MEDIA/WORKCODE NOT ON FILE CONSTANT          
         DS    CL2        31-32                                                 
PRNTBUKS DS    0CL98      33-130   **THE BUCKETS**                              
PRNTUSED DS    CL8        33-40    TIMES USED IN ACCTS(M) OR TRANS(WC)          
         DS    CL1        41                                                    
PRNTSJAC DS    CL8        42-49    TIMES USED IN SJ ACCOUNT REC                 
         DS    CL1        50                                                    
PRNTSJTR DS    CL8        51-58    TIMES USED IN SJ TRANSACTION REC             
         DS    CL1        59                                                    
PRNT1RTR DS    CL8        60-67    TIMES USED IN 1R TRANSACTION REC             
         DS    CL1        68                                                    
PRNTCATG DS    CL8        69-76    TIMES USED IN CATG REC                       
         DS    CL1        77                                                    
PRNTWKLT DS    CL8        78- 85   TIMES USED IN WORK CODE LIST REC             
         DS    CL1        86                                                    
PRNTRATE DS    CL8        87-94    TIMES USED IN RATE REC                       
         DS    CL1        95                                                    
PRNTADJR DS    CL8        96-103   TIMES USED IN ADJ RATE REC                   
         DS    CL1        104                                                   
PRNTEXPT DS    CL8        105-112  TIMES USED IN EXP TYPE REC                   
         DS    CL1        113                                                   
PRNTLLST DS    CL8        114-121  TIMES USED IN LIMIT LIST                     
         DS    CL1        122                                                   
PRNTGLST DS    CL8        123-130  TIMES USED IN GROUP LIST                     
         DS    CL3        131-133                                               
         EJECT                                                                  
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
*ACOFFALD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREPXW02 02/24/20'                                      
         END                                                                    
