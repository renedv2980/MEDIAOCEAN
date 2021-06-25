*          DATA SET GEKEY0A    AT LEVEL 155 AS OF 01/13/16                      
*PHASE TF050AB                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TF050A - PFM INTERFACE OVERLAY FOR CONTROL SYSTEM           *         
*                                                                     *         
*  CALLED FROM: PFM INTERFACE CONTROLLER (TF0500), WHICH CALLS        *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  OUTPUTS: KEY FOR THE RECORD TYPE ACCORDING TO USER INPUT           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TF050A - PFM INTERFACE OVERLAY FOR CONTROL FILE'                
TF050A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TF050A*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
EXIT0A   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SCREEN                                                 *         
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,CONRCRDH         POINT TO RECORD FIELD FIRST                  
         CLI   5(R2),0             NO RECORD?                                   
         BE    MISSFLD             MISSING RECORD                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R4,RECTABLE         POINT TO RECTABLE                            
         CLI   8(R2),C'?'          USER ASKED FOR HELP?                         
         BNE   VKRECLP             NOPE, CHECK ENTRY                            
*                                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         OI    STATFLAG,X'01'      HELP INVOKED                                 
         USING CONHEADH-64,RA                                                   
         XC    CONRCRD,CONRCRD     NULL THE DATA                                
         OI    CONRCRDH+6,X'80'    TRANSMIT THE FIELD                           
         GOTO1 CLRSCN,DMCB,CONP0H  CLEAR THE SCREEN                             
         GOTO1 DISPHELP,DMCB,RECTABLE  PRINT HELP TABLE                         
         B     EXIT0A                                                           
*                                                                               
VKRECLP  CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    INVLFLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R4)                                                    
         BE    VKPARTS             PARTS OF THE KEY                             
         LA    R4,L'RECTABLE(R4)                                                
         B     VKRECLP                                                          
         EJECT                                                                  
VKPARTS  DS    0H                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         TM    STATFLAG,X'01'      WAS HELP INVOKED??                           
         USING CONHEADH-64,RA                                                   
         BZ    VKPARTS5            NO                                           
         GOTO1 CLRSCN,DMCB,CONP0H  YES, CLEAR THE SCREEN                        
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         NI    STATFLAG,X'FF'-X'01'  HELP NOT INVOKED ANYMORE                   
         USING CONHEADH-64,RA                                                   
VKPARTS5 L     R4,8(R4)            GET ADDRESS OF KEY COMPONENTS                
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    R4,R1                    TO RELOCATION FACTOR                    
         NI    6(R2),X'FF'-X'40'   DON'T PUT CURSOR HERE AGAIN                  
         LA    R2,CONP0H           POINT TO 1ST PROTECTED FIELD                 
         ZIC   R1,0(R2)            GET LENGTH TO NEXT FIELD                     
         AR    R1,R2               POINT TO THE 1ST INPUT FIELD                 
         OI    6(R1),X'40'         CURSOR SHOULD BE HERE                        
         LR    R3,R4               SAVE POSITION IN TABLE                       
VKPARTLP CLI   0(R4),X'FF'         NO MORE PARTS OF THE KEY?                    
         BNE   VKPART10            THERE'S MORE                                 
         LR    R4,R3               DONE, NOW VALIDATE COMPONENTS                
*                                                                               
VKCLR    DS    0H                                                               
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)            NO, BUMP TO INPUT FIELD                      
         AR    R2,R1                                                            
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R1,CONPFH           ARE WE AT THE END YET?                       
         CR    R2,R1                                                            
         BNL   VK00                YES, GO VALIDATE INPUT                       
         ZIC   R1,0(R2)            BUMP TO NEXT PROTECTED FIELD                 
         AR    R2,R1                                                            
         B     VKCLR                                                            
*                                                                               
VKPART10 OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         MVC   8(8,R2),0(R4)                                                    
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         NI    1(R2),X'FF'-X'20'   UNPROTECT FIELD                              
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         LA    R4,LTAB(R4)         NEXT COMNPONENT IN KEY                       
         B     VKPARTLP                                                         
         EJECT                                                                  
***********************************************************************         
* CODE TO EXPEDITE THE NEW TABLES FOR RECORD ENTRIES                  *         
***********************************************************************         
VK00     LA    R2,CONI0H                                                        
         XC    INTKEY,INTKEY                                                    
         MVI   CXFLAG,X'FF'        HEX FLAG ON                                  
         MVI   INSERT,X'00'        GET NEXT INSERT POSITION                     
*                                                                               
VK10     DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         LAST FIELD DONE?                             
         BNE   VK40                                                             
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(2),=C'K,'                                                
         CLI   1(R4),X'00'         NO RECORD CODE                               
         BH    VK20                                                             
         MVC   SAVEKEY+2(L'SAVEKEY-2),INTKEY+1                                  
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK20     CLI   1(R4),X'01'         1-BYTE RECORD CODE                           
         BH    VK30                                                             
         CLI   2(R4),C'A'          IS RECORD CODE ALPHANUMERIC??                
         BNH   VK25                                                             
         CLI   2(R4),C'9'                                                       
         BH    VK25                                                             
         MVC   SAVEKEY+2(2),=C'C'''                                             
         MVC   SAVEKEY+4(1),2(R4)                                               
         MVI   SAVEKEY+5,C''''                                                  
         CLI   INTKEY+1,C'C'                                                    
         BNE   VK23                                                             
         MVC   SAVEKEY+5(L'SAVEKEY-5),INTKEY+3                                  
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK23     MVC   SAVEKEY+6(L'SAVEKEY-6),INTKEY+1                                  
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK25     MVC   SAVEKEY+2(2),=C'X'''                                             
         GOTO1 HEXOUT,DMCB,2(R4),SAVEKEY+4,1   1-BYTE REC. CODE                 
         MVI   SAVEKEY+6,C''''                                                  
         CLI   INTKEY+1,C'X'                                                    
         BNE   VK27                                                             
         MVC   SAVEKEY+6(L'SAVEKEY-6),INTKEY+3                                  
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK27     MVC   SAVEKEY+7(L'SAVEKEY-7),INTKEY+1                                  
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK30     MVC   SAVEKEY+2(2),=C'X'''                                             
         GOTO1 HEXOUT,DMCB,2(R4),SAVEKEY+4,2   2-BYTE REC. CODE                 
         MVI   SAVEKEY+8,C''''                                                  
         CLI   INTKEY+1,C'X'                                                    
         BNE   VK35                                                             
         MVC   SAVEKEY+8(L'SAVEKEY-8),INTKEY+3                                  
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK35     MVC   SAVEKEY+9(L'SAVEKEY-9),INTKEY+1                                  
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
*                                                                               
VK40     DS    0H                                                               
*                                                                               
         MVC   LENGTH,5(R2)        GET LENGTH OF FIELD                          
         CLC   LENGTH,8(R4)        INPUT LEN LESS THAN MIN LEN?                 
         BL    INVLFLD             YES, INVALID ENTRY                           
         CLC   LENGTH,9(R4)        INPUT LEN GREATER THAN MAX LEN?              
         BH    INVLFLD             YES, INVALID ENTRY                           
         MVC   PADNUM,9(R4)        GET DEFAULT NUMBER OF CHARS.                 
*                                                                               
         L     RF,10(R4)           GET A(BUILD ROUTINE)                         
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    RF,R1                 TO RELOCATION FACTOR                       
         BASR  RE,RF               GET FIELD, INSERT INTO KEY                   
         LA    R4,LTAB(R4)         NEXT KEY COMPONENT                           
         LA    R2,DENTRY(R2)       BUMP TO NEXT SCREEN HEADER                   
         B     VK10                                                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
*                                                                               
VK50     DS    0H                                                               
*                                                                               
         MVC   XKEY,INTKEY         SET TRANSFER KEY                             
         MVC   DISKADDR(L'DISKA+L'INTKEY),DISKA                                 
         USING CONHEADH-64,RA                                                   
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* PREPARE KEY FOR INCOMING HEX FIELD                                  *         
***********************************************************************         
VXKEY    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         STCM  R3,15,SAVER3        SAVE R3                                      
         ZIC   R3,FLDLEN           GET FIELD LENGTH                             
         LA    R4,0(R3,R4)                                                      
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    EXIT0A              YES RETURN                                   
         ICM   R3,15,SAVER3        RESTORE R3                                   
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         STCM  R3,15,SAVER3        SAVE R3                                      
         BE    EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* PREPARE KEY FOR INCOMING CHARACTER FIELD                            *         
***********************************************************************         
VCKEY    NTR1                                                                   
         BE    EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* INSERT CHARACTERS TO KEY                                            *         
***********************************************************************         
VCHAR    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'00'        CHAR. STRING SO FAR?                         
         BE    VCHAR10                                                          
         MVC   1(2,R3),=C'C'''     NO, INSERT CHAR. HEADER                      
         LA    R3,3(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,3(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VCHAR10  DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      DEFAULT TO SPACES                            
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C''''                                                      
         SR    R3,R4               POINT R3 TO FIELD                            
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT0A              STOP                                         
*                                                                               
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* INSERT CHARACTERS TO KEY WITH HEX 0'S FOR DEFAULT                   *         
***********************************************************************         
VXCHAR   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   LENGTH,0            SET DEFAULT                                  
         BE    VXCHAR20                                                         
*                                                                               
         CLI   CXFLAG,X'00'        CHAR. STRING SO FAR?                         
         BE    VXCHAR10                                                         
         MVC   1(2,R3),=C'C'''     NO, INSERT CHAR. HEADER                      
         LA    R3,3(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,3(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VXCHAR10 DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      DEFAULT TO SPACES                            
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C''''                                                      
         SR    R3,R4               POINT R3 TO FIELD                            
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT0A              STOP                                         
*                                                                               
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT0A                                                           
*                                                                               
VXCHAR20 DS    0H                                                               
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VXCHAR30                                                         
         MVC   1(2,R3),=C'X'''     NO, INSERT CHAR. HEADER                      
         LA    R3,3(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,3(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VXCHAR30 DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         LA    R4,0(R4,R4)         CHAR TO HEX IS DOUBLED                       
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=10C'0'     DEFAULT TO ZEROS                             
         ZIC   R4,PADNUM                                                        
         LA    R4,0(R4,R4)         CHAR TO HEX IS DOUBLED                       
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C''''                                                      
         SR    R3,R4               POINT R3 TO FIELD                            
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         BE    EXIT0A              STOP                                         
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CHAR. NUMBERS                                        *         
***********************************************************************         
VNUM     NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BE    VNUM10                                                           
         MVC   1(2,R3),=C'C'''     YES, INSERT CHAR. HEADER                     
         LA    R3,3(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,3(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VNUM10   DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=10C'0'     DEFAULT TO ZEROS                             
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C''''                                                      
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT0A              STOP                                         
*                                                                               
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         SR    R3,R4                                                            
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BINARY                                               *         
***********************************************************************         
VBIN     NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VBIN30                                                           
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         CLI   PADNUM,4                                                         
         BL    VBIN05                                                           
         C     R4,=F'65535'        BOUNDARY CHECK FOR 2-BYTE BINARY             
         BH    INVLFLD                                                          
         STCM  R4,3,WORK                                                        
         B     VBIN30                                                           
VBIN05   CLI   PADNUM,3                                                         
         BL    VBIN10                                                           
         CH    R4,=H'255'          BOUNDARY CHECK FOR 1-BYTE BINARY             
         BH    INVLFLD                                                          
         STC   R4,WORK                                                          
         B     VBIN20                                                           
VBIN10   CH    R4,=H'99'           BOUNDARY CHECK FOR YEAR                      
         BH    INVLFLD                                                          
VBIN20   STC   R4,WORK                                                          
*                                                                               
VBIN30   LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   PADNUM,4                                                         
         BL    VBIN40                                                           
         LA    R4,2(R4)            2-BYTE BINARY                                
VBIN40   LA    R4,2(R4)            1-BYTE BINARY                                
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BE    VBIN50                                                           
         MVC   1(2,R3),=C'X'''     NO, INSERT HEX HEADER                        
         LA    R3,3(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,3(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
*                                                                               
VBIN50   DS    0H                                                               
         CLI   PADNUM,3                                                         
         BH    VBIN60                                                           
         MVC   0(3,R3),=C'00'''                                                 
         CLI   LENGTH,0                                                         
         BE    EXIT0A                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT0A                                                           
VBIN60   DS    0H                                                               
         MVC   0(5,R3),=C'0000'''                                               
         CLI   LENGTH,0                                                         
         BE    EXIT0A                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),2                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE PHASE                                                  *         
***********************************************************************         
VPHASE   NTR1                                                                   
         CLI   8(R2),C'T'          ALL PHASES BEGIN WITH T                      
         BNE   INVLFLD                                                          
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(10,R3),=C'18X''00''X''0'                                       
         LA    R3,11(R3)           BUMP FORWARD                                 
         GOTO1 HEXIN,DMCB,9(R2),DUB,5                                           
         CLI   DMCB+15,0           VALIDATE HEX STRING                          
         BE    INVLFLD                                                          
         MVC   0(5,R3),9(R2)       MOVE IN PHASE                                
         LA    R3,5(R3)            BUMP FORWARD                                 
         MVI   0(R3),C''''                                                      
         ZIC   R4,INSERT                                                        
         LA    R4,16(R4)                                                        
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LANG                                                   *         
***********************************************************************         
VLANG    NTR1                                                                   
         CLI   5(R2),0                                                          
         BNE   VLANG10                                                          
         MVC   8(3,R2),=C'ENG'     DEFAULT IS ENGLISH                           
         MVI   5(R2),3             DEFAULT INPUT LENGTH                         
         OI    6(R2),X'80'         DISPLAY THE DEFAULT                          
VLANG10  GOTO1 GETFACT,DMCB,0      GET RETURN BLOCK FOR FACTSD                  
         L     R1,0(R1)            POINT TO INSERT POSITION                     
         USING FACTSD,R1                                                        
         L     R3,FAALANG          ADDRESS OF LANGUAGE TABLE                    
         DROP  R1                  NO NEED FOR THE DSECT RIGHT NOW              
         USING LANGTABD,R3                                                      
         LH    R4,0(R3)            LENGTH OF TABLE ENTRIES                      
         L     R5,2(R3)            END OF THE TABLE                             
         LA    R3,6(R3)            FIRST ENTRY OF LANGUAGE TABLE                
         ZIC   R6,5(R2)                                                         
         BCTR  R6,0                                                             
*                                                                               
* CHECKING THROUGH THE LANGUAGE TABLE                                           
*                                                                               
VLANG20  EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),LANGSHR     MATCH ON SHORT LANGUAGE NAME?                
         BE    VLANG30                                                          
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),LANGSHRN    MATCH ON NATIVE LANGUAGE NAME?               
         BE    VLANG30                                                          
         BXLE  R3,R4,VLANG20       WHILE R6,=R5, R6:=R6+R4                      
         B     INVLFLD             INVALID LANGUAGE                             
VLANG30  MVC   DUB(1),LANGCODE     GOOD LANGUAGE CODE                           
         XI    DUB,X'FF'           FLIP THE BITS OF LANG CODE                   
         CLI   DUB,X'FE'           IN U.S., LANG EUK NOT ALLOWED                
*&&UK*&& CLI   DUB,X'FD'           IN U.K., LANG EUS NOT ALLOWED                
         BE    INVLFLD                                                          
         XI    DUB,X'FF'           FLIP THE BITS BACK                           
*                                                                               
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         GOTO1 HEXOUT,DMCB,DUB,0(R3),1                                          
         MVI   2(R3),C''''                                                      
         LA    R4,2(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         B     EXIT0A                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LEVEL                                                  *         
***********************************************************************         
VLEVEL   NTR1                                                                   
         CLI   LENGTH,0                                                         
         BE    EXIT0A                                                           
         CLI   8(R2),C'A'                                                       
         BL    INVLFLD                                                          
         CLI   8(R2),C'C'                                                       
         BH    INVLFLD                                                          
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE ID NAME                                                *         
***********************************************************************         
VIDNAME  NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'14X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE ID NUMBER                                              *         
***********************************************************************         
VIDNUM   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'22X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'        FOOL VBIN                                    
         BAS   RE,VBIN                                                          
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE JCL ID                                                 *         
***********************************************************************         
VJCLID   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'13X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SYSTEM                                                 *         
***********************************************************************         
VSYSTEM  NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'10X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LINE ID                                                *         
***********************************************************************         
VLINEID  NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(6,R3),=C'6X''00'''                                             
         LA    R3,6(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,6(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE PASSWORD                                               *         
***********************************************************************         
VPASSWD  NTR1                                                                   
         CLI   LENGTH,0                                                         
         BH    VPASSWD5                                                         
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'10X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         B     EXIT0A                                                           
VPASSWD5 BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE TERMINAL NUMBER                                        *         
***********************************************************************         
VTNUM    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'22X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'        FOOL VBIN                                    
         BAS   RE,VBIN                                                          
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE PRT LINE ID                                            *         
***********************************************************************         
VPLINEID NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(10,R3),=C'5X''00''C''P'''                                      
         LA    R3,10(R3)            BUMP FORWARD                                
         ZIC   R4,INSERT                                                        
         LA    R4,10(R4)                                                        
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE AUTHORIZATION CODE                                     *         
***********************************************************************         
VAUTH    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'20X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'        FOOL VBIN                                    
         BAS   RE,VBIN                                                          
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SUB-RECORD TYPE                                        *         
***********************************************************************         
VSUBREC  NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'16X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'        FOOL VCHAR                                   
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE ALPHA USER-ID                                          *         
***********************************************************************         
VAUID    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'22X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'        FOOL VBIN                                    
         BAS   RE,VBIN                                                          
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE ALPHA AGENCY CODE                                      *         
***********************************************************************         
VAAC     NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'11X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'        FOOL VCHAR                                   
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE DATATYPE SYSTEM                                        *         
***********************************************************************         
VDSYS    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'10X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'        FOOL VCHAR                                   
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE ELEMENT KEY                                            *         
***********************************************************************         
VEKEY    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(6,R3),=C'4X''00'''                                             
         LA    R3,6(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,6(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
*                                                                               
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VEKEY10                                                          
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         STCM  R4,7,WORK                                                        
         C     R4,=F'16777215'     INVALID IF GREATER THAN X'FFFFFF'            
         BH    INVLFLD                                                          
*                                                                               
VEKEY10  LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         LA    R4,6(R4)            3-BYTE BINARY                                
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVC   1(2,R3),=C'X'''     NO, INSERT HEX HEADER                        
         LA    R3,3(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,3(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         MVC   0(7,R3),=C'000000'''                                             
         CLI   LENGTH,0                                                         
         BE    EXIT0A                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),3                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE MESSAGE TYPE                                           *         
***********************************************************************         
VMESST   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(6,R3),=C'7X''00'''                                             
         LA    R3,6(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,6(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'        FOOL VCHAR                                   
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE MESSAGE NAME                                           *         
***********************************************************************         
VMESSN   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(6,R3),=C'8X''00'''                                             
         LA    R3,6(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,6(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'        FOOL VBIN                                    
         BAS   RE,VBIN                                                          
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LOAD SYSTEM                                            *         
***********************************************************************         
VLSYS    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'18X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'        FOOL VCHAR                                   
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LOAD PROGRAM                                           *         
***********************************************************************         
VLPRG    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(5,R3),=C'X''00'''                                              
         LA    R3,5(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,5(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'        FOOL VCHAR                                   
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE DICTIONARY CODE                                        *         
***********************************************************************         
VDICT    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(7,R3),=C'10X''00'''                                            
         LA    R3,7(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,7(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'        FOOL VCHAR                                   
         BAS   RE,VCHAR                                                         
         B     EXIT0A                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE MESSAGE TYPE                                           *         
***********************************************************************         
VMSGT    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(6,R3),=C'7X''00'''                                             
         LA    R3,6(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,6(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'        FOOL VCHAR                                   
         BAS   RE,VCHAR                                                         
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         MVC   1(6,R3),=C'8X''00'''                                             
         LA    R3,6(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,6(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'        FOOL VBIN                                    
         B     EXIT0A                                                           
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MYERROR  GOTO1 ERREX2                                                           
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         EJECT                                                                  
*                                                                               
*                                CL8 - NAME OF THE RECORD TYPE                  
*                                AL4 - ADDRESS OF KEY PORTIONS                  
*                                                                               
RECTABLE DS    0CL12                                                            
         DC    C'LOAD    ',AL4(LOADREC)                                         
         DC    C'PHASE   ',AL4(PHASEREC)                                        
         DC    C'IDNAME  ',AL4(IDNAMREC)                                        
         DC    C'IDNUMBER',AL4(IDNUMREC)                                        
         DC    C'JCL     ',AL4(JCLREC)                                          
         DC    C'TERMLUID',AL4(TERMLREC)                                        
         DC    C'TERMNUM ',AL4(TERMNREC)                                        
         DC    C'TERMPRT ',AL4(TERMPREC)                                        
         DC    C'UP      ',AL4(UPREC)                                           
         DC    C'PA      ',AL4(PAREC)                                           
         DC    C'SL      ',AL4(SLREC)                                           
         DC    C'SA      ',AL4(SAREC)                                           
         DC    C'SP      ',AL4(SPREC)                                           
         DC    C'DTYPE   ',AL4(DTYPEREC)                                        
         DC    C'SPANEL  ',AL4(SPANREC)                                         
         DC    C'BM      ',AL4(BMREC)                                           
         DC    C'ENTRY   ',AL4(ENTRYREC)                                        
         DC    C'BROADCAS',AL4(BROADREC)                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                CL8 - NAME OF THE FIELD TO INPUT               
*                                XL1 - MINIMUM LENGTH OF INPUT                  
*                                XL1 - MAXIMUM LENGTH OF INPUT                  
*                                AL4 - ADDRESS OF VALIDATING ROUTINE            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
PHASEREC DC    C'PHASE   ',X'06',X'06',AL4(VPHASE)                              
STA      DC    C'LANG    ',X'00',X'03',AL4(VLANG)                               
STA1     DC    C'LEVEL   ',X'00',X'01',AL4(VLEVEL)                              
         DC    X'FF',X'02',X'0501'                                              
LOADREC  DC    C'SYSTEM  ',X'01',X'02',AL4(VLSYS)                               
         DC    C'PROGRAM ',X'01',X'02',AL4(VLPRG)                               
         DC    X'FF',X'02',X'0502'                                              
IDNAMREC DC    C'ID NAME ',X'01',X'0A',AL4(VIDNAME)                             
         DC    X'FF',X'01',C'I'                                                 
IDNUMREC DC    C'ID NUM  ',X'01',X'05',AL4(VIDNUM)                              
         DC    X'FF',X'01',C'I'                                                 
JCLREC   DC    C'JCL ID  ',X'01',X'0A',AL4(VJCLID)                              
         DC    C'SUB NUM ',X'01',X'03',AL4(VBIN)                                
         DC    X'FF',X'01',C'J'                                                 
TERMLREC DC    C'LINE ID ',X'01',X'04',AL4(VLINEID)                             
         DC    C'TERM ADD',X'00',X'04',AL4(VCHAR)                               
         DC    C'PASSWORD',X'00',X'0A',AL4(VPASSWD)                             
         DC    X'FF',X'01',C'T'                                                 
TERMNREC DC    C'NUMBER  ',X'01',X'05',AL4(VTNUM)                               
         DC    X'FF',X'01',C'T'                                                 
TERMPREC DC    C'LINE ID ',X'01',X'04',AL4(VPLINEID)                            
         DC    C'TERM ADD',X'00',X'04',AL4(VCHAR)                               
         DC    C'PAGE    ',X'00',X'0A',AL4(VPASSWD)                             
         DC    X'FF',X'01',C'T'                                                 
UPREC    DC    C'SYSTEM  ',X'01',X'01',AL4(VSYSTEM)                             
         DC    C'PROGRAM ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PAGE #  ',X'00',X'03',AL4(VBIN)                                
         DC    C'AGENCY  ',X'00',X'02',AL4(VXCHAR)                              
         DC    C'MEDIA   ',X'00',X'01',AL4(VXCHAR)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VXCHAR)                              
         DC    X'FF',X'01',C'U'                                                 
PAREC    DC    C'AGY ALPH',X'01',X'02',AL4(VCHAR)                               
         DC    C'AUTH NUM',X'00',X'05',AL4(VAUTH)                               
         DC    X'FF',X'01',C'0'                                                 
SLREC    DC    C'SUB-REC ',X'01',X'01',AL4(VSUBREC)                             
         DC    C'LIST ID ',X'00',X'06',AL4(VCHAR)                               
         DC    C'SUB-NUM ',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'01',C'W'                                                 
SAREC    DC    C'AU-ID   ',X'01',X'05',AL4(VAUID)                               
         DC    X'FF',X'01',C'5'                                                 
SPREC    DC    C'ALPHA AG',X'01',X'02',AL4(VAAC)                                
         DC    C'SYSTEM #',X'00',X'03',AL4(VBIN)                                
         DC    C'PROGRAM#',X'00',X'03',AL4(VBIN)                                
         DC    C'PHASE # ',X'00',X'03',AL4(VBIN)                                
         DC    C'NAME    ',X'00',X'08',AL4(VCHAR)                               
         DC    X'FF',X'01',X'01'                                                
DTYPEREC DC    C'SYSTEM  ',X'01',X'02',AL4(VDSYS)                               
         DC    C'PROGRAM ',X'00',X'03',AL4(VCHAR)                               
         DC    C'DATATYPE',X'00',X'07',AL4(VCHAR)                               
         DC    X'FF',X'02',X'000D'                                              
SPANREC  DC    C'SYSTEM  ',X'01',X'02',AL4(VCHAR)                               
         DC    C'PROGRAM ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PNL NAME',X'00',X'08',AL4(VCHAR)                               
         DC    C'AGY CODE',X'00',X'02',AL4(VCHAR)                               
         DC    C'MEDIA   ',X'00',X'01',AL4(VCHAR)                               
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PNL TYPE',X'00',X'01',AL4(VCHAR)                               
         DC    C'ELEM KEY',X'00',X'08',AL4(VEKEY)                               
         DC    X'FF',X'02',X'000A'                                              
BMREC    DC    C'MESS TYP',X'01',X'01',AL4(VMESST)                              
         DC    C'MESS NUM',X'00',X'05',AL4(VMESSN)                              
         DC    X'FF',X'02',X'0009'                                              
ENTRYREC DC    C'DICT CDE',X'01',X'08',AL4(VDICT)                               
         DC    C'ENTR CDE',X'00',X'08',AL4(VCHAR)                               
         DC    X'FF',X'02',X'0003'                                              
BROADREC DC    C'MSG TYPE',X'01',X'01',AL4(VMSGT)                               
         DC    C'MSG #   ',X'00',X'05',AL4(VBIN)                                
         DC    X'FF',X'02',X'0009'                                              
         EJECT                                                                  
DCODE    EQU   CONP1H-CONP0H                                                    
DENTRY   EQU   CONI1H-CONI0H                                                    
LTAB     EQU   STA1-STA                                                         
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         PRINT ON                                                               
       ++INCLUDE GEKEYFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
       ++INCLUDE GEPFMSAVE         (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE GEKEYWORKD        (SYSTEM AREAS)                               
         PRINT ON                                                               
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
DISKA    DS    XL4                                                              
INTKEY   DS    CL60                FLAG FOR CONDITION RECOUND FOUND             
SAVEKEY  DS    CL(L'INTKEY)                                                     
PREVKEY  DS    CL(L'INTKEY)                                                     
SAVER3   DS    F                   SAVES CONTENT OF REGISTER 3                  
LENGTH   DS    X                   INPUT FIELD LENGTH                           
PADNUM   DS    X                   NUMBER OF PADDING CHARS.                     
CXFLAG   DS    X                   X'00' IF CHAR, X'FF' IF HEX                  
INSERT   DS    X                   NEXT INSERT POSITION                         
FLDLEN   DS    X                   OUTPUT FIELD LENGTH                          
WORK1    DS    CL13                                                             
WORK2    DS    CL13                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'155GEKEY0A   01/13/16'                                      
         END                                                                    
